%% promhouse
%%
%% Copyright 2023 The University of Queensland
%% Author: Alex Wilson <alex@uq.edu.au>
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
%% IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
%% NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%

-module(poll_scheduler).

-behaviour(ra_machine).

-export([
    start/0
    ]).

start() ->
    Config = application:get_env(promhouse, ra, []),
    Nodes = proplists:get_value(nodes, Config, [node() | nodes()]),
    Servers = [{?MODULE, N} || N <- Nodes],
    ra:start_or_restart_cluster(default,
        ?MODULE_STRING, {module, ?MODULE, #{}}, Servers).

-type time() :: integer().
%% Milliseconds since UNIX epoch (system_time())
-type reltime() :: integer().
%% Milliseconds

-type poller_state() :: #{
    epoch => time(),
    interval => reltime(),
    exp_runtime => reltime(),
    runtime_window => [reltime()],
    cost_gradient => {float(), float()},
    state => dead | asleep | polling
    }.

-type key() :: binary().
-type pollers() :: #{key() => poller_state()}.

% schedule optimiser
% - simple gradient descent
% - cost function based on projected number of concurrent pollers using epoch + interval + runtime_window
% - go through all the epochs, adjusting a small delta in each direction
% - get a gradient estimate for each
% - each time the optimiser runs, take a fixed amount of adjustment (fraction of longest interval?)
% - scale it by the current max concurrency as fraction of total poller count
% - spread it amongst the epochs based on steepness of gradient

mark_times_up_to(X, E, I, R, Max, Acc0) ->
    Acc1 = Acc0#{(E+I*X) => true, (E+I*X+R) => true},
    if
        ((E+(I+1)*X+R) >= Max) -> Acc1;
        true -> mark_times_up_to(X+1, E, I, R, Max, Acc1)
    end.

-spec times_of_interest([poller_state()]) -> [time()].
times_of_interest(Ps) ->
    Max = lists:max(
        [E+I*2+R || #{epoch := E, interval := I, exp_runtime := R} <- Ps]),
    TMap = lists:foldl(fun (P, Acc) ->
        #{epoch := E, interval := I, exp_runtime := R} = P,
        mark_times_up_to(0, E, I, R, Max, Acc)
    end, #{}, Ps),
    lists:sort(maps:keys(TMap)).

-spec count_pollers_running_at(time(), [poller_state()]) -> integer().
count_pollers_running_at(T, Ps) ->
    lists:foldl(fun (P, Acc) ->
        #{epoch := E, interval := I, exp_runtime := R} = P,
        if
            (T < E) -> Acc;
            (((T - E) rem I) =< R) -> Acc + 1;
            true -> Acc
        end
    end, 0, Ps).

-type cost() :: float().

-spec cost([poller_state()]) -> cost().
cost(Ps) ->
    [T0 | TRem] = times_of_interest(Ps),
    Max = lists:max(TRem),
    cost(Ps, T0, TRem) / float(Max).

-spec cost([poller_state()], time(), [time()]) -> float().
cost(_Ps, _T, []) -> 0;
cost(Ps, T0, [T1 | TRem]) ->
    TDelta = T1 - T0,
    Count = count_pollers_running_at(T0, Ps),
    math:pow(1.2, Count) * Count * TDelta + cost(Ps, T1, TRem).

-spec update_cost_gradients(pollers()) -> {key(), cost(), pollers()}.
update_cost_gradients(Ps) ->
    Cost0 = cost(maps:values(Ps)),
    update_cost_gradients(undefined, 0, 0, Cost0, Ps, maps:keys(Ps)).
update_cost_gradients(MaxKey, _MaxGrad, MaxCost, _Cost0, Ps, []) ->
    {MaxKey, MaxCost, Ps};
update_cost_gradients(MaxKey0, MaxGrad0, MaxCost0, Cost0, Ps, [Key | Rest]) ->
    #{Key := P0} = Ps,
    #{epoch := E0, interval := I} = P0,
    Drifts = lists:seq(1, I div 2),

    {GradUp, CostUp} = lists:foldl(fun (Delta, {Acc, _}) ->
        Px = P0#{epoch => E0 + Delta},
        Factor = math:pow(0.9, Delta - 1),
        CostX = cost(maps:values(Ps#{Key => Px})),
        {Acc + (CostX - Cost0) * Factor, CostX}
    end, {0, Cost0}, Drifts),
    {GradDown, CostDown} = lists:foldl(fun (Delta, {Acc, _}) ->
        Px = P0#{epoch => E0 - Delta},
        Factor = math:pow(0.9, Delta - 1),
        CostX = cost(maps:values(Ps#{Key => Px})),
        {Acc + (CostX - Cost0) * Factor, CostX}
    end, {0, Cost0}, Drifts),

    Grad = {GradUp, GradDown},
    Pout = P0#{cost_gradient => Grad},

    {MaxKey1, MaxGrad1, MaxCost1} = if
        (GradUp < 0) and (GradUp < MaxGrad0) ->
            {Key, GradUp, CostUp};
        true ->
            {MaxKey0, MaxGrad0, MaxCost0}
    end,
    {MaxKey2, MaxGrad2, MaxCost2} = if
        (GradDown < 0) and (GradDown < MaxGrad1) ->
            {Key, GradDown, CostDown};
        true ->
            {MaxKey1, MaxGrad1, MaxCost1}
    end,

    update_cost_gradients(MaxKey2, MaxGrad2, MaxCost2, Cost0, Ps#{Key => Pout}, Rest).

-spec adjust_max_epoch(integer(), float(), pollers()) -> pollers().
adjust_max_epoch(TargetConcur, LimitFrac, Ps0) ->
    Limit = LimitFrac * lists:max(times_of_interest(maps:values(Ps0))),
    TargetCost = math:pow(1.2, TargetConcur) * TargetConcur,
    {MaxKey, MaxCost, Ps1} = update_cost_gradients(Ps0),
    case MaxKey of
        undefined -> Ps1;
        _ ->
            #{MaxKey := P0} = Ps1,
            #{epoch := E0, cost_gradient := {GradUp, GradDown}} = P0,
            {ED, Grad} = if
                (GradUp < GradDown) -> {1, abs(GradUp)};
                true -> {-1, abs(GradDown)}
            end,
            Adj0 = if
                (MaxCost > TargetCost) -> ceil(Grad / (MaxCost - TargetCost));
                true -> 0
            end,
            Adj1 = if
                (Adj0 > Limit) -> round(Limit);
                true -> Adj0
            end,
            E1 = E0 + ED * Adj1,
            P1 = P0#{epoch => E1},
            Ps1#{MaxKey => P1}
    end.

to_wavedrom(Ps) ->
    MaxT = lists:max(times_of_interest(maps:values(Ps))),
    Intervals = lists:seq(1, MaxT),
    Signals = maps:fold(fun (Key, P, L0) ->
        #{epoch := E, interval := I, exp_runtime := R} = P,
        {_, WaveRev} = lists:foldl(fun (T, {Last, Acc}) ->
            V0 = if
                (T < E) -> $0;
                (((T - E) rem I) =< R) -> $x;
                true -> $0
            end,
            V1 = case V0 of
                Last -> $.;
                _ -> V0
            end,
            {V0, [V1 | Acc]}
        end, {$:, []}, Intervals),
        Wave = list_to_binary(lists:reverse(WaveRev)),
        [#{<<"name">> => Key, <<"wave">> => Wave} | L0]
    end, [], Ps),
    #{<<"signal">> => Signals}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

toi_test() ->
    Ps0 = [
        #{epoch => 0, interval => 30, exp_runtime => 10},
        #{epoch => 2, interval => 30, exp_runtime => 10}
    ],
    ?assertMatch([0, 2, 10, 12, 30, 32, 40, 42, 60, 62, 70, 72], times_of_interest(Ps0)),
    Ps1 = [
        #{epoch => 0, interval => 10, exp_runtime => 5},
        #{epoch => 0, interval => 5, exp_runtime => 1}
    ],
    ?assertMatch([0, 1, 5, 6, 10, 11, 15, 16, 20, 21, 25], times_of_interest(Ps1)).

pra_test() ->
    Ps0 = [
        #{epoch => 0, interval => 30, exp_runtime => 10},
        #{epoch => 2, interval => 30, exp_runtime => 10}
    ],
    ?assertMatch(1, count_pollers_running_at(1, Ps0)),
    ?assertMatch(1, count_pollers_running_at(30, Ps0)),
    ?assertMatch(2, count_pollers_running_at(32, Ps0)),
    ?assertMatch(2, count_pollers_running_at(33, Ps0)),
    ?assertMatch(1, count_pollers_running_at(41, Ps0)),
    ?assertMatch(0, count_pollers_running_at(43, Ps0)),
    Ps1 = [
        #{epoch => 0, interval => 10, exp_runtime => 5},
        #{epoch => 0, interval => 5, exp_runtime => 1}
    ],
    ?assertMatch(2, count_pollers_running_at(5, Ps1)),
    ?assertMatch(1, count_pollers_running_at(6, Ps1)),
    ?assertMatch(0, count_pollers_running_at(7, Ps1)),
    ?assertMatch(2, count_pollers_running_at(10, Ps1)),
    ?assertMatch(2, count_pollers_running_at(11, Ps1)),
    ?assertMatch(1, count_pollers_running_at(12, Ps1)),
    ?assertMatch(2, count_pollers_running_at(15, Ps1)),
    ?assertMatch(1, count_pollers_running_at(16, Ps1)),
    ?assertMatch(0, count_pollers_running_at(17, Ps1)).

cost_test() ->
    Ps0 = [
        #{epoch => 0, interval => 10, exp_runtime => 5},
        #{epoch => 0, interval => 5, exp_runtime => 1}
    ],
    Cost0 = cost(Ps0),
    Ps1 = [
        #{epoch => 0, interval => 10, exp_runtime => 5},
        #{epoch => 3, interval => 5, exp_runtime => 1}
    ],
    Cost1 = cost(Ps1),
    ?assertMatch(X when X > 0, Cost0),
    ?assertMatch(X when X > 0, Cost1),
    ?assertMatch(X when X < Cost0, Cost1).

grad_test() ->
    Ps0 = #{
        a => #{epoch => 0, interval => 10, exp_runtime => 5},
        b => #{epoch => 0, interval => 5, exp_runtime => 1}
    },
    {MaxKey, MaxCost, Ps1} = update_cost_gradients(Ps0),
    ?assertMatch(#{
        a := #{epoch := 0, interval := 10, exp_runtime := 5, cost_gradient := {A, B}},
        b := #{epoch := 0, interval := 5, exp_runtime := 1, cost_gradient := {C, D}}
    } when (A < C) and (A < B) and (A < D) and (A < C), Ps1),
    ?assertMatch(a, MaxKey),
    ?assertMatch(X when (X > 0), MaxCost).

adjust_test() ->
    Ps0 = #{
        a => #{epoch => 0, interval => 15, exp_runtime => 5},
        b => #{epoch => 0, interval => 15, exp_runtime => 2},
        c => #{epoch => 0, interval => 5, exp_runtime => 2},
        d => #{epoch => 0, interval => 15, exp_runtime => 4}
    },

    Ps1 = adjust_max_epoch(1, 0.5, Ps0),
    io:format("~s\n", [jsx:encode(to_wavedrom(Ps1))]),
    ?assertMatch(#{
        a := #{epoch := 0},
        b := #{epoch := -2},
        c := #{epoch := 0},
        d := #{epoch := 0}
    }, Ps1),

    Ps2 = adjust_max_epoch(1, 0.5, Ps1),
    io:format("~s\n", [jsx:encode(to_wavedrom(Ps2))]),
    ?assertMatch(#{
        a := #{epoch := 2},
        b := #{epoch := -2},
        c := #{epoch := 0},
        d := #{epoch := 0}
    }, Ps2),

    Ps3 = adjust_max_epoch(1, 0.5, Ps2),
    io:format("~s\n", [jsx:encode(to_wavedrom(Ps3))]),
    ?assertMatch(#{
        a := #{epoch := 4},
        b := #{epoch := -2},
        c := #{epoch := 0},
        d := #{epoch := 0}
    }, Ps3),

    Ps4 = adjust_max_epoch(1, 0.5, Ps3),
    io:format("~s\n", [jsx:encode(to_wavedrom(Ps4))]),
    ?assertMatch(#{
        a := #{epoch := 4},
        b := #{epoch := -2},
        c := #{epoch := 0},
        d := #{epoch := 1}
    }, Ps4),

    Ps5 = adjust_max_epoch(1, 0.5, Ps4),
    io:format("~s\n", [jsx:encode(to_wavedrom(Ps5))]),
    ?assertMatch(#{
        a := #{epoch := 4},
        b := #{epoch := -2},
        c := #{epoch := 1},
        d := #{epoch := 1}
    }, Ps5).


converge(Target, Limit, Ps0) ->
    Ps1 = adjust_max_epoch(Target, Limit, Ps0),
    case Ps1 of
        Ps0 -> Ps0;
        _ -> converge(Target, Limit, Ps1)
    end.

adjust_perf_test_() ->
    {timeout, 60, fun() ->
    Ps0 = lists:foldl(fun (I, Acc) ->
        Acc#{I => #{epoch => 0, interval => 20, exp_runtime => 2}}
    end, #{}, lists:seq(1, 20)),
    Ps1 = converge(2, 0.5, Ps0),
    io:format("~s\n", [jsx:encode(to_wavedrom(Ps1))]),
    ?assertMatch(#{}, Ps1)
    end}.

-endif.
