%%%-------------------------------------------------------------------
%% @doc promhouse public API
%% @end
%%%-------------------------------------------------------------------

-module(promhouse_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    promhouse_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
