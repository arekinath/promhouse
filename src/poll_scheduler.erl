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
    runtime_window => [reltime()],
    et_err_prior => reltime(),
    et_err_integral => float(),
    state => dead | asleep | polling,
    }.

% schedule optimiser
% - simple gradient descent
% - cost function based on projected number of concurrent pollers using epoch + interval + runtime_window
% - go through all the epochs, adjusting a small delta in each direction
% - get a gradient estimate for each
% - each time the optimiser runs, take a fixed amount of adjustment (fraction of longest interval?)
% - scale it by the current max concurrency as fraction of total poller count
% - spread it amongst the epochs based on steepness of gradient

