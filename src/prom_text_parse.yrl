%% promhouse
%%
%% Copyright 2022 The University of Queensland
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

Nonterminals
    exposition
    line
    lines
    value
    labels
    label
    timestamp.

Terminals
    help_comment
    type_comment
    comment
    atom
    eol
    '{'
    '}'
    '='
    ','
    float
    integer
    string
    '+Inf'
    '-Inf'.

Rootsymbol exposition.

exposition -> line lines : ['$1' | '$2'].
exposition -> line : ['$1'].
lines -> eol line lines : ['$2' | '$3'].
lines -> eol line : ['$2'].
lines -> eol : [].

line -> comment :
    {comment, _, Val} = '$1',
    #prom_comment{text = Val}.
line -> help_comment atom :
    {help_comment, _, Text} = '$1',
    #prom_help{name = v('$2'), text = Text}.
line -> type_comment atom atom :
    #prom_type{name = v('$2'), type = a('$3')}.
line -> atom value :
    #prom_value{name = v('$1'), value = '$2'}.
line -> atom value timestamp :
    #prom_value{name = v('$1'), value = '$2', timestamp = '$3'}.
line -> atom '{' '}' value :
    #prom_value{name = v('$1'), value = '$4'}.
line -> atom '{' '}' value timestamp :
    #prom_value{name = v('$1'), value = '$4', timestamp = '$5'}.
line -> atom '{' labels '}' value :
    #prom_value{name = v('$1'), labels = maps:from_list('$3'), value = '$5'}.
line -> atom '{' labels '}' value timestamp :
    #prom_value{name = v('$1'), labels = maps:from_list('$3'), value = '$5',
                timestamp = '$6'}.

labels -> label ',' labels : ['$1' | '$3'].
labels -> label : ['$1'].

label -> atom '=' string :
    {atom, _, K} = '$1',
    {string, _, V} = '$3',
    {K, V}.

value -> '+Inf' : '+Inf'.
value -> '-Inf' : '-Inf'.
value -> float : {float,_,N} = '$1', N.
value -> integer : {integer,_,N} = '$1', N.

timestamp -> integer : {integer,_,N} = '$1', N.

Erlang code.

-include_lib("promhouse/include/records.hrl").
-export([file/1]).

v({atom, _, A}) -> A.

a({atom, _, A}) -> list_to_atom(A).

file(Path) ->
    case prom_text_lex:file(Path) of
        {ok, Tokens, _} ->
            ?MODULE:parse(Tokens);
        Err ->
            Err
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_metric_test() ->
    R = ?MODULE:file("test/prom-simple.txt"),
    ?assertMatch({ok, _}, R),
    {ok, Recs} = R,
    ?assertMatch([
        #prom_help{name = "metric_without_timestamp_and_labels"},
        #prom_type{name = "metric_without_timestamp_and_labels", type = gauge},
        #prom_value{name = "metric_without_timestamp_and_labels",
                    labels = #{},
                    value = 12.47,
                    timestamp = undefined}
    ], Recs).

example_test() ->
    R = ?MODULE:file("test/prom-example.txt"),
    ?assertMatch({ok, _}, R),
    {ok, Recs} = R,
    ?assertMatch([
        #prom_help{name = "http_requests_total"},
        #prom_type{name = "http_requests_total", type = counter},
        #prom_value{name = "http_requests_total",
                    labels = #{"method" := "post",
                               "code" := "200"},
                    value = 1027,
                    timestamp = 1395066363000},
        #prom_value{name = "http_requests_total",
                    labels = #{"method" := "post",
                               "code" := "400"},
                    value = 3,
                    timestamp = 1395066363000},
        #prom_comment{},
        #prom_value{name = "msdos_file_access_time_seconds",
                    value = 1.458255915e9,
                    labels = #{"path" := "C:\\DIR\\FILE.TXT",
                               "error" := "Cannot find file:\n\"FILE.TXT\""}},
        #prom_comment{},
        #prom_value{name = "metric_without_timestamp_and_labels"},
        #prom_comment{},
        #prom_value{name = "something_weird",
                    value = '+Inf',
                    timestamp = -3982045},
        #prom_comment{},
        #prom_help{name = "http_request_duration_seconds"},
        #prom_type{name = "http_request_duration_seconds", type = histogram},
        #prom_value{name = "http_request_duration_seconds_bucket",
                    labels = #{"le" := _}},
        #prom_value{name = "http_request_duration_seconds_bucket"},
        #prom_value{name = "http_request_duration_seconds_bucket"},
        #prom_value{name = "http_request_duration_seconds_bucket"},
        #prom_value{name = "http_request_duration_seconds_bucket"},
        #prom_value{name = "http_request_duration_seconds_bucket"},
        #prom_value{name = "http_request_duration_seconds_sum"},
        #prom_value{name = "http_request_duration_seconds_count"},
        #prom_comment{},
        #prom_help{name = "telemetry_requests_metrics_latency_microseconds"},
        #prom_type{name = "telemetry_requests_metrics_latency_microseconds", type = summary},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds",
                    labels = #{"quantile" := _}},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds"},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds"},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds"},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds"},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds_sum",
                    value = 1.7560473e+07},
        #prom_value{name = "telemetry_requests_metrics_latency_microseconds_count"}
    ], Recs).

slingshot_test() ->
    R = ?MODULE:file("test/slingshot.txt"),
    ?assertMatch({ok, _}, R),
    {ok, Recs} = R,
    M0 = [X || X = #prom_value{name = "system_files_open"} <- Recs],
    ?assertMatch([#prom_value{value = 403}], M0),
    M1 = [X || X = #prom_value{name = "pool_pages_idle",
                               labels = #{"pool" := "aesni"}} <- Recs],
    ?assertMatch([#prom_value{value = 1}], M1).

-endif.
