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

Definitions.

WS = [\s\t]
NL = [\r\n]
D = [0-9]
NM = [a-zA-Z][a-zA-Z0-9_]*

Rules.

\+Inf :
    {token, {'+Inf', TokenLine}}.
-Inf :
    {token, {'-Inf', TokenLine}}.
(\+|\-)?{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
    {token, {float, TokenLine, list_to_float(TokenChars)}}.
(\+|\-)?{D}+ :
    {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
#{WS}+[^\r\n]* :
    S = strip_comment_start(TokenChars),
    case S of
        [$H,$E,$L,$P,X | Rest] when (X == 16#20) or (X == $\t) ->
            Rest2 = strip_comment_start(Rest),
            {FirstWord, Rest3} = lists:splitwith(fun is_alphanum/1, Rest2),
            Rest4 = strip_comment_start(Rest3),
            {token, {help_comment, TokenLine, Rest4}, FirstWord};
        [$T,$Y,$P,$E,X | Rest] when (X == 16#20) or (X == $\t) ->
            {token, {type_comment, TokenLine}, Rest};
        _ ->
            {token, {comment, TokenLine, S}}
    end.
#[^\s\t][^\r\n]* :
    S = lists:sublist(TokenChars, 2),
    {token, {comment, TokenLine, S}}.
"(\\.|[^"])*" :
    S = lists:sublist(TokenChars, 2, TokenLen - 2),
    {token, {string, TokenLine, string_gen(S)}}.
=  : {token, {'=', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
,  : {token, {',', TokenLine}}.
{NM} : {token, {atom, TokenLine, TokenChars}}.
{WS}+ : skip_token.
{NL}+ : {token, {eol, TokenLine}}.

Erlang code.

-export([file/1]).

file(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            StrData = unicode:characters_to_list(Data, utf8),
            string(StrData);
        Err ->
            Err
    end.

is_alphanum(X) when (X >= $a) and (X =< $z) -> true;
is_alphanum(X) when (X >= $A) and (X =< $Z) -> true;
is_alphanum(X) when (X >= $0) and (X =< $9) -> true;
is_alphanum($_) -> true;
is_alphanum(_) -> false.

strip_comment_start([$# | R]) -> strip_comment_start(R);
strip_comment_start([16#20 | R]) -> strip_comment_start(R);
strip_comment_start([$\t | R]) -> strip_comment_start(R);
strip_comment_start(R) -> R.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
        O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;        %\n = LF
escape_char($r) -> $\r;        %\r = CR
escape_char($t) -> $\t;        %\t = TAB
escape_char($v) -> $\v;        %\v = VT
escape_char($b) -> $\b;        %\b = BS
escape_char($f) -> $\f;        %\f = FF
escape_char($e) -> $\e;        %\e = ESC
escape_char($s) -> $\s;        %\s = SPC
escape_char($d) -> $\d;        %\d = DEL
escape_char(C) -> C.
