%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Consistent Hash Exchange.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(ft_queue).

-export([new/0, in/2, in_r/2, out/1, out_r/1, len/1, from_list/1, to_list/1,
         is_empty/1, split_at/2, join/2]).

new() ->
    FT = finger_tree:new(fun measure/1, fun null/0, fun add/2),
    {FT, FT:empty()}.

in(A, {FT, T}) ->
    {FT, FT:cons_l(A, T)}.

in_r(A, {FT, T}) ->
    {FT, FT:cons_r(A, T)}.

out({FT, T}) ->
    {Result, T1} = FT:uncons_r(T),
    {Result, {FT, T1}}.

out_r({FT, T}) ->
    {Result, T1} = FT:uncons_l(T),
    {Result, {FT, T1}}.

len({FT, T}) ->
    FT:measure(T).

from_list(L) ->
    {FT, _T} = new(),
    {FT, FT:from_list(L)}.

to_list({FT, T}) ->
    FT:to_list(T).

is_empty({FT, T}) ->
    FT:is_empty(T).

split_at(N, {FT, T}) ->
    {Left, Right} = FT:split(fun (E) -> N < E end, T),
    {{FT, Left}, {FT, Right}}.

join({FT, T1}, {FT, T2}) ->
    {FT, FT:join(T1, T2)}.

%% Monoid
measure(_) -> 1.
null()     -> 0.
add(A, B)  -> A+B.
