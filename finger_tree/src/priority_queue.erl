%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Erlang Data Structures.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(priority_queue).

-export([new/0, in/3, out/1, from_list/1, to_list/1, is_empty/1, join/2]).

new() ->
    FT = finger_tree:new(fun measure/1, fun null/0, fun add/2),
    {FT, FT:empty()}.

in(A, Priority, {FT, T}) ->
    {FT, FT:cons_l({Priority, A}, T)}.

out({FT, T} = PQ) ->
    case is_empty(PQ) of
        true  -> {empty, PQ};
        false -> {split, L, {_Priority, A}, R} =
                     FT:split_tree(cmp(FT:measure(T)), null(), T),
                 {{value, A}, {FT, FT:join(L, R)}}
    end.

from_list(L) ->
    {FT, _T} = new(),
    {FT, FT:from_list(L)}.

to_list({FT, T}) ->
    FT:to_list(T).

is_empty({FT, T}) ->
    FT:is_empty(T).

join({FT, T1}, {FT, T2}) ->
    {FT, FT:join(T1, T2)}.

cmp(minus_infinity) -> fun (X) -> X end;
cmp({pri, Max}) -> fun (minus_infinity) -> false;
                       ({pri, X})       -> Max =< X
                   end.


%% Monoid
measure({Priority, _X}) -> {pri, Priority}.
null()     -> minus_infinity.
add(minus_infinity, B) -> B;
add(A, minus_infinity) -> A;
add({pri, A} = P, {pri, B}) when A >= B -> P;
add(_A, P) -> P.
