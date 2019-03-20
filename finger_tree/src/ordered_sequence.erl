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

-module(ordered_sequence).

-export([new/0, partition/2, insert/2, delete_all/2, uncons_r/1, uncons_l/1,
         is_member/2]).

new() ->
    FT = finger_tree:new(fun measure/1, fun null/0, fun add/2),
    {FT, FT:empty()}.

partition(X, {FT, T}) ->
    {T1, T2} = FT:split(split_pred(fun erlang:'>='/2, X), T),
    {{FT, T1}, {FT, T2}}.

insert(X, {FT, T}) ->
    {L, R} = FT:split(split_pred(fun erlang:'>='/2, X), T),
    {FT, FT:join(L, FT:cons_r(X, R))}.

delete_all(X, {FT, T}) ->
    {L, R} = FT:split(split_pred(fun erlang:'>='/2, X), T),
    {_, R1} = FT:split(split_pred(fun erlang:'>'/2, X), R),
    {FT, FT:join(L, R1)}.

uncons_r({FT, T}) ->
    {Result, T1} = FT:uncons_r(T),
    {Result, {FT, T1}}.

uncons_l({FT, T}) ->
    {Result, T1} = FT:uncons_l(T),
    {Result, {FT, T1}}.

is_member(X, {FT, T}) ->
    {_L, R} = FT:split(split_pred(fun erlang:'>='/2, X), T),
    case FT:uncons_r(R) of
        {{value, X}, _} -> true;
        _               -> false
    end.

split_pred(Cmp, X) ->
    fun (no_key)   -> false;
        ({key, E}) -> Cmp(E, X)
    end.

%% Monoid
measure(X) -> {key, X}.
null()     -> no_key.
add(A, no_key)  -> A;
add(_, B)       -> B.
