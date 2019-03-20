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

-module(ordered_sequence_map).

-export([new/0, partition/2, insert/3, delete_all/2, uncons_r/1, uncons_l/1,
         has_key/2]).

new() ->
    FT = finger_tree:new(fun measure/1, fun null/0, fun add/2),
    {FT, FT:empty()}.

partition(Key, {FT, T}) ->
    {T1, T2} = FT:split(split_pred(fun erlang:'>='/2, Key), T),
    {{FT, T1}, {FT, T2}}.

insert(Key, Value, {FT, T}) ->
    {L, R} = FT:split(split_pred(fun erlang:'>='/2, Key), T),
    {FT, FT:join(L, FT:cons_r({Key, Value}, R))}.

delete_all(Key, {FT, T}) ->
    {L, R} = FT:split(split_pred(fun erlang:'>='/2, Key), T),
    {_, R1} = FT:split(split_pred(fun erlang:'>'/2, Key), R),
    {FT, FT:join(L, R1)}.

uncons_r({FT, T}) ->
    {Result, T1} = FT:uncons_r(T),
    {Result, {FT, T1}}.

uncons_l({FT, T}) ->
    {Result, T1} = FT:uncons_l(T),
    {Result, {FT, T1}}.

has_key(Key, {FT, T}) ->
    {_L, R} = FT:split(split_pred(fun erlang:'>='/2, Key), T),
    case FT:uncons_r(R) of
        {{value, {Key, _Value}}, _} -> true;
        _                           -> false
    end.

split_pred(Cmp, Key) ->
    fun (no_key)   -> false;
        ({key, E}) -> Cmp(E, Key)
    end.

%% Monoid
measure({Key, _Value}) -> {key, Key}.
null()                 -> no_key.
add(A, no_key)         -> A;
add(_, B)              -> B.
