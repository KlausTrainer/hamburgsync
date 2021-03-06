% Copyright 2010,  Filipe David Manana  <fdmanana@apache.org>
% Web site:  http://github.com/fdmanana/term_cache
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% A simple, configurable and generic Erlang term cache.
% Keys and values can be any Erlang term.
%
% This implementation uses ets tables to store keys and values (one table for
% keys and another one for values).

-module(term_cache_ets).
-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include("term_cache.hrl").


-spec start(options()) -> {ok, pid()}.
start(Options) ->
    start(start, Options).


-spec start_link(options()) -> {ok, pid()}.
start_link(Options) ->
    start(start_link, Options).


-spec stop(cache()) -> ok.
stop(Cache) ->
    catch gen_server:call(Cache, stop),
    ok.


% gen_server callbacks

init(Options) ->
    Size = parse_size(value(size, Options, ?DEFAULT_SIZE)),
    Policy = value(policy, Options, ?DEFAULT_POLICY),
    BackupDb = value(backup_db, Options, undefined),
    State = #state{
        cache_size = Size,
        free = Size,
        policy = Policy,
        ttl = value(ttl, Options, ?DEFAULT_TTL),
        items = ets:new(cache_by_items, [set, private]),
        atimes = ets:new(cache_by_atimes, [ordered_set, private]),
        take_fun = case Policy of
            lru ->
                fun ets:first/1;
            mru ->
                fun ets:last/1
            end,
        backup_db = BackupDb
    },
    maybe_restore_from_backup_db(State),
    {ok, State}.


handle_cast({put, Key, Item}, #state{cache_size = CacheSize} = State) ->
    case term_size(Item) of
    ItemSize when ItemSize > CacheSize ->
        {noreply, State};
    ItemSize ->
        #state{
            items = Items,
            atimes = ATimes,
            ttl = Ttl,
            free = Free
        } = free_until(purge_item(Key, State), ItemSize),
        Now = os:timestamp(),
        Timer = set_timer(Key, Ttl),
        true = ets:insert(ATimes, {Now, Key}),
        true = ets:insert(Items, {Key, {Item, ItemSize, Now, Timer}}),
        maybe_put_in_backup_db(State, Key, Item),
        {noreply, State#state{free = Free - ItemSize}, ?GC_TIMEOUT}
    end;

handle_cast({delete, Key}, State) ->
    maybe_delete_from_backup_db(State, Key),
    {noreply, purge_item(Key, State)}.


handle_call({get, Key}, _From, State) ->
    #state{
        items = Items, atimes = ATimes, ttl = Ttl, hits = Hits, misses = Misses
    } = State,
    case ets:lookup(Items, Key) of
    [{Key, {Item, ItemSize, ATime, Timer}}] ->
        cancel_timer(Key, Timer),
        NewATime = os:timestamp(),
        true = ets:delete(ATimes, ATime),
        true = ets:insert(ATimes, {NewATime, Key}),
        NewTimer = set_timer(Key, Ttl),
        true = ets:insert(Items, {Key, {Item, ItemSize, NewATime, NewTimer}}),
        {reply, {ok, Item}, State#state{hits = Hits + 1}, ?GC_TIMEOUT};
    [] ->
        {reply, not_found, State#state{misses = Misses + 1}, ?GC_TIMEOUT}
    end;

handle_call(get_info, _From, State) ->
    #state{
        atimes = ATimes,
        free = Free,
        cache_size = CacheSize,
        hits = Hits,
        misses = Misses
    } = State,
    Info = [
        {size, CacheSize}, {free, Free}, {items, ets:info(ATimes, size)},
        {hits, Hits}, {misses, Misses}
    ],
    {reply, {ok, Info}, State, ?GC_TIMEOUT};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.


handle_info({expired, Key}, State) ->
    #state{items = Items, atimes = ATimes, free = Free} = State,
    [{Key, {_Item, ItemSize, ATime, _Timer}}] = ets:lookup(Items, Key),
    true = ets:delete(Items, Key),
    true = ets:delete(ATimes, ATime),
    {noreply, State#state{free = Free + ItemSize}, ?GC_TIMEOUT};

handle_info({restore, Key, Item}, State) ->
    case ets:lookup(State#state.items, Key) of
    [] -> gen_server:cast(self(), {put, Key, Item});
    _ -> ok
    end,
    {noreply, State};

handle_info(timeout, State) ->
    true = erlang:garbage_collect(),
    {noreply, State}.


terminate(_Reason, #state{items = Items, atimes = ATimes}) ->
    true = ets:delete(Items),
    true = ets:delete(ATimes).


code_change(_OldVsn, State, Extra) ->
    {new_cache_size, NewCacheSize} = Extra,
    Used = State#state.cache_size - State#state.free,
    CacheSize = parse_size(NewCacheSize),
    NewState = State#state{cache_size = CacheSize, free = CacheSize - Used},
    error_logger:info_msg(
        "upgrading ~p ...~nExtra: ~p~nState: ~p~nNewState: ~p~n",
        [?MODULE, Extra, State, NewState]),
    {ok, NewState}.


%% helper functions

purge_item(Key, #state{atimes = ATimes, items = Items, free = Free} = State) ->
    case ets:lookup(Items, Key) of
    [{Key, {_Item, ItemSize, ATime, Timer}}] ->
        cancel_timer(Key, Timer),
        true = ets:delete(ATimes, ATime),
        true = ets:delete(Items, Key),
        State#state{free = Free + ItemSize};
    [] ->
        State
    end.


free_until(#state{free = Free} = State, MinFreeSize) when Free >= MinFreeSize ->
    State;
free_until(State, MinFreeSize) ->
    State2 = free_cache_entry(State),
    free_until(State2, MinFreeSize).


free_cache_entry(#state{take_fun = TakeFun} = State) ->
    #state{atimes = ATimes, items = Items, free = Free} = State,
    ATime = TakeFun(ATimes),
    [{ATime, Key}] = ets:lookup(ATimes, ATime),
    [{Key, {_Item, ItemSize, ATime, Timer}}] = ets:lookup(Items, Key),
    cancel_timer(Key, Timer),
    true = ets:delete(ATimes, ATime),
    true = ets:delete(Items, Key),
    State#state{free = Free + ItemSize}.


set_timer(_Key, 0) ->
    undefined;
set_timer(Key, Interval) when Interval > 0 ->
    erlang:send_after(Interval, self(), {expired, Key}).


cancel_timer(_Key, undefined) ->
    ok;
cancel_timer(Key, Timer) ->
    case erlang:cancel_timer(Timer) of
    false ->
        receive {expired, Key} -> ok after 0 -> ok end;
    _TimeLeft ->
        ok
    end.


-spec start(start | start_link, options()) -> {ok, pid()}.
start(StartFun, Options) ->
    case value(name, Options, undefined) of
    undefined ->
        gen_server:StartFun(?MODULE, Options, []);
    Name ->
        gen_server:StartFun({local, Name}, ?MODULE, Options, [])
    end.


maybe_restore_from_backup_db(State) ->
    case State#state.backup_db of
    undefined -> ok;
    BackupDb -> gen_server:cast(BackupDb, {restore, self()})
    end.


maybe_put_in_backup_db(State, Key, Item) ->
    case State#state.backup_db of
    undefined -> ok;
    BackupDb -> gen_server:cast(BackupDb, {put, Key, Item})
    end.


maybe_delete_from_backup_db(State, Key) ->
    case State#state.backup_db of
    undefined -> ok;
    BackupDb -> gen_server:cast(BackupDb, {delete, Key})
    end.


value(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
    {value, {Key, Value}} ->
        Value;
    false ->
        Default
    end.


term_size(Term) when is_binary(Term) ->
    byte_size(Term);
term_size(Term) ->
    byte_size(term_to_binary(Term)).


parse_size(Size) when is_integer(Size) ->
    Size;
parse_size(Size) when is_atom(Size) ->
    parse_size(atom_to_list(Size));
parse_size(Size) ->
    {match, [Value1, Suffix]} = re:run(
        Size,
        [$^, "\\s*", "(\\d+)", "\\s*", "(\\w*)", "\\s*", $$],
        [{capture, [1, 2], list}]),
    Value = list_to_integer(Value1),
    case string:to_lower(Suffix) of
    [] ->
        Value;
    "b" ->
        Value;
    "kb" ->
        Value * 1024;
    "mb" ->
        Value * 1024 * 1024;
    "gb" ->
        Value * 1024 * 1024 * 1024
    end.
