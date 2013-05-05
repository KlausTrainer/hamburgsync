%% @doc term_cache.
-module(term_cache).

%% API
-export([start/0, stop/0]).
-export([get/2, get/3, put/3, delete/2, get_info/1]).

-include("term_cache.hrl").


%% @doc Start the term_cache server.
-spec start() -> ok.
start() ->
    application:start(sasl),
    application:start(term_cache).

%% @doc Stop the term_cache server.
-spec stop() -> ok.
stop() ->
    application:stop(term_cache).


-spec get(cache(), key()) -> {ok, item()} | not_found.
get(Cache, Key) ->
    gen_server:call(Cache, {get, Key}, infinity).

-spec get(cache(), key(), timeout()) -> {ok, item()} | not_found | timeout.
get(Cache, Key, Timeout) ->
    try
        gen_server:call(Cache, {get, Key}, Timeout)
    catch
    exit:{timeout, {gen_server, call, [Cache, {get, Key}, Timeout]}} ->
        timeout
    end.


-spec put(cache(), key(), item()) -> ok.
put(Cache, Key, Item) ->
    ok = gen_server:cast(Cache, {put, Key, Item}).


-spec delete(cache(), key()) -> ok.
delete(Cache, Key) ->
    ok = gen_server:cast(Cache, {delete, Key}).


-spec get_info(cache()) -> {ok, [info()]}.
get_info(Cache) ->
    gen_server:call(Cache, get_info).
