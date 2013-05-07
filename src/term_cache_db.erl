-module(term_cache_db).
-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).


-spec start(string()) -> {ok, pid()} | {error, term()}.
start(DbPath) ->
    gen_server:start({local, ?MODULE}, ?MODULE,
                          [DbPath], []).


-spec start_link(string()) -> {ok, pid()} | {error, term()}.
start_link(DbPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [DbPath], []).


-spec stop() -> ok.
stop() ->
    catch gen_server:call(?MODULE, stop),
    ok.


%% gen_server callbacks

init([DbPath]) ->
    {ok, bitcask:open(DbPath, [read_write, sync_on_put])}.


handle_call(stop, _From, State) ->
    bitcask:close(State),
    {stop, normal, ok, []}.


handle_cast({put, K, V}, State) ->
    bitcask:put(State, term_to_binary(K), term_to_binary(V)),
    {noreply, State};

handle_cast({delete, K}, State) ->
    bitcask:delete(State, term_to_binary(K)),
    {noreply, State};

handle_cast({restore, TermCache}, State) ->
    Fun = fun(K, V, Pid) ->
              Pid ! {restore, binary_to_term(K), binary_to_term(V)},
              Pid
          end,
    bitcask:fold(State, Fun, TermCache),
    {noreply, State}.


handle_info(timeout, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    bitcask:close(State).
