%% @doc Supervisor for the term_cache application.
-module(term_cache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% supervisor callback
-export([init/1]).


%% API

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link([{name, term_cache}]).

-spec start_link(term_cache:options()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).


%% supervisor callback

init(Options) ->
    TermCache = {term_cache_ets, {term_cache_ets, start_link, Options},
                 permanent, 2000, worker, [term_cache_ets]},
    Children = [TermCache],
    RestartStrategy = {one_for_one, 5, 4},
    {ok, {RestartStrategy, Children}}.
