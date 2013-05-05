%% @doc Callbacks for the term_ache application.
-module(term_cache_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).


%% @doc application start callback for term_cache.
-spec start(_Type, _StartArgs) -> {ok, pid()}.
start(_Type, _StartArgs) ->
    {ok, _Pid} = term_cache_sup:start_link().

%% @doc application stop callback for term_cache.
-spec stop(_State) -> ok.
stop(_State) ->
    ok.
