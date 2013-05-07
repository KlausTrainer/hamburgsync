-define(DEFAULT_POLICY, lru).
-define(DEFAULT_SIZE, "128Kb").    % bytes
-define(DEFAULT_TTL, 0).           % 0 means no TTL
-define(GC_TIMEOUT, 5000).

-record(state, {
    cache_size,
    free,       % free space
    policy,
    ttl,        % milliseconds
    items,
    atimes,
    take_fun,
    hits = 0,
    misses = 0,
    backup_db
}).

-type cache() :: pid() | atom().
-type key() :: term().
-type item() :: term().

-type options() :: [option()].
-type option() :: {name, atom()} | {policy, policy()} | {size, size()}
                  | {ttl, timeout()} | {backup_db, pid() | atom()}.
-type size() :: integer() | string() | binary() | atom().
-type policy() :: lru | mru.

-type info() :: {size, integer()} | {free, integer()} | {items, integer()}
                | {hits, integer()} | {misses, integer()}.

-export_type([options/0]).
