%%===========================================================================
%%     FileName: aque_db.erl
%%         Desc: AqueDB Interface
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-05 13:10:23
%%      History: Inspired by boss_db, https://github.com/evanmiller/boss_db
%%===========================================================================

-module(aque_db).

-export([start/0, start/1, start/2, stop/0]).

-export([
        insert/3,
        lookup/2,
        delete/2,
        count/1,
        counter/1, 
        update_counter/2,
        incr/1,
        all_keys/1,
        tab2list/1
    ]).

-export([
        insert/4,
        lookup/3,
        delete/3,
        count/2,
        counter/2, 
        update_counter/3,
        incr/2, 
        all_keys/2,
        tab2list/2
    ]).

-export([
        get_pool_status/0,
        get_pool_status/1
    ]).

-define(DEFAULT_TIMEOUT, (10 * 1000)).
-define(DEFAULT_POOLNAME, mnesia_aque_pool).

start() ->
    application:start(aque_db).

start(_Type, Options) ->
    start(Options).

-spec start(list()) -> {ok, pid()}.
start(Options) ->
    {ok, Sup} = aque_db_sup:start_link(),
    Tables = get_env(aque_tab),
    lists:foreach(fun(Adapter) ->
                {Register, AdapterName} = proplists:get_value(adapter, Adapter, {local, mnesia}),
                AdapterMod = list_to_atom(lists:concat(["aque_db_adapter_", AdapterName])),
                PoolName = list_to_atom(lists:concat([AdapterName, "_aque_pool"])),
                
                AdapterMod:start(Adapter),

                Args = [
                    {name, {Register, PoolName}},
                    {worker_module, aque_db_controller},
                    {size, 5}, {max_overflow, 10}|Adapter],
                supervisor:start_child(Sup, 
                    {PoolName, {poolboy, start_link, [Args]}, 
                        permanent, 2000, worker, [poolboy]}),

                io:format("Pool ~w start...ok!~n", [PoolName]),
                init_tab(PoolName, Tables)
        end, Options),
    {ok, Sup}.

stop() ->
    ok.

init_tab(PoolName, Tables) ->
    do_call(PoolName, {init_tab, Tables}).

-spec insert( atom(), any(), any() ) -> true | {error, _}.
insert(Tab, Key, Value) ->
    do_call(?DEFAULT_POOLNAME, {insert, Tab, Key, Value}).

-spec lookup( atom(), any() ) -> list() | {error, _}.
lookup(Tab, Key) ->
    do_call(?DEFAULT_POOLNAME, {lookup, Tab, Key}).

-spec delete( atom(), any() ) -> true | {error, _}.
delete(Tab, Key) ->
    do_call(?DEFAULT_POOLNAME, {delete, Tab, Key}).

-spec insert( atom() | tuple(), atom(), any(), any() ) -> true | {error, _}.
insert(Db, Tab, Key, Value) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {insert, Tab, Key, Value}).

-spec lookup( atom() | tuple(), atom(), any() ) -> list() | {error, _}.
lookup(Db, Tab, Key) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {lookup, Tab, Key}).

-spec delete( atom() | tuple(), atom(), any() ) -> true | {error, _}.
delete(Db, Tab, Key) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {delete, Tab, Key}).

-spec count( atom() ) -> integer().
count(Tab) ->
    do_call(?DEFAULT_POOLNAME, {count, Tab}).

-spec count( atom() | tuple(), atom() ) -> integer().
count(Db, Tab) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {count, Tab}).

-spec counter( atom() ) -> integer().
counter(Key) ->
    do_call(?DEFAULT_POOLNAME, {counter, Key}).

-spec counter( atom() | tuple(), atom() ) -> integer().
counter(Db, Key) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {counter, Key}).

-spec incr( atom() ) -> integer().
incr(Key) ->
    update_counter(Key, 1).

-spec incr( atom() | tuple(), atom()) -> integer().
incr(Db, Key) ->
    update_counter(Db, Key, 1).

-spec update_counter( atom(), integer() ) -> integer().
update_counter(Key, Num) ->
    do_call(?DEFAULT_POOLNAME, {update_counter, Key, Num}).

-spec update_counter( atom() | tuple(), atom(), integer() ) -> integer().
update_counter(Db, Key, Num) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {update_counter, Key, Num}).

-spec all_keys( atom() ) -> list().
all_keys(Tab) ->
    do_call(?DEFAULT_POOLNAME, {all_keys, Tab}).

-spec all_keys( atom() | tuple(), atom() ) -> list().
all_keys(Db, Tab) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {all_keys, Tab}).


-spec tab2list( atom() ) -> list().
tab2list(Tab) ->
    do_call(?DEFAULT_POOLNAME, {tab2list, Tab}).

-spec tab2list( atom() | tuple(), atom() ) -> list().
tab2list(Db, Tab) ->
    PoolName = make_pool_name(Db),
    do_call(PoolName, {tab2list, Tab}).

do_call(PoolName, Msg) ->
    Worker = poolboy:checkout(PoolName),
    Reply = gen_server:call(Worker, Msg, ?DEFAULT_TIMEOUT),
    poolboy:checkin(PoolName, Worker),
    Reply.

make_pool_name({Db, Database}) ->
    list_to_atom(lists:concat([Db, "_", Database, "_aque_pool"]));
make_pool_name(Db) ->
    list_to_atom(lists:concat([Db, "_aque_pool"])).

get_env(Key) ->
    case application:get_env(aque_db, Key) of
        undefined ->
            [];
        {ok, List} ->
            List
    end.

get_pool_status() ->
    get_pool_status(?DEFAULT_POOLNAME).

-spec get_pool_status( atom() ) -> ok.
get_pool_status(PoolName) ->
    {Workers, WorkerSup, Waiting, Monitors, Size, OverFlow, MaxOverFlow} = gen_fsm:sync_send_all_state_event(PoolName, status),
    io:format("Workers: ~p~nWorkerSup: ~p~nWaiting: ~p~nMonitors: ~p~nSize: ~p~nOverFlow: ~p~nMaxOverFlow: ~p~n",
        [Workers, WorkerSup, Waiting, Monitors, Size, OverFlow, MaxOverFlow]).

