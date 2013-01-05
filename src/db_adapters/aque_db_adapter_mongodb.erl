%%===========================================================================
%%     FileName: aque_db_adapter_mongodb.erl
%%         Desc: mongodb
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-05 13:26:48
%%      History:
%%===========================================================================
-module(aque_db_adapter_mongodb).

-behaviour(aque_db_adapter).

-export([start/1, stop/0, init/1, terminate/1]).

-export([
        init_tab/2,
        insert/4, 
        lookup/3, 
        delete/3, 
        count/2, 
        counter/2, 
        update_counter/3, 
        all_keys/2,
        tab2list/2
    ]).

-define(LOG(Name, Value), io:format("DEBUG: ~s: ~p~n", [Name, Value])).


start(_Options) ->
    application:start(mongodb).

stop() ->
    application:stop(mongodb),
    ok.

init(Options) ->
    Host = proplists:get_value(db_host, Options, "localhost"),
    Port = proplists:get_value(db_port, Options, 27017),
    Database = proplists:get_value(db_database, Options, test),
    WriteMode = proplists:get_value(db_write_mode, Options, safe),
    ReadMode = proplists:get_value(db_read_mode, Options, master),
    {ok, Connection} = mongo:connect({Host, Port}),
    % We pass around arguments required by mongo:do/5
    {ok, {WriteMode, ReadMode, Connection, Database}}.

terminate({_, _, Connection, _}) ->
    mongo:disconnect(Connection).

execute({WriteMode, ReadMode, Connection, Database}, Fun) ->
    mongo:do(WriteMode, ReadMode, Connection, Database, Fun).

%% --------------------------------------------------------

init_tab(_Conn, _Tables) ->
    ok.

insert(Conn, Collection, Key, Bin) ->
    Doc = {'_id', Key, bin, Bin},
    Res = execute(Conn, fun() ->
                mongo:repsert(Collection, {'_id', Key}, Doc)
        end),
    case Res of
        {ok, _} -> true;
        Error -> erlang:throw(Error)
    end.

lookup(Conn, Collection, Key) ->
    Res = execute(Conn, fun() ->
                mongo:find_one(Collection, {'_id', Key})
        end),
    case Res of
        {ok, {}} -> [];
        {ok, {{_, _, bin, Bin}}} -> Bin; 
        Error -> erlang:throw(Error)
    end.

delete(Conn, Collection, Key) ->
    Res = execute(Conn, fun() ->
                mongo:delete(Collection, {'_id', Key})
        end),
    case Res of
        {ok, ok} -> true;
        Error -> erlang:throw(Error)
    end.


count(Conn, Collection) ->
    {ok, Count} = execute(Conn, fun() -> 
                mongo:count(Collection, {})
        end),
    Count.

counter(Conn, Key) ->
    Res = execute(Conn, fun() ->
                mongo:find_one(counters, {'_id', list_to_binary(atom_to_list(Key))})
        end),
    case Res of
        {ok, {}} -> [];
        {ok, {{_, _, value, Num}}} -> Num; 
        Error -> erlang:throw(Error)
    end.

update_counter(Conn, Key, Num) ->
    Res = execute(Conn, fun() -> 
                 mongo:repsert(counters, 
                         {'key', list_to_binary(Key)},
                         {'$inc', {value, Num}}
                         )
        end),
    case Res of
        {ok, ok} -> counter(Conn, Key);
        {failure, Reason} -> {error, Reason};
        {connection_failure, Reason} -> {error, Reason}
    end.

all_keys(Conn, Collection) ->
    Res = execute(Conn, fun() ->
                mongo:find(Collection, {})
        end),
    case Res of
        {ok, Pid} -> [Key||{_, Key, _, _}<-mongo:rest(Pid)];
        Error -> erlang:throw(Error)
    end.

tab2list(Conn, counters) ->
    Res = execute(Conn, fun() ->
                mongo:find(counters, {})
        end),
    case Res of
        {ok, Pid} -> [{Key, Num}||{_, Key, _, Num}<-mongo:rest(Pid)];
        Error -> erlang:throw(Error)
    end;

tab2list(Conn, Collection) ->
    Res = execute(Conn, fun() ->
                mongo:find(Collection, {})
        end),
    case Res of
        {ok, Pid} -> [Bin||{_, _, _, Bin}<-mongo:rest(Pid)];
        Error -> erlang:throw(Error)
    end.
