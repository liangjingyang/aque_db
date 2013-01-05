%%===========================================================================
%%     FileName: aque_db_adapter_mysql.erl
%%         Desc: mysql
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-05 13:29:03
%%      History:
%%===========================================================================
-module(aque_db_adapter_mysql).

-behaviour(aque_db_adapter).

-export([init/1, terminate/1, start/1, stop/0]).

-export([
        insert/4, 
        lookup/3, 
        delete/3,
        count/2, 
        counter/2, 
        update_counter/3, 
        all_keys/2,
        tab2list/2
    ]).

start(_) ->
    ok.

stop() ->
    ok.

init(Options) ->
    DBHost = proplists:get_value(db_host, Options, "localhost"),
    DBPort = proplists:get_value(db_port, Options, 3306),
    DBUsername = proplists:get_value(db_username, Options, "guest"),
    DBPassword = proplists:get_value(db_password, Options, ""),
    DBDatabase = proplists:get_value(db_database, Options, "test"),
    DBIdentifier = proplists:get_value(db_shard_id, Options, mysql_aque_pool),
    Encoding = utf8,
    mysql_conn:start_link(DBHost, DBPort, DBUsername, DBPassword, DBDatabase, 
        fun(_, _, _, _) -> ok end, Encoding, DBIdentifier).

terminate(Pid) -> 
    exit(Pid, normal).

insert(Pid, Tab, Key, Bin) ->
    Query = [
        "REPLACE INTO " ++
        atom_to_list(Tab) ++
        " (`key`, `bin`) values (" ++ 
        pack_value(Key) ++ 
        "," ++
        pack_value(Bin) ++
        ")"
    ],
    Res = fetch(Pid, Query),
    %io:format("queue=~p, Res=~w~n", [Query, Res]),
    case Res of
        {updated, _} ->
            true;
        {error, MysqlRes} -> {error, mysql:get_result_reason(MysqlRes)}
    end.

lookup(Pid, Tab, Key) ->
    Res = fetch(Pid, [
            "SELECT * FROM " ++ 
            atom_to_list(Tab) ++ 
            " WHERE `key` = " ++ 
            pack_value(Key)
        ]),
    case Res of
        {data, MysqlRes} ->
            case mysql:get_result_rows(MysqlRes) of
                [] -> [];
                [Row] ->
                    Columns = mysql:get_result_field_info(MysqlRes),
                    Index = keyindex(list_to_binary("bin"), 2, Columns),
                    case lists:nth(Index, Row) of
                        undefined -> [];
                        Val -> 
                            Val 
                    end
            end;
        {error, MysqlRes} ->
            {error, mysql:get_result_reason(MysqlRes)}
    end.

delete(Pid, Tab, Key) ->
    Res = fetch(Pid, [
            "DELETE FROM " ++ 
            atom_to_list(Tab) ++ 
            " WHERE `key` = " ++ 
            pack_value(Key)
        ]),
    case Res of
        {updated, _} ->
            true;
        {error, MysqlRes} -> erlang:throw({error, mysql:get_result_reason(MysqlRes)})
    end.

all_keys(Pid, Tab) ->
    Res = fetch(Pid, [
            "SELECT `key` FROM " ++ 
            atom_to_list(Tab) 
        ]),
    case Res of
        {data, MysqlRes} ->
            case mysql:get_result_rows(MysqlRes) of
                [] -> [];
                List ->
                    case Tab of
                        counters ->
                            [binary_to_atom(B, utf8)||[B]<-List];
                        _ ->
                            [list_to_integer(binary_to_list(B))||[B]<-List]
                    end
            end;
        {error, MysqlRes} ->
            {error, mysql:get_result_reason(MysqlRes)}
    end.

tab2list(Pid, counters) ->
    Res = fetch(Pid, [
            "SELECT `key`, `value` FROM counters"  
        ]),
    case Res of
        {data, MysqlRes} ->
            case mysql:get_result_rows(MysqlRes) of
                [] -> [];
                List ->
                    [{binary_to_list(K), V}||[K, V]<-List]
            end;
        {error, MysqlRes} ->
            {error, mysql:get_result_reason(MysqlRes)}
    end;

tab2list(Pid, Tab) ->
    Res = fetch(Pid, [
            "SELECT `bin` FROM " ++ 
            atom_to_list(Tab) 
        ]),
    case Res of
        {data, MysqlRes} ->
            case mysql:get_result_rows(MysqlRes) of
                [] -> [];
                List ->
                    [R||[R] <- List]
            end;
        {error, MysqlRes} ->
            {error, mysql:get_result_reason(MysqlRes)}
    end.

count(Pid, Tab) ->
    Res = fetch(Pid, ["SELECT COUNT(*) AS count FROM " ++ atom_to_list(Tab)]),
    case Res of
        {data, MysqlRes} ->
            [[Count]] = mysql:get_result_rows(MysqlRes),
            Count;
        {error, MysqlRes} ->
            {error, mysql:get_result_reason(MysqlRes)}
    end.

counter(Pid, Key) ->
    Res = fetch(Pid, ["SELECT `value` FROM counters WHERE `key` = " ++ pack_value(Key)]),
    case Res of
        {data, MysqlRes} ->
            [[Value]] = mysql:get_result_rows(MysqlRes),
            Value;
        {error, _Reason} -> 0
    end.

update_counter(Pid, Key, Num) ->
    Query = ["UPDATE counters SET value = value + " ++ pack_value(Num) ++ 
            " WHERE `key` = " ++ pack_value(Key)],
    Res = fetch(Pid, Query),
    %io:format("packnum=~p, packkey=~p, S=~p, Res=~w~n", [pack_value(Num), pack_value(Key), Query, Res]),
    case Res of
        {updated, {_, _, _, 0, _, _}} ->
            Res1 = fetch(Pid, ["INSERT INTO counters (`key`, `value`) VALUES (",
                    pack_value(Key), ", ", pack_value(Num), ")"]),
            case Res1 of
                {updated, _} -> counter(Pid, Key); 
                {error, MysqlRes} -> {error, mysql:get_result_reason(MysqlRes)}
            end;
        {updated, _} ->
            counter(Pid, Key);
        {error, Reason} -> 
            {error, mysql:get_result_reason(Reason)}
    end.

%% ------------------------------------------------
keyindex(Key, N, TupleList) ->
    keyindex(Key, N, TupleList, 1).

keyindex(_Key, _N, [], _Index) ->
    undefined;
keyindex(Key, N, [Tuple|Rest], Index) ->
    case element(N, Tuple) of
        Key -> Index;
        _ -> keyindex(Key, N, Rest, Index + 1)
    end.

escape_sql(Value) ->
    escape_sql1(Value, []).

escape_sql1([], Acc) ->
    lists:reverse(Acc);
escape_sql1([$'|Rest], Acc) ->
    escape_sql1(Rest, [$', $'|Acc]);
escape_sql1([C|Rest], Acc) ->
    escape_sql1(Rest, [C|Acc]).

pack_datetime(DateTime) ->
    "'" ++ erlydtl_filters:date(DateTime, "Y-m-d H:i:s") ++ "'".

pack_now(Now) -> pack_datetime(calendar:now_to_datetime(Now)).

pack_value(undefined) ->
	"null";
pack_value(true) ->
    "TRUE";
pack_value(false) ->
    "FALSE";
pack_value(V) when is_atom(V) ->
    pack_value(atom_to_list(V));
pack_value(V) when is_binary(V) ->
    pack_value(binary_to_list(V));
pack_value(V) when is_list(V) ->
    "'" ++ escape_sql(V) ++ "'";
pack_value({MegaSec, Sec, MicroSec}) when is_integer(MegaSec) andalso is_integer(Sec) andalso is_integer(MicroSec) ->
    pack_now({MegaSec, Sec, MicroSec});
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    pack_datetime(Val);
pack_value(Val) when is_integer(Val) ->
    integer_to_list(Val);
pack_value(Val) when is_float(Val) ->
    float_to_list(Val).

fetch(Pid, Query) ->
    mysql_conn:fetch(Pid, [Query], self()).

