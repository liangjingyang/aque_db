%%===========================================================================
%%     FileName: aque_db_adapter_mnesia.erl
%%         Desc: mnesia
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-05 13:27:00
%%      History:
%%===========================================================================
-module(aque_db_adapter_mnesia).

-behaviour(aque_db_adapter).

-export([
        start/1, 
        stop/0,
        init/1, 
        terminate/1, 
        init_tab/2
    ]).

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

-record(aque_tab_rec, {key = 0, bin = <<>>}).


start(_) ->
    application:start(mnesia).

stop() ->
    application:stop(mnesia).

init(_Options) ->
    {ok, undefined}.

terminate(_) ->
    ok.

init_tab(_, Tables) ->
    lists:foreach(
        fun (counters) ->
                mnesia:create_table(counters, [
                        {attributes, [key, value]},
                        {disc_copies, [node()]}
                    ]);
            (Tab) ->
                mnesia:create_table(Tab, [
                        {record_name, aque_tab_rec}, 
                        {disc_copies, [node()]},
                        {attributes, record_info(fields, aque_tab_rec)}
                    ])
        end, Tables).

insert(_, Tab, Key, Bin) ->
    Res = mnesia:dirty_write(Tab, #aque_tab_rec{key = Key, bin = Bin}),
    case Res of
        ok -> true;
        _ -> Res
    end.

lookup(_, Tab, Key) ->
    case mnesia:dirty_read(Tab, Key) of
        [] ->
            [];
        [#aque_tab_rec{bin = Bin}] ->
            Bin
    end.

delete(_, Tab, Key) ->
    Res = mnesia:dirty_delete(Tab, Key),
    case Res of
        ok -> true;
        _ -> Res
    end.

all_keys(_Conn, Tab) ->
    mnesia:dirty_all_keys(Tab).

count(_Conn, Tab) ->
    length(mnesia:dirty_all_keys(Tab)).

counter(_Conn, Key) ->
    case mnesia:dirty_read(counters, Key) of
        [] ->
            0;
        [{_, _, Num}] ->
            Num
    end.

update_counter(_Conn, Key, Num) ->
    mnesia:dirty_update_counter(counters, Key, Num).

tab2list(_Conn, counters) ->
    List = mnesia:dirty_match_object(counters, {counters, '_', '_'}),
    [{Key, Value}||{_, Key, Value}<-List];
tab2list(_Conn, Tab) ->
    List = mnesia:dirty_match_object(Tab, #aque_tab_rec{_='_'}),
    [B||{_, _, B}<-List].
