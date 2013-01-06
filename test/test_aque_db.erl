%%===========================================================================
%%     FileName: test_aque_db.erl
%%         Desc: test
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-06 15:30:56
%%      History:
%%===========================================================================

-module(test_aque_db).

-compile(export_all).

-define(TEST_TAB, aque_db_test).
-define(TEST_REC_KEY, 922729).
-define(TEST_REC, {aque_tab_rec, ?TEST_REC_KEY, "2", a, [1,2]}).
-define(LOG(Format, Args), io:format(Format++"~n", Args)).

start() ->
    application:start(crypto),
    mnesia:create_schema([node()]),
    application:start(aque_db),
    test_db(mnesia),
    test_db(mysql),
    test_db(mongodb),
    ok.

test_db(Db) ->
    test_init_tab(Db),
    test_insert(Db),
    test_lookup(Db),
    test_count(Db),
    test_all_keys(Db),
    test_tab2list(Db),
    test_delete(Db),
    ?LOG("Test ~w FINISHED!~n", [Db]).

test_init_tab(Db) ->
    aque_db:init_tab(make_pool_name(Db), [?TEST_TAB]).

test_insert(Db) ->
    case aque_db:insert(Db, ?TEST_TAB, ?TEST_REC_KEY, ?TEST_REC) of
        true -> ?LOG("Test ~w insert ... ok!", [Db]);
        Res -> ?LOG("Test ~w insert ... error, Res: ~p", [Db, Res])
    end.
    
test_lookup(Db) ->
    case aque_db:lookup(Db, ?TEST_TAB, ?TEST_REC_KEY) of
        ?TEST_REC -> ?LOG("Test ~w lookup ... ok!", [Db]);
        Res -> ?LOG("Test ~w lookup ... error, Res: ~p", [Db, Res])
    end.

test_delete(Db) ->
    case aque_db:delete(Db, ?TEST_TAB, ?TEST_REC_KEY) of
        true -> ?LOG("Test ~w delete ... ok!", [Db]);
        Res -> ?LOG("Test ~w delete ... error, Res: ~p", [Db, Res])
    end.

test_count(Db) ->
    case aque_db:count(Db, ?TEST_TAB) of
        Count when is_integer(Count) andalso Count >= 1 -> 
            ?LOG("Test ~w count ... ok!", [Db, Count]);
        Res -> 
            ?LOG("Test ~w count ... error, Res: ~p", [Db, Res])
    end.

test_all_keys(Db) ->
    case aque_db:all_keys(Db, ?TEST_TAB) of
        List when is_list(List) ->
            case lists:member(?TEST_REC_KEY, List) of
                true -> ?LOG("Test ~w all_keys ... ok!", [Db]);
                false -> ?LOG("Test ~w all_keys ... error, Res: not found ?TEST_REC_KEY~nList: ~p", [Db, List])
            end;
        Res -> ?LOG("Test ~w all_keys ... error, Res: ~p", [Db, Res])
    end.

test_tab2list(Db) ->
    case aque_db:tab2list(Db, ?TEST_TAB) of
        List when is_list(List) ->
            case lists:member(?TEST_REC, List) of
                true -> ?LOG("Test ~w tab2list ... ok!", [Db]);
                false -> ?LOG("Test ~w tab2list ... error, Res: not found ?TEST_REC~nList: ~p", [Db, List])
            end;
        Res -> ?LOG("Test ~w tab2list ... error, Res: ~p", [Db, Res])
    end.

make_pool_name({Db, Database}) ->
    list_to_atom(lists:concat([Db, "_", Database, "_aque_pool"]));
make_pool_name(Db) ->
    list_to_atom(lists:concat([Db, "_aque_pool"])).

