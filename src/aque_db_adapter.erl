%%===========================================================================
%%     FileName: aque_db_adapter.erl
%%         Desc: adapter behaviour
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-05 13:31:02
%%      History:
%%===========================================================================
-module(aque_db_adapter).
-export([behaviour_info/1]).

-spec behaviour_info( atom() ) -> [ {atom(), integer()} ] | undefined.
behaviour_info(callbacks) ->
    [
        {start, 1}, {stop, 0}, {init, 1}, {terminate, 1},
        {insert, 4}, {lookup, 3}, {delete, 3}, {tab2list, 2},
        {all_keys, 2}, {count, 2}, {counter, 2}, {update_counter, 3}
    ];
behaviour_info(_Other) ->
    undefined.
