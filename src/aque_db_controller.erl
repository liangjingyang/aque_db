%%===========================================================================
%%     FileName: aque_db_controller.erl
%%         Desc: pool worker process
%%       Author: liangjingyang
%%        Email: ljy0922@gmail.com
%%     HomePage: http://www.cnblogs.com/liangjingyang
%%      Version: 0.0.1
%%   LastChange: 2013-01-05 13:06:19
%%      History:
%%===========================================================================
-module(aque_db_controller).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        adapter, 
        connection
    }).

start_link() ->
    start_link([]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Options) ->
    {_, AdapterName} = proplists:get_value(adapter, Options, {local, mnesia}),
    Adapter = list_to_atom(lists:concat(["aque_db_adapter_", AdapterName])),
    process_flag(trap_exit, true),
    {ok, Conn} = Adapter:init(Options),
    {ok, #state{adapter = Adapter, connection = Conn}}.

handle_call({insert, Tab, Key, Value}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    Bin = term_to_binary(Value),
    {reply, Adapter:insert(Conn, Tab, Key, Bin), State};

handle_call({lookup, Tab, Key}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    Bin = Adapter:lookup(Conn, Tab, Key),
    case Bin of
        [] ->
            Reply = [];
        _ ->
            Reply = binary_to_term(Bin)
    end,
    {reply, Reply, State};

handle_call({delete, Tab, Key}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    {reply, Adapter:delete(Conn, Tab, Key), State};

handle_call({all_keys, Tab}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    {reply, Adapter:all_keys(Conn, Tab), State};

handle_call({tab2list, counters}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    {reply, Adapter:tab2list(Conn, counters), State};

handle_call({tab2list, Tab}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    Bin = Adapter:tab2list(Conn, Tab),
    case Bin of
        [] ->
            Reply = [];
        _ ->
            Reply = [binary_to_term(B)||B<-Bin]
    end,
    {reply, Reply, State};

handle_call({count, Tab}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    {reply, Adapter:count(Conn, Tab), State};

handle_call({counter, Counter}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    {reply, Adapter:counter(Conn, Counter), State};

handle_call({update_counter, Key, Num}, _From, State) ->
    #state{adapter = Adapter, connection = Conn} = State,
    {reply, Adapter:update_counter(Conn, Key, Num), State};

handle_call(state, _From, State) ->
    {reply, State, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(stop, State) ->
    {stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
    {stop, shutdown, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Adapter = State#state.adapter,
    Conn = State#state.connection,
    Adapter:terminate(Conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

