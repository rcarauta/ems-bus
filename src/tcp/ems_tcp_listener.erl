%%********************************************************************
%% @title Module ems_tcp_listener
%% @version 1.0.0
%% @doc Listener module for tcp server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_tcp_listener).

-behavior(gen_server). 

-include("../include/ems_tcp.hrl").

%% Server API
-export([start_link/4, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% state
-record(state, {lsocket, 
				conf,
				handler}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start_link({IP, Port}, Conf, TcpOpts, Handler) -> 
    gen_server:start_link(?MODULE, {{IP, Port}, Conf, TcpOpts, Handler}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({{IP, Port}, Conf, TcpOpts, Handler}) ->
	process_flag(trap_exit, true),
	TcpOpts2 = [{ip, IP}|TcpOpts],
	case gen_tcp:listen(Port, TcpOpts2) of
		{ok, LSocket} ->
			AcceptCount = proplists:get_value(accept_count, Conf, 12),
			ConfWorker = #conf_worker{allowed_address 	= proplists:get_value(allowed_address, Conf, []),
									  accept_count 		= AcceptCount,
									  accept_timeout 	= proplists:get_value(accept_timeout, Conf, 60000),
									  recv_timeout 		= proplists:get_value(recv_timeout, Conf, 2000)},
			NewState = #state{lsocket = LSocket,
							  conf = ConfWorker,
							  handler = Handler},
			start_accept_workers(AcceptCount, LSocket, NewState),
			ems_logger:info("Listening tcp packets on ~s:~p with ~p workers.", [inet:ntoa(IP), Port, AcceptCount]),
			{ok, NewState};
		{error, eaddrnotavail} ->
			ems_logger:error("Network interface to the IP ~p not available, ignoring this interface...", [inet:ntoa(IP)]),
			{error, eaddrnotavail};    
		{error, Reason} -> 
			{error, Reason}
     end.	

handle_cast(new_accept, State) ->
    NewState = start_accept_worker(State),
    {noreply, NewState};

handle_cast(shutdown, State=#state{lsocket = undefined}) ->
    {stop, normal, State};
    
handle_cast(shutdown, State=#state{lsocket = LSocket}) ->
    gen_tcp:close(LSocket),
    {stop, normal, State#state{lsocket = undefined}};

handle_cast(_, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
    
handle_info({'EXIT', _Pid, Reason}, State) ->
	start_accept_worker(State),
	ems_logger:info("tcp worker crash, starting new. Reason: ~p.", [Reason]),
	{noreply, State};

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(_Reason, #state{lsocket = undefined}) ->
    ok;
   
terminate(_Reason, #state{lsocket = LSocket}) ->
    gen_tcp:close(LSocket),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

start_accept_worker(#state{lsocket = LSocket,
						   conf = Conf,
						   handler = Handler}) ->
    ems_tcp_accept:start_link({self(), LSocket, Conf, Handler}).

start_accept_workers(0,_,_) ->
    ok;

start_accept_workers(Num, LSocket, State) ->
    start_accept_worker(State),
    start_accept_workers(Num-1, LSocket, State).

stop_accept_worker(Pid) ->	
	gen_server:stop(Pid).
