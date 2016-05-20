%%********************************************************************
%% @title Module ems_http_listener
%% @version 1.0.0
%% @doc Listener module for HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_listener).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {lsocket, 
				allowed_address,
				accept_count,
				accept_timeout,
				recv_timeout,
				worker_count = 0,
				total_reqs = 0,
				min_accept,
				max_accept,
				max_worker,
				keepalive,
				nodelay}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Port, IpAddress) -> 
    gen_server:start_link(?MODULE, {Port, IpAddress}, []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({Port, IpAddress}) ->
	process_flag(trap_exit, true),
	Conf = ems_config:getConfig(),
	Opts = [binary, 
			{packet, 0}, 
			{active, false},
			{buffer, 8000},
			{send_timeout_close, true},
			{send_timeout, ?TCP_SEND_TIMEOUT}, 
			{keepalive, Conf#config.tcp_keepalive}, 
			{nodelay, Conf#config.tcp_nodelay},
			{backlog, ?TCP_BACKLOG},
			{ip, IpAddress},
			{reuseaddr, true},
			{delay_send, false}],
	case gen_tcp:listen(Port, Opts) of
		{ok, LSocket} ->
			NewState = #state{lsocket = LSocket,
							  allowed_address = Conf#config.tcp_allowed_address_t,
							  accept_count = Conf#config.tcp_max_http_worker,
							  min_accept = Conf#config.tcp_max_http_worker,
							  max_accept = Conf#config.tcp_max_http_worker*2,
							  max_worker = 10,
							  accept_timeout = Conf#config.tcp_accept_timeout,
							  keepalive = Conf#config.tcp_keepalive,
							  nodelay = Conf#config.tcp_nodelay,
							  recv_timeout = Conf#config.tcp_recv_timeout},
			start_accept_workers(NewState#state.accept_count, LSocket, NewState),
			ems_logger:info("Listening http packets on ~s:~p with ~p workers.", [inet:ntoa(IpAddress), Port, NewState#state.accept_count]),
			{ok, NewState};
		{error,eaddrnotavail} ->
			ems_logger:error("Network interface to the IP ~p not available, ignoring this interface...", [inet:ntoa(IpAddress)]),
			{ok, #state{}};    
		Error -> Error
     end.	

handle_cast(new_accept, State) ->
    NewState = start_accept_worker(State),
    {noreply, NewState};

handle_cast({timeout_accept, Pid}, State = #state{accept_count = AcceptCount,
												  min_accept = MinAccept}) when AcceptCount > MinAccept ->
	ems_logger:info("Stop accept worker ~p. State: ~p.", [Pid, State]),
	stop_accept_worker(Pid),
	{noreply, State};

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
	ems_logger:info("Http worker crash, starting new. Reason: ~p.", [Reason]),
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
						   allowed_address = AllowedAddress,
						   accept_timeout = AcceptTimeout,
						   recv_timeout = RecvTimeout}) ->
    ems_http_worker:start_link({self(), LSocket, AllowedAddress, AcceptTimeout, RecvTimeout}).

start_accept_workers(0,_,_) ->
    ok;

start_accept_workers(Num, LSocket, State) ->
    start_accept_worker(State),
    start_accept_workers(Num-1, LSocket, State).

stop_accept_worker(Pid) ->	
	gen_server:stop(Pid).
