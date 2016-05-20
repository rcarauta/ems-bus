%%********************************************************************
%% @title Module ems_tcp_accept
%% @version 1.0.0
%% @doc Module responsible for processing HTTP requests.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_tcp_accept).

-behavior(gen_server). 

-include("../include/ems_tcp.hrl").

%% Server API
-export([start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).


% state
-record(state, {supervisor,	 		%% tcp listener
				lsocket,		 	%% socket of listener
				socket,			 	%% socket of request
				conf,				%% conf of worker
				handler		
			}
		).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

   
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).
    
stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================


%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Supervisor, LSocket, Conf, Handler}) ->
    State = #state{supervisor = Supervisor,
				   lsocket = LSocket, 
				   conf = Conf,
				   handler = Handler},
    {ok, State, 0}.


handle_cast(shutdown, State) ->
    io:format("Shutdown tcp worker.\n"),
    {stop, normal, State}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info(timeout, State) -> accept_request(State);

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(Reason, #state{socket = undefined}) ->
	io:format("Terminate tcp worker. Reason: ~p\n", [Reason]),
    ok;

terminate(_Reason, #state{socket = Socket}) ->
	gen_tcp:close(Socket),
	%io:format("Terminate tcp worker. Reason: ~p\n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

accept_request(State=#state{lsocket = LSocket, 
							conf = #conf_worker{accept_timeout = AcceptTimeout}}) ->
	case gen_tcp:accept(LSocket, AcceptTimeout) of
		{ok, Socket} -> 
			accept_peername(Socket, State),
			accept_request(State);
		{error, closed} -> 
			% ListenSocket is closed
			ems_logger:info("Listener tcp was closed, stopping accept."),
			{stop, normal, State};
		{error, timeout} ->
			% no connection is established within the specified time
			%ems_logger:info("No connection is established within the specified time."),
			accept_request(State);
		{error, PosixError} ->
			PosixErrorDescription = ems_tcp_util:posix_error_description(PosixError),
			ems_logger:error("PosixError ~p in tcp worker accept.", [PosixErrorDescription]),
			accept_request(State)
	end.

	
accept_peername(Socket, State = #state{conf = #conf_worker{allowed_address = AllowedAddress}}) ->
	case inet:peername(Socket) of
		{ok, {IP,_Port}} -> 
			case IP of
				{127, 0, _,_} -> 
					handle_request(Socket, State);
				_ -> 
					%% It is in the range of IP addresses authorized to access the bus?
					case ems_tcp_util:match_ip_address(AllowedAddress, IP) of
						true -> 
							handle_request(Socket, State);
						false -> 
							gen_tcp:close(Socket),
							ems_logger:warn("Host ~s not authorized!", [inet:ntoa(IP)])
					end
			end;
		_ -> 
			gen_tcp:close(Socket),
			ems_logger:error("Incorrect peername in tcp worker accept.")
	end.	


handle_request(Socket, #state{handler = Handler,
							  conf = #conf_worker{recv_timeout = RecvTimeout}}) ->
	spawn(fun() -> 
				case gen_tcp:recv(Socket, 0, RecvTimeout) of
					{ok, RequestBin} ->
						gen_server:call(Handler, {new_request, Socket, RequestBin});
					{error, closed} -> 
						ems_logger:info("Socket request has been closed in handle request.");
					{error, timeout} ->
						ems_logger:info("Timeout in handle request."),
						gen_tcp:close(Socket);
					{error, PosixError} ->
						PosixErrorDescription = ems_tcp_util:posix_error_description(PosixError),
						ems_logger:error("PosixError ~p in tcp worker handle request.", [PosixErrorDescription]),
						gen_tcp:close(Socket)
				end
		  end).


	
