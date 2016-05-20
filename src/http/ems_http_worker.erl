%%********************************************************************
%% @title Module ems_http_worker
%% @version 1.0.0
%% @doc Module responsible for processing HTTP requests.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_worker).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/1, start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).


% state
-record(state, {supervisor,	 		%% http listener
				lsocket,		 	%% socket of listener
				socket	,		 	%% socket of request
				allowed_address, 	%% range of IP addresses that can access the server
				accept_timeout,     %% timeout for accept
				recv_timeout		%% timeout for receive packet
			}
		).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start(Args) -> 
    gen_server:start_link(?MODULE, Args, []).
    
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

init({Supervisor, LSocket, AllowedAddress, AcceptTimeout, RecvTimeout}) ->
    State = #state{supervisor = Supervisor,
				   lsocket = LSocket, 
				   allowed_address = AllowedAddress,
				   accept_timeout = AcceptTimeout,
				   recv_timeout = RecvTimeout},
    {ok, State, 0}.


handle_cast(shutdown, State) ->
    io:format("Shutdown http worker.\n"),
    {stop, normal, State}.

handle_call(_Msg, _From, State) ->
	{reply, _Msg, State}.

handle_info(timeout, State=#state{lsocket = undefined}) ->
	{noreply, State};

handle_info(timeout, State) -> accept_request(State);

%handle_info({tcp, Socket, RequestBin}, State) -> process_request(Socket, RequestBin, State);

handle_info({tcp_closed, _Socket}, State) ->
	{noreply, State#state{socket = undefined}};

handle_info({HttpCode, RID, Reply}, State) ->
	io:format("pronto\n\n\n"),
	case ems_request:get_request_em_andamento(RID) of
		{ok, Request} -> 
			io:format("get request em andamento\n"),
			ems_request:registra_request(Request),
			ems_eventmgr:notifica_evento(ok_request, {HttpCode, Request, Reply}),
			{noreply, State};
		{erro, notfound} -> 
			io:format("notfound\n"),
			{noreply, State}
	end;

handle_info(_Msg, State) ->
   {noreply, State}.

handle_info(State) ->
   {noreply, State}.

terminate(Reason, #state{socket = undefined}) ->
	io:format("Terminate http worker. Reason: ~p\n", [Reason]),
    ok;

terminate(_Reason, #state{socket = Socket}) ->
	gen_tcp:close(Socket),
	%io:format("Terminate http worker. Reason: ~p\n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

accept_request(State=#state{lsocket = LSocket, 
							accept_timeout = AcceptTimeout}) ->
	case gen_tcp:accept(LSocket, AcceptTimeout) of
		{ok, Socket} -> 
			accept_peername(Socket, State),
			accept_request(State);
		{error, closed} -> 
			% ListenSocket is closed
			ems_logger:info("Listener http was closed, stopping accept."),
			{stop, normal, State};
		{error, timeout} ->
			% no connection is established within the specified time
			%ems_logger:info("No connection is established within the specified time."),
			accept_request(State);
		{error, PosixError} ->
			PosixErrorDescription = ems_tcp_util:posix_error_description(PosixError),
			ems_logger:error("PosixError ~p in http worker accept.", [PosixErrorDescription]),
			accept_request(State)
	end.
	
accept_peername(Socket, #state{allowed_address = AllowedAddress,
							   recv_timeout = RecvTimeout}) ->
	case inet:peername(Socket) of
		{ok, {Ip,_Port}} -> 
			case Ip of
				{127, 0, _,_} -> 
					handle_request(Socket, RecvTimeout);
				_ -> 
					%% It is in the range of IP addresses authorized to access the bus?
					case ems_http_util:match_ip_address(AllowedAddress, Ip) of
						true -> 
							handle_request(Socket, RecvTimeout);
						false -> 
							gen_tcp:close(Socket),
							ems_logger:warn("Host ~s not authorized!", [inet:ntoa(Ip)])
					end
			end;
		_ -> 
			gen_tcp:close(Socket),
			ems_logger:error("Incorrect peername in http worker accept.")
	end.	

handle_request(Socket, RecvTimeout) ->
	spawn(fun() -> 
				case gen_tcp:recv(Socket, 0, RecvTimeout) of
					{ok, RequestBin} ->
						process_request(Socket, RequestBin);
					{error, closed} -> 
						ems_logger:info("Socket request has been closed in handle request.");
					{error, timeout} ->
						ems_logger:info("Timeout in handle request."),
						gen_tcp:close(Socket);
					{error, PosixError} ->
						PosixErrorDescription = ems_tcp_util:posix_error_description(PosixError),
						ems_logger:error("PosixError ~p in http worker handle request.", [PosixErrorDescription]),
						gen_tcp:close(Socket)
				end
		  end).


process_request(Socket, RequestBin) ->
	try
		case ems_http_util:encode_request(Socket, RequestBin, self()) of
			 {ok, Request} -> 
				% TCP_LINGER2 for Linux
				inet:setopts(Socket,[{raw,6,8,<<30:32/native>>}]),
				% TCP_DEFER_ACCEPT for Linux
				inet:setopts(Socket,[{raw, 6,9, << 30:32/native >>}]),
				dispatch_request(Request);
			 {error, Request, Reason} -> 
				 Response = ems_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
				 send_response(400, Reason, Request, Response);
			 {error, invalid_http_header} -> 
				gen_tcp:close(Socket),
				ems_logger:error("Invalid HTTP request, close socket.")
		end
	catch
		_Exception:_ReasonEx ->
			gen_tcp:close(Socket),
			ems_logger:error("Internal http error in process_request, close socket.")
	end.
		

dispatch_request(Request = #request{type = "OPTIONS"}) ->
	Response = ems_http_util:encode_options_response(),
	send_response(200, ok, Request, Response);

dispatch_request(Request = #request{type = "GET", url = "/"}) ->
	Response = ems_http_util:encode_its_works_response(),
	send_response(200, ok, Request, Response);

dispatch_request(Request = #request{type = "GET", url = "/favicon.ico"}) ->
	Response = ems_http_util:encode_favicon_response(),
	send_response(200, ok, Request, Response);

dispatch_request(Request) ->
	case ems_dispatcher:dispatch_request(Request) of
		{ok, Request2} -> 
			RefMonitor = erlang:monitor(process, Request2#request.service_pid),
			receive 
				{ok, Result} -> 
					Response = ems_http_util:encode_response(<<"200">>, Result),
					send_response(200, ok, Request2, Response),
					io:format("200 - processou e enviou!\n");
				{ok, Result, MimeType} -> 
					Response = ems_http_util:encode_response(<<"200">>, Result, MimeType),
					send_response(200, ok, Request2, Response),
					io:format("200 - processou e enviou!\n");
				{error, Reason} ->
					Response = ems_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
					send_response(400, Reason, Request2, Response),
					io:format("200 - processou e enviou!\n");
				{'DOWN', _MonitorRef, _Type, _Object, _Info} ->
					Response = ems_http_util:encode_response(<<"400">>, ?HTTP_ERROR_503),
					send_response(400, service_down, Request2, Response),
					io:format("400 - processou morreu!\n")
				after Request2#request.service#service.timeout ->	
					Response = ems_http_util:encode_response(<<"503">>, ?HTTP_ERROR_503),
					send_response(503, timeout, Request2, Response),
					io:format("503 - timeout ~p!\n", [Request2#request.service#service.timeout])
			end,
			erlang:demonitor(RefMonitor, [flush, info]);
		{error, ReasonDisp} ->	
			Response = ems_http_util:encode_response(<<"400">>, ?HTTP_ERROR_400),
			send_response(400, ReasonDisp, Request, Response)
	end.

send_response(_HttpCode, _Reason, Request, Response) ->
	%T2 = ems_util:get_milliseconds(),
	%Latencia = T2 - Request#request.t1,
	case gen_tcp:send(Request#request.socket, Response) of
		ok -> gen_tcp:close(Request#request.socket);
		{error, timeout} -> gen_tcp:close(Request#request.socket);
		{error, closed} -> ok;
		_ -> gen_tcp:close(Request#request.socket)
	end,
	%Request2 = Request#request{latency = Latencia, 
	%						   code = HttpCode, 
	%						   reason = Reason},
	%ems_request:finaliza_request(Request2)
	ok.
	
