%%********************************************************************
%% @title Module ems_http_server
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_server).

-behavior(gen_server). 

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").

%% Server API
-export([start/0, stop/0]).


%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% estado do servidor
-record(state, {listener=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
 	Config = ems_config:getConfig(),
	ServerConf = get_server_conf(Config),
	TcpOpts = get_tcp_opts(Config),
	Handler = self(),
	ems_tcp_server:start_link(Config#config.tcp_listen_address_t, ServerConf, TcpOpts, Handler).
			     
handle_cast(shutdown, State) ->
    {stop, normal, State}.


handle_call({new_request, Socket, RequestBin}, _From, State) ->
	new_request(Socket, RequestBin),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, [], State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%====================================================================
%% Internal functions
%%====================================================================

get_server_conf(#config{tcp_allowed_address_t = AllowedAddress,
						tcp_accept_timeout = AcceptCount,
						tcp_recv_timeout = RecvTimeout}) ->
	[{allowed_address, AllowedAddress},
	 {accept_count, AcceptCount},
	 {recv_timeout, RecvTimeout}].

get_tcp_opts(#config{tcp_keepalive = KeepAlive,
					 tcp_nodelay = NoDelay}) ->
	[ 	binary, 
		{packet, 0}, 
		{active, false},
		{buffer, 8000},
		{send_timeout_close, true},
		{send_timeout, ?TCP_SEND_TIMEOUT}, 
		{keepalive, KeepAlive}, 
		{nodelay, NoDelay},
		{backlog, ?TCP_BACKLOG},
		{reuseaddr, true},
		{delay_send, false}
	].

new_request(Socket, RequestBin) ->
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
	

