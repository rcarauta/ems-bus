%%********************************************************************
%% @title Module ems_tcp_server
%% @version 1.0.0
%% @doc Main module tcp server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_tcp_server).

-behavior(gen_server). 

%% Server API
-export([start_link/4, stop/0]).

%% Client API
-export([start_listeners/1, start_listener/2, stop_listener/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

% state
-record(state, {listener = [], 
				listen_address,
				tcp_opts,
				conf,
				handler}).

-define(SERVER, ?MODULE).

%%====================================================================
%% Server API
%%====================================================================

start_link(ListenAddress, Conf, TcpOpts, Handler) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, {ListenAddress, Conf, TcpOpts, Handler}, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 

%%====================================================================
%% Client API
%%====================================================================

start_listeners(ListenAddress) ->
	gen_server:cast(?SERVER, {start_listeners, ListenAddress}).
 
start_listener(IP, Port) ->
	gen_server:cast(?SERVER, {start_listener, {IP, Port}}).

stop_listener(IP, Port) ->
	gen_server:call(?SERVER, {stop_listener, {IP, Port}}).
	
 
%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init({ListenAddress, Conf, TcpOpts, Handler}) ->
	case do_start_listeners(ListenAddress, #state{conf = Conf, 
												  tcp_opts = TcpOpts,
												  handler = Handler}) of
		{ok, State} ->	{ok, State};
		{error, _Reason, State} -> {stop, State}
	end.
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({start_listener, {IP, Port}}, State) ->
	case do_start_listener({IP, Port}, State) of
		{ok, NewState} 	 -> {noreply, NewState};
		{error, _Reason} -> {noreply, State}
	end;

handle_cast({start_listeners, ListenAddress}, State) ->
	case do_start_listeners(ListenAddress, State) of
		{ok, NewState} 	 -> {noreply, NewState};
		{error, _Reason} -> {noreply, State}
	end.

handle_call({stop_listener, {IP, Port}}, _From, State) ->
	case do_stop_listener({IP, Port}, State) of
		{ok, NewState}  -> {reply, ok, NewState};
		{error, Reason} -> {reply, {error, Reason}, State}
	end.

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

do_start_listeners([], State) -> {ok, State};

do_start_listeners([H|T], State) ->
	case do_start_listener(H, State) of
		{ok, NewState}  -> do_start_listeners(T, NewState);
		{error, Reason} -> {error, Reason}
	end.

do_start_listener({IP, Port}, State = #state{listener = Listener, 
											 conf = Conf,
											 tcp_opts = TcpOpts,
											 handler = Handler}) ->
	case ems_tcp_listener:start_link({IP, Port}, Conf, TcpOpts, Handler) of
		{ok, PidListener} ->
			NewState = State#state{listener=[{PidListener, {IP, Port}}|Listener]},
			{ok, NewState};
		{error, Reason} ->
			{error, Reason}
	end.

do_stop_listener({IP, Port}, State = #state{listener = Listener}) ->
	case [ S || {S,{I, P}} <- Listener, {I, P} == {IP, Port}] of
		[PidListener|_] ->
			gen_server:call(PidListener, shutdown),
			NewState = State#state{listener=lists:delete({PidListener, {IP, Port}}, Listener)},
			{ok, NewState};
		_ -> 
			{error, enolisten}
	end.
