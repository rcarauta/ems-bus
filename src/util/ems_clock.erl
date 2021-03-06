%%********************************************************************
%% @title Module ems_clock
%% @version 1.0.0
%% @doc Module ems_clock
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_clock).

-behaviour(gen_server).


%% Server API
-export([start/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([local_time/0, local_time_str/0, local_time_str/1]).

-record(state, {tref = undefined}).

%% API.

start(_Args) ->	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

local_time() -> calendar:local_time().

local_time_str() -> ets:lookup_element(?MODULE, clock, 2).

local_time_str(LocalTime) ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = LocalTime,
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
	?MODULE = ets:new(?MODULE, [set, protected, named_table, {read_concurrency, true}]),
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	LocalTimeStr = lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])),
	TRef = erlang:send_after(1000, self(), update),
	ets:insert(?MODULE, {clock, LocalTimeStr}),
	{ok, #state{tref = TRef}}.

handle_call(stop, _From, State) ->
	{stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(_Msg, State) ->
	{noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(update, #state{tref=TRef0}) ->
	%% Cancel the timer in case an external process sent an update message.
	erlang:cancel_timer(TRef0),
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	LocalTimeStr = lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])),
	TRef1 = erlang:send_after(1000, self(), update),
	ets:insert(?MODULE, {clock, LocalTimeStr}),
	{noreply, #state{tref = TRef1}};
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
	ok.

-spec code_change(_, State, _) -> {ok, State} when State::#state{}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

