%%********************************************************************
%% @title Module info
%% @version 1.0.0
%% @doc It provides information about the bus in runtime.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_info_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/ems_config.hrl").

%% Server API
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([execute/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {count_reqs = 0}). 


%%====================================================================
%% Server API
%%====================================================================

start() -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
 
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop() ->
    gen_server:cast(?SERVER, shutdown).
 
 
%%====================================================================
%% Cliente API
%%====================================================================
 
execute(Request)	->
	ems_pool:cast(ems_info_service_pool, {info, Request}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({info, {_, From}}, #state{count_reqs = Count}) ->
	Result = get_info(),
	ems_pool:checkin(ems_info_service_pool, self()),
	From ! {ok, Result},
	{noreply, #state{count_reqs = Count + 1}}.
    
handle_call({info, _Request}, _From, #state{count_reqs = Count}) ->
	Result = get_info(),
	{reply, Result, #state{count_reqs = Count + 1}}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(Reason, _State) ->
	io:format("Terminate ems_info_service. Reason: ~p\n", [Reason]),
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================
    
get_info() -> 
	SchedId = erlang:system_info(scheduler_id),
	SchedNum = erlang:system_info(schedulers),
	ProcCount = erlang:system_info(process_count),
	ProcLimit = erlang:system_info(process_limit),
	ProcMemUsed = erlang:memory(processes_used),
	ProcMemAlloc = erlang:memory(processes),
	MemTot = erlang:memory(total),
	#{<<"SchedulerId">> => SchedId, 
	  <<"Num scheduler">> => SchedNum, 
	  <<"Process count">> => ProcCount, 
	  <<"Process limit">> => ProcLimit, 
	  <<"Memory used">> => ProcMemUsed,
	  <<"Memory allocated">> => ProcMemAlloc, 
	  <<"TotalMemoryAllocated">> => MemTot}. 

