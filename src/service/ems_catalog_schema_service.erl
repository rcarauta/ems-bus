%%********************************************************************
%% @title Módulo ems_catalog_schema_service
%% @version 1.0.0
%% @doc Módulo de serviço ems_catalog_schema_service
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_catalog_schema_service).

-behavior(gen_server). 
-behaviour(poolboy_worker).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


%% Server API  
-export([start/0, start_link/1, stop/0]).

%% Cliente interno API
-export([all/2, get/2, insert/2, update/2, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/1, handle_info/2, terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

%  Armazena o estado do service. 
-record(state, {}). 


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
 
all(Request, From)	->
	ems_pool:cast(ems_catalog_schema_pool, {all, Request, From}).

get(Request, From)	->
	ems_pool:cast(ems_catalog_schema_pool, {get, Request, From}).
	
insert(Request, From)	->
	ems_pool:cast(ems_catalog_schema_pool, {insert, Request, From}).

update(Request, From)	->
	ems_pool:cast(ems_catalog_schema_pool, {update, Request, From}).

delete(Request, From)	->
	ems_pool:cast(ems_catalog_schema_pool, {delete, Request, From}).
	

%%====================================================================
%% gen_server callbacks
%%====================================================================
 
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}. 
    
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast({all, Request, _From}, State) ->
	Result = do_list_catalog(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, Result}),
	%gen_server:cast(From, {service, Request, Result}),
	{noreply, State};

handle_cast({insert, Request, _From}, State) ->
	Reply = do_insert(Request, State),
	ems_eventmgr:notifica_evento(ok_request, {service, Request, {ok, Reply}}),
	{noreply, State}.

handle_call(_Param, _From, State) ->
	{reply, ok, State}.

handle_info(State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
%%====================================================================
%% Funções internas
%%====================================================================

do_list_catalog(_Request, _State) -> 
	ems_catalog:all().
	
do_insert(Request = #request{payload = CatalogJson}, _State) ->
	io:format("catalog is ~p\n", [CatalogJson]),
	
	1.
	
	
