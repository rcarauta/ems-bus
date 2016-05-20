%%********************************************************************
%% @title Module ems_dispatcher
%% @version 1.0.0
%% @doc Responsible for forwarding the requests to / from the REST services.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_dispatcher).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").

%% Client API
-export([start/0, dispatch_request/1]).

 

%%====================================================================
%% Client API
%%====================================================================

start() ->
    try
		ets:new(ctrl_node_dispatch, [set, named_table, public])
	catch
		_:_ -> ok
	end.
 

%% @doc Dispatches the request to the service registered in the catalog
dispatch_request(Request) ->
	case ems_catalog:lookup(Request) of
		{Service, ParamsMap, QuerystringMap} -> 
			case ems_auth_user:autentica(Service, Request) of
				{ok, User} ->
					case get_work_node(Service#service.host, 
									   Service#service.host,	
									   Service#service.host_name, 
									   Service#service.module_name, 1) of
						{ok, Node} ->
							Ref = executa_service(Node, Service, Request),
							Request2 = Request#request{user = User, 
													   node_exec = Node,
													   service_pid = Ref,
													   service = Service,
													   params_url = ParamsMap,
													   querystring_map = QuerystringMap},
							{ok, Request2};
						{error, service_out} -> 
							{error, service_out}
					end;
				{error, no_authorization} -> 
					{error, no_authorization}
			end;
		{error, timeout} ->
			{error, timeout};
		{error, notfound} -> 
			{error, notfound}
	end.


%% @doc Execute in local (Erlang service)
executa_service(_Node, #service{host='', 
						  	    module=Module, 
							    function=Function}, 
					   #request{rid = RID,
								uri = Uri,
								type = Type,
								params_url = ParamsUrl,
								querystring_map = QueryStringMap,
								payload = Payload,
								content_type = ContentType}) ->
	Msg = {{RID, Uri, Type, ParamsUrl, QueryStringMap, Payload, ContentType}, self()},
	apply(Module, Function, [Msg]);


%% @doc Execute external service
executa_service(Node, #service{module_name = ModuleName, 
							   function_name = FunctionName, 
							   module = Module}, 
					  #request{rid = RID,
							   uri = Uri,
							   type = Type,
							   params_url = ParamsUrl,
							   querystring_map = QueryStringMap,
							   payload = Payload,
							   content_type = ContentType}) ->
	io:format("jboss\n\n"),
	% Envia uma mensagem assíncrona para o serviço
	Ref = {Module, Node},
	Msg = {{RID, Uri, Type, ParamsUrl, QueryStringMap, Payload, ContentType,  ModuleName, FunctionName}, self()},
	Ref ! Msg,
	Ref.
	

get_work_node('', _, _, _, _) -> {ok, node()};

get_work_node([], HostList, HostNames, ModuleName, 1) -> 
	get_work_node(HostList, HostList, HostNames, ModuleName, 2);

get_work_node([], _HostList, _HostNames, _ModuleName, 2) -> 
	{error, service_out};

get_work_node([_|T], HostList, HostNames, ModuleName, Tentativa) -> 
	%% Localiza a entrada do módulo na tabela hash
	case ets:lookup(ctrl_node_dispatch, ModuleName) of
		[] -> 
			% não encontrou, vamos selecionar o índice do primeiro node
			Index = 1;
		[{_, Idx}] -> 
			% encontrou um node que foi utilizado anteriormente, vamos usar o próximo
			ets:delete(ctrl_node_dispatch, ModuleName),
			Index = Idx+1
	end,
	% Pegamos o primeiro node quando Index maior que o tamanho da lista de nodes disponíveis
	case Index > length(HostList) of
		true -> Index2 = 1;
		false -> Index2 = Index
	end,
	% Inserimos na tabela hash os dados de controle
	ets:insert(ctrl_node_dispatch, {ModuleName, Index2}),

	% Qual node vamos selecionar
	Node = lists:nth(Index2, HostList),
	
	% Este node está vivo? Temos que rotear para um node existente
	case net_adm:ping(Node) of
		pong -> {ok, Node};
		pang -> get_work_node(T, HostList, HostNames, ModuleName, Tentativa)
	end.
		
