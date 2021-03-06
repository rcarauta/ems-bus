%%********************************************************************
%% @title Module ems_ldap_handler
%% @version 1.0.0
%% @doc Main module HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_ldap_handler).

-behaviour(ranch_protocol).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
-include("../include/ems_http_messages.hrl").
-include("../include/LDAP.hrl").

%  Stores the state of the service.
-record(state, {datasource,		 	%% datasource
				middleware,		 	%% middleware to service logic
				admin,		 		%% admin ldap
				password_admin}).   %% Password of admin ldap

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Service) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Service]),
	{ok, Pid}.

init(Ref, Socket, Transport, [#service{datasource = Datasource,
										middleware = Middleware,
										properties = Props}]) ->
	ok = ranch:accept_ack(Ref),
	LdapAdmin = maps:get(<<"ldap_admin">>, Props),
	LdapPasswdAdmin = maps:get(<<"ldap_password_admin">>, Props),
    State = #state{datasource = Datasource,
				   middleware = Middleware,
				   admin = LdapAdmin,
				   password_admin = LdapPasswdAdmin
			   },
	loop(Socket, Transport, State).

loop(Socket, Transport, State) ->
	case Transport:recv(Socket, 0, 5000) of
		{ok, Data} ->
			LdapMessage = decode_ldap_message(Data),
			?DEBUG("Ldap msg is ~p\n", [LdapMessage]),
			MessageID = LdapMessage#'LDAPMessage'.messageID,
			Result = handle_request(LdapMessage, State),
			case Result of
				{ok, unbindRequest} ->
					Transport:close(Socket);
				{ok, Msg} -> 
					Response = lists:map(fun(M) -> encode_response(MessageID, M) end, Msg),
					Transport:send(Socket, Response)
			end,
			loop(Socket, Transport, State);
		_ ->
			ok = Transport:close(Socket)
	end.


encode_response(MessageID, Msg) ->
	Response = #'LDAPMessage'{messageID = MessageID,
							  protocolOp = Msg,
							  controls = asn1_NOVALUE},
    case asn1rt:encode('LDAP', 'LDAPMessage', Response) of
        {ok, Result} -> Result;
        {error, Reason} -> {error, Reason}
    end.


decode_ldap_message(RequestBin) ->
	case asn1rt:decode('LDAP', 'LDAPMessage', RequestBin) of
        {ok, {'LDAPMessage', _MessageID, _ProtocolOp, _} = LdapMsg} ->
			LdapMsg;
		{error, Reason} -> 
			{error, Reason}
    end.

  
handle_request({'LDAPMessage', _,
					{bindRequest, #'BindRequest'{version = _Version, 
												 name = Name, 
												 authentication = {_, Password}}},
				 _}, State = #state{admin = AdminLdap, 
									password_admin = PasswordAdminLdap}) ->
	 <<Cn:3/binary, _/binary>> = Name,
	case Cn of
		<<"cn=">> ->
			BindResponse = case Name =:= AdminLdap andalso Password =:= PasswordAdminLdap of
				true -> make_bind_response(success, Name);
				_-> make_bind_response(invalidCredentials, Name)
			end;
		<<"uid">> -> 
			<<_:4/binary, UserLogin/binary>> = hd(binary:split(Name, <<",">>)),
			BindResponse = case middleware_autentica(UserLogin, Password, State) of
				{error, _Reason} ->	make_bind_response(invalidCredentials, Name);
				ok -> make_bind_response(success, Name)
			end
	end,
	{ok, [BindResponse]};
handle_request({'LDAPMessage', _,
					{searchRequest, #'SearchRequest'{baseObject = _BaseObject, 
													scope = _Scope, 
													derefAliases = _DerefAliases, 
													sizeLimit = _SizeLimit, 
													timeLimit = _TimeLimit, 
													typesOnly = _TypesOnly, 
													filter =  {equalityMatch, {'AttributeValueAssertion', <<"uid">>, UsuLoginBin}},
													attributes = _Attributes}},
				 _}, State = #state{admin = AdminLdap}) ->
	UserLogin = UsuLoginBin,
	case middleware_find_user_by_login(UserLogin, State) of
		{error, _Reason} ->
			ResultDone = make_result_done(invalidCredentials),
			{ok, [ResultDone]};
		{ok, UserRecord} ->
			ResultEntry = make_result_entry(UsuLoginBin, UserRecord, AdminLdap),
			ResultDone = make_result_done(success),
			{ok, [ResultEntry, ResultDone]}
	end;
handle_request({'LDAPMessage', _, 
					{unbindRequest, _},
				 _}, _State) ->
	{ok, unbindRequest};
handle_request({'LDAPMessage', _, 
					_UnknowMsg,
				 _}, _State) ->
	{ok, unbindRequest}.
	

make_object_name(UsuId) ->
	R1 = [<<"uid="/utf8>>, UsuId, <<",ou=funcdis,ou=Classes,dc=unb,dc=br"/utf8>>],
	R2 = iolist_to_binary(R1),
	R2.

make_bind_response(unavailable, _) ->
	make_bind_response(unavailable, <<"">>, <<"LDAP database or server machine unavailable!!!">>);

make_bind_response(ResultCode, MatchedDN) ->
	make_bind_response(ResultCode, MatchedDN, <<"">>).

make_bind_response(ResultCode, MatchedDN, DiagnosticMessage) ->
	{bindResponse, #'BindResponse'{resultCode = ResultCode,
												  matchedDN = MatchedDN,
												  diagnosticMessage = DiagnosticMessage,
												  referral = asn1_NOVALUE,
												  serverSaslCreds = asn1_NOVALUE}
	}.

make_result_entry(UsuLogin, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}, AdminLdap) ->
	ObjectName = make_object_name(UsuLogin),
	{searchResEntry, #'SearchResultEntry'{objectName = ObjectName,
										  attributes = [#'PartialAttribute'{type = <<"uid">>, vals = [UsuLogin]},
														#'PartialAttribute'{type = <<"cn">>, vals = [AdminLdap]},
														#'PartialAttribute'{type = <<"mail">>, vals = [UsuEmail]},
														#'PartialAttribute'{type = <<"cpf">>, vals = [UsuCpf]},
														#'PartialAttribute'{type = <<"passwd">>, vals = [UsuSenha]},
														#'PartialAttribute'{type = <<"givenName">>, vals = [UsuNome]},
														#'PartialAttribute'{type = <<"employeeNumber">>, vals = [UsuId]}
														]
										}
	}.


make_result_done(ResultCode) ->
	{searchResDone, #'LDAPResult'{resultCode = ResultCode, 
								  matchedDN = <<"">>, 
								  diagnosticMessage = <<"">>,
								  referral = asn1_NOVALUE}
	
	}.
	

middleware_autentica(UserLogin, UserPassword, #state{middleware = Middleware, 
																  datasource = Datasource}) ->
	case code:ensure_loaded(Middleware) of
		{module, _} ->
			case erlang:function_exported(Middleware, autentica, 3) of
				true -> apply(Middleware, autentica, [UserLogin, UserPassword, Datasource]);
				false -> {error, einvalid_middleware}
			end;
		{error, Reason} -> {
			error, {Reason, Middleware}}
	end.

middleware_find_user_by_login(UserLogin, #state{middleware = Middleware, 
												datasource = Datasource}) ->
	case code:ensure_loaded(Middleware) of
		{module, _} ->
			case erlang:function_exported(Middleware, find_user_by_login, 2) of
				true -> 
					case apply(Middleware, find_user_by_login, [UserLogin, Datasource]) of
						{ok, {UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}} ->
							?DEBUG("user ~p\n", [{UsuId, UsuNome, UsuCpf, UsuEmail, UsuSenha}]),
							UserRecord2 = {format_user_field(UsuId),
										   format_user_field(UsuNome),
										   format_user_field(UsuCpf),
										   format_user_field(UsuEmail),
										   format_user_field(UsuSenha)}, 
							{ok, UserRecord2};
						Error -> Error
					end;
				false -> {error, einvalid_middleware}
			end;
		{error, Reason} -> {
			error, {Reason, Middleware}}
	end.


format_user_field(null) -> <<"">>;
format_user_field([]) -> <<"">>;
format_user_field(Number) when is_integer(Number) -> list_to_binary(integer_to_list(Number));
format_user_field(Text) -> list_to_binary(string:strip(Text)).
	

