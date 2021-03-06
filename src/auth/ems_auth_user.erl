%%********************************************************************
%% @title Module ems_auth_user
%% @version 1.0.0
%% @doc Module responsible for authenticating users.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_auth_user).

-include("../include/ems_config.hrl").
-include("../include/ems_schema.hrl").
    
-export([autentica/2]).

autentica(Service, Request) ->
	try
		case Service#service.authentication of
			<<"Basic">> -> do_basic_authorization(Request);
			<<>> -> {ok, anonimo}
		end
	catch
		_Exception:_Reason ->  {error, no_authorization} 
	end.

do_basic_authorization(Request) ->
	case Request#request.authorization /= "" of
		true -> 
			[Authorization|[UserNameEPassword|_]] = string:tokens(Request#request.authorization, " "),
			case Authorization =:= "Basic" of
				true -> 
					UserNameEPassword2 = base64:decode_to_string(UserNameEPassword),
					[UserName|[Password|_]] = string:tokens(UserNameEPassword2, ":"),
					case ems_user:call({find_by_username_and_password, list_to_binary(UserName), list_to_binary(Password)}) of
						{ok, User} -> {ok, User};
						_ -> {error, no_authorization}
					end;
				false -> {error, no_authorization}
			end;
		false -> {error, no_authorization}
	end.
 	


