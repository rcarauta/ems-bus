%%********************************************************************
%% @title http_messages
%% @version 1.0.0
%% @doc Contém definições de mensagens HTTP.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

%% Messages
-define(MSG_SERVICO_NAO_ENCONTRADO, "Service ~p not found.").
-define(MSG_SERVICO_NAO_DISP, "Service ~p not found.").
-define(MSG_SERVICO_FALHOU, "Service ~p failed to meet request.Reason: ~p.").


%% Messages HTTP in JSON format
-define(HTTP_ERROR_400, <<"{\"error\":\"HTTP 400\",\"message\":\"Invalid request.\"}"/utf8>>).
-define(HTTP_ERROR_401, <<"{\"error\":\"HTTP 401\",\"message\":\"Unauthorized access.\"}"/utf8>>).
-define(HTTP_ERROR_404, <<"{\"error\":\"HTTP 404\",\"message\":\"Service not found.\"}"/utf8>>).
-define(HTTP_ERROR_404_FILE_NOT_FOUND, <<"{\"error\":\"HTTP 404\",\"message\":\"File not found.\"}"/utf8>>).
-define(HTTP_ERROR_415, <<"{\"error\":\"HTTP 415\",\"message\":\"The server only accepts JSON.\"}"/utf8>>).
-define(HTTP_ERROR_502(Motivo), io_lib:format(<<"{\"error\":\"HTTP 502\",\"message\":\"Service failed to meet request. Reason: ~p.\"}"/utf8>>, [Motivo])).
-define(HTTP_ERROR_502, <<"{\"error\":\"HTTP 502\",\"message\":\"Service failed to meet request.\"}"/utf8>>).
-define(HTTP_ERROR_503, <<"{\"error\":\"HTTP 503\",\"message\":\"Service is unavailable.\"}"/utf8>>).
