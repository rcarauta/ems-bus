%%********************************************************************
%% @title ems_tcp.hrl
%% @version 1.0.0
%% @doc ems_tcp.hrl
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

% conf of workers
-record(conf_worker, {allowed_address,
					  accept_count,
					  accept_timeout,
					  recv_timeout}).
