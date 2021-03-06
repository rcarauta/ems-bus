%%********************************************************************
%% @title Módulo ems_page
%% @version 1.0.0
%% @doc Contém funções para compilação de paginas Django
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_util).

-compile(export_all).

-include("../../include/ems_config.hrl").

-export([sleep/1,
		 timestamp_str/0,
		 json_encode/1,
		 json_decode/1,
		 hd_or_empty/1,
		 json_decode_as_map/1,
		 tuple_to_binlist/1, 
		 list_to_binlist/1,
		 binary_to_bool/1,
		 binary_to_integer/1,
		 mes_extenso/1,
		 binlist_to_list/1,
		 join_binlist/2,
		 list_to_ets/3,
		 profile/0,
		 make_rowid_from_url/2,
		 get_params_from_url/1,
		 get_rowid_and_params_from_url/2,
		 string_is_integer/1,
		 read_file_as_map/1,
		 node_is_live/1,
		 get_node_name/0,
		 get_priv_dir/0,
		 get_working_dir/0,
		 json_encode_table/2,
		 json_decode_as_map_file/1]).


%% Retorna o hash da url e os parâmetros do request
hashsym_and_params(S) when is_binary(S) -> hashsym_and_params(binary_to_list(S), 1, 0, []);
hashsym_and_params(S) -> hashsym_and_params(S, 1, 0, []).

hashsym_and_params([], _Idx, Hash, Params) -> {Hash, maps:from_list(Params)};
hashsym_and_params([H|_] = L, Idx, Hash, Params) when H >= 48 andalso H =< 57 -> 
	{L2, P} = hashsym_and_params_id(L, 0),
	P2 = case Idx of
			1 -> {<<"id">>, P};
			_ -> {list_to_binary("id_" ++ erlang:integer_to_list(Idx)), P}
		 end,
	hashsym_and_params(L2, Idx+1, Hash, [P2 | Params]);
hashsym_and_params([H|T], Idx, Hash, Params) -> hashsym_and_params(T, Idx, (Hash + H) bsl 1, Params).

hashsym_and_params_id([], P) -> {[], P};
hashsym_and_params_id([H|T], P) when H == 47 -> {T, P};
hashsym_and_params_id([H|T], P) -> hashsym_and_params_id(T, P * 10 + H - $0).


%% Retorna o hash da url (uso em tempo de execução)
hashsym(S) when is_binary(S) -> hashsym(binary_to_list(S), 0);
hashsym(S) -> hashsym(S, 0).

hashsym([], Hash) -> Hash;
hashsym([H|T], Hash) when H >= 48 andalso H =< 57 -> hashsym(T, Hash);
hashsym([H|T], Hash) -> hashsym(T, (Hash + H) bsl 1).


%% Retorna o hash da url (uso no carregamento dos catálogos)	
make_rowid(S) when is_binary(S) -> make_rowid(binary_to_list(S), 0);
make_rowid(S) -> make_rowid(S, 0).

make_rowid([], Hash) -> Hash;
make_rowid([H|T], Hash) when H == 58 -> make_rowid(make_rowid_id(T), Hash);
make_rowid([H|T], Hash) when H >= 48 andalso H =< 57 -> make_rowid(T, Hash);
make_rowid([H|T], Hash) -> make_rowid(T, (Hash + H) bsl 1).

make_rowid_id([]) -> [];
make_rowid_id([H|T]) when H == 47 -> T;
make_rowid_id([_|T]) -> make_rowid_id(T).


get_priv_dir() ->
	{ok, Path} = file:get_cwd(),
	Path ++ "/priv".

get_working_dir() ->
	{ok, Path} = file:get_cwd(),
	Path.


%% @doc Dorme por um determinado tempo
sleep(T) ->
    receive
	    after T -> true
    end.

%% @doc Retorna o timestamp em formato texto
timestamp_str() ->
	{{Ano,Mes,Dia},{Hora,Min,Seg}} = calendar:local_time(),
	lists:flatten(io_lib:format("~p/~p/~p ~p:~p:~p", [Dia, Mes, Ano, Hora, Min, Seg])).

date_to_string({{Ano,Mes,Dia},{_Hora,_Min,_Seg}}) ->
    lists:flatten(io_lib:format("~2..0B/~2..0B/~4..0B", [Dia, Mes, Ano])).

tuple_to_binlist(T) ->
	L = tuple_to_list(T),
	list_to_binlist(L).

list_to_binlist([]) -> [];
list_to_binlist(<<>>) -> [];
list_to_binlist(<<V/binary>>) -> [V];
list_to_binlist([H|T]) -> [item_to_binary(H)|list_to_binlist(T)].

binlist_to_list(<<>>) -> [];
binlist_to_list([]) -> [];
binlist_to_list([H|T]) -> [binary_to_list(H)|binlist_to_list(T)].

join_binlist([], _) -> "";
join_binlist(BinList, Str) -> string:join(binlist_to_list(BinList), Str).

item_to_binary([]) -> <<>>;
item_to_binary(<<I/binary>>) -> I;
item_to_binary(T) when is_tuple(T) -> 
	tuple_to_binlist(T);
item_to_binary(L) when is_list(L) -> 
	case io_lib:printable_list(L) of
		true -> iolist_to_binary(L);
		false -> list_to_binlist(L)
	end;
item_to_binary(I) when is_integer(I) -> 
	I;
item_to_binary(I) when is_float(I) -> 
	I;
item_to_binary(I) when is_atom(I) -> 
	[I2] = io_lib:format("~p", [I]),
	iolist_to_binary(I2);
item_to_binary(I) when is_map(I) -> I;
item_to_binary(I) -> iolist_to_binary(I).


%% @doc Converte dados Erlang para JSON
json_encode([]) -> <<>>;
json_encode(T) when is_tuple(T) ->
	L = tuple_to_binlist(T),
	?JSON_LIB:encode(L);
json_encode(L) when is_list(L) ->
	case io_lib:printable_list(L) of
		true -> L2 = iolist_to_binary(L);
		false -> L2 = list_to_binlist(L)
	end,
	?JSON_LIB:encode(L2);
json_encode(Value)-> ?JSON_LIB:encode(Value).


json_decode_as_map_file(FileName) ->
	case file:read_file(FileName) of
		{ok, JSON} -> json_decode_as_map(JSON);
		{error, enoent} -> {error, einvalid_json_filename}
	end.


%% @doc Converte um JSON para dados Erlang usando map
json_decode_as_map(JSON) ->
	try
		Dados1 = binary_to_list(JSON),
		Dados2 = lists:flatten(re:replace(Dados1, "[\t\r\n]", "", [global, {return,list}])),
		Dados3 = list_to_binary(Dados2),
		Result = ?JSON_LIB:decode(Dados3, [return_maps]),
		{ok, Result}
	catch
		_Exception:Reason -> {error, Reason}
	end.

%% @doc Converte um JSON para dados Erlang
json_decode(JSON) ->
	try
		JSON2 = case check_encoding_bin(JSON) of
			latin1 -> unicode:characters_to_binary(binary_to_list(JSON), latin1, utf8);
			utf8 -> JSON;
			_ -> erlang:raise(einvalid_json_encoding)
		end,
		T = ?JSON_LIB:decode(JSON2),
		{ok, element(1, T)}
	catch
		_Exception:Reason -> {error, Reason}
	end.
	
%% @doc Retorna o primeiro item da lista ou vazio
hd_or_empty(List) when length(List) > 0 -> 
	hd(List);

%% @doc Retorna o primeiro item da lista ou vazio	
hd_or_empty(_) -> [].

%% @doc Retorna a string com aspas
quote(Str) -> lists:flatten([$", Str, $"]).


%% @doc Boolean indicando se DateTime ocorreu no período (min, hour, day, week, year)
no_periodo(DateTime, Periodo) ->
	S1 = calendar:datetime_to_gregorian_seconds(DateTime),
	S2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	case Periodo of
		"min"   ->  (S2 - S1) =< 60;
		"hour"  ->  (S2 - S1) =< 3600;
		"day"   ->  (S2 - S1) =< 86400;
		"week"  ->  (S2 - S1) =< 604800;
		"month" ->  (S2 - S1) =< 2629800;
		"year"  ->  (S2 - S1) =< 31557600;
		_ -> erlang:error(badarg)
	end.


%% @doc Obtém a hora atual em milisegundos
-spec get_milliseconds() -> integer().
get_milliseconds() ->
   {Mega, Sec, Micro} = erlang:timestamp(),
   (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%% @doc Remove o último backslash da Url
remove_ult_backslash_url("/") -> "/";
remove_ult_backslash_url(Url) -> 
	case lists:suffix("/", Url) of
		true -> lists:droplast(Url);
		false -> Url
	end.

%% @doc Função name case
name_case([H|T]) when H >= $a, H =< $z -> 
	[H + ($A - $a) | T];
name_case(outros) -> outros.


%% @doc Primeiro caracter de cada palabra em caixa alta
modernize([H|T]) -> 
	Tokens = string:tokens([H|T], " "),
	Lista = [name_case(S) || S <- Tokens],
	string:join(Lista, " ").

%% @doc Converte boolean binário para o atom true|false
binary_to_bool(true) -> true;
binary_to_bool(false) -> false;
binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false;
binary_to_bool(<<"1">>) -> true;
binary_to_bool(<<"0">>) -> false;
binary_to_bool(<<1>>) -> true;
binary_to_bool(<<0>>) -> false;
binary_to_bool(_) -> false.

binary_to_integer(Bin) -> list_to_integer(binary_to_list(Bin)).

%% @doc Retorna o mês por extenso a partir do ordinal
mes_extenso(1) -> "Janeiro";
mes_extenso(2) -> "Fevereiro";
mes_extenso(3) -> "Março";
mes_extenso(4) -> "Abril";
mes_extenso(5) -> "Maio";
mes_extenso(6) -> "Junho";
mes_extenso(7) -> "Julho";
mes_extenso(8) -> "Agosto";
mes_extenso(9) -> "Setembro";
mes_extenso(10) -> "Outubro";
mes_extenso(11) -> "Novembro";
mes_extenso(12) -> "Dezembro";
mes_extenso(_) -> erlang:error(badarg).

%% @doc Retorna um ets a partir de uma lista
list_to_ets(List, Name, Options) ->
	Ets = ets:new(Name, Options),
	lists:foreach(fun(X) -> ets:insert(Ets, X) end, List),
	Ets.
	
profile() ->
	fprof:trace([stop]),
	fprof:profile(),
	fprof:analyse([totals, {dest, "fprof.txt"}]).

new_rowid_service(<<Url/binary>>, <<Type/binary>>) ->	
	[PrefixUrl|Url2] = binary_to_list(Url),
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, list_to_binary(Url2)]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end;

new_rowid_service(Url, Type) ->	
	[PrefixUrl|Url2] = Url,
	case PrefixUrl of
		$^ -> iolist_to_binary([Type, <<"#">>, Url2]);
		_  -> iolist_to_binary([Type, <<"#">>, Url])
	end.

make_rowid_from_url(<<Url/binary>>, <<Type/binary>>) ->	
	make_rowid_from_url(binary_to_list(Url), binary_to_list(Type));

make_rowid_from_url(Url, Type) ->	
	Ret1 = parse_url(Url),
	Ret2 = lists:map(fun({U, _}) -> U end, Ret1),
	Ret3 = string:join(Ret2, "/"),
	iolist_to_binary([Type, <<"#/">>, Ret3]).

get_rowid_and_params_from_url(<<Url/binary>>, <<Type/binary>>) ->	
	get_rowid_and_params_from_url(binary_to_list(Url), binary_to_list(Type));

get_rowid_and_params_from_url(Url, Type) ->
	UrlParsed = parse_url(Url),
	UrlParsed2 = lists:map(fun({U, _}) -> U end, UrlParsed),
	UrlParsed3 = string:join(UrlParsed2, "/"),
	Rowid = iolist_to_binary([Type, <<"#/">>, UrlParsed3]),
	ParamsUrl = [{list_to_binary(U), P} || {[_|U], P} <- UrlParsed, P /= [] ],
	ParamsUrlMap = maps:from_list(ParamsUrl),
	{Rowid, ParamsUrlMap}.
	

get_params_from_url(Url) -> [X || {_, P} = X <- parse_url(Url), P /= [] ].


parse_url(Url) ->	
	Url2 = string:tokens(Url, "/"),
	try
		parse_url_tail(Url2, 1, [])
	catch error:badarg ->
		erlang:error(einvalid_id_object)
	end.

parse_url_tail([], _, L) -> lists:reverse(L);
	
parse_url_tail([H|T], SeqId, L) ->	
    {UrlParte, Param, SeqId2} = parse_parte_url(H, SeqId),
	parse_url_tail(T, SeqId2, [{UrlParte, Param} | L]).
	
parse_parte_url([H|_] = UrlParte, SeqId) ->
	if
		H >= 49 andalso H =< 57 ->
			SeqId_ = case SeqId of
				1 -> ":id";
				_ -> ":id_" ++ integer_to_list(SeqId)
			end,
			{SeqId_, list_to_integer(UrlParte), SeqId+1};
		H =:= 45 ->
			erlang:error(einvalid_id_object);
		true -> {UrlParte, [], SeqId}
	end.


string_is_integer(S) ->
    try
        _ = list_to_integer(S),
        true
    catch error:badarg ->
        false
    end.

read_file_as_map(FileName) ->
	case file:read_file(FileName) of
		{ok, Arq} -> 
			case json_decode_as_map(Arq) of
				{ok, Json} -> {ok, Json};
				_ -> {error, enojsonformat}
			end;
		Error -> Error
	end.

node_is_live(Node) -> 
	case net_adm:ping(Node) of
		pong -> 1;
		_ -> 0
	end.

% Retorna somente a parte do name do node sem a parte do hostname após @
get_node_name() -> hd(string:tokens(atom_to_list(node()), "@")).

json_field_format_table([]) -> null;
json_field_format_table("0.0") -> "0.0";
json_field_format_table(Value) when is_float(Value) -> Value;
json_field_format_table(Value) when is_integer(Value) -> Value;
json_field_format_table(Value) when is_boolean(Value) -> Value;
json_field_format_table(null) -> null;
json_field_format_table(Data = {{_,_,_},{_,_,_}}) -> date_to_string(Data);
json_field_format_table(Value) when is_list(Value) -> json_field_strip_and_escape(utf8_list_to_string(Value));
json_field_format_table(Value) -> throw({error, {"Could not serialize the value ", [Value]}}).

json_field_strip_and_escape([]) ->	null;
json_field_strip_and_escape(Value) -> 
	case string:strip(Value) of
		[] -> null;
		V -> [case Ch of 
					34 -> "\\\""; 
					_ -> Ch 
			  end || Ch <- V]
	end.


json_encode_table(Fields, Records) ->
	Objects = lists:map(fun(T) -> 
							   lists:zipwith(fun(Fld, Value) -> 
													io_lib:format(<<"\"~s\":~p"/utf8>>, [Fld, json_field_format_table(Value)]) 
											 end,  Fields, tuple_to_list(T))
					end, Records), 
	Objects2 = lists:map(fun(Obj) -> 
									lists:flatten(["{", string:join(Obj, ", "), "}"]) 
						 end, Objects),
	Objects3 = string:join(Objects2, ", "),
	Result = unicode:characters_to_binary(["[", Objects3, "]"]),
	Result.


utf8_list_to_string(Value) ->
	try
		case check_encoding_bin(list_to_binary(Value)) of
			utf8 -> unicode:characters_to_list(mochiutf8:valid_utf8_bytes(list_to_binary(Value)));
			latin1 -> unicode:characters_to_list(Value, utf8)
		end
	catch
		_Exception:Reason -> 
			io:format("utf8_list_to_string error ~p with value ~p\n", [Reason, Value]), 
			<<>>
	end.
	

utf8_list_to_binary(Value) -> binary_to_list(utf8_list_to_string(Value)).

utf8_binary_to_list(Value) ->
	case unicode:characters_to_list(Value) of
		{error, _, _ } -> Value;
		Value2 -> Value2
	end.


check_encoding_bin(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin,utf8,utf8) of
	Bin ->
	    utf8;
	_ ->
	    latin1
    end.


