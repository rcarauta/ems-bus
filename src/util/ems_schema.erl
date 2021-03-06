%%********************************************************************
%% @title Módule ems_schema
%% @version 1.0.0
%% @doc Module ems_schema
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_schema).

-compile(export_all).
-compile({parse_transform, exprecs}).

-include("../include/ems_schema.hrl").

-export([to_record/2, to_list/1, to_list/2, to_json/1, new/1, new_/1]).

-export_records([user, catalog_schema, schema_type, produto]).


% to_record
to_record(Json, Type) when is_binary(Json), is_atom(Type) ->
	{ok, JsonMap} = ems_util:json_decode(Json),
	JsonStruct = {struct, JsonMap},
	NewRecord = new(Type),
	json_rec:to_rec(JsonStruct, ?MODULE, NewRecord);
to_record(Json, Record) when is_binary(Json), is_tuple(Record)->
	{ok, JsonMap} = ems_util:json_decode(Json),
	JsonStruct = {struct, JsonMap},
	json_rec:to_rec(JsonStruct, ?MODULE, Record);
to_record(Map, Type) when is_map(Map), is_atom(Type) ->
	List = maps:to_list(Map),
	JsonStruct = {struct, List},
	NewRecord = new(Type),
	json_rec:to_rec(JsonStruct, ?MODULE, NewRecord);
to_record(Map, Record) when is_map(Map), is_tuple(Record)->
	List = maps:to_list(Map),
	JsonStruct = {struct, List},
	json_rec:to_rec(JsonStruct, ?MODULE, Record);
to_record(DestRecord, Source) when is_tuple(DestRecord) ->
	ListRec = to_list(DestRecord),
	MapRec = maps:from_list(ListRec),
	to_record(MapRec, Source);
to_record(_, _) -> erlang:error(einvalid_to_record).


% to_list (all fields)
to_list(Record) when is_tuple(Record)-> 
	try
		{struct, Result} = json_rec:to_json(Record, ?MODULE),
		lists:reverse(Result)
	catch 
		_Exception:_Reason -> lists:reverse(to_list_tuple(Record))
	end;
to_list(Type) when is_atom(Type)-> 
	NewRecord = new(Type),
	to_list(NewRecord);
to_list(ListRecord) when is_list(ListRecord)-> 
    F = fun(Record) -> 
		{struct, Result} = json_rec:to_json(Record, ?MODULE),
		Result
	end,
	lists:map(F, ListRecord);
to_list(_) -> erlang:error(einvalid_to_list).


to_list_tuple(Tuple) ->
	case size(Tuple) rem 2 == 0 of
		true ->  
			T2 = tuple_to_binlist(Tuple),
			to_list_tuple(T2, []);
		_ -> erlang:error(einvalid_to_list)
	end.


tuple_to_binlist(T) ->
	L = tuple_to_list(T),
	list_to_binlist(L).

list_to_binlist([]) -> [];
list_to_binlist(<<>>) -> [];
list_to_binlist(<<V/binary>>) -> [V];
list_to_binlist([H|T]) -> [item_to_binary(H)|list_to_binlist(T)].

item_to_binary([]) -> <<>>;
item_to_binary(<<I/binary>>) -> I;
item_to_binary(T) when is_tuple(T) -> 
	tuple_to_binlist(T);
item_to_binary(L) when is_list(L) -> 
	case io_lib:printable_list(L) of
		true -> 
			L2 = [case Ch of 
					34 -> "\\\""; 
					_ -> Ch 
				  end || Ch <- L],
			iolist_to_binary(L2);
		false -> list_to_binlist(L)
	end;
item_to_binary(I) when is_integer(I) -> I;
item_to_binary(I) when is_float(I) -> I;
item_to_binary(I) when is_atom(I) -> 
	[I2] = io_lib:format("~p", [I]),
	iolist_to_binary(I2);
item_to_binary(I) when is_map(I) -> I;
item_to_binary(I) -> iolist_to_binary(I).


to_list_tuple([], L) ->	L;	
to_list_tuple([F|[V|T]], L) ->	
	to_list_tuple(T, [{F, V} | L]).


% to_list (selective fields)
to_list(Record, FieldList) when is_tuple(Record) -> 
	Record2 = to_list(Record),
	[X || X <- Record2, lists:member(element(1, X), FieldList)];
to_list(ListRecord, FieldList) -> 
	to_list(ListRecord, FieldList, []).

to_list([], _, Result) -> Result;
to_list([H|T], FieldList, Result) ->
	List = to_list(H, FieldList),
	to_list(T, FieldList, [List | Result]).



% to_json
to_json(Record) when is_tuple(Record)-> 
	ListTuple = to_list(Record),
	iolist_to_binary([<<"{"/utf8>>, to_json_rec(ListTuple, []), <<"}"/utf8>>]);
to_json(List) when is_list(List) -> 
	iolist_to_binary([<<"["/utf8>>, to_json_list(List, []), <<"]"/utf8>>]).

	
to_json_rec([], L) -> lists:reverse(L);
to_json_rec([{F, V}|[]], L) -> 
    to_json_rec([], [[<<"\""/utf8>>, F, <<"\""/utf8>>, <<":"/utf8>>, to_value(V)] | L]);
to_json_rec([{F, V}|T], L) -> 
    to_json_rec(T, [[<<"\""/utf8>>, F, <<"\""/utf8>>, <<":"/utf8>>, to_value(V), <<","/utf8>>] | L]).

	
to_json_list([], L) ->	lists:reverse(L);
to_json_list([H|[]], L) ->
  to_json_list([], [to_json(H) | L]);
to_json_list([H|T], L) ->
	to_json_list(T, [[to_json(H), <<","/utf8>>] | L]).


to_value(<<"undefined">>) -> <<"null"/utf8>>;
to_value(<<>>) -> <<"null"/utf8>>;
to_value([]) -> <<"null"/utf8>>;
to_value(null) -> <<"null"/utf8>>;
to_value("0.0") -> <<"0.0"/utf8>>;
to_value(Value) when is_float(Value) -> list_to_binary(mochinum:digits(Value));
to_value(Value) when is_integer(Value) -> integer_to_binary(Value);
to_value(Value) when is_atom(Value) -> 
	[<<"\""/utf8>>, atom_to_list(Value), <<"\""/utf8>>];
to_value(Value) when is_binary(Value) -> 
	[<<"\""/utf8>>, Value, <<"\""/utf8>>];
to_value(true) -> <<"true"/utf8>>;
to_value(false) -> <<"false"/utf8>>;
to_value(Data = {{_,_,_},{_,_,_}}) -> 
	ems_util:date_to_string(Data);
to_value([<<Key/binary>>, <<Value/binary>>]) -> 
	[<<"{\""/utf8>>, Key, <<"\":\""/utf8>>, Value, <<"\"}"/utf8>>];
to_value(Value) when is_list(Value) -> 
	case io_lib:printable_list(Value) of 
		true ->	json_field_strip_and_escape(ems_util:utf8_list_to_string(Value));
		_ -> to_json(list_to_tuple(Value))
	end;
to_value(Value) when is_map(Value) ->
	ems_util:json_encode(Value);
to_value(Value) -> throw({error, {"Could not serialize the value ", [Value]}}).


json_field_strip_and_escape([]) ->	<<"null"/utf8>>;
json_field_strip_and_escape(<<>>) -> <<"null"/utf8>>;
json_field_strip_and_escape(Value) -> 
	case string:strip(Value) of
		[] -> <<"null"/utf8>>;
		ValueStrip -> 
			ValueEscaped = [case Ch of 
									34 -> "\\\""; 
									_ -> Ch 
							end || Ch <- ValueStrip],
			[<<"\""/utf8>>, ValueEscaped, <<"\""/utf8>>]
	end.


new(catalog_schema) -> #catalog_schema{};
new(user) -> #user{};
new(schema_type) -> #schema_type{};
new(produto) -> #produto{};
new(_) -> erlang:error(einvalid_type).


new_(catalog_schema) -> #catalog_schema{_ = '_'};
new_(user) -> #catalog_schema{_ = '_'};
new_(_) -> erlang:error(einvalid_type).
  
	
    



