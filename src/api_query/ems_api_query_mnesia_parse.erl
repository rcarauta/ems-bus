%%********************************************************************
%% @title Module ems_api_query_mnesia_parse
%% @version 1.0.0
%% @doc It provides parse api query mnesia
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_api_query_mnesia_parse).

-export([generate_dynamic_query/6, generate_dynamic_query/3]).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").


generate_dynamic_query(FilterJson, Fields, _Datasource, Limit, Offset, _Sort) ->
	FieldList = parse_fields(Fields),
	FilterList = parse_filter(FilterJson),
	%SortSmnt = parse_sort(Sort),
	LimitSmnt = parse_limit(Limit, Offset),
	{ok, {FieldList, FilterList, LimitSmnt}}.

generate_dynamic_query(_Id, Fields, _Datasource) ->
	FieldList = parse_fields(Fields),
	{ok, FieldList}.

   
parse_fields([]) -> [];
parse_fields(Fields) -> 
	case string:tokens(string:strip(Fields), ",") of
		[] -> erlang:error(einvalid_fields);
		Fields2 -> string:join(Fields2, ",")
	end.

parse_filter(<<>>) -> [];
parse_filter(Filter) ->    
    case ems_util:json_decode(Filter) of
		{ok, Filter2} -> parse_filter(Filter2, []);
		_ -> erlang:error(einvalid_filter)
	end.


parse_filter([], []) -> [];
parse_filter([], Filter) -> lists:reverse(Filter);
parse_filter([H|T], Filter) ->
	{Param, Op, Value} = parse_condition(H),
	parse_filter(T, [{Param, Op, Value} | Filter]).

	
parse_condition({<<Param/binary>>, Value}) when is_integer(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_integer);
parse_condition({<<Param/binary>>, Value}) when is_boolean(Value) -> 
	Param2 = binary_to_list(Param), 
	parse_condition(Param2, Value, sql_boolean);
parse_condition({<<Param/binary>>, Value}) -> 
	Param2 = binary_to_list(Param), 
	Value2 = binary_to_list(Value),
	parse_condition(Param2, Value2, sql_varchar).
	
parse_condition(Param, Value, _DataType) -> 
	{Param2, Op} = parse_name_and_operator(Param),
	{Param2, Op, Value}.	


parse_name_and_operator(Param) ->
	case string:str(Param, "__") of
		Idx when Idx > 1 ->
			Name = string:sub_string(Param, 1, Idx-1),
			Op = string:sub_string(Param, Idx+2),
			case lists:member(Op, ["e", "ne", "gt", "gte", "lt", "lte"]) of
				true -> 
					Op2 = format_mnesia_operator(Op),
					{Name, Op2};
				_ -> throw({einvalid_condition, Param})
			end;
		0 -> {Param, "=="};
		_ -> throw({einvalid_condition, Param})
	end.


format_mnesia_operator("e") -> "==";
format_mnesia_operator("ne") -> "=/=";
format_mnesia_operator("gt") -> ">";
format_mnesia_operator("gte") -> ">=";
format_mnesia_operator("lt") -> "<" ;
format_mnesia_operator("lte") -> "=<";
format_mnesia_operator(_) -> erlang:error(invalid_operator_filter).


parse_limit(Limit, Offset) when Limit > 0, Offset >= 0, Limit =< ?MAX_LIMIT_API_QUERY, Offset =< ?MAX_OFFSET_API_QUERY -> {Limit, Offset};
parse_limit(_, _) -> erlang:error(einvalid_limit_filter).



	

