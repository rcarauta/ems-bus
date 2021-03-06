%%********************************************************************
%% @title Module ems_db
%% @version 1.0.0
%% @doc Module that provides interface with the database.
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_db).

-export([start/0]).
-export([get/2, all/1, insert/1, update/1, delete/2, existe/1, match_object/1, 
		 match/2, find/2, find/3, find/5, find_by_id/3, filter/2, 
		 filter_with_limit/4, select_fields/2, 
		 find_first/2, find_first/3, find_first/4]).
-export([init_sequence/2, sequence/1, sequence/2, current_sequence/1]).
-export([init_counter/2, counter/2, current_counter/1, inc_counter/1, dec_counter/1]).
-export([get_connection/1, release_connection/1]).


-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").



%% *********** Database schema creation ************

start() ->
	create_database([node()]),
	ems_cache:new(ems_db_odbc_connection_cache),
	ems_cache:new(ems_db_parsed_query_cache).
	
create_database(Nodes) ->
	% Define a pasta de armazenamento dos databases
	filelib:ensure_dir(?DATABASE_PATH ++ "/"),
	application:set_env(mnesia, dir, ?DATABASE_PATH),

	mnesia:create_schema(Nodes),
	
	mnesia:start(),

    mnesia:create_table(user, [{type, set},
							   {disc_copies, Nodes},
							   {attributes, record_info(fields, user)}]),

    mnesia:create_table(sequence, [{type, set},
								   {disc_copies, Nodes},
								   {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(counter, [{type, set},
											 {ram_copies, Nodes},
											 {attributes, record_info(fields, sequence)}]),

    mnesia:create_table(request, [{type, set},
								  {ram_copies, Nodes},
								  {attributes, record_info(fields, request)},
								  {index, [#request.timestamp]}]),

    mnesia:create_table(ctrl_sqlite_table, [{type, set},
											{disc_copies, Nodes},
											{attributes, record_info(fields, ctrl_sqlite_table)}]),

    mnesia:create_table(catalog_schema, [{type, set},
										 {disc_copies, Nodes},
										 {attributes, record_info(fields, catalog_schema)}]),

    mnesia:create_table(produto, [{type, set},
	 							  {disc_copies, Nodes},
								  {attributes, record_info(fields, produto)}]),

	ok.



%% *********** Functions for CRUD ************

get(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	get(RecordType, Id2);

get(RecordType, Id) when is_number(Id) ->
	Query = fun() ->
		mnesia:read(RecordType, Id)
	end,
	case mnesia:transaction(Query) of
		{atomic, []} -> {error, enoent};
		{atomic, [Record|_]} -> {ok, Record};
		{aborted, _Reason} -> {erro, aborted}
	end;

get(_RecordType, _) -> {error, enoent}.


all(RecordType) ->
	Query = fun() ->
		  qlc:e(
			 qlc:q([X || X <- mnesia:table(RecordType)])
		  )
	   end,
	{atomic, Records} = mnesia:transaction(Query),
	{ok, Records}.

insert(Record) ->
	RecordType = element(1, Record),
	F = fun() ->
		case element(2, Record) of
			undefined ->
				Id = sequence(RecordType),
				Record1 = setelement(2, Record, Id),
				mnesia:write(Record1),
				Record1;
			Id -> 
				case mnesia:read(RecordType, Id) of
					[] -> mnesia:write(Record),
						  Record;
					_ -> {error, ealready_exist}
				end
		end
	end,		
	{atomic, Result} = mnesia:transaction(F),
	{ok, Result}.

update(Record) ->
	Write = fun() -> mnesia:write(Record) end,
	mnesia:transaction(Write),
	ok.

delete(RecordType, Id) when is_list(Id) ->
	Id2 = list_to_integer(Id),
	delete(RecordType, Id2);
	
delete(RecordType, Id) ->
	Delete = fun() -> mnesia:delete({RecordType, Id}) end,
	mnesia:transaction(Delete),
	ok.

%% @doc Verifica se um registro existe
match_object(Pattern) ->	
	{atomic, Records} = mnesia:transaction(fun() -> 
												mnesia:match_object(Pattern) 
										   end),
	Records.


    


%% @doc Verifica se um registro existe
existe(Pattern) ->	
	case match_object(Pattern) of
		[] -> false;
		_ -> true
	end.

	

%% ************* Funções para gerar sequences *************

init_sequence(Name, Value) ->
     {atomic, ok} =	mnesia:transaction(fun() ->
						mnesia:write(#sequence{key=Name, index=Value})
					end),
     ok.

sequence(Name) ->  mnesia:dirty_update_counter(sequence, Name, 1).

current_sequence(Name) -> mnesia:dirty_update_counter(sequence, Name, 0).

sequence(Name, Inc) -> mnesia:dirty_update_counter(sequence, Name, Inc).


%% ************* Funções para gerar counters *************

init_counter(Name, Value) ->
     {atomic, ok} =	mnesia:transaction(fun() ->
						mnesia:write(#counter{key=Name, index=Value})
					end),
     ok.

inc_counter(Name) ->  mnesia:dirty_update_counter(counter, Name, 1).
dec_counter(Name) ->  mnesia:dirty_update_counter(counter, Name, -1).
current_counter(Name) -> mnesia:dirty_update_counter(counter, Name, 0).
counter(Name, Inc) -> mnesia:dirty_update_counter(counter, Name, Inc).
     



% Get the connection from a datasource (sqlserver, sqlite, ou mnesia)
get_connection(Datasource = #service_datasource{type = sqlserver}) ->
	get_odbc_connection(Datasource);
get_connection(Datasource = #service_datasource{type = csvfile}) ->
	get_odbc_connection_csv_file(Datasource);
get_connection(Datasource = #service_datasource{type = mnesia}) ->
	{ok, Datasource}.

% Release a conection from a datasource
release_connection(#service_datasource{type = mnesia}) -> ok;
release_connection(Datasource) -> ems_odbc_pool:release_connection(Datasource).


get_odbc_connection(Datasource) -> ems_odbc_pool:get_connection(Datasource).


get_odbc_connection_csv_file(Datasource = #service_datasource{connection = FileName,
															  table_name = TableName,
															  csv_delimiter = Delimiter}) -> 
	FileNamePath = ?CSV_FILE_PATH ++ "/" ++ FileName,
	case filelib:last_modified(FileNamePath) of
		0 -> {error, ecsvfile_not_exist};
		LastModified ->
			DatabaseExist = filelib:is_file(?DATABASE_SQLITE_PATH), 
			F = fun() ->
				Ctrl = ems_util:hd_or_empty(mnesia:read(ctrl_sqlite_table, FileName)),
				case Ctrl =:= [] orelse not DatabaseExist orelse Ctrl#ctrl_sqlite_table.last_modified =/= LastModified of
					true ->
						Csv2SqliteCmd = lists:flatten(io_lib:format("~s '~s\' '~s' '~s' '~s'",
																	 [?CSV2SQLITE_PATH,
																	  ?DATABASE_SQLITE_PATH, 
																	  TableName, 
																	  FileNamePath, 
																	  Delimiter])),
						ems_logger:info("OS command: ~p.", [Csv2SqliteCmd]),
						os:cmd(Csv2SqliteCmd),
						mnesia:write(#ctrl_sqlite_table{file_name = FileName, last_modified = LastModified});
					false -> 
						% não foi necessário fazer a carga dos dados do arquivo para o banco sqlite
						ok
				end
			end,
			mnesia:activity(transaction, F),
			get_odbc_connection(Datasource#service_datasource{type = sqlite, connection = ?DATABASE_SQLITE_STRING_CONNECTION})
	end.


%create_sqlite_virtual_table_from_csv_file(FileName, TableName, _PrimaryKey) -> 
%	{ok, Conn} = ems_db:get_odbc_connection("DRIVER=SQLite;Version=3;New=True;"),
%	odbc:sql_query(Conn, "select load_extension(\"/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so\")"),
%	CreateTableDDL = lists:flatten(io_lib:format("create virtual table ~s using csvtable(\"~s\")", [TableName, FileName])),
%	odbc:sql_query(Conn, CreateTableDDL),
%	odbc:commit(Conn, commit),
%	{ok, Conn}.


%
% Find object by id
% Ex.: ems_db:find_by_id(catalog_schema, 1, [id, name]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find_by_id(Tab, Id, FieldList) ->
	case get(Tab, Id) of
		{ok, Record} -> select_fields(Record, FieldList);
		Error -> Error
	end.

%
% Find objects
% Ex.: ems_db:find(catalog_schema, [{id, "==", 1}]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find(Tab, FilterList) -> find(Tab, [], FilterList).

%
% Find objects
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find(Tab, FieldList, FilterList) ->
    Records = filter(Tab, FilterList),
	select_fields(Records, FieldList).


%
% Find objects with limits
% Ex.: ems_db:find(catalog_schema, [id, name], [{id, "==", 1}], 1, 1).
% Sample result is [[{<<"id">>,1},{<<"name">>,<<"exemplo">>}]]
%
find(Tab, FieldList, FilterList, Limit, Offset) ->
    Records = filter_with_limit(Tab, FilterList, Limit, Offset),
	select_fields(Records, FieldList).


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [{id, "==", 1}]).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
find_first(Tab, FilterList) -> find_first(Tab, [], FilterList).


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [id, name], [{id, "==", 1}]).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
find_first(Tab, FieldList, FilterList) ->
    case filter_with_limit(Tab, FilterList, 1, 1) of
		[] -> {error, enoent};
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end.


%
% Find first objects
% Ex.: ems_db:find_first(catalog_schema, [id, name], [{id, "==", 1}], 1).
% Sample result is [{<<"id">>,1},{<<"name">>,<<"exemplo">>}]
%
find_first(Tab, FieldList, FilterList, Offset) ->
    case filter_with_limit(Tab, FilterList, 1, Offset) of
		[] -> {error, enoent};
		[FirstRecord|_] -> select_fields(FirstRecord, FieldList)
	end.
	


%	
% Filter objects
% Ex.: ems_db:filter(catalog_schema, [{id, "==", 1}]). 
% Sample result is 
%[{catalog_schema,1,<<"exemplo">>,<<"schema de exemplo">>,
%                 #{<<"properties">> => #{<<"age">> => #{<<"description">> => <<"Age in years">>,
%                       <<"minimum">> => 0,
%                       <<"type">> => <<"integer">>},
%                     <<"firstName">> => #{<<"type">> => <<"string">>},
%                     <<"lastName">> => #{<<"type">> => <<"string">>}},
%                   <<"required">> => [<<"firstName">>,<<"lastName">>],
%                   <<"title">> => <<"Example Schema">>,
%                   <<"type">> => <<"object">>}}]
%
filter(Tab, []) -> 
	F = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(Tab)])
		  )
	   end,
	mnesia:activity(transaction, F);
filter(Tab, [{F1, "==", V1}]) ->
	Fields =  mnesia:table_info(catalog_schema, attributes),
	Fld1 = field_index(F1, Fields, 2),
	Fun = fun() -> 
				qlc:e(qlc:q([R || R <- mnesia:table(Tab), element(Fld1, R) == field_value(V1)])) 
		  end,
	mnesia:activity(transaction, Fun);
filter(Tab, FilterList) when is_list(FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join(lists:map(fun({F, Op, V}) ->
												Fld = field_index(F, FieldsTable, 2),
												io_lib:format("element(~s, R) ~s ~p", [integer_to_list(Fld), Op,  field_value(V)])
										  end, FilterList), ","),
			ExprQuery = lists:flatten(io_lib:format("[R || R <- mnesia:table(~p), ~s].", [Tab, Where])),
			ParsedQuery = qlc:string_to_handle(ExprQuery),
			mnesia:activity(transaction, fun () -> qlc:eval(ParsedQuery) end)
		end,
	ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList}, F);
filter(Tab, FilterTuple) when is_tuple(FilterTuple) ->
	filter(Tab, [FilterTuple]).



%	
% Filter objects with limit
% Ex.: ems_db:filter_with_limit(catalog_schema, [{id, "==", 1}], 1, 1). 
% Sample result is 
%[{catalog_schema,1,<<"exemplo">>,<<"schema de exemplo">>,
%                 #{<<"properties">> => #{<<"age">> => #{<<"description">> => <<"Age in years">>,
%                       <<"minimum">> => 0,
%                       <<"type">> => <<"integer">>},
%                     <<"firstName">> => #{<<"type">> => <<"string">>},
%                     <<"lastName">> => #{<<"type">> => <<"string">>}},
%                   <<"required">> => [<<"firstName">>,<<"lastName">>],
%                   <<"title">> => <<"Example Schema">>,
%                   <<"type">> => <<"object">>}}]
%
filter_with_limit(Tab, [], Limit, Offset) -> 
	F = fun() ->
		  qlc:e(
			 qlc:q([R || R <- mnesia:table(Tab), element(2, R) >= Offset, element(2, R) =< Limit + Offset - 1])
		  )
	   end,
	mnesia:activity(transaction, F);
filter_with_limit(Tab, [{F1, "==", V1}], Limit, Offset) ->
	Fields =  mnesia:table_info(catalog_schema, attributes),
	Fld1 = field_index(F1, Fields, 2),
	Fun = fun() -> 
				Records = qlc:e(qlc:q([R || R <- mnesia:table(Tab), element(Fld1, R) == field_value(V1)])),
				lists:sublist(Records, Offset, Limit) 
		  end,
	mnesia:activity(transaction, Fun);
filter_with_limit(Tab, FilterList, Limit, Offset) when is_list(FilterList) -> 
	F = fun() ->
			FieldsTable =  mnesia:table_info(Tab, attributes),
			Where = string:join(lists:map(fun({F, Op, V}) ->
												Fld = field_index(F, FieldsTable, 2),
												io_lib:format("element(~s, R) ~s ~p", [integer_to_list(Fld), Op,  field_value(V)])
										  end, FilterList), ","),
			ExprQuery = lists:flatten(io_lib:format("[R || R <- mnesia:table(~p), ~s].", [Tab, Where])),
			ParsedQuery = qlc:string_to_handle(ExprQuery),
			mnesia:activity(transaction, fun () -> 
											Records = qlc:eval(ParsedQuery),
											lists:sublist(Records, Offset, Limit) 
										 end)
		end,
	ems_cache:get(ems_db_parsed_query_cache, ?LIFE_TIME_PARSED_QUERY, {Tab, FilterList, Limit, Offset}, F);
filter_with_limit(Tab, FilterTuple, Limit, Offset) when is_tuple(FilterTuple) -> 	
	filter_with_limit(Tab, [FilterTuple], Limit, Offset).



% match objects and faster than filter
% Ex.: ems_db:match(catalog_schema, [{id, 1}]).
% Sample result is like filter function
match(Tab, FilterList) -> 
	FieldsTable =  mnesia:table_info(Tab, attributes),
	Record = ems_schema:new_(Tab),
	Match = match(Tab, FilterList, FieldsTable, Record),
	mnesia:activity(transaction, fun() -> mnesia:match_object(Match) end).
		
match(_, [], _, Record) -> Record;
match(Tab, [{F, _, V}|T], FieldsTable, Record) -> 
	Fld = field_index(F, FieldsTable, 2),
	Record2 = setelement(Fld, Record, field_value(V)),
    match(Tab, T, FieldsTable, Record2);
match(Tab, [{F, V}|T], FieldsTable, Record) -> 
	Fld = field_index(F, FieldsTable, 2),
	Record2 = setelement(Fld, Record, field_value(V)),
    match(Tab, T, FieldsTable, Record2).


% select fields of object or list objects
% Ex.: ems_db:select_fields(#user{id = 1, name = "agilar", email = "evertonagilar@gmail.com"}, [name]).
% Sample result is [{<<"name">>,"agilar"}]
select_fields(ListRecord, []) -> ListRecord;
select_fields(ListRecord, FieldList) -> 
    FieldList2 = ems_util:list_to_binlist(FieldList),
	ems_schema:to_list(ListRecord, FieldList2).


field_index(_, [], _) -> erlang:error(einvalid_field_filter);
field_index(Field, Fields, Idx) when is_list(Field) -> 
	field_index(list_to_atom(Field), Fields, Idx);
field_index(Field, [F|Fs], Idx) ->
	case Field == F of
		true -> Idx;
		_ -> field_index(Field, Fs, Idx+1)
	end.


field_value(V) when is_list(V) -> list_to_binary(V);
field_value(V) -> V.


