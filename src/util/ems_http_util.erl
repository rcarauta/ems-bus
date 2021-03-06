%%********************************************************************
%% @title Module ems_http_util
%% @version 1.0.0
%% @doc Module with useful functions for the HTTP server
%% @author Everton de Vargas Agilar <evertonagilar@gmail.com>
%% @copyright ErlangMS Team
%%********************************************************************

-module(ems_http_util).

-compile(export_all).

-include("../../include/ems_config.hrl").
-include("../../include/ems_schema.hrl").
-include("../../include/ems_http_messages.hrl").


encode_request(Uri) -> encode_request("GET", Uri).

encode_request(Method, Uri) -> 
	Method = Method,
	UriRaw = http_uri:encode(Uri),
	HttpParams = #{'Accept' => "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
				   'Accept-Encoding' => "gzip, deflate, sdch",
				   'Accept-Language' => "pt-BR,pt;q=0.8,en-US;q=0.6,en;q=0.4",
				   'Cache-Control' => "max-age=0",
				   'Connection' => "keep-alive",
				   'Cookie' => "__utma=111872281.1848474434.1463598791.1464613625.1464618604.7; __utmz=111872281.1463598791.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); pin_nav=true",
				   'Host' => "localhost:2301",
				   'User-Agent' => "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.29 Safari/537.36",
				   "Dnt" => "1",
				   "Upgrade-Insecure-Requests" => "1"},
	Http_Version = "HTTP/1.1",
	Payload = <<>>,
	encode_request(Method, UriRaw, HttpParams, Http_Version, Payload, undefined, undefined).

encode_request(Method, UriRaw, HttpParams, Http_Version, Payload, Socket, WorkerSend) ->
	try
		Uri = unicode:characters_to_list(mochiutf8:valid_utf8_bytes(list_to_binary(mochiweb_util:unquote(UriRaw))), utf8),
		RID = erlang:system_time(),
		Timestamp = calendar:local_time(),
		T1 = ems_util:get_milliseconds(),
		[Url|Querystring] = string:tokens(Uri, "?"),
		case length(Querystring) =< 1 of
			 true ->
				Url2 = ems_util:remove_ult_backslash_url(Url),
				Content_Length = maps:get('Content-Length', HttpParams, 0),
				Content_Type = maps:get('Content-Type', HttpParams, "application/json"),
				Accept = maps:get('Accept', HttpParams, "*/*"),
				Accept_Encoding = maps:get('Accept-Encoding', HttpParams, ""),
				User_Agent = maps:get('User-Agent', HttpParams, ""),
				Cache_Control = maps:get('Cache-Control', HttpParams, "false"),
				Host = maps:get('Host', HttpParams, ""),
				QuerystringMap = parse_querystring(Querystring),
				Authorization = maps:get('Authorization', HttpParams, ""),
				{Rowid, Params_url} = ems_util:hashsym_and_params(Url2),
				Request = #request{
					rid = RID,
					rowid = Rowid,
					type = Method,
					uri = Uri,
					url = Url2,
					version = Http_Version,
					querystring = Querystring,
					querystring_map = QuerystringMap,
					params_url = Params_url,
					content_length = Content_Length,
					content_type = Content_Type,
					accept = Accept,
					user_agent = User_Agent,
					accept_encoding = Accept_Encoding,
					cache_control = Cache_Control,
					host = Host,
					socket = Socket, 
					t1 = T1, 
					payload = binary_to_list(Payload), 
					payload_map = decode_payload(Payload),
					timestamp = Timestamp,
					authorization = Authorization,
					worker_send = WorkerSend,
					protocol = http
				},	
				{ok, Request};
			_ -> {error, einvalid_querystring}
		end
	catch
		_Exception:Reason -> {error, Reason}
	end.

encode_request_mochiweb(MochiReq, WorkerSend) ->
	try
		UriRaw = MochiReq:get(raw_path),
		Uri = unicode:characters_to_list(mochiutf8:valid_utf8_bytes(list_to_binary(mochiweb_util:unquote(UriRaw))), utf8),
		RID = erlang:system_time(),
		Timestamp = calendar:local_time(),
		T1 = ems_util:get_milliseconds(),
		[Url|Querystring] = string:tokens(Uri, "?"),
		Payload = MochiReq:recv_body(),
		case length(Querystring) =< 1 of
			 true ->
				Url2 = ems_util:remove_ult_backslash_url(Url),
				Content_Length = MochiReq:get(body_length),
				Content_Type = MochiReq:get_header_value("content-type"),
				Accept = MochiReq:get_header_value("accept"),
				Accept_Encoding = MochiReq:get_header_value("accept-encoding"),
				User_Agent = MochiReq:get_header_value("user-agent"),
				Cache_Control = MochiReq:get_header_value("cache-control"),
				Host = MochiReq:get(peer),
				QuerystringMap = parse_querystring(Querystring),
				Authorization = MochiReq:get_header_value("authorization"),
				{Rowid, Params_url} = ems_util:hashsym_and_params(Url2),
				Request = #request{
					rid = RID,
					rowid = Rowid,
					type = atom_to_list(MochiReq:get(method)),
					uri = Uri,
					url = Url2,
					version = MochiReq:get(version),
					querystring = Querystring,
					querystring_map = QuerystringMap,
					params_url = Params_url,
					content_length = Content_Length,
					content_type = Content_Type,
					accept = Accept,
					user_agent = User_Agent,
					accept_encoding = Accept_Encoding,
					cache_control = Cache_Control,
					host = Host,
					socket = MochiReq:get(socket), 
					t1 = T1, 
					payload = Payload, 
					payload_map = decode_payload(Payload),
					timestamp = Timestamp,
					authorization = Authorization,
					worker_send = WorkerSend,
					protocol = http,
					result_cache = false
				},	
				{ok, Request};
			_ -> {error, einvalid_querystring}
		end
	catch
		_Exception:Reason -> {error, Reason}
	end.


encode_request_cowboy(CowboyReq, WorkerSend) ->
	try
		Url2 = binary_to_list(cowboy_req:path(CowboyReq)),
		RID = erlang:system_time(),
		Timestamp = calendar:local_time(),
		T1 = ems_util:get_milliseconds(),
		Method = binary_to_list(cowboy_req:method(CowboyReq)),
		case cowboy_req:qs(CowboyReq) of
			<<>> -> 
				Querystring = <<>>,
				QuerystringMap = #{};
			Q -> 
				Querystring = binary_to_list(Q),
				QuerystringMap = parse_querystring([Querystring])
		end,
		{ok, Payload, _Req} = cowboy_req:read_body(CowboyReq),
		Content_Length = cowboy_req:body_length(CowboyReq),
		Content_Type = cowboy_req:header(<<"content-type">>, CowboyReq),
		Accept = cowboy_req:header(<<"accept">>, CowboyReq),
		Accept_Encoding = cowboy_req:header(<<"accept-encoding">>, CowboyReq),
		User_Agent = cowboy_req:header(<<"user-agent">>, CowboyReq),
		Cache_Control = cowboy_req:header(<<"cache-control">>, CowboyReq),
		Host = cowboy_req:host(CowboyReq),
		Authorization = cowboy_req:header(<<"authorization">>, CowboyReq),
		IfModifiedSince = cowboy_req:header(<<"if-modified-since">>, CowboyReq),
		IfNoneMatch = cowboy_req:header(<<"if-none-match">>, CowboyReq),
		PayloadMap = decode_payload(Payload),
		UrlHash = erlang:phash2(Url2),
		{Rowid, Params_url} = ems_util:hashsym_and_params(Url2),
		Request = #request{
			rid = RID,
			rowid = Rowid,
			type = Method,
			uri = Url2,
			url = Url2,
			version = cowboy_req:version(CowboyReq),
			querystring = Querystring,
			querystring_map = QuerystringMap,
			params_url = Params_url,
			content_length = Content_Length,
			content_type = Content_Type,
			accept = Accept,
			user_agent = User_Agent,
			accept_encoding = Accept_Encoding,
			cache_control = Cache_Control,
			host = Host,
			payload = Payload, 
			payload_map = PayloadMap,
			timestamp = Timestamp,
			authorization = Authorization,
			worker_send = WorkerSend,
			if_modified_since = IfModifiedSince,
			if_none_match = IfNoneMatch,
			protocol = http,
			result_cache = false,
			t1 = T1,
			url_hash = UrlHash
		},	
		{ok, Request}
	catch
		_Exception:Reason -> {error, Reason}
	end.


-spec parse_if_modified_since(binary() | undefined) -> calendar:datetime().
parse_if_modified_since(undefined) -> undefined;
parse_if_modified_since(IfModifiedSince) -> cow_date:parse_date(IfModifiedSince).


%%-spec get_http_header(Header::list()) -> tuple.
encode_request(Socket, RequestBin, WorkerSend) ->
	case decode_http_request(RequestBin) of
		{Method, UriRaw, HttpParams, Http_Version, Payload} -> 
			encode_request(Method, UriRaw, HttpParams, 
						   Http_Version, Payload, 
						   Socket, WorkerSend);
		Error -> 
			ems_logger:error("Error in request: ~p\n", [RequestBin]),
			Error
	end.


%% @doc Gera o response HTTP
encode_response(<<Codigo/binary>>, <<Payload/binary>>, <<MimeType/binary>>) ->
	encode_response(Codigo, Payload, MimeType, undefined).
	
encode_response(<<Codigo/binary>>, <<Payload/binary>>, <<MimeType/binary>>, Header) ->
	PayloadLength = list_to_binary(integer_to_list(size(Payload))),
	Response = [<<"HTTP/1.1 "/utf8>>, Codigo, <<" OK\n"/utf8>>,
				<<"Server: ErlangMS\n"/utf8>>,
				<<"Content-Type: "/utf8>>, MimeType, <<"\n"/utf8>>,
				<<"Content-Length: "/utf8>>, PayloadLength, <<"\n"/utf8>>,
				<<"Access-Control-Allow-Origin: *\n"/utf8>>,
				<<"Access-Control-Allow-Methods: GET, PUT, POST, DELETE, OPTIONS\n"/utf8>>,
				<<"Access-Control-Allow-Headers: Content-Type, Content-Range, Content-Disposition, Content-Description, X-Requested-With, X-CSRFToken, X-CSRF-Token, Authorization\n"/utf8>>,
				case Header of undefined -> header_cache_control(MimeType); _ -> Header end,
				<<"\n\n"/utf8>>, 
	            Payload],
	Response2 = iolist_to_binary(Response),
	Response2.


encode_response(Codigo, []) ->
	encode_response(Codigo, <<"[]">>, <<"application/json; charset=utf-8"/utf8>>);
encode_response(<<Codigo/binary>>, []) ->
	encode_response(Codigo, <<"[]">>, <<"application/json; charset=utf-8"/utf8>>);
encode_response(<<Codigo/binary>>, <<>>) ->
	encode_response(Codigo, <<"[]">>, <<"application/json; charset=utf-8"/utf8>>);
encode_response(<<Codigo/binary>>, <<Payload/binary>>) ->
	encode_response(Codigo, Payload, <<"application/json; charset=utf-8"/utf8>>);
encode_response(Codigo, Payload) when is_tuple(Payload) ->
    Payload2 = ems_schema:to_json(Payload),
    encode_response(Codigo, Payload2).
						
header_cache_control(<<"application/x-javascript">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"text/css">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/x-icon">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/png">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/gif">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/jpeg">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"image/bmp">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<"application/font-woff">>) ->
	<<"Cache-Control: max-age=290304000, public"/utf8>>;
header_cache_control(<<_MimeType/binary>>) ->
	<<"Cache-Control: no-cache"/utf8>>.

parse_querystring(Q) ->
	Q1 = httpd:parse_query(Q),
	Q2 = [{iolist_to_binary(P), 
		   iolist_to_binary(case V of
										[34|_] -> string:substr(V, 2, length(V)-2);
										_  -> V
						    end)}  || {P,V} <- Q1],
	maps:from_list(Q2).


rid_to_string(RID) -> integer_to_list(RID).

method_to_string(Method) when is_atom(Method) -> atom_to_list(Method);
method_to_string(Method) -> Method.

decode_http_header(Headers, Params) ->
    case erlang:decode_packet(httph, Headers, []) of
        { ok, http_eoh, Rest } -> 
			{maps:from_list(Params), Rest};
        { ok, {http_header,_,P,_,V}, Rest } ->
            decode_http_header(Rest, [{P, V} | Params])
    end.

decode_http_request(RequestBin) ->
	case erlang:decode_packet(http_bin, RequestBin, []) of
		{ok, {http_error, _}, _} ->
			ems_logger:error("RequestBin -> ~p", [RequestBin]),
			{error, http_error};
		{ok, Req, Rest} ->
			{http_request, Method, {abs_path, Uri}, {Http_Version_Major, Http_Version_Minor}} = Req,
			Http_Version = io_lib:format("HTTP/~p.~p", [Http_Version_Major, Http_Version_Minor]),
			case decode_http_header(Rest, []) of
				{error, ReasonDecodeHeader} -> {error, ReasonDecodeHeader};
				{Http_Params, Payload} -> {method_to_string(Method), 
										   binary_to_list(Uri), 
										   Http_Params, 
										   Http_Version,
										   Payload}
			end;
		{error, Reason} -> 
			ems_logger:error("RequestBin -> ~p", [RequestBin]),
			{error, Reason}
	end.


%% @doc Decodifica o payload e transforma em um tipo Erlang
decode_payload(undefined) -> #{};
decode_payload(<<>>) -> #{};
decode_payload(PayloadBin) ->
	case ems_util:json_decode_as_map(PayloadBin) of
		{ok, PayloadMap} -> PayloadMap;
		{error, _Reason} -> erlang:error(invalid_payload)
	end.
	
get_http_header_adicionais(Header) ->
	Header1 = lists:map(fun(H) -> get_param_header(H, []) end, Header),
	maps:from_list(Header1).

%% @doc Retorna uma tupla com o name do cabecalho e o seu valor
%% Ex.: get_param_header("Host: localhost:2301", [])  =>  {"host","localhost:2301"}
get_param_header([], Key) -> {string:to_lower(lists:reverse(Key)), []};
get_param_header([H|T], Key) ->
	case H of
		$: -> 
			P = string:to_lower(lists:reverse(Key)),
			V = format_header_value(P, T),
			{P, V};
		_ -> get_param_header(T, [H|Key])
	end.


%% @doc formata o valor do header (String, Integer)
format_header_value("content-length", Value) ->
	Value1 = string:strip(Value),
	Value2 = list_to_integer(Value1),
	case is_content_length_valido(Value2) of
		true -> Value2;
		false -> 0
	end;

format_header_value(_, Value) -> 
	string:strip(Value).

is_content_length_valido(N) when N < 0; N > ?HTTP_MAX_POST_SIZE -> false;
is_content_length_valido(_) -> true.

%% @doc Retorna booleano se o método é suportado pelo servidor
is_metodo_suportado(<<"GET">>) -> true;
is_metodo_suportado(<<"POST">>) -> true;
is_metodo_suportado(<<"PUT">>) -> true;
is_metodo_suportado(<<"DELETE">>) -> true;
is_metodo_suportado(<<"OPTIONS">>) -> true;
is_metodo_suportado("GET") -> true;
is_metodo_suportado("POST") -> true;
is_metodo_suportado("PUT") -> true;
is_metodo_suportado("DELETE") -> true;
is_metodo_suportado("OPTIONS") -> true;
is_metodo_suportado(_) -> false.

%% @doc Indica se a URL é valida
is_url_valido(Url) ->
	case re:run(Url, "^((http:\/\/)|(\/))?([a-z_0-9\-]+\.)?[a-z_0-9\-]+\.[a-z_0-9]{2,4}(\.[a-z0-9]{2,4})?(\/.*)?$") of
		nomatch -> false;
		_ -> true
	end.

mask_ipaddress_to_tuple(<<IpAddress/binary>>) ->
	mask_ipaddress_to_tuple(binary_to_list(IpAddress));
	
mask_ipaddress_to_tuple(IpAddress) ->
	L = string:tokens(IpAddress, "."),
	L2 = lists:map(fun(X) -> 
								case X of
									"*" -> '_';
									_ -> list_to_integer(X)
								end
					end, L),
	list_to_tuple(L2).


%% @doc Retorna true se Ip2 combina com algum Ip da lista Ip1
match_ip_address([H|T]=Ip1,	Ip2) when erlang:is_list(Ip1) ->
	case match_ip_address(H, Ip2) of
		true -> true;
		false -> match_ip_address(T, Ip2)
	end;

%% @doc Retorna true se Ip2 combina com Ip1
match_ip_address([], _) -> false;
match_ip_address(Ip1, 	Ip2) ->
   {O1, O2, O3, O4} = Ip1,
   {X1, X2, X3, X4} = Ip2,
   (O1 == '_' orelse O1 == X1) andalso
   (O2 == '_' orelse O2 == X2) andalso
   (O3 == '_' orelse O3 == X3) andalso
   (O4 == '_' orelse O4 == X4).
	
	
%% @doc Retorna o mime-type do arquivo
mime_type(".htm") -> <<"text/html">>;
mime_type(".html") -> <<"text/html">>;
mime_type(".xhtml") -> <<"application/xhtml+xml">>;
mime_type(".css") -> <<"text/css">>;
mime_type(".js") -> <<"application/x-javascript">>;
mime_type(".png") -> <<"image/png">>;
mime_type(".xml") -> <<"application/xml">>;
mime_type(".ico") -> <<"image/x-icon">>;
mime_type(".gif") -> <<"image/gif">>;
mime_type(".jpeg") -> <<"image/jpeg">>;
mime_type(".jpg") -> <<"image/jpeg">>;
mime_type(".bmp") -> <<"image/bmp">>;
mime_type(".pdf") -> <<"application/pdf">>;
mime_type(".txt") -> <<"text/plain">>;
mime_type(".ttf") -> <<"application/font-woff">>;
mime_type(".stl") -> <<"application/SLA">>;
mime_type(".stp") -> <<"application/STEP">>;
mime_type(".step") -> <<"application/STEP">>;
mime_type(".dwg") -> <<"application/acad">>;
mime_type(".ez") -> <<"application/andrew-inset">>;
mime_type(".ccad") -> <<"application/clariscad">>;
mime_type(".drw") -> <<"application/drafting">>;
mime_type(".tsp") -> <<"application/dsptype">>;
mime_type(".dxf") -> <<"application/dxf">>;
mime_type(".xls") -> <<"application/excel">>;
mime_type(".unv") -> <<"application/i-deas">>;
mime_type(".jar") -> <<"application/java-archive">>;
mime_type(".hqx") -> <<"application/mac-binhex40">>;
mime_type(".cpt") -> <<"application/mac-compactpro">>;
mime_type(".pot") -> <<"application/vnd.ms-powerpoint">>;
mime_type(".ppt") -> <<"application/vnd.ms-powerpoint">>;
mime_type(".dms") -> <<"application/octet-stream">>;
mime_type(".lha") -> <<"application/octet-stream">>;
mime_type(".lzh") -> <<"application/octet-stream">>;
mime_type(".oda") -> <<"application/oda">>;
mime_type(".ogg") -> <<"application/ogg">>;
mime_type(".ogm") -> <<"application/ogg">>;
mime_type(".pgp") -> <<"application/pgp">>;
mime_type(".ai") -> <<"application/postscript">>;
mime_type(".eps") -> <<"application/postscript">>;
mime_type(".ps") -> <<"application/postscript">>;
mime_type(".prt") -> <<"application/pro_eng">>;
mime_type(".rtf") -> <<"application/rtf">>;
mime_type(".smi") -> <<"application/smil">>;
mime_type(".smil") -> <<"application/smil">>;
mime_type(".sol") -> <<"application/solids">>;
mime_type(".vda") -> <<"application/vda">>;
mime_type(".xlm") -> <<"application/vnd.ms-excel">>;
mime_type(".cod") -> <<"application/vnd.rim.cod">>;
mime_type(".pgn") -> <<"application/x-chess-pgn">>;
mime_type(".cpio") -> <<"application/x-cpio">>;
mime_type(".csh") -> <<"application/x-csh">>;
mime_type(".deb") -> <<"application/x-debian-package">>;
mime_type(".dcr") -> <<"application/x-director">>;
mime_type(".dir") -> <<"application/x-director">>;
mime_type(".dxr") -> <<"application/x-director">>;
mime_type(".gz") -> <<"application/x-gzip">>;
mime_type(".hdf") -> <<"application/x-hdf">>;
mime_type(".ipx") -> <<"application/x-ipix">>;
mime_type(".ips") -> <<"application/x-ipscript">>;
mime_type(".skd") -> <<"application/x-koan">>;
mime_type(".skm") -> <<"application/x-koan">>;
mime_type(".skp") -> <<"application/x-koan">>;
mime_type(".skt") -> <<"application/x-koan">>;
mime_type(".latex") -> <<"application/x-latex">>;
mime_type(".lsp") -> <<"application/x-lisp">>;
mime_type(".scm") -> <<"application/x-lotusscreencam">>;
mime_type(".mif") -> <<"application/x-mif">>;
mime_type(".com") -> <<"application/x-msdos-program">>;
mime_type(".exe") -> <<"application/octet-stream">>;
mime_type(".cdf") -> <<"application/x-netcdf">>;
mime_type(".nc") -> <<"application/x-netcdf">>;
mime_type(".pl") -> <<"application/x-perl">>;
mime_type(".pm") -> <<"application/x-perl">>;
mime_type(".rar") -> <<"application/x-rar-compressed">>;
mime_type(".sh") -> <<"application/x-sh">>;
mime_type(".shar") -> <<"application/x-shar">>;
mime_type(".swf") -> <<"application/x-shockwave-flash">>;
mime_type(".sit") -> <<"application/x-stuffit">>;
mime_type(".sv4cpio") -> <<"application/x-sv4cpio">>;
mime_type(".sv4crc") -> <<"application/x-sv4crc">>;
mime_type(".tar.gz") -> <<"application/x-tar-gz">>;
mime_type(".tgz") -> <<"application/x-tar-gz">>;
mime_type(".tar") -> <<"application/x-tar">>;
mime_type(".tcl") -> <<"application/x-tcl">>;
mime_type(".texi") -> <<"application/x-texinfo">>;
mime_type(".texinfo") -> <<"application/x-texinfo">>;
mime_type(".man") -> <<"application/x-troff-man">>;
mime_type(".me") -> <<"application/x-troff-me">>;
mime_type(".ms") -> <<"application/x-troff-ms">>;
mime_type(".roff") -> <<"application/x-troff">>;
mime_type(".t") -> <<"application/x-troff">>;
mime_type(".tr") -> <<"application/x-troff">>;
mime_type(".ustar") -> <<"application/x-ustar">>;
mime_type(".src") -> <<"application/x-wais-source">>;
mime_type(".zip") -> <<"application/zip">>;
mime_type(".tsi") -> <<"audio/TSP-audio">>;
mime_type(".au") -> <<"audio/basic">>;
mime_type(".snd") -> <<"audio/basic">>;
mime_type(".kar") -> <<"audio/midi">>;
mime_type(".mid") -> <<"audio/midi">>;
mime_type(".midi") -> <<"audio/midi">>;
mime_type(".mp2") -> <<"audio/mpeg">>;
mime_type(".mp3") -> <<"audio/mpeg">>;
mime_type(".mpga") -> <<"audio/mpeg">>;
mime_type(".aif") -> <<"audio/x-aiff">>;
mime_type(".aifc") -> <<"audio/x-aiff">>;
mime_type(".aiff") -> <<"audio/x-aiff">>;
mime_type(".m3u") -> <<"audio/x-mpegurl">>;
mime_type(".wax") -> <<"audio/x-ms-wax">>;
mime_type(".wma") -> <<"audio/x-ms-wma">>;
mime_type(".rpm") -> <<"audio/x-pn-realaudio-plugin">>;
mime_type(".ram") -> <<"audio/x-pn-realaudio">>;
mime_type(".rm") -> <<"audio/x-pn-realaudio">>;
mime_type(".ra") -> <<"audio/x-realaudio">>;
mime_type(".wav") -> <<"audio/x-wav">>;
mime_type(".pdb") -> <<"chemical/x-pdb">>;
mime_type(".ras") -> <<"image/cmu-raster">>;
mime_type(".ief") -> <<"image/ief">>;
mime_type(".jpe") -> <<"image/jpeg">>;
mime_type(".jp2") -> <<"image/jp2">>;
mime_type(".tif") -> <<"image/tiff">>;
mime_type(".tiff") -> <<"image/tiff">>;
mime_type(".pnm") -> <<"image/x-portable-anymap">>;
mime_type(".pbm") -> <<"image/x-portable-bitmap">>;
mime_type(".pgm") -> <<"image/x-portable-graymap">>;
mime_type(".ppm") -> <<"image/x-portable-pixmap">>;
mime_type(".rgb") -> <<"image/x-rgb">>;
mime_type(".xbm") -> <<"image/x-xbitmap">>;
mime_type(".xwd") -> <<"image/x-xwindowdump">>;
mime_type(".iges") -> <<"model/iges">>;
mime_type(".igs") -> <<"model/iges">>;
mime_type(".mesh") -> <<"model/mesh">>;
mime_type(".msh") -> <<"model/mesh">>;
mime_type(".silo") -> <<"model/mesh">>;
mime_type(".vrml") -> <<"model/vrml">>;
mime_type(".wrl") -> <<"model/vrml">>;
mime_type(".asc") -> <<"text/plain">>;
mime_type(".c") -> <<"text/plain">>;
mime_type(".cc") -> <<"text/plain">>;
mime_type(".f90") -> <<"text/plain">>;
mime_type(".f") -> <<"text/plain">>;
mime_type(".hh") -> <<"text/plain">>;
mime_type(".m") -> <<"text/plain">>;
mime_type(".rtx") -> <<"text/richtext">>;
mime_type(".sgm") -> <<"text/sgml">>;
mime_type(".sgml") -> <<"text/sgml">>;
mime_type(".tsv") -> <<"text/tab-separated-values">>;
mime_type(".jad") -> <<"text/vnd.sun.j2me.app-descriptor">>;
mime_type(".etx") -> <<"text/x-setext">>;
mime_type(".dl") -> <<"video/dl">>;
mime_type(".fli") -> <<"video/fli">>;
mime_type(".flv") -> <<"video/flv">>;
mime_type(".gl") -> <<"video/gl">>;
mime_type(".mp4") -> <<"video/mp4">>;
mime_type(".mpe") -> <<"video/mpeg">>;
mime_type(".mpeg") -> <<"video/mpeg">>;
mime_type(".mpg") -> <<"video/mpeg">>;
mime_type(".mov") -> <<"video/quicktime">>;
mime_type(".qt") -> <<"video/quicktime">>;
mime_type(".viv") -> <<"video/vnd.vivo">>;
mime_type(".vivo") -> <<"video/vnd.vivo">>;
mime_type(".asf") -> <<"video/x-ms-asf">>;
mime_type(".asx") -> <<"video/x-ms-asx">>;
mime_type(".wmv") -> <<"video/x-ms-wmv">>;
mime_type(".wmx") -> <<"video/x-ms-wmx">>;
mime_type(".wvx") -> <<"video/x-ms-wvx">>;
mime_type(".avi") -> <<"video/x-msvideo">>;
mime_type(".movie") -> <<"video/x-sgi-movie">>;
mime_type(".mime") -> <<"www/mime">>;
mime_type(".ice") -> <<"x-conference/x-cooltalk">>;
mime_type(".vrm") -> <<"x-world/x-vrml">>;
mime_type(".spx") -> <<"audio/ogg">>;
mime_type(".bz2") -> <<"application/x-bzip2">>;
mime_type(".doc") -> <<"application/msword">>;
mime_type(".z") -> <<"application/x-compress">>;
mime_type(".m4a") -> <<"audio/mpeg">>;
mime_type(".csv") -> <<"text/csv">>;
mime_type(_) -> <<"application/octet-stream">>.
