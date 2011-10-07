%% Author: skruger
%% Created: Jul 16, 2011
%% Description: TODO: Add description to proxy_protocol_xmpp
-module(proxy_protocol_xmpp).

%%
%% Include files
%%

-include("surrogate.hrl").

-include_lib("exmpp/include/exmpp.hrl").

%%
%% Exported Functions
%%

-behaviour(proxy_protocol).
-behaviour(proxy_mod).

-export([proxy_mod_start/1,proxy_mod_stop/1]).
-export([handle_protocol/1]).

%%
%% API Functions
%%

proxy_mod_start(_Opts) ->
	exmpp:start(),
	ok.

proxy_mod_stop(_Opts) ->
	ok.

%% stream:error conditions
% remote-connection-failed (see notes below)
% host-unknown (see notes below)
% system-shutdown (Use while disabling a server for maintenance which can include moving clusters)
% Does not prperly handle dialback protocol http://www.ietf.org/rfc/rfc3920.txt

handle_protocol(#proxy_listener{listen_port=ListenPort}=PListener) ->
	Parser = exmpp_xml:start_parser([{root_depth,1}]),
	try
		case read_stream(Parser,PListener#proxy_listener.client_sock) of
			{ok,[Stream|OtherXMPP],Data} ->
				To = exmpp_xml:get_attribute_as_list(Stream,<<"to">>,""),
				case mod_host_pool:get_pool_by_host(To) of
					{ok,Pool} ->
						TargetList = [{pool,Pool,ListenPort,3}],
						?INFO_MSG("Connect to server ~p via ~p~n",[To,TargetList]),
						
						case proxy_protocol:tcp_connect(TargetList) of
							{ok,ServerSock} ->
								gen_socket:send(ServerSock,Data),
 								proxy_connect:bridge_client_server(PListener#proxy_listener.client_sock,ServerSock);
							_ ->
								%% On error imiplement remote-connection-failed RFC6120 4.9.3.15
								XMPP_Err = xmpp_error('remote-connection-failed',Stream),
								?INFO_MSG("XMPP Error: ~p~n~p~n",[Stream,XMPP_Err]),
								gen_socket:send(PListener#proxy_listener.client_sock,XMPP_Err)
						end;
					_ ->
						case get_declared_ns(Stream,"db") of
							'jabber:server:dialback' ->
								?ERROR_MSG("Server dialback on port ~p.",[ListenPort]),
								start_dialback([Stream|OtherXMPP],Data,PListener,Parser);
%% 								XMPP_Err = xmpp_error('other',Stream),
%% 								gen_socket:send(PListener#proxy_listener.client_sock,XMPP_Err);
							_ ->
								%% On unknown host implement host-unknown RFC6120 4.9.3.6
								XMPP_Err = xmpp_error('host-unknown',Stream),
								?INFO_MSG("XMPP Error connecting to unknown host: ~p (port ~p)~n~p~n~p~n",[To,ListenPort,Stream,OtherXMPP]),
								gen_socket:send(PListener#proxy_listener.client_sock,XMPP_Err)
						end
				end;
			Err ->
				?ERROR_MSG("Error reading xmpp stream: ~p~n",[Err]),
				ok
		end
	catch
		_:XErr ->
			?ERROR_MSG("XMPP Error: ~p~n~p~n",[XErr,erlang:get_stacktrace()]),
			ok
	end,
	exmpp_xml:stop_parser(Parser),
	ok.

start_dialback([_Stream|_OtherXMPP]=S,Data,PListener,Parser) ->
	random:seed(now()),
	Step3ID = integer_to_list(random:uniform(100000)),
	?ERROR_MSG("Starting dialback.~n~p~n~p~n",[S,Data]),
	DecNS = [{'http://etherx.jabber.org/streams',"stream"},
			 {'jabber:server',none},
			 {'jabber:server:dialback',"db"}],
	Attr = [#xmlattr{name= <<"id">>,value=list_to_binary(Step3ID)}],
	Step3Stream = #xmlel{ns='http://etherx.jabber.org/streams',declared_ns=DecNS,name=stream,attrs=Attr,children=undefined},
	Step3Bin = iolist_to_binary(exmpp_stream:to_iolist(Step3Stream)),
	?ERROR_MSG("Bin: ~p~n",[Step3Bin]),
	gen_socket:send(PListener#proxy_listener.client_sock,Step3Bin),
	case read_stream(Parser,PListener#proxy_listener.client_sock) of
		{ok,[Step4Result|VerifyXML],Step4BinData} ->
			?ERROR_MSG("Got Step4:~p~n~p~n",[Step4Result,Step4BinData]),
			start_authoritative_dialback(Step4Result,VerifyXML,PListener,Parser);
		_ ->
			gen_socket:close(PListener#proxy_listener.client_sock),
			ok
	end.
	
start_authoritative_dialback(Step4,Step8Verify,#proxy_listener{listen_port=ListenPort}=PListener,ClientParser) ->
	%% New parser for server side connection.
	Parser = exmpp_xml:start_parser([{root_depth,1}]),
	DecNS = [{'http://etherx.jabber.org/streams',"stream"},
			 {'jabber:server',none},
			 {'jabber:server:dialback',"db"}],
	Step6ID = integer_to_list(random:uniform(100000)),
	Attr = [#xmlattr{name= <<"id">>,value=list_to_binary(Step6ID)}],
	Step6Stream = #xmlel{ns='http://etherx.jabber.org/streams',declared_ns=DecNS,name=stream,attrs=Attr,children=undefined},
	Step6Bin = iolist_to_binary(exmpp_stream:to_iolist(Step6Stream)),
	To = exmpp_xml:get_attribute_as_list(Step4,<<"to">>,""),
	case mod_host_pool:get_pool_by_host(To) of
		{ok,Pool} ->
			TargetList = [{pool,Pool,ListenPort,3}],
			?INFO_MSG("Connect to server ~p via ~p~n",[To,TargetList]),
			case proxy_protocol:tcp_connect(TargetList) of
				{ok,ServerSock} ->
					?ERROR_MSG("Sending Step6:~n~p~n~p~n",[Step6Stream,Step6Bin]),
					gen_socket:send(ServerSock,Step6Bin),
					case read_stream(Parser,ServerSock) of
						{ok,Step7XML,Step7Bin} ->
							?ERROR_MSG("Got Step7 back: ~n~p~n~p~n",[Step7XML,Step7Bin]),
							authoritative_dialback_handshake(Step8Verify,ServerSock,PListener,Parser);
						{error,closed} ->
							?ERROR_MSG("Step7 Read error from authoritative server.",[]),
							gen_socket:close(PListener#proxy_listener.client_sock);
						Step7Err ->
							?ERROR_MSG("Step7 Read stream failed in start_authoritative_dialback()~n~p~n",[Step7Err]),
							gen_socket:close(PListener#proxy_listener.client_sock)
					end;
				_ ->
					gen_socket:close(PListener#proxy_listener.client_sock)
			end;
		_ ->
			gen_socket:close(PListener#proxy_listener.client_sock)
	end,
	exmpp_xml:stop_parser(Parser).

authoritative_dialback_handshake([],ServerSock,PListener,_Parser) ->
	?ERROR_MSG("Verify messages normally come from the originator of the connection!  Failing!",[]),
%% 	XMPP_Err = xmpp_error(other,Stream),
%% 	gen_socket:send(PListener#proxy_listener.client_sock,XMPP_Err);
	gen_socket:close(ServerSock),
	gen_socket:close(PListener#proxy_listener.client_sock);
authoritative_dialback_handshake([Step8Verify|_],ServerSock,PListener,Parser) ->
	Step8Bin = exmpp_xml:document_to_binary(Step8Verify),
	gen_socket:send(ServerSock,Step8Bin),
	case read_stream(Parser,ServerSock) of
		{ok,[#xmlel{}=Step9XML|_],Step9Bin} ->
			?ERROR_MSG("Got Step9 result:~n~p~n~p~n",[Step9XML,Step9Bin]),
			gen_socket:send(PListener#proxy_listener.client_sock,Step9Bin),
			case exmpp_xml:get_attribute_as_list(Step9XML,<<"type">>,"") of
				<<"valid">> ->
					proxy_connect:bridge_client_server(PListener#proxy_listener.client_sock,ServerSock);
				"valid" ->
					proxy_connect:bridge_client_server(PListener#proxy_listener.client_sock,ServerSock);
				Step9TypeErr ->
					?ERROR_MSG("Step 9 Type wasn't \"valid\": ~p~n",[Step9TypeErr]),
					ok
			end;
		Step9Err ->
			?ERROR_MSG("Step9 Read stream failed:~n~p~n",[Step9Err]),
			ok
	end.
	

get_declared_ns(#xmlel{declared_ns=DeclNS},Type) ->
	get_declared_ns2(DeclNS,Type).

get_declared_ns2([],_Type) ->
	undefined;
get_declared_ns2([{NSName,NsType}|_R],Type) when Type==NsType ->
	NSName;
get_declared_ns2([{_NSName,_NsType}|R],Type) ->
	get_declared_ns2(R,Type).
%%
%% Local Functions
%%

xmpp_error('host-unknown',StreamIn) ->
	StartStream = xmpp_start_stream(StreamIn),
	Err = "<stream:error><host-unknown xmlns='urn:ietf:params:xml:ns:xmpp-streams'/></stream:error></stream:stream>",
	iolist_to_binary([StartStream,Err]);
xmpp_error('system-shutdown',StreamIn) ->
	StartStream = xmpp_start_stream(StreamIn),
	Err = "<stream:error><system-shutdown xmlns='urn:ietf:params:xml:ns:xmpp-streams'/></stream:error></stream:stream>",
	iolist_to_binary([StartStream,Err]);
xmpp_error(_,StreamIn) ->
	StartStream = xmpp_start_stream(StreamIn),
	Err = "<stream:error><internal-server-error xmlns='urn:ietf:params:xml:ns:xmpp-streams'/></stream:error></stream:stream>",
	iolist_to_binary([StartStream,Err]).


xmpp_start_stream(StreamIn) ->
	<<Id:16/integer>> = crypto:rand_bytes(2), 
	case get_stream_opts(StreamIn) of
		{From,undefined,Ns} ->
			io_lib:format("<?xml version='1.0' ?><stream:stream from='~s' id='~p' version='1.0' xml:lang='en' xmlns='~s' xmlns:stream='http://etherx.jabber.org/streams'>",[From,Id,Ns]);
		{From,To,Ns} ->
			io_lib:format("<?xml version='1.0' ?><stream:stream from='~s' to='~s' id='~p' version='1.0' xml:lang='en' xmlns='~s' xmlns:stream='http://etherx.jabber.org/streams'>",[From,To,Id,Ns])
	end.

get_stream_opts(Stream) ->
	{xmlelement,_,Attr,_} = exmpp_xml:xmlel_to_xmlelement(Stream),
	Ns = proplists:get_value("xmlns",Attr,"jabbber:client"),
%% 	?ERROR_MSG("XMPP Client: ~p~n",[Ns]),
	To =	exmpp_xml:get_attribute_as_list(Stream,<<"to">>,undefined),
	From =	exmpp_xml:get_attribute_as_list(Stream,<<"from">>,undefined),
	{To,From,Ns}.

read_stream(Parser,Sock) ->
	read_stream(Parser,<<>>,Sock).

read_stream(Parser,XMLBin,Sock) ->
	case gen_socket:recv(Sock,0,10000) of
		{error,Reason} ->
			{error,Reason};
		{ok, Data0} ->
			Data = <<XMLBin/binary,Data0/binary>>,
%% 			?ERROR_MSG("Try parsing data:~n~p~n",[Data]),
			case exmpp_xml:parse(Parser,trim_data(Data)) of
				continue ->
					read_stream(Parser,Data,Sock);
				Elements ->
					{ok,Elements,Data}
			end;
		Other ->
			{error,{bad_gen_socket_return,Other}}
	end.

%% trim_data() is needed because <?xml ... ?> was causing exmpp_xml:parse() to fail.
trim_data(<<"<?xml",Rest/binary>>) ->
	trim_data2(Rest);
trim_data(Binary) ->
	Binary.
trim_data2(<<"?>",Data/binary>>) ->
	Data;
trim_data2(<<_:1/binary,Data/binary>>) ->
	trim_data2(Data).
