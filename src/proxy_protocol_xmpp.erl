%% Author: skruger
%% Created: Jul 16, 2011
%% Description: TODO: Add description to proxy_protocol_xmpp
-module(proxy_protocol_xmpp).

%%
%% Include files
%%

-include("surrogate.hrl").

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

handle_protocol(#proxy_listener{listen_port=ListenPort}=PListener) ->
	Parser = exmpp_xml:start_parser([{root_depth,1}]),
	try
		case read_stream(Parser,PListener#proxy_listener.client_sock) of
			{ok,[Stream|OtherXMPP],Data} ->
				To = exmpp_xml:get_attribute_as_list(Stream,<<"to">>,""),
%% 				?ERROR_MSG("Got open stream for ~p:~n~p~n~p~n",[To,Data,Stream]),
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
						%% On unknown host implement host-unknown RFC6120 4.9.3.6
						XMPP_Err = xmpp_error('host-unknown',Stream),
						
						?INFO_MSG("XMPP Error connecting to unknown host: ~p (port ~p)~n~p~n~p~n",[To,ListenPort,Stream,OtherXMPP]),
						gen_socket:send(PListener#proxy_listener.client_sock,XMPP_Err)
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
