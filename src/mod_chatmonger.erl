%%% -------------------------------------------------------------------
%%% Author  : skruger
%%% Description :
%%%
%%% Created : Aug 30, 2011
%%% -------------------------------------------------------------------
-module(mod_chatmonger).

-behaviour(proxy_mod).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("surrogate.hrl").

-include_lib("kernel/src/inet_dns.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([proxy_mod_start/1,proxy_mod_stop/1]).
-export([http_api/3]).


-record(state, {config}).

-record(dns_srv,{name,priority,weight,port,host}).
-record(dns_a,{name,addr}).

%% ====================================================================
%% External functions
%% ====================================================================

proxy_mod_start(Conf) ->
	supervisor:start_child(surrogate_sup,{?MODULE,{?MODULE,start_link,[Conf]},permanent,2000,worker,[?MODULE]}),
	proxy_protocol_http_admin:register_module(?MODULE,?MODULE,http_api),
	ok.

proxy_mod_stop(_Conf) ->
	supervisor:terminate_child(surrogate_sup,?MODULE),
	proxy_protocol_http_admin:unregister_module(?MODULE),
	supervisor:delete_child(surrogate_sup,?MODULE),
	ok.

start_link(Conf) ->
	gen_server:start_link({local,?MODULE},?MODULE,Conf,[]).

get_srv_records(Domain) ->
	case inet_res:nslookup("_xmpp-client._tcp."++Domain,in,srv) of
		{ok,#dns_rec{header=#dns_header{qr=true},anlist=Response}} ->
			?ERROR_MSG("Got dns: ~p~n",[Response]),
			[ {struct,
			   [{"name",list_to_binary(Name)},{"priority",list_to_binary(integer_to_list(Priority))},
				{"weight",list_to_binary(integer_to_list(Weight))},
				{"port",list_to_binary(integer_to_list(Port))},{"host",list_to_binary(Host)}]} 
			|| #dns_rr{domain = Name,data={Priority,Weight,Port,Host}} <- Response];
		Other ->
			?ERROR_MSG("Other response: ~p~n",[Other]),
			[]
	end.

get_a_records(Domain) ->
	case inet_res:nslookup(Domain,in,a) of
		{ok,#dns_rec{header=#dns_header{qr=true},anlist=Response}} ->
			?ERROR_MSG("Got dns: ~p~n",[Response]),
			[ {struct,
			   [{"name",list_to_binary(Name)},
				{"addr",list_to_binary(proxylib:format_inet(AddrTup))}]}
			|| #dns_rr{domain=Name,data=AddrTup} <- Response];
		Other ->
			?ERROR_MSG("Other response: ~p~n",[Other]),
			[]
	end.

http_api(["dns",Domain],#http_admin{method='GET'}= _Request,_Conf) -> %% when Request#http_admin.has_auth == true
	SrvRecs = get_srv_records(Domain),
	ARecs = get_a_records(Domain),
	?ERROR_MSG("Srv records: ~p~n",[ARecs]),
	Json = {struct,[{"srv",SrvRecs},{"a",ARecs}]},
	{200,[{"Content-Type","application/json"}],iolist_to_binary(mochijson2:encode(Json))};
http_api(["dns",Domain],#http_admin{method='POST'}= Request,_Conf) -> %% when Request#http_admin.has_auth == true
	DnsRecs = parse_dns_json(mochijson2:decode(Request#http_admin.body)),
	UpdRecs = make_dns_updates(DnsRecs),
	UpdateTxt = ["update delete ",Domain," A\n","update delete _xmpp-client._tcp.",Domain," SRV\n",UpdRecs,"send\nanswer\n"],
	UpdateBin = iolist_to_binary(UpdateTxt),
	TFile = "/tmp/mongerdns-"++Domain, 
	file:write_file(TFile,UpdateBin),
	R = os:cmd("nsupdate "++TFile), 
	?ERROR_MSG("Post DNS record ~p: ~p~n~s~n~p~n",[Domain,DnsRecs,binary_to_list(UpdateBin),R]),
	JsonOut = {struct,[{"result",iolist_to_binary(R)}]},
	{200,[{"Content-Type","application/json"}],iolist_to_binary(mochijson2:encode(JsonOut))};
http_api(Path,Request,_Conf) when Request#http_admin.has_auth == true ->
	Err = io_lib:format("Not found in ~p: ~p~n",[?MODULE,Path]),
	{404,[{'Content-type',"text/plain"}],iolist_to_binary(Err)};
http_api(_Path,_Request,_Conf) ->
	{401,[{"WWW-Authenticate","Basic realm=\"mod_cluster_admin\""}],iolist_to_binary("Authorization required")}.


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Conf) ->
    {ok, #state{config = Conf}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


make_dns_updates(DnsRecs) ->
	make_dns_updates2(DnsRecs,[]).

make_dns_updates2([],Acc) ->
	Acc;
make_dns_updates2([#dns_a{name=Name,addr=Addr}|R],Acc) ->
	Upd = ["update add ",Name," 600 A ",Addr,"\n"],
	make_dns_updates2(R,[Upd|Acc]);
make_dns_updates2([#dns_srv{name=Name,host=Host,priority=Pri,weight=Wei,port=Port}|R],Acc) ->
	Upd = ["update add ",Name," 1200 SRV ",Pri," ",Wei," ",Port," ",Host,"\n"],
	make_dns_updates2(R,[Upd|Acc]);
make_dns_updates2([_|R],[Acc]) ->
	make_dns_updates2(R,Acc).

parse_dns_json({struct,Recs}) ->
	parse_dns_json2(Recs,[]).

parse_dns_json2([],Acc) ->
	Acc;
parse_dns_json2([{<<"srv">>,SrvRecs}|R],Acc) ->
	Recs = lists:map(fun parse_dns_json_srv/1,SrvRecs),
	parse_dns_json2(R,Recs++Acc);
parse_dns_json2([{<<"a">>,ARecs}|R],Acc) ->
	Recs = lists:map(fun parse_dns_json_a/1,ARecs),
	parse_dns_json2(R,Recs++Acc);
parse_dns_json2([_|R],Acc) ->
	parse_dns_json2(R,Acc).

parse_dns_json_srv({struct,El}) ->
	parse_dns_json_srv2(El,#dns_srv{}).

parse_dns_json_srv2([],Acc) ->
	Acc;
parse_dns_json_srv2([{<<"name">>,NameBin}|R],Acc) ->
	parse_dns_json_srv2(R,Acc#dns_srv{name=binary_to_list(NameBin)});
parse_dns_json_srv2([{<<"priority">>,Bin}|R],Acc) ->
	parse_dns_json_srv2(R,Acc#dns_srv{priority=binary_to_list(Bin)});
parse_dns_json_srv2([{<<"weight">>,Bin}|R],Acc) ->
	parse_dns_json_srv2(R,Acc#dns_srv{weight=binary_to_list(Bin)});
parse_dns_json_srv2([{<<"port">>,Bin}|R],Acc) ->
	parse_dns_json_srv2(R,Acc#dns_srv{port=binary_to_list(Bin)});
parse_dns_json_srv2([{<<"host">>,Bin}|R],Acc) ->
	parse_dns_json_srv2(R,Acc#dns_srv{host=binary_to_list(Bin)});
parse_dns_json_srv2([_|R],Acc) ->
	parse_dns_json_srv2(R,Acc).

parse_dns_json_a({struct,El}) ->
	parse_dns_json_a2(El,#dns_a{}).

parse_dns_json_a2([],Acc) ->
	Acc;
parse_dns_json_a2([{<<"name">>,Bin}|R],Acc) ->
	parse_dns_json_a2(R,Acc#dns_a{name=binary_to_list(Bin)});
parse_dns_json_a2([{<<"addr">>,Bin}|R],Acc) ->
	parse_dns_json_a2(R,Acc#dns_a{addr=binary_to_list(Bin)});
parse_dns_json_a2([_|R],Acc) ->
	parse_dns_json_a2(R,Acc).
