-module(snarkapp).
-export([install/1,uninstall/1,getsnark/1,start/2,start/0,putsnark/3,putsnark/5,stop/1,out/1,init/0,delsnark/1,updatesnark/2]).
-behavior(application).
-include("/appvar/include/snark_includes.hrl").

init() ->
    start().

start() ->
    start(normal,[]).

start(normal, []) ->
    %%Priv = ?config(priv_dir, Config), 
    %%application:set_env(mnesia, dir, Priv), 
    application:start(mnesia),
    mnesia:wait_for_tables([snark_jokes],5000),    
    snark_sup:start_link();
start({takeover, _OtherNode}, []) ->
    %%application:set_env(mnesia, dir, Priv), 
    application:start(mnesia),
    mnesia:wait_for_tables([snark_jokes],5000),    
    snark_sup:start_link().
stop(_State) ->
    ok.

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%

getsnark(Snark) ->
    snarkserver:get(Snark).

putsnark(Title,JokeLine,Punchline) ->
    snarkserver:put(Title,JokeLine,Punchline).

putsnark(Title,JokeLine,Punchline,Author,AddedBy) ->
    snarkserver:put(Title,JokeLine,Punchline,Author,AddedBy).

delsnark(Id) ->
    snarkserver:delete(Id).

updatesnark(Id,{Title,JokeLine,Punchline,Author,AddedBy}) -> 
    snarkserver:update(Id,{Title,JokeLine,Punchline,Author,AddedBy}).

%% YAWS out function responds to web queries
out(Arg) -> 
    snarkresponder:out(Arg).


install(Nodes) ->
  %%  Priv = ?config(priv_dir, Config), 
    %%application:set_env(mnesia, dir, Priv), 
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes,application,start,[mnesia]),
    mnesia:create_table(snark_id,[{type, set},{disc_copies,Nodes}]),
    mnesia:create_table(snark_jokes, [
        {attributes, record_info(fields,snark_jokes)},
        {index, [#snark_jokes.jlabel,#snark_jokes.dateAdded]},
        {type, ordered_set},
        {disc_copies, Nodes}
    ]),
    rpc:multicall(Nodes,application,stop,[mnesia]).

uninstall(Nodes) ->
    rpc:multicall(Nodes,application,stop,[mnesia]),
    ok = mnesia:delete_schema(Nodes).
