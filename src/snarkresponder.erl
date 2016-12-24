-module(snarkresponder).
-behaviour(gen_server).

% Responds to events from YAWS
% calls snarkutils


%% API
-export([start/0, stop/0, start_link/0,out/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_Args) ->
  {ok,[]}.

stop() ->
    gen_server:call(snarkresponder, stop).


out(Arg) -> 
    gen_server:call(snarkresponder,{out,Arg}).
    
handle_call({out,Arg}, _From, State) ->
    Snark = snarkutils:out(Arg),
    {reply, Snark, State};
handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.





