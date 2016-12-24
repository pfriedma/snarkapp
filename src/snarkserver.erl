-module(snarkserver).
-behaviour(gen_server).
%% API
-export([start/0, stop/0, start_link/0, get/1,put/3,put/5,delete/1,update/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(_Args) ->
  {ok,[]}.

stop() ->
    gen_server:call(snarkserver, stop).

% API - Snarkapp calls these 
get(Id) -> 
    gen_server:call(snarkserver, {getsnark,Id}).
put(Label,Joke,Punchline) ->
    gen_server:call(snarkserver, {putsnark, {Label,Joke,Punchline,"unknown","unknown"}}).
put(SLabel,SJoke,SPunchline,SAuthor,SAddedBy) -> 
    gen_server:call(snarkserver, {putsnark, {SLabel,SJoke,SPunchline,SAuthor,SAddedBy}}).
delete(Id) -> 
    gen_server:call(snarkserver, {delsnark,Id}).
update(Id,Data) -> 
    gen_server:call(snarkserver, {updatesnark,Id,Data}).

% Hendlers
handle_call({getsnark,"latest"}, _From, State) ->
    Snark = snarkdb:latest(),
    {reply, Snark, State};
handle_call({getsnark,"all"}, _From, State) ->
    Snark = snarkdb:all(),
    {reply,Snark,State};
handle_call({getsnark,Id}, _From, State) when is_integer(Id) ->
    Snark = snarkdb:get_snark_by_id(Id),
    {reply, Snark, State};
handle_call({getsnark,Name}, _From, State) ->
    Snark = snarkdb:get_snark_by_label(Name),
    {reply, Snark, State};
handle_call({putsnark,{SLabel,SJoke,SPunchline,SAuthor,SAddedBy}}, _From, State) ->
    Rsp = snarkdb:add_snark(SLabel,SJoke,SPunchline,SAuthor,SAddedBy),
     {reply, Rsp, State};
handle_call({updatesnark,Id,{SLabel,SJoke,SPunchline,SAuthor,SAddedBy}}, _From, State) ->
    Rsp = snarkdb:update_snark(Id,SLabel,SJoke,SPunchline,SAuthor,SAddedBy),
     {reply, Rsp, State};
handle_call({delsnark,Id}, _From, State) when is_integer(Id) ->
    Snark = snarkdb:del_snark(Id),
    {reply, Snark, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.





