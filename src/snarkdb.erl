-module(snarkdb).
-export([all/0,add_snark/3,add_snark/5,get_snark_id/1,get_snark_by_id/1,get_snark_by_label/1,latest/0,del_snark/1,update_snark/6]).
-include("/appvar/include/snark_includes.hrl").

%% DB functions used by snarkserver

add_snark(SLabel,SJoke,SPunchline,SAuthor,SAddedBy) ->
    F = fun() ->
       SDateAdded = calendar:local_time(),
       SId = mnesia:dirty_update_counter(snark_id,id,1),
       mnesia:write(#snark_jokes{jid=SId,jlabel=SLabel,joke=SJoke,punchLine=SPunchline,author=SAuthor,addedby=SAddedBy,dateAdded=SDateAdded})
    end,
    mnesia:activity(transaction, F).

update_snark(SId,SLabel,SJoke,SPunchline,SAuthor,SAddedBy) ->
    F = fun() ->
        SDateAdded = calendar:local_time(),
        [P] = mnesia:wread({snark_jokes, SId}),
         mnesia:write(P#snark_jokes{jid=SId,jlabel=SLabel,joke=SJoke,punchLine=SPunchline,author=SAuthor,addedby=SAddedBy,dateAdded=SDateAdded})
    end,
    mnesia:activity(transaction, F).

add_snark(SLabel,SJoke,SPunchline) -> 
    add_snark(SLabel,SJoke,SPunchline,"unknown","unknown").

get_snark_by_id(Id) ->
    mnesia:dirty_read(snark_jokes,Id).

get_snark_by_label(JLabel) ->
    mnesia:dirty_match_object(snark_jokes,{snark_jokes,'_',JLabel,'_','_','_','_','_'}).

get_snark_id(JLabel) ->
    [{_,ID,_,_,_,_,_}|_] = get_snark_by_label(JLabel),
    ID.

del_snark(Id) ->
    F = fun() ->
        mnesia:delete({snark_jokes,Id})
    end,
    mnesia:activity(transaction,F).

latest() ->
    % This is probably wrong (maybe snark should be an ordered set?)
    Id = mnesia:dirty_last(snark_jokes),
    get_snark_by_id(Id).

all() ->
    F = fun() ->
       mnesia:foldl(fun(X,Acc) -> [X|Acc] end, [], snark_jokes)
    end,
    mnesia:activity(transaction, F).