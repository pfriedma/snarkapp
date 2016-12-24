-module(snarkwrapper).
-behaviour(application).
-export([stop/0,start/0,start/2,stop/1]).

start() ->
    application:start(mnesia),
    application:start(snarkapp).

stop() ->
    aplication:stop(snarkapp).

start(_,_) ->
    start().

stop(_) ->
    stop().