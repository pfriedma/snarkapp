-module(snark_SUITE). 
-include_lib("common_test/include/ct.hrl").
-export([ init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([add_joke/1]).
-export([get_joke/1]).

all() -> [add_joke]. 

init_per_suite(Config) -> 
    Priv = ?config(priv_dir, Config), 
    application:set_env(mnesia, dir, Priv), 
    snarkapp:install([node()]), 
    application:start(mnesia), 
    application:start(snark), 
    Config. 

end_per_suite(_Config) -> 
    application:stop(mnesia), ok.

init_per_testcase(add_joke, Config) ->
    Config.

end_per_testcase(_,_Config) ->
    ok.

add_joke(_Config) ->
   % {error, unable_to_add} = snarkapp:add_snark("foo"),
    ok = snarkapp:add_snark("test","Joke?","Punchline!!").

get_joke() -> get_joke(0).    

get_joke(Id) ->  
    snarkapp:get_snark(Id).
