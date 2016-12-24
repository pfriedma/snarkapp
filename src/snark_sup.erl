-module(snark_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).


%% We register it so that it's guaranteed to be unique
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Using a SOFO strategy because we get to have many
%% supervisees of the same type.
init([]) ->
    {ok, {{one_for_one, 5, 10},
          [{snarkmon,
            {snarkmon, start_link, []},
            permanent,
            5000,
            supervisor,
            [snarkmon]
          }, 
        {snarkrespmon,
            {snarkrespmon, start_link, []},
            permanent,
            5000,
            supervisor,
            [snarkrespmon]
    }]}}.