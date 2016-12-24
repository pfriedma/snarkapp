{application,
     snarkapp, 
        [
        { description, "Really?!"}, 
        {vsn, "1.0.0"}, 
        {modules, [snarkapp, snarkmon, snarkdb,snarkutils,snarkserver,snarkrespmon,snarkresponder,snark_sup]}, 
        {applications, [stdlib, kernel, mnesia]},
        {mod, {snarkapp, []}}
        ]
}.
