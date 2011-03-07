-module(test).
-export([test/0, slave/2]).
-import(

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ts  % <-- CHANGE THIS TO YOUR TUPLESPACE MODULE NAME 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
,[in/2,out/2,new/0]
). 

%% dave Oct 2000
%% karol Jan 2010

%% Module for performing a quick test on lab3 module.
%% To test your lab, compile it: 
%% c(linda).
%% then compile the test: 
%% c(test).
%% then run:
%% test:test().

%% Here is a run of the test on a correct tuplespace:

%% (node@chalmers)1> c(test).
%% {ok,test}
%% (node@chalmers)2> test:test().
%% TEST: new tuplespace TS created
%% TEST: in(TS, {fish,any}) by process <0.46.0>
%% TEST: in(TS, {fowl,any}) by process <0.47.0>
%% TEST: out(TS, {fish,salmon})
%% TEST: out(TS, {fowl,chicken})
%% Process <0.47.0>
%%      Correct. in operation: {fowl,any} returned tuple: {fowl,chicken}
%% Process <0.46.0>
%%      Correct. in operation: {fish,any} returned tuple: {fish,salmon}
%% TEST: out(TS, {fish,chips})
%% TEST: in(TS, {any,chips}) by process <0.48.0>
%% Process <0.48.0>
%%      Correct. in operation: {any,chips} returned tuple: {fish,chips}
%% TEST: in(TS, {any,any}) by process <0.49.0>
%% Correct. Tuplespace appears to be empty.
%% finished
%% (node@chalmers)3> 

test() ->
    Delay = 500,
    process_flag(trap_exit, true),
    TS = new(),
    link(TS),
    io:format("TEST: new tuplespace TS created~n", []),

    Slave1 = spawn_in_test(TS, {fish,any}),
    sleep(Delay),

    Slave2 = spawn_in_test(TS, {fowl,any}),
    sleep(Delay),

    out_test(TS, {fish,salmon}),
    sleep(Delay),

    out_test(TS, {fowl,chicken}), 
    sleep(Delay),

    replytest(Slave2, {fowl,any}, {fowl,chicken}),
    sleep(Delay),

    replytest(Slave1, {fish,any}, {fish,salmon}),
    sleep(Delay),

    out_test(TS, {fish,chips}), 
    sleep(Delay),

    Slave3 = spawn_in_test(TS, {any,chips}),
    sleep(Delay),

    replytest(Slave3, {any,chips}, {fish,chips}),
    sleep(Delay),

    Slave4 = spawn_in_test(TS, {any,any}),
    sleep(Delay),

    receive
	{Slave4, Tup} ->
	    io:format("Error. Empty tuplespace, but received: ~w~n",[Tup])
    after
        1000 ->   
	    io:format("Correct. Tuplespace appears to be empty.~n"),
	    exit(Slave4, this_is_it),
	    exit(TS, this_is_it),
	    collect_exits([Slave1, Slave2, Slave3, Slave4, TS]),
	    finished
    end.

%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(T) ->
    receive
    after
	T -> true
    end.

out_test(Tuplespace, Tup) ->
    io:format("TEST: out(TS, ~w)~n", [Tup]),
    out(Tuplespace, Tup).

% spawns a slave task to perform an in test. This function 
% returns the slave's Pid. The slave will forward the result of the 
% in operation to the caller.

spawn_in_test(Tuplespace, Pat) -> 
    S = spawn_link(test, slave, [Tuplespace, {self(), Pat}]),
    io:format("TEST: in(TS, ~w) by process ~w~n", [Pat, S]),
    S.

%% Get a tuple matching Item from Tuplespace T and send it to Pid
slave(T, {Pid,Item}) ->
    case in(T, Item) of
	R -> Pid!{self(), R}
    end.

%% Tests whether the reply from a Slave task matches the expected Tuple
replytest(Slave, Pat, Tup) -> 
    io:format("Process ~w~n", [Slave]),
    receive
	{Slave,Tup} ->
	    io:format("     Correct. in operation: ~w returned tuple: ~w~n", [Pat, Tup]);
        {Slave,Bad} ->
	    io:format("     Error. in with pattern: ~w returned tuple: ~w~n", [Pat, Bad])
    after 
        5000 ->   
	    io:format("     Error. No response for in operation with pattern: ~w~n", [Pat])
    end.

collect_exits([]) ->
    done;
collect_exits([Pid | Pids]) ->
    receive
	{'EXIT', Pid, _} ->
	    collect_exits(Pids)
    end.
