-module(test).
-export([run/0]).

%% start two processes f1 & f2. 
%%
%% test 1: f2 will ask for {test, any} which
%% does not exist, so it will block. after 500ms f1 will add
%% {test, erlang} which will match f2s request and it will be
%% returned and unblock.
%%
%% test 2: fetch existing tuple, which should be returned
%% immediately.
%%
%% test 3: after that we test to see if the correct tuple 
%% is returned when multiple tuples exist in TS.
run() ->
    TS = ts:new(),
    link(TS),
    F1 = fun() ->
        sleep(500), 
        ts:out(TS, {test, erlang}),
        ts:out(TS, {test, java}),
        Res1 = ts:in(TS, {debug, any}),
        assert({debug, any}, Res1, {debug, erlang}) % test 2
    end,
    F2 = fun() ->
        Res1 = ts:in(TS, {test, any}),
        assert({test, any}, Res1, {test, erlang}), % test 1
        sleep(1000),
        Res2 = ts:in(TS, {any, java}),
        assert({any, java}, Res2, {test, java}), % test 3
        ts:out(TS, {test, erlang}),
        ts:out(TS, {test, debug}),
        ts:out(TS, {debug, erlang})
    end,
    spawn(F1),
    spawn(F2).

%% simple assert function
assert(Pattern, Match, Expected) ->
    io:format("~w should return ~w: ", [Pattern, Expected]),
    case Expected =:= Match of
        true -> io:format("Correct!~n");
        false -> io:format("Incorrect.~n")
    end.

%% sleep function
sleep(T) ->
    receive
    after
	T -> true
    end.