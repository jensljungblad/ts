-module(ts). 
-export([new/0, in/2, out/2, ts/2]). 

% returns the pid of a new tuplespace
new() ->
    spawn_link(fun ts/0).

% retrieve tuple matching pattern from tuplespace
in(TS, Pattern) ->
    TS ! {self(), Pattern},
    receive
        Result ->
            Result
    end.
    
% put tuple in tuplespace
out(TS,Tuple) ->
    TS ! {Tuple}.
    
% do the loopy loop
ts() -> ts([], []).
ts(Tuples, WaitingList) ->
    receive 
        % handle in messages
        {From, Pattern} ->
            case recursive_match(Pattern, Tuples, []) of
                {FoundTuple, NewTuples} -> 
                    From ! FoundTuple,
                    ts(NewTuples, WaitingList);
                false -> 
                    ts(Tuples, [{From, Pattern}|WaitingList])
            end;
        % handle out messages
        {Tuple} ->
            case recursive_match({any, Tuple}, WaitingList, []) of
                {{From, _}, NewWaitingList} ->
                    From ! Tuple,
                    ts(Tuples, NewWaitingList);
                false -> ts([Tuple|Tuples], WaitingList)
            end
    end. 
    
% recursive matching returns matched tuple and new tuples list
recursive_match(_, [], _)->
    false;
recursive_match(Pattern, Tuples, NewTuples) ->
    [Head|Tail] = Tuples,
    case match(Pattern, Head) of
        true -> {Head, NewTuples ++ Tail};
        false -> recursive_match(Pattern, Tail, [Head|NewTuples])
    end.
    
% match pattern with tuple
match(any,_) -> true;
match(_,any) -> true;
match(P,Q) when is_tuple(P), is_tuple(Q) -> 
    match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> 
    case match(P,L) of
        true -> match(PS,LS); 
        false -> false
    end;
match(P,P) -> true;
match(_,_) -> false.