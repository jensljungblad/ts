-module(ts). 
-export([new/0, in/2, out/2, ts/2]). 

% returns the pid of a new tuplespace
new() ->
    spawn_link(fun ts/0).

% retrieve tuple matching pattern from tuplespace
in(TS, Pattern) ->
    Ref = make_ref(),
    TS ! {self(), Ref, Pattern},
    receive
        {Ref, Result} ->
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
        {From, Ref, Pattern} ->
            case recursive_match(Pattern, Tuples, []) of
                {FoundTuple, NewTuples} -> 
                    From ! {Ref, FoundTuple},
                    ts(NewTuples, WaitingList);
                false -> 
                    ts(Tuples, [{From, Ref, Pattern}|WaitingList])
            end;
        % handle out messages
        {Tuple} ->
            case match_list(Tuple, WaitingList, []) of
                false -> ts([Tuple|Tuples], WaitingList);
                {Waiting, List}  ->
                    {From, Ref, _} = Waiting,
                    From ! {Ref, Tuple},
                    ts(Tuples, List)
            end
    end. 

% match tuple against waiting list.
% returns false if no match is found, else a tuple with the
% information about the waiting process and the new list.
match_list(_, [], _) ->
    false;
match_list(Tuple, [Head|Tail], NewList) ->
    {_, _, Pattern} = Head,
    case match(Pattern, Tuple) of
        true ->
            {Head, NewList ++ Tail};
        false ->
            match_list(Tuple, Tail, [Head | NewList])
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
match(P,Q) when is_tuple(P), is_tuple(Q) -> 
    match(tuple_to_list(P),tuple_to_list(Q));
match([P|PS],[L|LS]) -> 
    case match(P,L) of
        true -> match(PS,LS); 
        false -> false
    end;
match(P,P) -> true;
match(_,_) -> false.