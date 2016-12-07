%%%%%
%
% Module for making explicit
% list indexing easier 
%
% Use 
%
% Author: Tony Lindgren
%%%%%

:- module(arr, [arr/2, arr/3, pp_arr/1]).

:- use_module([library(assoc), library(lists), library(pairs)]).

%Create another version from 1 -> forward or replace this....later(has to change spline implemtation)
%Create Array of size = Index with Vars as values [from 0 -> forward]
arr(Var, L):-
  is_list(L),!,
  length(L, NoElem),
  arr(Var, NoElem),
  add_arr_vals(L, 0, Var).
arr(Var, Index):-
  arr(Var, Index, _),!.	
arr(Var, I, Value):-
  \+ attvar(Var), 
  Index is I - 1,!,	
  (var(Value) ->
    numlist(0, Index, Keys),
    length(Keys, L),
    length(Values, L),
    pairs_keys_values(Pairs, Keys, Values),
    list_to_assoc(Pairs, Arr)
  ;
    numlist(0, Index, Keys),
    length(Keys, L),
    set_list(L, Value, Values),
    pairs_keys_values(Pairs, Keys, Values),
    list_to_assoc(Pairs, Arr)
  ),
  put_attr(Var, arr, Arr).

%Get/put value
arr(Var, Index, Value):-
  attvar(Var),
  integer(Index),
  (atomic(Value) ->                         %PUT, cannot store variables!
    get_attr(Var, arr, Arr), 
    is_assoc(Arr),
    put_assoc(Index, Arr, Value, NewArr),
    put_attr(Var, arr, NewArr)
  ;
    get_attr(Var, arr, Arr),
    is_assoc(Arr),
    get_assoc(Index, Arr, Value)
  ).

add_arr_vals([], _, _).
add_arr_vals([Val|Vals], I, Var):-
 arr(Var, I, Val),
 NewI is I + 1,
 add_arr_vals(Vals, NewI, Var).

%pretty_print
pp_arr(Var):-
  get_attr(Var, arr, Arr), 
  is_assoc(Arr),	
  assoc_to_values(Arr, List),
  write(List).
  	
set_list(0, _, []).
set_list(L, Value, [Value|Values]):-
  NewL is L - 1,	
  set_list(NewL, Value, Values).