:- module(munge,[munge/9]). 
:- use_module([library(lists), library(random), util, distributions]).

munge(TrainExList, RulesL, SampleSize, Prob, Variance, Head, ClassIndex, ClassT, GeneratedEx):-    
   TrainExList=[Ex|_],
   functor(Ex, _, Arity),
   att_min_max(TrainExList, Arity, ClassIndex, AttMinMaxList),
   collect_conditions(RulesL, ClassIndex, ClassConditionList), 
   clean_conditions(ClassConditionList, NClassConditionList),
   examples_to_index_f(TrainExList, Arity, ClassIndex, IndexExL),
   to_index_small_form(NClassConditionList, IndexExL, ClassIndex, NClassCondIL),!,
   keysort(NClassCondIL, SortedClassConds),
   %write('start pairin ex'),nl,
   pair_examples(IndexExL, AttMinMaxList, [], Arity, ClassIndex, Pairs),
   %write('finished pairin ex'),nl,
   generate_new_examples(SampleSize, Pairs, Pairs, Prob, Variance, [], Arity, ClassIndex, GeneratedValues),
   classify_exs(GeneratedValues, SortedClassConds, Head, ClassT, ClassIndex, GeneratedEx). 

classify_exs([], _, _, _, _, []). 
classify_exs([vals(ValuesList)|Rest], Rules, Head, ClassT, ClassI, [Ex|GenExs]):-
   copy_term(Head, CHead),
   create_head(ValuesList, CHead, Ex),
   classify_small_ex(Rules, ValuesList, ClassT, NewClassT),
   get_majority_class(NewClassT, MajClass1),
   (MajClass1 = tie ->
     keysort(NewClassT, Ordered),
     reverse(Ordered, RevOrder),
     RevOrder = [_-MajClass|_]
   ;
     MajClass = MajClass1
   ),
   arg(ClassI, Ex, MajClass),!,
   classify_exs(Rest, Rules, Head, ClassT, ClassI, GenExs).

generate_new_examples(0, _, _, _, _, GenVals, _, _, GenVals).
generate_new_examples(1, _, _, _, _, GenVals, _, _, GenVals).
generate_new_examples(SampleSize, [], PairsList, Prob, Variance,  GenVals, Arity, ClassIndex, GeneratedV):-
   generate_new_examples(SampleSize, PairsList, PairsList, Prob, Variance, GenVals, Arity, ClassIndex, GeneratedV).
generate_new_examples(SampleSize, [Pair|Pairs], OrigPairs, Prob, Variance, GenVals, Arity, ClassIndex, GeneratedV):-
   NewSampleSize is SampleSize - 2,
   generate_values(Arity, ClassIndex, Pair, Prob, Variance, GenVals1, GenVals2),
   generate_new_examples(NewSampleSize, Pairs, OrigPairs, Prob, Variance, [vals(GenVals1), vals(GenVals2)|GenVals], Arity, ClassIndex, GeneratedV).

generate_values(0, _, _, _, _, [], []).
generate_values(ClassIndex, ClassIndex, Pair, Prob, Variance, [index(ClassIndex)-_|Rest1], [index(ClassIndex)-_|Rest2]):-
   NewAttribute is ClassIndex - 1,!,	
   generate_values(NewAttribute, ClassIndex, Pair, Prob, Variance, Rest1, Rest2).
generate_values(Attribute, ClassIndex, pair(ex(Ex1), ex(Ex2)), Prob, Variance, [index(Attribute)-Val1|Rest1],[index(Attribute)-Val2|Rest2]):-
   NewAttribute is Attribute - 1,!,
   member(index(Attribute)-AttVal1, Ex1),
   member(index(Attribute)-AttVal2, Ex2),
   random(0, 100, RndVal),
   (RndVal =< Prob ->    
     SD is abs(AttVal1 - AttVal2) / Variance,
     normal(AttVal1, SD, Val1),
     normal(AttVal2, SD, Val2)
   ;
     Val1 = AttVal1,
     Val2 = AttVal2
   ),
   generate_values(NewAttribute, ClassIndex, pair(ex(Ex1), ex(Ex2)), Prob, Variance, Rest1, Rest2).

%Finds closest pairs - by simply summing the absolute values between examples
%and keeping the example with lowest difference
pair_examples([], _, _, _, _, []).
pair_examples([Ex|Rest], AttMinMaxList, Prev, Arity, ClassIndex, Pairs):-
   decimals(BigValue),
   append(Prev, Rest, AllbutOne),
   find_closest_ex(AllbutOne, Ex, AttMinMaxList, Arity, ClassIndex, BigValue-_, Rest, _NewRest, pair(Ex1, Ex2)),!, 
   delete(Rest, Ex2, NewR),         %less cases to test...
   pair_examples(NewR, AttMinMaxList, [Ex|Prev], Arity, ClassIndex, RestPairs),  %Rest = NewRest if we want to remove examples!
   append([pair(Ex1,Ex2), pair(Ex2, Ex1)], RestPairs, Pairs).

find_closest_ex([], Ex1, _, _, _, _-Ex2, Rest, NewRest, pair(Ex1, Ex2)):-
   delete(Rest, Ex2, NewRest).
find_closest_ex([Ex2|Rest], Ex, AttMinMaxL, Arity, ClassIndex, TVal-TEx, OrgRest, NewRest, PartPair):-
   summarize_abs_diff(Arity, AttMinMaxL, ClassIndex, Ex, Ex2, 0, ExVal),!,
   (ExVal < TVal ->
      find_closest_ex(Rest, Ex, AttMinMaxL, Arity, ClassIndex, ExVal-Ex2, OrgRest, NewRest, PartPair)
   ;
      find_closest_ex(Rest, Ex, AttMinMaxL, Arity, ClassIndex, TVal-TEx, OrgRest, NewRest, PartPair)
   ).

summarize_abs_diff(0, _, _, _, _, ExVal, ExVal).
summarize_abs_diff(ClassIndex, AttMinMaxL, ClassIndex, Ex1, Ex2, TVal, ExVal):-
   NewAttribute is ClassIndex - 1,!,	
   summarize_abs_diff(NewAttribute, AttMinMaxL, ClassIndex, Ex1, Ex2, TVal, ExVal).
summarize_abs_diff(Attribute, AttMinMaxL, ClassIndex, ex(Ex1), ex(Ex2), TVal, ExVal):-
   member(index(Attribute)-Val1,Ex1),
   member(index(Attribute)-Val2,Ex2),
   member(index(Attribute)-[Min, Max], AttMinMaxL),
   normalize(Min, Max, Val1, NVal1),
   normalize(Min, Max, Val2, NVal2),
   AbsDiff is abs(NVal1 - NVal2),
   NewTVal is AbsDiff + TVal,
   NewAttribute is Attribute - 1,!,	
   summarize_abs_diff(NewAttribute, AttMinMaxL, ClassIndex, ex(Ex1), ex(Ex2), NewTVal, ExVal).

