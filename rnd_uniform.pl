:- module(random_uniform,[rnd_uniform/7]).
:- use_module([library(lists), util]).

rnd_uniform(TrainExList, RulesList, SampleSize, Head, ClassIndex, ClassT, GeneratedEx):-
   TrainExList=[Ex|_],
   functor(Ex, _, Arity),
   att_min_max(TrainExList, Arity, ClassIndex, AttMinMaxList),
   collect_conditions(RulesList, ClassIndex, ClassConditionList),
   clean_conditions(ClassConditionList, NClassConditionList),
   examples_to_index_f(TrainExList, Arity, ClassIndex, IndexExL),
   to_index_small_form(NClassConditionList, IndexExL, ClassIndex, NClassCondIL),
   generate_examples(SampleSize, NClassCondIL, Head, ClassT, ClassIndex, AttMinMaxList, GeneratedEx).

generate_examples(0, _, _, _, _, _, []).
generate_examples(SampleSize, Rules, Head, ClassT, ClassI, AttMinMaxList, [Ex|GeneratedEx]):-
   gen_rnd_values(AttMinMaxList, ValuesList),
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
   arg(ClassI, Ex, MajClass),
   NewSampleSize is SampleSize - 1,!,
   generate_examples(NewSampleSize, Rules, Head, ClassT, ClassI, AttMinMaxList, GeneratedEx).

%Uniform value selection!
%add these options to input-ParameterList
gen_rnd_values([], []).
gen_rnd_values([I-[From, To]|R], [I-Value|Rest]):-
   number(From),
   number(To),
   rnd_float(From, To, Value),
   gen_rnd_values(R, Rest).
gen_rnd_values([I-[Same, Same]|R], [I-Same|Rest]):-
   gen_rnd_values(R, Rest).
