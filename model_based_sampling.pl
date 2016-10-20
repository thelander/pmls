:- module(model_based_sampling,[mbs/7]).
:- use_module([library(lists), library(random), library(assoc), library(terms), util, distributions]).

%%
% Model based sampling (MBS) uses an ensemble to select values for attributes to create
% syntetic examples labeld by the ensamble.
% mbs(+RulesList, +SampleSize, +TrainExList, +Head, +ClassIndex, +ClassT, -GenExs)
mbs(RulesList, SampleSize, TrainExList, Head, ClassIndex, ClassT, GenExs):-
   TrainExList=[Ex|_],
   functor(Ex, _, Arity),
   att_min_max(TrainExList, Arity, ClassIndex, AttMinMax),
   collect_conditions(RulesList, ClassIndex, ClassConditionList),
   clean_conditions(ClassConditionList, NClassConditionList),
   examples_to_index_f(TrainExList, Arity, ClassIndex, IndexExL),
   to_index_small_form(NClassConditionList, IndexExL, ClassIndex, NClassCondIL),!,
   keysort(NClassCondIL, SortedClassConds),
   length(TrainExList, NoEx),
   mean_std_dev_movement(IndexExL, NoEx, ClassIndex, MeanL, StdDevL),
   term_hash([],HashV),
   generate_problist(SortedClassConds, FirstProbL),
   list_to_assoc([HashV-cov(prob(MeanL, StdDevL, FirstProbL))], AVLTree),
   get_non_used_atts(AttMinMax, SortedClassConds, NotUsedL),
   keysort(AttMinMax, SAttMinMax),
   sort(NotUsedL, SNotUsedL),
   %write('Starting generating syntetic examples using mbs'),nl,
   gen_exs_from_ensemble(SAttMinMax, SNotUsedL, SampleSize, AVLTree, FirstProbL, SortedClassConds, AttMinMax, Head, Arity, ClassIndex, ClassT, GenExs). %,
   %write('finished generating syntetic examples using mbs'),nl.

%%
% generates a probabilitylist prob(From, To, ProbList)
% generate_problist(+Rules, -ProbTuple)
generate_problist(Rules, prob(1, NoSamples, RndRules)):-
   summarize_samples(Rules, NoSamples),
   random_permutation(Rules, RndRules).

summarize_samples([], 0).
summarize_samples([Size-_|Rest], NoSamples):-
   summarize_samples(Rest, RestSamples),!,
   NoSamples is Size + RestSamples.

%%
% filter rules on index values only
% filter_rules(+Rules, +IndexL, +[], -NewTempL).
filter_rules([], _, NewTempL, NewTempL).
filter_rules([Size-rule(Cond, Mean, StdDev, ClassT)|R], I, TempList, NewTempL):-
   at_least_one_member(I, Cond),
   filter_rules(R, I, [Size-rule(Cond, Mean, StdDev, ClassT)|TempList], NewTempL).
filter_rules([_|R], I, TempList, NewTempL):-
   filter_rules(R, I, TempList, NewTempL).

at_least_one_member([I-_|_], Cond):-
   member(I-_, Cond),!.
at_least_one_member([_|R], Cond):-
   at_least_one_member(R, Cond).

%%
% checks the rules if there are any attibute(s) not used by any of the rules, if so it reruns it
% get_non_used_atts(+IndexKeysL, +RulesL, -NotUsedL).
get_non_used_atts([], _, []).
get_non_used_atts([I-_|R], Rules, NonUsedL):-
   is_used(Rules, I),!,
   get_non_used_atts(R, Rules, NonUsedL).
get_non_used_atts([I-_|R], Rules, [I|NonUsed]):-
   get_non_used_atts(R, Rules, NonUsed).

is_used([_-rule(Conditions, _, _, _)|_], I):-
   member(I-_, Conditions).
is_used([_|R], I):-
   is_used(R, I).

%%
% Keep track on the number of syntetic examples to generate and calls the predicate model_based_sampling/13 accordingly
% gen_exs_from_ensemble(+Indexs, +NotUsedI, +SS, +AVLTree, +FirstProbList, +Rules, +AttMinMax, +Head, +Arity, +ClassI, +ClassT, -SynteticExs).
gen_exs_from_ensemble(_, _, 0, _, _, _, _, _, _, _, _, []).
gen_exs_from_ensemble(Indexs, NotUsedI, Pno, AVLTree, FirstProbList, Rules, AttMinMax, Head, Arity, ClassI, ClassT, [Ex|GenExs]):-
   model_based_sampling(Indexs, NotUsedI, Rules, Rules, AVLTree, FirstProbList, Head, AttMinMax, ClassI, ClassT, [], NewAVLTree, Ex),
   NewPno is Pno - 1,!,
   gen_exs_from_ensemble(Indexs, NotUsedI, NewPno, NewAVLTree, FirstProbList, Rules, AttMinMax, Head, Arity, ClassI, ClassT, GenExs).


%%
% Algorithm and strategy to use to create syntetic data (model based sampling)
% Presented in more detail in the papper
% model_based_sampling(+ValuesL, +NotUsedI, +Rules, +OriginalRules, +AVLTree, +ProbList, +Head, +AttMinMax, +ClassI, +ClassT, +ValuesList, +FAVLTRee, -GenEx):-
model_based_sampling([], _, _,  Rules, NewAVLTree, _, Head, _AttMinMax, ClassI, ClassT, ValuesList, NewAVLTree, Ex):-
   %all attributes covered - classify generated Example
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
   arg(ClassI, Ex, MajClass).
%Special case empty rules
model_based_sampling(ValuesL, [], [], OrgRules, AVLTree,  prob(_, _, empty), Head, AttMinMax, ClassI, ClassT, ValuesList, FAVLTree, GenEx):-
        term_hash([], HashV),
        get_assoc(HashV, AVLTree, cov(prob(MeanL, StdDevL, _))),
        generate_rest_of_values(ValuesL, MeanL, StdDevL, GenValues),
        append(ValuesList, GenValues, FinalValues),
        keysort(FinalValues, SFinalValues),
        model_based_sampling([], [], [], OrgRules, AVLTree, prob(_, _, empty), Head, AttMinMax, ClassI, ClassT, SFinalValues, FAVLTree, GenEx).
model_based_sampling(ValuesL, [], Rules, OrgRules, AVLTree, prob(From, To, ProbL), Head, AttMinMax, ClassI, ClassT, ValuesList, FAVLTRee, GenEx):-
   my_random(From, To, Value),
   find_rule(Value, ProbL, Rule),
   gen_rnd_values(Rule, ValuesList, AttMinMax, norm_dist, RndValues),
   %write('ValuesList: '),write(ValuesList),nl,
   %write('RndValues: '),write(RndValues),nl,
   remove_key(ValuesL, RndValues, RemValues),
   keysort(RemValues, SRemValues),
   append(ValuesList, RndValues, NewValuesList),
   keysort(NewValuesList, SNewValuesL),
   term_hash(SRemValues, HashKey),
   %length(RndValues, NoNewValues),
   %write('Generated (used): '), write(NoNewValues),write(' values for example'),nl,
   %length(SRemValues, NoValuesL),
   %write('number of attributes left: '), write(NoValuesL),nl,
   (SRemValues = [] ->
        model_based_sampling([], [], [], OrgRules, AVLTree, prob(_,_,empty), Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
   ;
        (get_assoc(HashKey, AVLTree, _) ->
        %Has matching hash
                get_assoc(HashKey, AVLTree, cov(prob(_,_,ThisRules))),
                (ThisRules = empty ->
                        model_based_sampling(SRemValues, [], [], OrgRules, AVLTree, prob(_,_,empty), Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
                ;
                        filter_left_list(ThisRules, SNewValuesL, OkValsL),
                        (OkValsL = [] ->
                                model_based_sampling(SRemValues, [], [], OrgRules, AVLTree, prob(_,_,empty), Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
                        ;
                                generate_problist(OkValsL, OkValsProb),
                                %write('re-using information from AVLTree'),nl,
                                model_based_sampling(SRemValues, [], OkValsL, OrgRules, AVLTree, OkValsProb, Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
                        )
                )
        ;
        %Create new node
                (filter_rules(Rules, SRemValues, [], FilteredRules),  %Filters only on index, no exact at least one member of rules that we need!
                        (FilteredRules = [] ->
                                put_assoc(HashKey, AVLTree, cov(prob(_,_,empty)), NewAVLTree),
                                model_based_sampling(SRemValues, [], [], OrgRules, NewAVLTree,  prob(_,_,empty), Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
                        ;
                                generate_problist(FilteredRules, FilteredProb),
                                put_assoc(HashKey, AVLTree, cov(FilteredProb), NewAVLTree),
                                filter_left_list(FilteredRules, SNewValuesL, OkValsL),
                                (OkValsL = [] ->
                                        model_based_sampling(SRemValues, [], [], OrgRules, NewAVLTree,  prob(_,_,empty), Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
                                ;
                                        generate_problist(OkValsL, OkValsProb),
                                        model_based_sampling(SRemValues, [], OkValsL, OrgRules, NewAVLTree,  OkValsProb, Head, AttMinMax, ClassI, ClassT, SNewValuesL, FAVLTRee, GenEx)
                                )
                        )
                )

        )
    ).
%Do we have notused indexes, remove them
model_based_sampling(ValuesL, NotUsedI, Rules, OriginalRules, AVLTree, ProbList, Head, AttMinMax, ClassI, ClassT, ValuesList, FAVLTRee, GenEx):-
   %Case of notUsedIndexes
   NotUsedI \== [],!,
   term_hash([],HashV),
   get_assoc(HashV, AVLTree, cov(prob(MeanL, StdDevL, _))),
   generate_not_used_values(NotUsedI, MeanL, StdDevL, GenValues),
   remove_key(ValuesL, GenValues, RemValues),
   keysort(RemValues, SRemValues),
   append(ValuesList, GenValues, FinalValues),
   keysort(FinalValues, SFinalValues),
   %length(NotUsedI, NoNotUsed),
   %write('Generated (not used): '), write(NoNotUsed),write(' values for example'),nl,
   %length(SRemValues, NoValuesL),
   %write('number of attributes left: '), write(NoValuesL),nl,
   model_based_sampling(SRemValues, [], Rules, OriginalRules, AVLTree, ProbList, Head, AttMinMax, ClassI, ClassT, SFinalValues, FAVLTRee, GenEx).

generate_not_used_values([], _, _, []).
generate_not_used_values([I|R], MeanL, StdDevL, [I-Value|GenValues]):-
   member(I-Mean, MeanL),
   member(I-StdDev, StdDevL),
   normal(Mean, StdDev, Value),
   generate_not_used_values(R, MeanL, StdDevL, GenValues).

generate_rest_of_values([], _, _, []).
generate_rest_of_values([I-_|R], MeanL, StdDevL, [I-Value|GenValues]):-
   member(I-Mean, MeanL),
   member(I-StdDev, StdDevL),
   normal(Mean, StdDev, Value),
   generate_rest_of_values(R, MeanL, StdDevL, GenValues).

remove_key([], _, []).
remove_key([I-_|R], RndValues, RemValues):-
   member(I-_, RndValues),!,
   remove_key(R, RndValues, RemValues).
remove_key([I-Value|R], RndValues, [I-Value|RemValues]):-
   remove_key(R, RndValues, RemValues).

gen_rnd_values(_-rule(CondL, MeanL, StdDevL, _ClassT), ValuesList, _AttMinMax, norm_dist, RndValues):-
   my_subtract(CondL, ValuesList, GenCondL),                                        %only generate values for attributes which lack values
   get_rnd_value(GenCondL, MeanL, StdDevL, norm_dist, RndValues).

my_subtract([], _, []).
my_subtract([I-_|R], ValuesList, GenCondL):-
   member(I-_, ValuesList),!,
   my_subtract(R, ValuesList, GenCondL).
my_subtract([Elem|R], ValuesList, [Elem|GenCondL]):-
   my_subtract(R, ValuesList, GenCondL).

%%
% Generate syntetic value using normal distrbution from rule
% get_rnd_value(+IndexCond, +MeanL, +StdDevL, +norm_dist, -SynteticValues).
get_rnd_value([], _MeanL, _StdDevL, norm_dist, []).
get_rnd_value([I-[non, non]|CondL], MeanL, StdDevL, norm_dist, [I-Val|RndVals]):-
   member(I-Mean, MeanL),
   member(I-StdDev, StdDevL),
   normal(Mean, StdDev, Val),
   get_rnd_value(CondL, MeanL, StdDevL, norm_dist, RndVals).
get_rnd_value([I-[non, To]|CondL], MeanL, StdDevL, norm_dist, [I-Val|RndVals]):-
   member(I-Mean, MeanL),
   member(I-StdDev, StdDevL),
   repeat,
     normal(Mean, StdDev, Val),
     Val < To,
   get_rnd_value(CondL, MeanL, StdDevL, norm_dist, RndVals).
get_rnd_value([I-[From, non]|CondL], MeanL, StdDevL, norm_dist, [I-Val|RndVals]):-
   member(I-Mean, MeanL),
   member(I-StdDev, StdDevL),
   repeat,
     normal(Mean, StdDev, Val),
     From =< Val,
   get_rnd_value(CondL, MeanL, StdDevL, norm_dist, RndVals).
get_rnd_value([I-[From, To]|CondL], MeanL, StdDevL, norm_dist, [I-Val|RndVals]):-
   member(I-Mean, MeanL),
   member(I-StdDev, StdDevL),
   repeat,
     normal(Mean, StdDev, Val),
     From =< Val,
     Val < To,
   get_rnd_value(CondL, MeanL, StdDevL, norm_dist, RndVals).

%%
% filter the list on indexes and values
% filter_left_list(+List, +Values, -FilteredList).
filter_left_list(List, [], List).  %No Conditions to filter away
filter_left_list([], _, []).
filter_left_list([Elem|LeftL], ValuesList, Rest):-
    contradictory(ValuesList, Elem),!,
    filter_left_list(LeftL, ValuesList, Rest).
filter_left_list([E|LeftL], ValuesList, [E|Rest]):-
    filter_left_list(LeftL, ValuesList, Rest).

contradictory([], _):-!,fail.
contradictory([I-Val|_], Element):-
    member(I-[From, non], Element),
    number(From),
    From > Val,!.
contradictory([I-Val|_], Element):-
    member(I-[non, To], Element),
    number(To),
    Val >= To,!.
contradictory([I-Val|_], Element):-
    member(I-[From, To], Element),
    number(From),
    number(To),
    From > Val,
    Val >= To,!.
contradictory([_|Rest], Element):-
    !,contradictory(Rest, Element).

%%
% Select leafnode according to the probability distibution
% find_rule(+Value, +RuleL, -Rules)
find_rule(Value, [CovSize-rule(CondL, MeanL, StdDevL, ClassT)|_], CovSize-rule(CondL, MeanL, StdDevL, ClassT)):-
   CovSize >= Value.
find_rule(Value, [CovSize-_|Rest], CondL):-
   NewValue is Value - CovSize,
   find_rule(NewValue, Rest, CondL).
