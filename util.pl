:- module(util, [pp_i_rule_set/3, pp_i_rule/3, pp_bit_v/3, pp_i_ex/3, assert_bb/2, retract_bb/2, retract_all/2,
		 my_random/3, rnd_float/3, rule_to_index_form/2,
		 log_2_mult/1, decimals/1, get_majority_class/2, check_majority_class/3,
		 mean_std_dev_movement/5, att_min_max/4, join_lists/3, examples_to_index_f/4,
		 classify_ex/4, classify_small_ex/4, fires/2, collect_conditions/3, collect_conditions_part/3, clean_conditions/2,
		 to_index_form/4,  to_index_small_form/4, create_head/3, feature_data_type/3, normalize/4, power_set/2]).
:- use_module([library(lists), library(random), distributions]).

:- volatile log_2_mult/1.
:- dynamic log_2_mult/1.

setup:-
    Multiple is 1 / log(2),
    assert(log_2_mult(Multiple)).

pp_i_rule_set([], _, _).
pp_i_rule_set([rule_set(Rules)|RuleSet], Bits, NoArgs):-
   pp_i_rule(Rules, Bits, NoArgs),
   pp_i_rule_set(RuleSet, Bits, NoArgs).
	
pp_i_rule([], _, _).
pp_i_rule([rule_i(I)-Rule|IRules], Bits, NoArgs):-
    write(Rule), write(': '),
    pp_bit_v(Bits, NoArgs, I),
    pp_i_rule(IRules, Bits, NoArgs).

pp_i_ex([], _, _).
pp_i_ex([(_,ex_i(I)-Ex)|Rest], Bits, NoArgs):-
    write(Ex), write(': '),
    pp_bit_v(Bits, NoArgs, I),
    pp_i_ex(Rest, Bits, NoArgs).
%%
% pretty print for bitvectors
pp_bit_v(Bits, Args, Number):-
	integer(Number),
	TotBits is Bits * Args,
	pp_bit(TotBits, Number).

pp_bit(0, Number):-
	Bit is Number /\ 1, %last bit
	(Bit > 0 ->
	 write(1)
	;
	 write(0)
	), nl.
pp_bit(TempBits, Number):-
	bit_template(TempBits, 1, 0, FBit),
	Bit is Number /\ FBit,
	(Bit > 0 ->
	 write(1)
	;
	 write(0)
	), 
	NewTempBits is TempBits - 1,
	pp_bit(NewTempBits, Number).

bit_template(0, 0, FBit, FBit).
bit_template(Bits, 1, TempBitB, FBit):-
	NewTBitB is TempBitB + 1,
	NewTempBitB is NewTBitB << 1,
	NewBits is Bits - 1,
	bit_template(NewBits, 0, NewTempBitB, FBit).  %Flag set to 0
bit_template(Bits, 0, TempBitB, FBit):-
	NewTempBitB is TempBitB << 1,
	NewBits is Bits - 1,
	bit_template(NewBits, 0, NewTempBitB, FBit).  %Flag set to 0


assert_bb([],_).
assert_bb([BK|BKs],Key):-
    assert(Key:BK),
    assert_bb(BKs,Key).

retract_bb([],_).
retract_bb([BK|BKs],Key):-
    retractall(Key:BK),
    retract_bb(BKs,Key).

retract_all([],_).
retract_all([BK|BKs],Key):-
    my_retract(Key:BK),
    retract_all(BKs,Key).

my_retract(R):-
    retract(R),!,
    my_retract(R).
my_retract(_):-
    true.

%%
% Checks class tuple and returns the majority class or tie if two or more classes are equal
% get_majority_class(+ClassT, -MajorityClass)

get_majority_class([First|Rest], MajorityClass):-
    get_maj_class(Rest, First, MajorityClass).

get_maj_class([], _-MajorityClass, MajorityClass).
get_maj_class([Freq-_|Rest], MajFreq-MajClass, MajorityClass):-
    MajFreq > Freq,
    get_maj_class(Rest, MajFreq-MajClass, MajorityClass).
get_maj_class([Freq-_|Rest], MajFreq-_MajClass, MajorityClass):-
    MajFreq = Freq,
    get_maj_class(Rest, MajFreq-tie, MajorityClass).
get_maj_class([Freq-Class|Rest], MajFreq-_, MajorityClass):-
    MajFreq < Freq,
    get_maj_class(Rest, Freq-Class, MajorityClass).

%%
% Checks class tuple and returns the majority class or tie if two or more classes are equal
% get_majority_class(+ClassT, RegionClass -MajorityClass)

check_majority_class([First|Rest], RegionClass, MajorityClass):-
   check_maj_class(Rest, First, MajClass),
   (MajClass = tie(TieList) ->
	(member(RegionClass, TieList) ->
	    MajorityClass = RegionClass
	;
	    MajorityClass = tie(TieList)
	)
    ;
	MajorityClass = MajClass
    ).

check_maj_class([], _-MajorityClass, MajorityClass).
check_maj_class([Freq-_|Rest], MajFreq-MajClass, MajorityClass):-
    MajFreq > Freq,
    check_maj_class(Rest, MajFreq-MajClass, MajorityClass).
check_maj_class([Freq-OMajClass|Rest], MajFreq-MajClass, MajorityClass):-
    MajFreq = Freq,
    check_maj_class(Rest, MajFreq-tie([OMajClass,MajClass]), MajorityClass).
check_maj_class([Freq-Class|Rest], MajFreq-_, MajorityClass):-
    MajFreq < Freq,
    check_maj_class(Rest, Freq-Class, MajorityClass).

mean_std_dev_movement([], 0, _, [], []). %Special case! when the node is empty!
mean_std_dev_movement([ex(Ex)|ExList], NoEx, ClassI, MeanList, StdDevList):-
   %subtract(Ex, [index(ClassI)-_], TempL),
   delete(Ex, index(ClassI)-_, TempL),
   to_zero(TempL, ZeroTempL),
   mean(ExList, NoEx, ClassI, TempL, MeanList),
   std_dev([ex(Ex)|ExList], MeanList, NoEx, ClassI, ZeroTempL, StdDevList).

to_zero([], []).
to_zero([I-_|R], [I-0|ZRest]):-
   to_zero(R, ZRest).

std_dev([], _MeanList, NoEx, ClassI, TempList, StdDevL):-
   apply_sqrt(TempList, NoEx, ClassI, StdDevL).
std_dev([ex(Ex)|Exs], MeanList, NoEx, ClassI, TempList, StdDevL):-
   h_std_ex(Ex, MeanList, ClassI, TempList, NewTempList),
   std_dev(Exs, MeanList, NoEx, ClassI, NewTempList, StdDevL).

apply_sqrt([], _, _, []).
apply_sqrt([I-Val|R], NoEx, ClassI, [I-StdDev|MeanList]):-
   (NoEx = 1 ->
     Variance is Val / NoEx
   ;
     Variance is Val / (NoEx - 1)
   ),
   StdDev is sqrt(Variance),
   apply_sqrt(R, NoEx, ClassI, MeanList).

h_std_ex([], _, _, TempList, TempList).
h_std_ex([ClassI-_|Vals], MeanList, CIndex, TempList, NewTempList):- %ignore classes
  ClassI = index(CIndex),
  h_std_ex(Vals, MeanList, ClassI, TempList, NewTempList).
h_std_ex([I-Val|Vals], MeanList, ClassI, TempList, NewTempList):-
  I \== index(ClassI),
  select(I-TempVal, TempList, Rest),
  member(I-MeanVal, MeanList),
  SqrDiff is (Val - MeanVal)^2,
  NewTempVal is TempVal + SqrDiff,
  h_std_ex(Vals, MeanList, ClassI, [I-NewTempVal|Rest], NewTempList).


mean([], NoEx, ClassI, TempList, MeanList):-
   apply_div(TempList, NoEx, ClassI, MeanList).
mean([ex(Ex)|Exs], NoEx, ClassI, TempList, MeanList):-
   handle_ex(Ex, ClassI, TempList, NewTempList),
   mean(Exs, NoEx, ClassI, NewTempList, MeanList).

apply_div([], _, _, []).
apply_div([I-Val|R], NoEx, ClassI, [I-Mean|MeanList]):-
   Mean is Val / NoEx,
   apply_div(R, NoEx, ClassI, MeanList).

handle_ex([], _, TempList, TempList).
handle_ex([ClassI-_|Vals], CIndex, TempList, NewTempList):- %ignore classes
  index(CIndex) = ClassI,
  handle_ex(Vals, ClassI, TempList, NewTempList).
handle_ex([I-Val|Vals], ClassI, TempList, NewTempList):-
  I \== index(ClassI),
  select(I-TempVal, TempList, Rest),
  NewTempVal is TempVal + Val,
  handle_ex(Vals, ClassI, [I-NewTempVal|Rest], NewTempList).

%%
% Find the maximum and minimum values for each attribute
% att_min_max(+ListOfExamples, +Arity, ClassIndex, -AttributesWithMinAndMaxValues)
% +ListOfExamples: List consisting of the examples to be used
% +Arity: Number of attributes
% +ClassIndex: The index to the classlabel, i.e. head(A1,A2,A3,pos) would indicate 4 as ClassIndex
% -AttributeWithMinAndMaxValues: Returns the attributes min and max values in the format AttributeIndex-(min, max)
att_min_max([Ex|TrainExs], Arity, ClassIndex, AttMinMaxL):-
    generate_attribut_index_vals(Ex, Arity, ClassIndex, AttIndexVals),
    update_min_max([Ex|TrainExs], AttIndexVals, AttMinMaxL).


%%
% update the minimum an maximum values for each attribute
% update_min_max(+TrainExList, +AttIndexVals, -AttMinMaxList)
% +TrainExList: The training examples
% +AttIndexVals: Temporary boundries
% -AttMinMaxList: Final boundries
update_min_max([], AttIndexVals, AttIndexVals).
update_min_max([Ex|TrainExs], AttIndexVals, AttMinMaxL):-
    update_values(AttIndexVals, Ex, NewAttIndexVals),
    update_min_max(TrainExs, NewAttIndexVals, AttMinMaxL).

%%
% Check and updats boundries if needed
% update_values(+Ex, +AttIndexVals, -NewAttIndexVals)
% +Ex: Example with values
% +AttIndexVals: Current boundries for each attribute
% -NewAttIndexVals: Updated (if needed) boundries for each attribute
update_values([], _, []).
update_values([index(I)-[Min,Max]|AttIndexVals], Ex, [index(I)-[Min, Max]|NewAttIndexVals]):-
    arg(I, Ex, Value),
    Value >= Min,
    Value =< Max,
    update_values(AttIndexVals, Ex, NewAttIndexVals).
update_values([index(I)-[Min,Max]|AttIndexVals], Ex, [index(I)-[Value, Value]|NewAttIndexVals]):-
    arg(I, Ex, Value),
    Value < Min,
    Value > Max,
    update_values(AttIndexVals, Ex, NewAttIndexVals).
update_values([index(I)-[Min,Max]|AttIndexVals], Ex, [index(I)-[Value, Max]|NewAttIndexVals]):-
    arg(I, Ex, Value),
    Value < Min,
    update_values(AttIndexVals, Ex, NewAttIndexVals).
update_values([index(I)-[Min,Max]|AttIndexVals], Ex, [index(I)-[Min, Value]|NewAttIndexVals]):-
    arg(I, Ex, Value),
    Value > Max,
    update_values(AttIndexVals, Ex, NewAttIndexVals).


normalize(Same, Same, Val1, Val1).
%normalize(Min, _, Min, 0.001).         %close to 0, here we assume parts off 1000 for each att
%normalize(_, Max, Max, 0.999).         %close to 1, -"-
normalize(Min, Max, Val1, NVal1):-
    TotSize is Max - Min,
     NVal1 is (Val1 - Min) / TotSize.

%%
% Generates an attribute index from an example
% generate_attribut_index_vals(+Ex, +Artity, +ClassIndex, -AttIndexsVals)
% +Ex: Example to see how many attributs each example has
% +Arity: Number of attributes
% +ClassIndex: The index to the classlabel, i.e. head(A1,A2,A3,pos) would indicate 4 as ClassIndex
% -AttIndexFreqs: Returns a list Key-Value pairs, i.e. index(n)-(min,max)
generate_attribut_index_vals(_, Arity, ClassIndex, AttIndexVals):-
    create_index_val(Arity, ClassIndex, AttIndexVals).

%%
% Creates an index_freqency key pair excluding the class index
% create_index_val(+Arity, +ClassIndex, -AttIndexVals).
% +Arity: Number of indexes to create
% +ClassIndex: The index to the classlabel, i.e. head(A1,A2,A3,pos) would indicate 4 as ClassIndex
% -AttIndexVals: Returns a list Key-Value pairs, i.e. index(n)-(min,max)
create_index_val(0, _, []).
create_index_val(ClassIndex, ClassIndex, AttIndexValsList):-
    NewArity is ClassIndex - 1,!,
    create_index_val(NewArity, ClassIndex, AttIndexValsList).
create_index_val(Arity, ClassIndex, [index(Arity)-[Max, Min]|RestAttIndexVals]):-
    NewArity is Arity - 1,
    current_prolog_flag(min_tagged_integer, Min),
    current_prolog_flag(max_tagged_integer, Max),
    create_index_val(NewArity, ClassIndex, RestAttIndexVals).

%%
% Joins folds (list of lists)
% join_lists(+ListOfListFoldTrainExamplesList, -List)
% +ListOfListFoldTrainExamplesList: List of List
% -List: Joined list
join_lists([], [], []).
join_lists([List|Lists], FinalList, [ExNo|Exs]):-
    length(List, ExNo),
    join_lists(Lists, RestFList, Exs),
    append(List, RestFList, FinalList).



%%
% Generates an attribute index from an example
% generate_attribut_index_vals(+Ex, +Artity, +ClassIndex, -AttIndexsVals)
% +Ex: Example to see how many attributs each example has
% +Arity: Number of attributes
% +ClassIndex: The index to the classlabel, i.e. head(A1,A2,A3,pos) would indicate 4 as ClassIndex
% -AttIndexFreqs: Returns a list Key-Value pairs, i.e. index(n)-(min,max)
examples_to_index_f([], _, _, []).
examples_to_index_f([Ex|Exs], Arity, ClassIndex, [ex(IndexEx)|IExs]):-
    create_index_val(Ex, Arity, ClassIndex, IndexEx),
    examples_to_index_f(Exs, Arity, ClassIndex, IExs).
%Error handeling
examples_to_index_f([Ex|Exs], Arity, ClassIndex, [ex(IndexEx)|IExs]):-
    %trace,
    create_index_val(Ex, Arity, ClassIndex, IndexEx),
    examples_to_index_f(Exs, Arity, ClassIndex, IExs).
%%
% Creates an index_freqency key pair excluding the class index
% create_index_val(+Arity, +ClassIndex, -AttIndexVals).
% +Arity: Number of indexes to create
% +ClassIndex: The index to the classlabel, i.e. head(A1,A2,A3,pos) would indicate 4 as ClassIndex
% -AttIndexVals: Returns a list Key-Value pairs, i.e. index(n)-(min,max)
create_index_val(_, 0, _, []).
create_index_val(Ex, ClassIndex, ClassIndex, [index(ClassIndex)-Class|Rest]):-
    arg(ClassIndex, Ex, Class),
    NewArity is ClassIndex - 1,!,
    create_index_val(Ex, NewArity, ClassIndex, Rest).
create_index_val(Ex, Arity, ClassIndex, [index(Arity)-Value|Rest]):-
    arg(Arity, Ex, Value),
    NewArity is Arity - 1,!,
    create_index_val(Ex, NewArity, ClassIndex, Rest).

%%
% Classify small rule tuple
%
classify_small_ex([], _, NewClassT, NewClassT).
classify_small_ex([_-rule(Conditions, _, _, ClassDist)|Rules], ValuesList, ClassT, FinalClassT):-
   fires(Conditions, ValuesList),!,
   keysort(ClassDist,Classes),
   reverse(Classes,[_-Class|_]),
   select(No-Class, ClassT, Rest),
   NewNo is No + 1,
   append([NewNo-Class], Rest, NewClassT),
   classify_small_ex(Rules, ValuesList, NewClassT, FinalClassT).
classify_small_ex([_|Rules], ValuesList, ClassT, FinalClassT):-
   %\+(fires(Cond, ValuesList)),
   classify_small_ex(Rules, ValuesList, ClassT, FinalClassT).

%%
%
% classify using valueslist as ulabeld example and the rules
classify_ex([], _, NewClassT, NewClassT).
classify_ex([_-rule(Conditions, _, _, _, ClassDist)|Rules], ValuesList, ClassT, FinalClassT):-
   fires(Conditions, ValuesList),
   keysort(ClassDist,Classes),
   reverse(Classes,[_-Class|_]),
   select(No-Class, ClassT, Rest),
   NewNo is No + 1,
   append([NewNo-Class], Rest, NewClassT),
   classify_ex(Rules, ValuesList, NewClassT, FinalClassT).
classify_ex([_-rule(Cond, _, _, _, _)|Rules], ValuesList, ClassT, FinalClassT):-
   \+(fires(Cond, ValuesList)),
   classify_ex(Rules, ValuesList, ClassT, FinalClassT).

fires([], _).
fires([index(I)-[From, non]|R], ValuesList):-
    number(From),
    member(index(I)-Val, ValuesList),
    From =< Val,!,
    fires(R, ValuesList).
fires([index(I)-[non, To]|R], ValuesList):-
    number(To),
    member(index(I)-Val, ValuesList),
    Val < To,!,
    fires(R, ValuesList).
fires([index(I)-[From, To]|R], ValuesList):-
    number(From),
    number(To),
    member(index(I)-Val, ValuesList),
    From =< Val,
    Val < To,!,
    fires(R, ValuesList).

create_head([], Ex, Ex).
create_head([index(I)-Value|Values], Head, Ex):-
    arg(I, Head, Value),
    create_head(Values, Head, Ex).

%%
% Collect the conditions to a list from rules
% collect_conditions(+ListOfRules, +ClassIndex, -AllConditions, ClassConditionList, RuleNumberList)
% +ListOfRules: Input as a list of rules in the format of RuleNo-(H:-B1,B2,...,Bn)
% +ClassIndex: The index to the classlabel, i.e. head(A1,A2,A3,pos) would indicate 4 as ClassIndex
% -AllConditions: Returns the set of all conditions
% -ClassConditionList: Returns ClassConditionList in the following format Class-Conditions,
%                      where Conditions is a list on the format AttributeIndex op Value, i.e. index(1) < 9
% -RuleNumberList: Returns a list of Rule numbers, i.e. 23,  that corresponds to the ClassConditionList.
%                  Hence the first instance of ClassConditionList is derived from the
%                  rule with the number first in the RuleNumberList
collect_conditions([], _, []).
collect_conditions([rule_set(RuleL)|RulesL], ClassIndex, FinalRules):-
    collect_condition(RuleL, ClassIndex, RulePart),
    collect_conditions(RulesL, ClassIndex, RestParts),
    append([rule_set(RulePart)], RestParts, FinalRules).
collect_conditions([tree(RuleL)|RulesL], ClassIndex, FinalRules):-
    collect_condition(RuleL, ClassIndex, RulePart),
    collect_conditions(RulesL, ClassIndex, RestParts),
    append([tree(RulePart)], RestParts, FinalRules).    %keep tree structure in a better way ...later
%collect_conditions([RuleL|RulesL], ClassIndex, FinalRules):-
%    collect_condition(RuleL, ClassIndex, RulePart),
%    collect_conditions(RulesL, ClassIndex, RestParts),
%    append(RulePart, RestParts, FinalRules).

collect_conditions_part([], _, []).
collect_conditions_part([RuleL|RulesL], ClassIndex, [RulePart|RestParts]):-
    collect_condition(RuleL, ClassIndex, RulePart),
    collect_conditions_part(RulesL, ClassIndex, RestParts).


collect_condition([], _, []).
collect_condition([(H:-B)|Rules], ClassI, [rule(Conditions,ClassT)|RRules]):-
    arg(ClassI, H, ClassT),
    H =..[_|HIndex],
    collect_conds(HIndex, B, Conditions),
    collect_condition(Rules, ClassI, RRules).

%%
% Collect one condition
collect_conds(H, (Cond,Conds), [ACond|Cs]):-
   cond_to_atom(H, Cond, ACond),
   collect_conds(H, Conds, Cs),!.
collect_conds(H, (Cond), [ACond]):-
   cond_to_atom(H, Cond, ACond).

%%
% Transform to index format
cond_to_atom(HIndex, Cond < Val, index(Index) < Val):-
    nth1(Index, HIndex, CondVar),
    CondVar == Cond.
cond_to_atom(HIndex, Cond >= Val, index(Index) >= Val):-
    nth1(Index, HIndex, CondVar),
    CondVar == Cond.


%%
%  removes contraditory "rules" completely and removes subsume conditions
%  clean_conditions(+ClassConditionList, -NewClassConditionList)
%  +ClassConditionList: Class-ConditionList
%  -NewClassConditionList: Class-NewConditionList
clean_conditions([], []).
clean_conditions([rule_set(TreeRules)|Trees], Result):-
    clean_condition(TreeRules, PartRes),
    clean_conditions(Trees, RestRes),
    append([rule_set(PartRes)], RestRes, Result).
clean_conditions([tree(TreeRules)|Trees], [tree(TreeRules)|Trees]).
    %clean_condition(TreeRules, PartRes),
    %clean_conditions(Trees, RestRes),
    %append([tree(PartRes)], RestRes, Result).

clean_condition([], []).
%clean_condition([rule([],_ClassT)|CCs], NewClassConditionList):-
%    !,clean_condition(CCs, NewClassConditionList).  %remove empty rules! (default rule)
clean_condition([rule(ConditionList,_ClassT)|CCs], NewClassConditionList):-
    contradictory(ConditionList),!,
    clean_condition(CCs, NewClassConditionList).
clean_condition([rule(ConditionList,ClassT)|CCs], [rule(NewConditionList,ClassT)|NewClassConditionList]):-
    clean_cond(ConditionList, NewConditionList),
    clean_condition(CCs, NewClassConditionList).
%%
% removes subsumed conditions if they exist, i.e. index(7) >= 5  && index(7) >= 2 -> index(7) >= 5
% clean_condition(+ConditionList, -NewConditionList),
% +ConditionList: List of conditions in the format Cond Op Val, i.e. index(7) >= 5
% -NewConditionList: New List of conditions

clean_cond([], []).
clean_cond([Cond|CList], [NewCond|NewConds]):-
    remove_subsumed(CList, Cond, NewCond, NewCList),!,
    clean_cond(NewCList, NewConds).


%%
% check if Cond Op Val has subsumed values or is itself subsumed in CList,
% it returns the most specific Cond and a new list where all subsumptioned Conditions is removed
% remove_subsumed(+CList, +Conditionl, -NewCond, -NewCList)
% +CList: List of conditions in the format Cond Op Val, i.e. index(7) >= 5
% +Condition: conditions in the format Cond Op Val, i.e. index(7) >= 5
% -NewCond: conditions in the format Cond Op Val, i.e. index(7) >= 5
% -NewCList: List of conditions in the format Cond Op Val, i.e. index(7) >= 5

remove_subsumed([], Cond, Cond, []).
remove_subsumed([Cond < Val2|CL], Cond < Val1, NewCond, NewCList):-
    (Val1 =< Val2 ->
       %write('Remove subsumed (1): '), write(Val1), write(' < '), write(Val2),nl,
       remove_subsumed(CL, Cond < Val1, NewCond, NewCList),!
    ;
       %write('Remove subsumed (1): '), write(Val1), write(' >= '), write(Val2),nl,
       remove_subsumed(CL, Cond < Val2, NewCond, NewCList),!
    ).
remove_subsumed([Cond >= Val2|CL], Cond >= Val1, NewCond, NewCList):-
    (Val1 >= Val2 ->
       %write('Remove subsumed (2): '), write(Val1), write(' >= '), write(Val2),nl,
       remove_subsumed(CL, Cond >= Val1, NewCond, NewCList),!
    ;
       %write('Remove subsumed (2): '), write(Val1), write(' < '), write(Val2),nl,
       remove_subsumed(CL, Cond >= Val2, NewCond, NewCList),!
    ).
remove_subsumed([C|CL], Cond, NewCond, [C|NewCList]):-
    remove_subsumed(CL, Cond, NewCond, NewCList).


%%
% succeds if the conditionList is indeed contradictory
% contradictory(+ConditionList)
% +ConditionList: List of conditions in the format Cond Op Val, i.e. index(7) >= 5

contradictory([Cond < Val1|CList]):-
    member(Cond >= Val2, CList),
    Val1 < Val2,
    write('Contradiction (1): '), write(Cond), write(' < '), write(Val1), write(CList),nl, !.
contradictory([Cond >= Val1|CList]):-
    member(Cond < Val2, CList),
    Val1 > Val2,
    write('Contradiction (2): '), write(Cond), write(' >= '), write(Val1),write(CList),nl, !.
contradictory([_|CList]):-
    !,contradictory(CList).



%%
% changes format
%
rule_to_index_form([], []).
rule_to_index_form([tree(TreeRules)|Rest], Result):-
    rule_to_index_f(TreeRules, PartRes),
    rule_to_index_form(Rest, RestRes),
    append([tree(PartRes)], RestRes, Result).
rule_to_index_form([rule_set(TreeRules)|Rest], Result):-
    rule_to_index_f(TreeRules, PartRes),
    rule_to_index_form(Rest, RestRes),
    append([rule_set(PartRes)], RestRes, Result).
    
rule_to_index_f([], []).
rule_to_index_f([rule(Id, Cond, ClassT)|Rest], [rule(Id, NewCond, ClassT)|NewRest]):- %Id for debugging
   to_index_format(Cond, NewCond),!,
   rule_to_index_f(Rest, NewRest).

%%
% changes format
%
to_index_small_form([], _TrainEx, _ClassI, []).
to_index_small_form([tree(Rules)|Rest], TrainEx, ClassI, FRes):-
   to_index_small_form1(Rules, TrainEx, ClassI, PartRes),
   to_index_small_form(Rest, TrainEx, ClassI, RestRes),
   append(PartRes, RestRes, FRes).
   

to_index_small_form1([], _, _, []).
to_index_small_form1([rule(Cond, ClassT)|Rest], TrainEx, ClassI, [NoEx-rule(NewCond, MeanL, StdDevL, ClassT)|NewRest]):-
   to_index_format(Cond, NewCond),
   filter_examples(TrainEx, NewCond, ExInNode),
   length(ExInNode, NoEx),
   mean_std_dev_movement(ExInNode, NoEx, ClassI, MeanL, StdDevL),!,
   to_index_small_form1(Rest, TrainEx, ClassI, NewRest).
%%
% changes format
%
to_index_form([], _, _, []).
to_index_form([rule(Cond, ClassT)|Rest], TrainEx, ClassI, [NoEx-rule(NewCond, ExInNode, MeanL, StdDevL, ClassT)|NewRest]):-
   to_index_format(Cond, NewCond),
   filter_examples(TrainEx, NewCond, ExInNode),
   length(ExInNode, NoEx),
   mean_std_dev_movement(ExInNode, NoEx, ClassI, MeanL, StdDevL),!,
   to_index_form(Rest, TrainEx, ClassI, NewRest).
%Error handeling
to_index_form([rule(Cond, ClassT)|Rest], TrainEx, ClassI, [NoEx-rule(NewCond, ExInNode, MeanL, StdDevL, ClassT)|NewRest]):-
   %trace,
   to_index_format(Cond, NewCond),
   filter_examples(TrainEx, NewCond, ExInNode),
   length(ExInNode, NoEx),
   mean_std_dev_movement(ExInNode, NoEx, ClassI, MeanL, StdDevL),
   to_index_form(Rest, TrainEx, ClassI, NewRest).

filter_examples([], _, []).
filter_examples([ex(Ex)|Exs], Conds, [ex(Ex)|R]):-
   fires(Conds, Ex),!,
   filter_examples(Exs, Conds, R).
filter_examples([_|Exs], Conds, R):-
   filter_examples(Exs, Conds, R).

%%
% Change format of conditions so that we use index as key
% to_index_format(+CondL, -CondIL),
% +CondL: List of Conditions
% -CondIL: List of conditions in the format of: index(4)-[From,To] or non if no value exist
to_index_format([index(V)-[From,To]|Rest], [index(V)-[From,To]|Rest]).  %is already on index format.
to_index_format([], []).
to_index_format([index(V) < To|Rest], [index(V)-[From,To]|R]):-
    (select(index(V) >= From, Rest, NewRest) ->
       %((From = To),  -> write('Same1'),nl;true),
       !,to_index_format(NewRest, R)
    ;
       From = non,!,
       to_index_format(Rest, R)
    ).
to_index_format([index(V) >= From|Rest], [index(V)-[From,To]|R]):-
    (select(index(V) < To, Rest, NewRest) ->
       %((From = To) -> write('Same2'),nl;true),!,
       !,to_index_format(NewRest, R)
    ;
       To = non,!,
       to_index_format(Rest, R)
    ).

rnd_float(X, X, X).
rnd_float(F, T, V):-
    Ff is float(F),
    Tf is float(T),
    random(Ff, Tf, V).

my_random(X, X, X).
my_random(F, T, V):-
    random(F, T, V).


%%%
% The retrives the datatypes from background knowledge
%
feature_data_type(Head, Key, FeatureDTList):-
    clause(Key:Head, B),
    %write(B),nl,
    %functor(B,_, Arity), %can be ignored
    %write(Arity),nl,
    extract_all(B, FeatureDTList).%,
    %write('FeatureDTList: '), write(FeatureDTList),nl.

extract_all(B, [BClause|BCs]):-
    arg(1, B, BClause),
    arg(2, B, Bs),!,
    extract_all(Bs, BCs).
extract_all(Arg, [Arg]).

%%
% Power set - from a list of atoms it returns a list of all possible combinations of the input atoms
%
power_set(InputList, PowerSet):-
   setof(X, subseq0(InputList, X), PowerSet).

decimals(100000000).

:-setup.

