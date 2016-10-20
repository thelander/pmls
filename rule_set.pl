
%%
% Ensamble induction method using SAC
%
% Author: Tony Lindgren
%

:- module(rule_set,[rule_set/9]).
:- use_module([library(lists), library(random), util]).

rule_set(TrainExList, BKList, Parameters, Head, BBKey, ClassIndex, ClassTupleC, RoughRuleSet, CHead):-
    %write('Starting to build rule set...'),nl,
	%trace,
    assert_bb(BKList,BBKey), %assert background knowledge, used when building the tree
    feature_data_type(Head, BBKey, FeatureDTList),
    member(classification, Parameters),
    count_classes(TrainExList, BBKey, FeatureDTList, ClassIndex, ClassTupleC, ClassList),!,
    %create_class_tuples(BBKey, ClassT),
    %copy_term(ClassT, ClassTupleC),
    build_rule_set(TrainExList, ClassList, BBKey, Parameters, FeatureDTList, ClassIndex, ClassTupleC, RoughRuleSet),
    %write('finished building rule set...cleaning up..'),nl,
    retract_bb([Head], BBKey),
    keysort(ClassList, SortedTuples),
    reverse(SortedTuples, [_-DefaultClass|_]),
    copy_term(Head,CHead),
    CHead =.. [_|Args],
    nth1(ClassIndex, Args, DefaultClass).

build_rule_set([], _, _, _, _, _, _, []).
build_rule_set(TrainExList, ClassList, BBKey, Parameters, FeatureDTList, ClassIndex, ClassTupleL, [Rule|Rules]):-
    copy_term(ClassTupleL, CClassTupleL1),
    %length(TrainExList,NoEx),
    %write('Building rules from tot no ex: '),write(NoEx),nl,
    build_rule(TrainExList, ClassList, [], FeatureDTList, BBKey, Parameters, ClassIndex, CClassTupleL1, Rule, [], NewTrainExList),
    do_count_classes(NewTrainExList, ClassIndex, CClassTupleL1, NewClassList),!,
    %count_classes(NewTrainExList, BBKey, FeatureDTList, ClassIndex, _, NewClassList),!,
    build_rule_set(NewTrainExList, NewClassList, BBKey, Parameters, FeatureDTList, ClassIndex, ClassTupleL, Rules).

build_rule([], ClassList, Conditions, _, _, _, _, _, rule(Conditions, ClassList), Rest, Rest). %:-
%    write('Out of examples'),!. %should not end up here
build_rule(TrainExList, ClassList, Conditions, _, _, Parameters, _, _, rule(Conditions, ClassList), Rest, Rest):-
    length(TrainExList, NoEx),
    member(min_cov(MinCov),Parameters),
    MinCov >= NoEx,!.
    %write('min_coverage'),nl.
build_rule(TrainExList, ClassList, Conditions, _, _, Parameters, _, _, rule(Conditions, ClassList), Rest, Rest):-
    member(min_margin(MMargin),Parameters),
    length(TrainExList, NoEx),
    check_class_margin(ClassList, NoEx, MarginL),
    sort(MarginL, SortedMargin),
    reverse(SortedMargin, [CP1-_,CP2-_|_]),
    Margin is abs(CP1 - CP2),
    Margin > MMargin,!.
    %write('Clean enough'),nl.
build_rule(TrainExList, ClassList, Conditions, FeatureDTList, BBKey, Parameters, ClassIndex, ClassTupleL, TRule, TRest, FRest):-
    %write('Refining rule'),nl,
    %test from here
    %trace,
    select_feature(FeatureDTList, TrainExList, ClassTupleL, ClassList, ClassIndex, 1, FeatureRanking),!,
    %write('After select_feature'),nl,
    %nl,write(FeatureRanking),nl,
    select_value(FeatureRanking, Parameters, ClassIndex, F, ValueList, Op),
    %write('After select_value'),nl,
    copy_term(ClassTupleL, CClassT),
    get_inside_outside(ValueList, TrainExList, ClassIndex, CClassT, BelowExamples, BelowDist, AboveExamples, AboveDist, Split),
    (Split = cannot_split ->
      TRule = rule(Conditions, ClassList),
      TRest = FRest
    ;
      %write('After get_inside_outside'),nl,
      %length(BelowExamples, NoBelow),
      %length(BelowExamples, NoAbove),
      (Op = < ->                                               
        %write('New Feature: '),write(F), write(' < '), write(Split),nl,
        %write('No Examples < covered by new rule: '), write(NoBelow),nl,
        %write('With Distribution: '), write(BelowDist),nl,
        append(AboveExamples, TRest, NewTRest1), 
        build_rule(BelowExamples, BelowDist, [F < Split|Conditions], FeatureDTList, BBKey, Parameters, ClassIndex, ClassTupleL, TRule, NewTRest1, FRest)
      ;
        %write('New Feature: '),write(F), write(' >= '), write(Split),nl,
        %write('No Examples >= covered by new rule: '), write(NoAbove),nl,
        %write('With Distribution: '), write(AboveDist),nl,
        append(BelowExamples, TRest, NewTRest2),
        build_rule(AboveExamples, AboveDist, [F >= Split|Conditions], FeatureDTList, BBKey, Parameters, ClassIndex, ClassTupleL, TRule, NewTRest2, FRest)
      )
    ).

%%	This baby should call build tree, be sure to add all children to
%	the tree...fix a stopping criterion at build tree as well...
select_value(FeatureRanking, Param, ClassIndex, F, List, Op):- 
        (member(rnd_feature(yes), Param) ->
           length(FeatureRanking, FLength),
           repeat,             
             random(0, FLength, Rnd),
             FeatureSel is  Rnd + 1,
             FeatureSel \== ClassIndex,   
           nth1(FeatureSel,FeatureRanking,F1-List1-_-Op1),
           F = F1,
           List = List1,
	   Op = Op1
	 ;  
	  find_highest_gain(FeatureRanking, F2-List2-_-Op2),
          F = F2,
          List = List2,
	  Op = Op2
        ).

remove_numeric(numeric(F), F).

%Only works for numerical, right now
find_highest_gain([First|Rest],Result):-
	find_highest_gain(Rest,First,Result).
find_highest_gain([],Result,Result).
find_highest_gain([Feature-Split-Gain-Op|Rest],_-[_,_]-TGain-_,Result):-
	TGain < Gain,!,
	find_highest_gain(Rest,Feature-Split-Gain-Op,Result).
find_highest_gain([_|Rest],Temp,Result):-
	find_highest_gain(Rest,Temp,Result).

get_inside_outside([LeftSplitList, [Val2-(_,_)|_]], TrainExList, ClassIndex, ClassT, BelowExamples, BelowDist, AboveExamples, AboveDist, Split):-
	last(LeftSplitList, Val1-(_,_)),
        %trace,
	decimals(D),
	Diff is Val2 - Val1,
	Term is Diff / 2,
	Middle is round((Val1 + Term) * D) / D,!,    
        (Val1 =:= Val2 ->
            %trace,
	    %write('Single-'),
            Split is round(Val1 * D) / D
	 ;
 	    %write('Val2: '),write(Val2), write(' Val1: '),write(Val1),nl,
	    %write('Middle-'),
	    %write('Split: '), write(Split),nl,
	    %write('Middle: '), write(Middle),nl, 
            Split = Middle
	    
        ),
	%write('LeftVal: '),write(Val1),nl,
	%write('RightVal: '),write(Val2),nl,
	%write('split: '),write(Split),nl,
	get_examples(LeftSplitList, TrainExList, BelowExamples, AboveExamples),!,
	%write('After get examples'),nl,
        %length(LeftSplitList,LSPL), write(LSPL),nl,
        %length(LeftExamples,LEL), write(LEL),nl,
        %length(TrainExList,TEL), write(TEL),nl,
        %length(RL,REL), write(REL),nl,
        %create_class_tuples(BBKey, ClassTL),
	copy_term(ClassT, CClassTL1),
	do_count_classes(BelowExamples, ClassIndex, CClassTL1, BelowDist),
	%write('After do count classes 1'),nl,
	copy_term(ClassT, CClassTL2),
	do_count_classes(AboveExamples, ClassIndex, CClassTL2, AboveDist).
	%write('After do count classes 2'),nl.
%We cannot split this guy - left´s call it a node!
get_inside_outside([LeftSplitList, RightSplitList], _TrainExList, _ClassIndex, _ClassT, _BelowExamples, _BelowDist, _AboveExamples, _AboveDist, cannot_split):-
	(LeftSplitList = []
	;
	 RightSplitList = []
	).
	%write('We cannot split this guy - left´s call it a node!').
/*
        write('LeftSplitList: '),write(LeftSplitList),nl,
	write('RightSplitList: '),write(RigthSplitList),nl,
	write('TrainExList: '), write(TrainExList),nl,
	write('ClassIndex: '), write(ClassIndex),nl,
	write('ClassT: '),write(ClassT),nl,
	write('BelowExamples: '), write(BelowExamples),nl,
	write('BelowDist: '),write(BelowDist),nl,
	write('AboveExamples: '),write(AboveExamples),nl,
	write('AboveDist: '),write(AboveDist),nl,
	write('Split: '),write(Split),nl,trace.
*/
get_examples(SplitList, TrainExList, LeftList, RightList):-
	%trace,
        left_part(SplitList, TrainExList, LeftList),
        list_difference_eq(TrainExList, LeftList, RightList).

list_difference_eq([], _, []).
list_difference_eq([Ex|Train], LeftList, RightList):-
        member(Ex, LeftList),!,
        list_difference_eq(Train, LeftList, RightList).
list_difference_eq([Ex|Train], LeftList, [Ex|RightList]):-
        list_difference_eq(Train, LeftList, RightList).
         
left_part([], _, []).
left_part([_-(Key,_)|VKCs], TrainExList, [Ex|LeftEx]):-
	nth1(Key, TrainExList, Ex),
	left_part(VKCs, TrainExList, LeftEx).

select_feature([], _TrainExList, _, _ClassDist, _ClassIndex, _I, []).
select_feature([class(_)|FeatureDTList], TrainExList, ClassTupleL, ClassDist, ClassIndex, I, FSGs):- %Do not use class for splitt
    !,select_feature(FeatureDTList, TrainExList, ClassTupleL, ClassDist, ClassIndex, I, FSGs).
select_feature([F|FeatureDTList], TrainExList, ClassTupleL, ClassDist, ClassIndex, I, [F-Split-Gain-Op|FSGs]):-
	%write('Innan split...'),nl,
	%write('ClassDist: '),write(ClassDist),nl,
	%write('TrainExList: '), write(TrainExList),
        split(TrainExList, ClassDist, ClassTupleL, ClassIndex, I, F, Split-Gain-Op),
	%Split=[LS,RS],
	%last(LS, Val1-(_,_)),
	%RS=[Val2-(_,_)|_],
	%(Val1 = Val2 ->    %Do not split into impossible values!
	%   FGain = 0
	%;
	%   FGain = Gain
	%),	 
        %write('F-Split-Gain: '), write(F-Split-Gain),nl,
        %Split=[LS,RS],
        %length(LS,LenLs),
        %length(RS,LenRs), 
        %write('Left: '),write(LenLs),nl, 
        %write('Right: '),write(LenRs),nl, 
        NewI is I + 1,
	%write('I = '),write(I),nl,
	%(I = 3 -> trace; true),!,
	select_feature(FeatureDTList, TrainExList, ClassTupleL, ClassDist, ClassIndex, NewI, FSGs).

split([], _, _, _, _, _, []). %:- write('Out of Examples').
%split(_, _, _, _, _, class(_), []-0). %:-write('Using classlabel - remove').
split(Exs, ClassDist, ClassTupleL, ClassIndex, I, numeric(_), Split-Gain-Operator):-
	%trace,
	get_all_values(Exs, ClassIndex, 1, I, KeyValue),
	%length(Exs, NoEx),
	%write('NoEx: '), write(NoEx),nl,
        %length(KeyValue,KVL),
        %write('KVL: '),write(KVL),nl,
	keysort(KeyValue, Sorted),
        %length(Sorted,SKVL),
        %write('SortedKVL: '),write(SKVL),nl,
        %trace,
	%write(Sorted),nl,
	%write(ClassDist),nl,
	%write(I),nl,
	%write(ClassTupleL),nl,
	find_best_binary_split(Sorted, [], _, ClassDist, I, [[],[]], _-first, Split-Gain, _, ClassTupleL, Operator).
	%nl,
	%write(Split),nl,write(Gain), nl, write(Operator),nl.

find_best_binary_split([], _, _, _, _, _, Split-Value, Split-Value, Op, _, Op).
find_best_binary_split([Arg-(Key,Class)|Sorted], _, _, ClassDist, I, [OldLC,_], _-first, Split-Value, Operator, ClassTupleL, FOp):- 
	find_change([Arg-(Key,Class)|Sorted], NewClass, Class, [0-Class], LeftC, Arg, LSplit, RSplit),
        %select(_N-Class,ClassTuplesL,Rest),
        %append(LeftC,Rest,CompLeftC), 
	merge_count(ClassDist, OldLC, LeftC, NewLeftC),
        right_count(NewLeftC, ClassDist, RightC),
        %length(LSplit,NLCLen),
        %length(RSplit,RCLen),
        %TotCount is NLCLen + RCLen,
        %write('Temp Tot Len(1): '), write(TotCoun),nl,
	i_gain([NewLeftC], ClassDist, ClassTupleL, TLValue),
	i_gain([RightC], ClassDist, ClassTupleL, TRValue),!,
	%write('LeftSide: '),write(NewLeftC), write('LeftGainV: '), write(TLValue),nl,
	%write('RightSide: '),write(RightC),  write('RightGainV: '), write(TRValue),nl,
	get_highest(TLValue, TRValue, TValue, Operator),
	%write('We choose: '),write(TValue), write(Operator),nl,
	find_best_binary_split(RSplit, LSplit, NewClass, ClassDist, I, [NewLeftC, RightC], [LSplit, RSplit]-TValue, Split-Value, Operator, ClassTupleL, FOp).
find_best_binary_split([Arg-(Key,C)|Exs], OldLSplit, Class, ClassDist, I, [OldLC, _OldRC], [OLSplit, ORSplit]-TempV, Split-Value, TempOp, ClassTupleL, FOp):-
	find_change([Arg-(Key,C)|Exs], NewClass, Class, [0-Class], LeftC, Arg, LSplit, RSplit),
	merge_count(ClassDist, OldLC, LeftC, NewLeftC),
	append(OldLSplit,LSplit,NewLSplit),
	right_count(NewLeftC, ClassDist, RightC),
        %length(NewLSplit,NLCLen),
        %length(RSplit,RCLen),
        %TotCount is NLCLen + RCLen,
        %write('Temp Tot Len(2): '),write(TotCount),nl,  
        %trace,
	%i_gain([NewLeftC, RightC], ClassDist, TValue),!,   OLD  for trees
	i_gain([NewLeftC], ClassDist, ClassTupleL, TLValue),
	i_gain([RightC], ClassDist, ClassTupleL, TRValue),!,
	%write('LeftSide: '),write(NewLeftC), write('LeftGainV: '), write(TLValue),nl,
	%write('RightSide: '),write(RightC),  write('RightGainV: '), write(TRValue),nl,
	get_highest(TLValue, TRValue, TValue, Operator),
	%write('We choose: '),write(TValue), write(' '),write(Operator),nl,
	(TValue > TempV ->
          %write('Found better gain value: '),write(TValue), write(' '),write(Operator),nl,
	  %write('LeftSide: '),write(NewLeftC), write('LeftGainV: '), write(TLValue),nl,
	  %write('RightSide: '),write(RightC),  write('RightGainV: '), write(TRValue),nl,
 	  find_best_binary_split(RSplit, NewLSplit, NewClass, ClassDist, I, [NewLeftC, RightC], [NewLSplit, RSplit]-TValue, Split-Value, Operator, ClassTupleL, FOp)
	;
	  find_best_binary_split(RSplit, NewLSplit, NewClass, ClassDist, I, [NewLeftC, RightC], [OLSplit, ORSplit]-TempV, Split-Value, TempOp, ClassTupleL, FOp)
	).

get_highest(TLValue, TRValue, TLValue, <):-
	TLValue > TRValue.
get_highest(TLValue, TRValue, TRValue, >=):-
	TLValue =< TRValue.

merge_count([], _OldLC, _NewLC, []).
merge_count([_-Class|Classes], OldLC, NewLC, [NewestC-Class|NewestLCs]):-
	member(OldC-Class,OldLC),
	member(NewC-Class,NewLC),!,
	NewestC is OldC + NewC,
	merge_count(Classes, OldLC, NewLC, NewestLCs).
merge_count([_-Class|Classes], OldLC, NewLC, [OldC-Class|NewestLCs]):-
	member(OldC-Class,OldLC),!,
	merge_count(Classes, OldLC, NewLC, NewestLCs).
merge_count([_-Class|Classes], OldLC, NewLC, [NewC-Class|NewestLCs]):-
	member(NewC-Class,NewLC),!,
	merge_count(Classes, OldLC, NewLC, NewestLCs).
merge_count([_|Classes], OldLC, NewLC, NewestLCs):-
	merge_count(Classes, OldLC, NewLC, NewestLCs).

right_count([], ClassDist, ClassDist).
right_count([LCount-Class|LCs], ClassDist, [RCount-Class|RCs]):-
        select(TCount-Class, ClassDist, NewClassDist),
	RCount is TCount - LCount,
      	right_count(LCs, NewClassDist, RCs).

i_gain(CountList, ClassDist, ClassTupleL, Gain):-
	%write(ClassDist),nl,
	%trace,
        laplace_corr(ClassDist, ClassTupleL, NewClassDist, CountList, NewCountList),  %check this one later one
	sum(NewClassDist, 0, Tot),
	NewCountList = [NCountL],
	sum(NCountL, 0, Size),
        %sum_l(CountList, PartTot),
        log_2_mult(Multiple),
	i(NewClassDist, Tot, Tot, Multiple, 0, IVal),
	i(NewCountList, Tot, Multiple, 0, NewIVal),!,
	Gain is  Size * (IVal - NewIVal).

laplace_corr(ClassDist, ClassTupleL, NewClassDist, [CountList], [NewCountList]):-
        copy_term(ClassTupleL, CClassTuple1),
	add_one_to_all(CClassTuple1, ClassDist, NewClassDist),
        copy_term(ClassTupleL, CClassTuple2),
	add_one_to_all(CClassTuple2, CountList, NewCountList).

add_one_to_all([], _, []).
add_one_to_all([_-Class|CC], List, [NCount-Class|NCC]):-
    (member(OtherC-Class,List) ->
      NCount is OtherC + 1
    ;
      NCount is 1
    ),
    add_one_to_all(CC, List, NCC).

%i/5 - only for features
%i([], _, _, FVal, FVal).
i([L], Tot, Mul, TVal, FVal):-
	%is_list(L),!,   
        sum(L, 0, Size),
	i(L, Tot, Size,  Mul, TVal, FVal).

%i/6 - only for tot, (and called by i/5)
i([], Tot, Size, _, Entropy, FVal):-
    FVal is Entropy * Size / Tot,!.
i([0-_|R], Tot, Size, Mul, TVal, FVal):-
        i(R, Tot, Size, Mul, TVal, FVal).
i([E-_|R], Tot, Size, Mul, TVal, FVal):-
	Quota is E / Size,
        (Quota < 0 ->
	   trace, true
	 ;
	   NTVal is TVal + -(Quota * log(Quota) * Mul),
	   i(R, Tot, Size, Mul, NTVal, FVal)
         ).

sum_l([],0):-!.
sum_l([L|Ls],T):-
    sum(L, 0, T1),
    sum_l(Ls,T2),
    T is T1 + T2.

sum([],Tot,Tot):-!.
sum([Count-_|Cs],TTot,FTot):-
	NewTTot is TTot + Count,
	sum(Cs, NewTTot,FTot).

find_change([], Class, Class, FCount, FCount, _, [], []).
find_change([Arg-(Key,Class)|AKCs], NewClass, Class, TempCount, FCount, _, [Arg-(Key,Class)|Rest], RSplit):- %Detect change
        select(TempC-C,TempCount,RTempC),
	NewTCount is TempC + 1,
        append([NewTCount-C], RTempC, NewTempCount),!,
	find_change(AKCs, NewClass, Class, NewTempCount, FCount, Arg, Rest, RSplit).
find_change([Arg-(Key,C)|AKCs], NewClass, Class, TempCount, FCount, Arg, [Arg-(Key,C)|Rest], RSplit):- %Fetch equal values
    (select(TempC-C, TempCount, RTempC) ->	
        NewTCount is TempC + 1,!,
        append([NewTCount-C], RTempC, NewTempCount),
    	find_end(AKCs, NewClass, Class, NewTempCount, FCount, Arg, Rest, RSplit) %look for end
    ;    
        append([1-C], TempCount, NewTempCount),
	find_end(AKCs, NewClass, Class, NewTempCount, FCount, Arg, Rest, RSplit)
    ). 
find_change([Arg-(Key,NewClass)|AKCs], NewClass, _Class, FCount, FCount, _, [],[Arg-(Key,NewClass)|AKCs]). %end
find_end([Arg-(Key,C)|AKCs], NewClass, Class, TempCount, FCount, Arg, [Arg-(Key,C)|Rest], RSplit):- %Fetch equal values
    (select(TempC-C, TempCount, RTempC) ->	
        NewTCount is TempC + 1,!,
        append([NewTCount-C], RTempC, NewTempCount),
    	find_end(AKCs, NewClass, Class, NewTempCount, FCount, Arg, Rest, RSplit) %look for end
    ;    
        append([1-C], TempCount, NewTempCount),
	find_end(AKCs, NewClass, Class, NewTempCount, FCount, Arg, Rest, RSplit)
     ).
find_end([Arg-(Key,NewClass)|AKCs], NewClass, _Class, FCount, FCount, _, [],[Arg-(Key,NewClass)|AKCs]). %end

get_all_values([],_, _Key,_I,[]).
get_all_values([Ex|Exs], ClassIndex, Key, I, [Arg-(Key,Class)|KeyVals]):-
	arg(I,Ex,Arg),
	arg(ClassIndex,Ex,Class),
	NewKey is Key + 1,
	get_all_values(Exs, ClassIndex, NewKey, I, KeyVals).

count_classes(TrainEx, Key, FeatureDTList, I, ClassTuplesL, ClassTuplesList):-  
    findall(C,Key:class(C),ClassesL),
    class_tuple_c(ClassesL, ClassTuplesL), 
    nth1(I, FeatureDTList, class(_)),!,
    do_count_classes(TrainEx, I, ClassTuplesL, ClassTuplesList).

do_count_classes([], _, CTL, CTL).
do_count_classes([Ex|ExList], I, CTL, ClassList):-
	arg(I,Ex,C),
	select(Count-C,CTL,CTLPart),
	NewCount is Count + 1,!,
	do_count_classes(ExList, I, [NewCount-C|CTLPart], ClassList).

/*do_count_classes(ExList, I, CTL, ClassList):-
	write('Found an error!!!'),nl,
	write(ExList),nl,
	write(I),nl,
	write(CTL),nl,
	write(ClassList),nl, trace.
*/
create_class_tuples(Key, ClassTuplesL):-
    findall(C,Key:class(C),ClassesL),
    class_tuple_c(ClassesL, ClassTuplesL).

class_tuple_c([],[]).
class_tuple_c([Class|Classes],[0-Class|Tuples]):-
	class_tuple_c(Classes,Tuples).

check_class_margin([],_,[]).
check_class_margin([Count-_|Counts],NoTot,[Percent-_|Ps]):-
	Percent is Count / NoTot,
	check_class_margin(Counts,NoTot,Ps).