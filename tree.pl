%%
% Ensamble induction method using DAC
%
% Author: Tony Lindgren
%

:- module(tree,[tree/9, create_class_tuples/2]).
:- use_module([library(lists), library(random), util]).

tree(TrainExList, BKList, Parameters, Head, BBKey, ClassIndex, ClassT, Tree, CHead):-
    %write('Starting to build tree...'),nl,	
    assert_bb(BKList,BBKey), %assert background knowledge, used when building the tree
    feature_data_type(Head, BBKey, FeatureDTList),
    member(classification, Parameters),
    count_classes(TrainExList, BBKey, FeatureDTList, ClassIndex, ClassTupleC, ClassList),!,
    Tree = dac_node(Head, FeatureDTList, ClassList, _Children),
    build_tree(TrainExList, BBKey, Parameters, ClassIndex, ClassTupleC, Tree),
    write('finished building tree...cleaning up..'),nl,
    create_class_tuples(BBKey, ClassT),
    retract_bb([Head], BBKey),
    keysort(ClassList, SortedTuples),
    reverse(SortedTuples, [_-DefaultClass|_]),
    copy_term(Head,CHead),
    CHead =.. [_|Args],
    nth1(ClassIndex, Args, DefaultClass). 

%Add Laplace-correction?
build_tree([], _, _, _, _, dac_node(_, _, _,no_examples_left)):-!.
build_tree(TrainExList,_BBKey, Parameters, _, _, dac_node(_,_,_,min_cov)):-
    length(TrainExList, NoEx),
    member(min_cov(MinCov),Parameters),
    MinCov >= NoEx,!.
    %write('Min cov Leaf'),nl.
build_tree(TrainExList, _BBKey, Parameters, _, _, dac_node(_,_,ClassList,clean_enough)):-
    member(min_margin(MMargin),Parameters),
    length(TrainExList, NoEx),
    check_class_margin(ClassList, NoEx, MarginL),
    sort(MarginL, SortedMargin),
    reverse(SortedMargin, [CP1-_,CP2-_|_]),
    Margin is abs(CP1 - CP2),
    Margin > MMargin,!.
    %write('Clean enough'),nl.
build_tree(TrainExList, BBKey, Parameters, ClassIndex, ClassTupleL, dac_node(_Head,FeatureDTList,ClassList,ChildrenNodes)):-
    %write('Splitting'),nl,
    select_feature(FeatureDTList, TrainExList, ClassTupleL, ClassList, ClassIndex, 1, FeatureRanking),!,
    select_values(FeatureRanking, BBKey, Parameters, FeatureDTList, ClassIndex, TrainExList, ChildrenNodes).

check_class_margin([],_,[]).
check_class_margin([Count-_|Counts],NoTot,[Percent-_|Ps]):-
	Percent is Count / NoTot,
	check_class_margin(Counts,NoTot,Ps).

%%	This baby should call build tree, be sure to add all children to
%	the tree...fix a stopping criterion at build tree as well...
select_values(FeatureRanking, BBKey, Param, FeatureDTList,  ClassIndex, TrainExList, ChildrenNodes):-
        (member(rnd_feature(yes), Param) ->
           length(FeatureRanking, FLength),
           repeat,             
             random(0, FLength, Rnd),
             FeatureSel is  Rnd + 1,
             FeatureSel \== ClassIndex,   
           nth1(FeatureSel,FeatureRanking,F1-List1-_),
           F = F1,
           List = List1
	 ;  
	   find_highest_gain(FeatureRanking, F2-List2-_),
          F = F2,
          List = List2
        ),!,
        remove_numeric(F, NewF),
	apply_split(List, BBKey, Param, FeatureDTList, ClassIndex, TrainExList, NewF, ChildrenNodes).

remove_numeric(numeric(F), F).

%Only works for numerical, right now
find_highest_gain([First|Rest],Result):-
	find_highest_gain(Rest,First,Result).
find_highest_gain([],Result,Result).
find_highest_gain([Feature-Split-Gain|Rest],_-[_,_]-TGain,Result):-
	TGain < Gain,!,
	find_highest_gain(Rest,Feature-Split-Gain,Result).
find_highest_gain([_|Rest],Temp,Result):-
	find_highest_gain(Rest,Temp,Result).

%Apply Binary split -write for categoric case!
apply_split([LeftSplitList, [Val2-(_,_)|_]], BBKey, Param, FeatureDTList, Index, TrainExList, F, [LeftChildren,RightChildren]):-
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
            Split = Middle
	    
        ),
	%write('LeftVal: '),write(Val1),nl,
	%write('RightVal: '),write(Val2),nl,
	%write('split: '),write(Split),nl,
	get_examples(LeftSplitList, TrainExList, LeftExamples, RightExamples),!,
        %length(LeftSplitList,LSPL), write(LSPL),nl,
        %length(LeftExamples,LEL), write(LEL),nl,
        %length(TrainExList,TEL), write(TEL),nl,
        %length(RL,REL), write(REL),nl,
        create_class_tuples(BBKey, ClassTL),
        do_count_classes(LeftExamples, Index, ClassTL, LClassTupleL),
        LeftChildren = dac_node(F < Split, FeatureDTList, LClassTupleL, _LeftChildrenNodes),!,
        %write(LeftExamples),nl, 
	build_tree(LeftExamples, BBKey, Param, Index, LClassTupleL, LeftChildren),
	do_count_classes(RightExamples, Index, ClassTL, RClassTupleL),
        RightChildren = dac_node(F >= Split, FeatureDTList, RClassTupleL, _RightChildrenNodes),!,         
        %write(RightExamples),nl,
	build_tree(RightExamples, BBKey, Param, Index, RClassTupleL, RightChildren).

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
    select_feature(FeatureDTList, TrainExList, ClassTupleL, ClassDist, ClassIndex, I, FSGs).
select_feature([F|FeatureDTList], TrainExList, ClassTupleL, ClassDist, ClassIndex, I, [F-Split-FGain|FSGs]):-
        split(TrainExList, ClassDist, _, ClassIndex, I, F, Split-Gain),
	Split=[LS,RS],
	last(LS, Val1-(_,_)),
	RS=[Val2-(_,_)|_],
	(Val1 = Val2 ->    %Do not split into impossible values!
	   FGain = 0
	;
	   FGain = Gain
	),	 
        %write('F-Split-Gain: '), write(F-Split-Gain),nl,
        %Split=[LS,RS],
        %length(LS,LenLs),
        %length(RS,LenRs), 
        %write('Left: '),write(LenLs),nl, 
        %write('Right: '),write(LenRs),nl, 
        NewI is I + 1,
	select_feature(FeatureDTList, TrainExList, ClassTupleL, ClassDist, ClassIndex, NewI, FSGs).

split([], _, _, _, _, _, []). %:- write('Out of Examples').
%split(_, _, _, _, _, class(_), []-0). %:-write('Using classlabel - remove').
split(Exs, ClassDist, ClassTupleL, ClassIndex, I, numeric(_), Split-Gain):-
	get_all_values(Exs, ClassIndex, 1, I, KeyValue),
        %length(KeyValue,KVL),
        %write('KVL: '),write(KVL),nl,
	keysort(KeyValue, Sorted),
        %length(Sorted,SKVL),
        %write('SortedKVL: '),write(SKVL),nl,
        %trace,  
	find_best_binary_split(Sorted, [], _, ClassDist, I, [[],[]], ClassTupleL-first, Split-Gain).

find_best_binary_split([], _, _, _, _, _, Split-Value, Split-Value).
find_best_binary_split([Arg-(Key,Class)|Sorted], _, _, ClassDist, I, [OldLC,_], _-first, Split-Value):- 
	find_change([Arg-(Key,Class)|Sorted], NewClass, Class, [0-Class], LeftC, Arg, LSplit, RSplit),
        %select(_N-Class,ClassTuplesL,Rest),
        %append(LeftC,Rest,CompLeftC), 
	merge_count(ClassDist, OldLC, LeftC, NewLeftC),
        right_count(NewLeftC, ClassDist, RightC),
        %length(LSplit,NLCLen),
        %length(RSplit,RCLen),
        %TotCount is NLCLen + RCLen,
        %write('Temp Tot Len(1): '), write(TotCoun),nl,  
	i_gain([NewLeftC, RightC], ClassDist, TValue),!,
	find_best_binary_split(RSplit, LSplit, NewClass, ClassDist, I, [NewLeftC, RightC], [LSplit, RSplit]-TValue, Split-Value).
find_best_binary_split([Arg-(Key,C)|Exs], OldLSplit, Class, ClassDist, I, [OldLC, _OldRC], [OLSplit, ORSplit]-TempV, Split-Value):-
	find_change([Arg-(Key,C)|Exs], NewClass, Class, [0-Class], LeftC, Arg, LSplit, RSplit),
	merge_count(ClassDist, OldLC, LeftC, NewLeftC),
	append(OldLSplit,LSplit,NewLSplit),
	right_count(NewLeftC, ClassDist, RightC),
        %length(NewLSplit,NLCLen),
        %length(RSplit,RCLen),
        %TotCount is NLCLen + RCLen,
        %write('Temp Tot Len(2): '),write(TotCount),nl,  
        %trace,
	i_gain([NewLeftC, RightC], ClassDist, TValue),!,
	(TValue > TempV ->
	  find_best_binary_split(RSplit, NewLSplit, NewClass, ClassDist, I, [NewLeftC, RightC], [NewLSplit, RSplit]-TValue, Split-Value)
	;
	  find_best_binary_split(RSplit, NewLSplit, NewClass, ClassDist, I, [NewLeftC, RightC], [OLSplit, ORSplit]-TempV, Split-Value)
	).

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

i_gain(CountList, ClassDist, Gain):-
        %laplace_corr(ClassDist, NewClassDist, CountList, NewCountList),  %chec this one later one
	sum(ClassDist, 0, Tot),
        %sum_l(CountList, PartTot),
        log_2_mult(Multiple),
	i(ClassDist, Tot, Tot, Multiple, 0, IVal),
	i(CountList, Tot, Multiple, 0, NewIVal),!,
	Gain is IVal -  NewIVal.
/*
laplace_corr(ClassDist, NewClassDist, CountList, NewCountList):-
    length(CountList, L),
    add_size(ClassDist, L, NewClassDist),
    add_size(CountList, 1, NewCountList).

add_size([], _, []).
add_Size([Count-Class|CC], Size, [NCount-Class|NCC]):-
    NCount is Count + Size,
    add_size(CC, NCC).
*/
%i/5 - only for features
i([], _, _, FVal, FVal).
i([L|R], Tot, Mul, TVal, FVal):-
	%is_list(L),!,   
        sum(L, 0, Size),
	i(L, Tot, Size,  Mul, TVal, FVal1),
        i(R, Tot, Mul, 0, FVal2),!,
        FVal is FVal1 + FVal2.

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
    nth1(I, FeatureDTList, class(_)),
    do_count_classes(TrainEx, I, ClassTuplesL, ClassTuplesList).

create_class_tuples(Key, ClassTuplesL):-
    findall(C,Key:class(C),ClassesL),
    class_tuple_c(ClassesL, ClassTuplesL).

class_tuple_c([],[]).
class_tuple_c([Class|Classes],[0-Class|Tuples]):-
	class_tuple_c(Classes,Tuples).

do_count_classes([], _, CTL, CTL).
do_count_classes([Ex|ExList], I, CTL, ClassList):-
	arg(I,Ex,C),
	select(Count-C,CTL,CTLPart),
	NewCount is Count + 1,
	do_count_classes(ExList, I, [NewCount-C|CTLPart], ClassList).
















