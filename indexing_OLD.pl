%%%%
% Module for indexing rules and examples
%
% Author: Tony Lindgren
%
% version 0.0.1
% This module takes either rules or examples and create and index for them.
% Example calls,
% index_examples([ex(3,6,pos),ex(2,8,pos),ex(4,6,neg),ex(5,7,neg)], [index(1)-[2,5], index(2)-[6,8]], 3, R).
% index_rules([rule([index(1)-[non,4]],[2-pos, 0-neg]),rule([index(1)-[4,non]],[2-neg,0-pos])], [index(1)-[2,5], index(2)-[6,8]], R).

:- module(indexing, [index_examples/5, index_rules/4, %get_max_index/3,
		     %get_min_max_indexes/3,
		     build_index_tree/4]).
:- use_module([library(pldoc), library(plunit), library(clpb), util]).

	   

%returns the maximum index for the an example 
get_max_index(Ex, ClassIndex, MaxIndex):-
	functor(Ex, _, No),
	MaxBitIndex = 0xFF, %8 bits - 1 byte, put as parameter..later..
	BitShift is msb(MaxBitIndex + 1),
	%Assuming that we have one class label
	%MaxIndex is (No - 1) * (MaxBitIndex + 1). 
	get_max_b(No, 1, ClassIndex, MaxBitIndex, BitShift, MaxIndex).

get_max_b(0, MaxIndex, _, _, _, MaxIndex).
get_max_b(ClassIndex, TIndex, ClassIndex, MaxBitIndex, BitShift, MaxIndex):-
	NewNo is ClassIndex - 1,
	get_max_b(NewNo, TIndex, ClassIndex, MaxBitIndex, BitShift, MaxIndex).
get_max_b(No, TIndex, ClassIndex, MaxBitIndex, BitShift, MaxIndex):-
	MovedI is TIndex << BitShift,
	NewTI is MovedI + MaxBitIndex,
	NewNo is No - 1,
	get_max_b(NewNo, NewTI, ClassIndex, MaxBitIndex, BitShift, MaxIndex).

build_index_tree(IndexedRules, Parameters, AttMinMax, node([], [], NoRules, LeftT, RightT)):-
	length(IndexedRules, NoRules),
	member(node_size(NodeSize), Parameters),
	MaxBitIndex = 0xFF, %8 bits - 1 byte, put as parameter..later..
	BitShift is msb(MaxBitIndex + 1),
	%Get all possible permuations
	BitV=[_,_,_,_,_,_,_,_], findall(BitV,labeling(BitV), Perms),
	set_zero(BitShift, ZList),
	delete(Perms, ZList, NewPerms),
	build_tree(NodeSize, Parameters, NewPerms, AttMinMax, MaxBitIndex, BitShift, dummy1-dummy2, IndexedRules, [node(_, _, NoRules, LeftT, RightT)]).

set_zero(0, []).
set_zero(C, [0|R]):-
	NC is C - 1,
	set_zero(NC, R).

%Found leaf 
build_tree(NodeSize, _, _, _, _, _, _, IndexedRules, [node(leaf-size, IndexedRules, NoRules, [], [])]):-
	NoRules =< NodeSize.%,
	%write('Found leaf node').
build_tree(_NodeSize, _Parameters, _, _AttMinMax, _, _, PrevSplitV-PrevSplitV, IndexedRules, [node(leaf-similar, IndexedRules, _NoRules, [], [])]):-
	%only apply if the number of rules is lower or equal to that of the number of trees
	%motivation for this is that if no variation is low this could a bound  
	similar_conds(IndexedRules).
build_tree(NodeSize, Parameters, Perms, AttMinMax, _MaxBitIndex, BitShift, _-PrevSplitVector, IndexedRules, [node(FSplitVector, IRules, NoRules, LeftT, RightT)]):-
	%trace,
	length(AttMinMax, NoArgs),
	%NewNoArgs is NoArgs - 1, 
	find_best_split(NoArgs, BitShift, IndexedRules, NoRules, Perms, NoArgs, dummy-0, SplitVector, 0, 0, LeftRules, RightRules),!,
	length(LeftRules, NoLeftR),
        length(RightRules, NoRightR),
	%write('Split node into: '),nl,
	%write('Left part: '), write(NoLeftR),nl,
	%write('Right part: '), write(NoRightR),nl,
	%write('with splitVector: '),pp_bit_v(BitShift, NoArgs,SplitVector),nl,nl,
	((NoLeftR > 0, NoRightR > 0) ->
	  FSplitVector = SplitVector,
	  IRules = [],
	  LeftT = [node(_, _, NoLeftR, _, _)],
	  build_tree(NodeSize, Parameters, Perms, AttMinMax, _, BitShift, PrevSplitVector-SplitVector, LeftRules, LeftT),
	  RightT = [node(_, _, NoRightR, _, _)],
	  build_tree(NodeSize, Parameters, Perms, AttMinMax, _, BitShift, PrevSplitVector-SplitVector, RightRules, RightT)
       ;
	  %else finished
          %write('Stop as we have one leg wich are zero'),nl,
	  FSplitVector = leaf-no_split,
	  IRules = IndexedRules,
	  LeftT = [],
	  RightT = []
	).
 
similar_conds([]).
similar_conds([rule_i(_IndexV)-_|R]):-
	%write(IndexV),write('  '),pp_bit_v(8, 4, IndexV),
	similar_conds(R).
	%check_conds(IR, Conds).

check_conds([], _).
check_conds([rule_i(_)-rule(RConds, L,_)|R], Conds):-
	length(RConds, L),
	length(Conds, L),  %Must be equally long else fail!!
	check_each_cond(RConds, Conds),!,
	check_conds(R, Conds).

check_each_cond([], _).
check_each_cond([index(I)-[non, _]|R], Conds):-
	member(index(I)-[non,_], Conds),!,
        check_each_cond(R, Conds).
check_each_cond([index(I)-[_, non]|R], Conds):-
	member(index(I)-[_, non], Conds),!,
        check_each_cond(R, Conds).
check_each_cond([index(I)-[F, _]|R], Conds):-
	member(index(I)-[F,_], Conds),!,
        check_each_cond(R, Conds).
check_each_cond([index(I)-[_, T]|R], Conds):-
	member(index(I)-[_, T], Conds),!,
        check_each_cond(R, Conds).	  

find_best_split(0, _, _, _, _, _, SplitVector-_Score, SplitVector, LeftR, RightR, LeftR, RightR).
find_best_split(NoArgs, BitShift, IndexedRules, NoRules, Perms, NumArgs, TempSplitV-TempScore, FSplitVector, TLeft, TRight, FLeftRules, FRightRules):-
        best_arg_score(Perms, BitShift, NumArgs, NoArgs, IndexedRules, NoRules, 0, NewTempScore, dummy, NewTempSplitVector, [], [], NewLeft, NewRight), 
	NewNoArgs is NoArgs - 1,       
        (NewTempScore > TempScore ->
	   %write('NewTempSplitVector: '), write(NewTempSplitVector), write('  '), pp_bit_v(BitShift, NumArgs, NewTempSplitVector),nl,
	   find_best_split(NewNoArgs, BitShift, IndexedRules, NoRules, Perms, NumArgs, NewTempSplitVector-NewTempScore, FSplitVector, NewLeft, NewRight, FLeftRules, FRightRules) 
	;
	   find_best_split(NewNoArgs, BitShift, IndexedRules, NoRules, Perms, NumArgs, TempSplitV-TempScore, FSplitVector, TLeft, TRight, FLeftRules, FRightRules)
	).

best_arg_score([], _, _, _, _, _, FScore, FScore, NewSplitV, NewSplitV, FinalL, FinalR, FinalL, FinalR).
best_arg_score([Perm|Perms], BitShift, NumArgs, NoArgs, IndexedRules, NoRules, TempScore, FScore, TempSplitV, FSplitV, TempL, TempR, FinalL, FinalR):-
	create_perm_index_vector(NumArgs, BitShift, NoArgs, Perm, 1, NewSplitV, 1, NewArgV),
	%pp_bit_v(BitShift, NumArgs, NewSplitV),
	arg_score(IndexedRules, NewSplitV, NewArgV, 0, 0, 0, In, Between, Out, LeftL, RightL),
	InScore is In / NoRules,
        OutScore is Out / NoRules,
	LegSize is InScore + OutScore, 
	BetweenScore is Between / NoRules,
	BalanceScore is 1 - abs(InScore - OutScore),   %premiere small differencens between size of children
	NewScore is (BalanceScore + LegSize) - BetweenScore,
	%write('InScore: '), write(InScore),nl,
	%write('OutScore: '), write(OutScore),nl,
	%write('BetweenScore: '), write(BetweenScore),nl,
	%write('BalanceScore: '), write(BalanceScore),nl,
	%write('NewScore: '), write(NewScore),nl,
	(NewScore > TempScore ->
           %write('New better Score: '), write(NewScore),nl,
	   best_arg_score(Perms, BitShift, NumArgs, NoArgs, IndexedRules, NoRules, NewScore, FScore, NewSplitV, FSplitV, LeftL, RightL, FinalL, FinalR)
	;
           best_arg_score(Perms, BitShift, NumArgs, NoArgs, IndexedRules, NoRules, TempScore, FScore, TempSplitV, FSplitV, TempL, TempR, FinalL, FinalR)
	).

%Example
/*
New better Score: 0.5454545454545454
New better Score: 0.6363636363636364
New better Score: 1
NewTempSplitVector: 4303290368  100000000011111110000000000000000

Split node into: 
Left part: 11
Right part: 0
with splitVector: 100000000011111110000000000000000


   Call: (21) _G27856=[node(_G34134, [rule_i(4295426048)-rule([index(3)-[4.95|...]], [43-irisvirginica, 1-irisversicolor, ... - ...]), rule_i(4295426048)-rule([index(...)-[...|...], ... - ...], [1-irisversicolor, ... - ...|...]), rule_i(4412866560)-rule([... - ...|...], [... - ...|...]), rule_i(4295426048)-rule([...], [...|...]), rule_i(...)-rule(..., ...), ... - ...|...], 11, _G34137, _G34138)] ? l
4295426048  100000000000001110000000000000000
4295426048  100000000000001110000000000000000
4412866560  100000111000001110000000000000000
4295426048  100000000000001110000000000000000
4416536576  100000111001111110000000000000000
4420730880  100000111011111110000000000000000
4420730880  100000111011111110000000000000000
4295426048  100000000000001110000000000000000
4420730880  100000111011111110000000000000000
4295426048  100000000000001110000000000000000
4420730880  100000111011111110000000000000000
*/
%The way we separate rules seems okay!!
%arg_score([rule_i(4295426048)-_,rule_i(4295426048)-_,rule_i(4412866560)-_,rule_i(4295426048)-_,rule_i(4416536576)-_,rule_i(4420730880)-_,rule_i(4420730880)-_,rule_i(4295426048)-_,rule_i(4420730880)-_,rule_i(4295426048)-_,rule_i(4420730880)-_], 4303290368, 4311678976, 0, 0, 0, L, R) 
arg_score([], _, _, In, Between, Out, In, Between, Out, [], []).
arg_score([rule_i(BitVector)-rule(Cond, NoC, ClassT)|IRules], SplitV, ArgV, TIn, TBetween, TOut, In, Between, Out, LeftL, RightL):-
	InVector is BitVector /\ SplitV, %All Index values shared by Rule and Split
        BetweenVector is BitVector /\ ArgV,       %All Index values shared by Rule and the whole attribute index
        InCoverage is popcount(InVector),         %Number of common index values of rule and split
	NoHits is popcount(BetweenVector),	  %Number of common index values of rule and whole attribute index
	%pp_bit_v(8,4,BitVector),
	%pp_bit_v(8,4,SplitV),
	%pp_bit_v(8,4,ArgV),
	%pp_bit_v(8,4,InVector),
	%pp_bit_v(8,4,BetweenVector),
	%write('InCoverage is :'), write(InCoverage),nl,
	%write('NoHits is :'), write(NoHits),nl,
        (NoHits =:= 1 ->
	  %Between - as the rule do not use the attribute
	   NewTBetween is TBetween + 1,
	   arg_score(IRules, SplitV, ArgV, TIn, NewTBetween, TOut, In, Between, Out, NLeftL, NRightL),
	   LeftL = [rule_i(BitVector)-rule(Cond, NoC, ClassT)|NLeftL],
	   RightL = [rule_i(BitVector)-rule(Cond, NoC, ClassT)|NRightL]
        ; 
           (InCoverage =:= 1 ->
	     %Out - as the rule and splitv do not share any indexes
             NewTOut is TOut + 1,
	     arg_score(IRules, SplitV, ArgV, TIn, TBetween, NewTOut, In, Between, Out, NLeftL, NRightL),
	     LeftL = NLeftL, 
	     RightL = [rule_i(BitVector)-rule(Cond, NoC, ClassT)|NRightL] 
	   ;
	     (NoHits > InCoverage ->
               %Between - as Number of index attributes is bigger for the BetweenVector than that of the splitVector 
	       NewTBetween is TBetween + 1,
	       arg_score(IRules, SplitV, ArgV, TIn, NewTBetween, TOut, In, Between, Out, NLeftL, NRightL),
	       LeftL = [rule_i(BitVector)-rule(Cond, NoC, ClassT)|NLeftL],
	       RightL = [rule_i(BitVector)-rule(Cond, NoC, ClassT)|NRightL]
	     ;
	       %In - as here implicitly the number of index atributes are lowerfor the betweenVector or equal to that of the splitVector
	       NewTIn is TIn + 1,
	       arg_score(IRules, SplitV, ArgV, NewTIn, TBetween, TOut, In, Between, Out, NLeftL, NRightL),
	       LeftL = [rule_i(BitVector)-rule(Cond, NoC, ClassT)|NLeftL],
	       RightL = NRightL
	     )
	   )
	).
	 
	
create_perm_index_vector(1, 0, _, _, Vector, Vector, ArgV, ArgV).
create_perm_index_vector(Args, 0, Arg, Perm, TVector, Vector, TArgV, ArgV):-
	Args > 1,
	NewArgs is Args - 1,
	create_perm_index_vector(NewArgs, 8, Arg, Perm, TVector, Vector, TArgV, ArgV).
create_perm_index_vector(NoArgs, _, NoArgs, Perm, TVector, Vector, TArgV, ArgV):-
	!,create_perm_v(Perm, TVector, NewTVector, TArgV, NewTArgV), 
        create_perm_index_vector(NoArgs, 0, NoArgs, Perm, NewTVector, Vector, NewTArgV, ArgV).
create_perm_index_vector(NoArgs, Bits, Args, Perm, TVector, Vector, TArgV, ArgV):-
	NoArgs \== Args,
	NewTVector is TVector << 1,
	NewTArgV is TArgV << 1,
	NewBits is Bits - 1,
        create_perm_index_vector(NoArgs, NewBits, Args, Perm, NewTVector, Vector, NewTArgV, ArgV).

create_perm_v([], TVector, TVector, TArgV, TArgV).
create_perm_v([V|Vs], TVector, FTVector, TArgV, ArgV):-
	NewTVector is TVector << 1,
	NewTArgV is TArgV << 1,
	(V =:= 1 ->
	  NTArgV is NewTArgV + 1,
	  NTVector is NewTVector + 1
	;
  	  NTArgV is NewTArgV + 1,
	  NTVector = NewTVector
	),
	create_perm_v(Vs, NTVector, FTVector, NTArgV, ArgV).

empty_key_val(0, []).
empty_key_val(NoArgs, [0-NoArgs|TmpKeyVals]):-
	NewNoArgs is NoArgs - 1,
	empty_key_val(NewNoArgs, TmpKeyVals).

update_l_values([], _, []).
update_l_values([rule_i(LBI,_)-Rule|Rest], SplitV, [rule_i(LBI, SplitV)-Rule|Rest2]):-
	update_l_values(Rest, SplitV, Rest2).

update_r_values([], _, []).
update_r_values([rule_i(_,HBI)-Rule|Rest], SplitV, [rule_i(SplitV,HBI)-Rule|Rest2]):-
	update_r_values(Rest, SplitV, Rest2).
	
filter_rules([], _, [], [], 0, 0, 0). 
filter_rules([rule_i(LBI,HBI)-rule(Cond, ClassT)|IRules], Middle, [rule_i(LBI,HBI)-rule(Cond, ClassT)|TLR], TRR, NoL, NoR, NoSplitts):-
	HBI =< Middle,!,
	filter_rules(IRules, Middle, TLR, TRR, RestNoL, NoR, NoSplitts),
	NoL is RestNoL + 1.
filter_rules([rule_i(LBI,HBI)-rule(Cond, ClassT)|IRules], Middle, TLR, [rule_i(LBI,HBI)-rule(Cond, ClassT)|TRR], NoL, NoR, NoSplitts):-
	Middle < LBI,!,
	filter_rules(IRules, Middle, TLR, TRR, NoL, RestNoR, NoSplitts),
	NoR is RestNoR + 1.
filter_rules([rule_i(LBI,HBI)-rule(Cond, ClassT)|IRules], Middle, [rule_i(LBI,Middle)-rule(Cond, ClassT)|TLR], [rule_i(Middle,HBI)-rule(Cond, ClassT)|TRR], NoL, NoR, NoSplitts):-
	filter_rules(IRules, Middle, TLR, TRR, NoL, NoR, RestNoSplitts),
	NoSplitts is RestNoSplitts + 1.

find_best_split(Middle, _MaxDiff, SearchL, _From, _To, TLR, _NoL, TRR, _NoR, _NoSplitts, Middle, _-HistoricSteps, _IndexRules, TLR, TRR):-
	%number of steps enough
	length(HistoricSteps, Len),
	Len > SearchL.
	%write('Found best split: no steps'),nl.
find_best_split(Middle, MaxDiff, _SearchL, _From, _To, TLR, NoL, TRR, NoR, _NoSplitts, Middle, _, _IndexRules, TLR, TRR):-
	%Or max diff lower than threshold
	Diff is abs(NoL - NoR),
	Diff =< MaxDiff.
	%write('Found best split: below max diff'),nl.
find_best_split(Middle, MaxDiff, SearchL, From, To, _TLR, NoL, _TRR, NoR, _NoSplitts, SplitV, StepSize-HistoricSteps, IndexRules, LeftR, RightR):-   %Go Left
	NoL > NoR,!,
	(HistoricSteps=[r,l|_] ->
	  NewStepSize is StepSize * 2
	;
	  NewStepSize = StepSize
	),
	NewM is Middle - ((Middle - From) / NewStepSize),
	filter_rules(IndexRules, NewM, NLR, NRR, NNoL, NNoR, NNoSplitts),
	find_best_split(NewM, MaxDiff, SearchL, From, To, NLR, NNoL, NRR, NNoR, NNoSplitts, SplitV, NewStepSize-[l|HistoricSteps], IndexRules, LeftR, RightR).
find_best_split(Middle, MaxDiff, SearchL, From, To, _TLR, _NoL, _TRR, _NoR, _NoSplitts, SplitV, StepSize-HistoricSteps, IndexRules, LeftR, RightR):-   %Go Right
	(HistoricSteps=[l,r|_] ->
	  NewStepSize is StepSize * 2
	;
	  NewStepSize = StepSize
	),
	NewM is Middle + ((To - Middle) / NewStepSize),
	filter_rules(IndexRules, NewM, NLR, NRR, NNoL, NNoR, NNoSplitts),
	find_best_split(NewM, MaxDiff, SearchL, From, To, NLR, NNoL, NRR, NNoR, NNoSplitts, SplitV, NewStepSize-[r|HistoricSteps], IndexRules, LeftR, RightR).

/**
 index_rules(+List, -List) is det.
*/
%Note that msb and lsb returns index from 0
index_rules([], _, _, []).
index_rules([tree(TreeRule)|Rules], BitSize, ArgMinMax, Result):-
	index_r(TreeRule, BitSize, ArgMinMax, IndexedPred),
	index_rules(Rules, BitSize, ArgMinMax, RestPred),
	append([tree(IndexedPred)], RestPred, Result).
index_rules([rule_set(TreeRule)|Rules], BitSize, ArgMinMax, Result):-
	index_r(TreeRule, BitSize, ArgMinMax, IndexedPred),
	index_rules(Rules, BitSize, ArgMinMax, RestPred),
	append([rule_set(IndexedPred)], RestPred, Result).

index_r([Rule|Rules], BitSize, ArgMinMax, IndexedPred):-
	%ArgMinMax =.. [_|Args],
	length(ArgMinMax, NoArgs),
	%get_maximum_bitvector(MaxBitIndex),
	%MaxBitIndex = 0xFF, %8 bits, should be converteed to parameter?!
	get_bit_index(BitSize, MaxBitIndex),
	BitShift is msb(MaxBitIndex + 1), %add one t onto 8 bits
	build_precision_list(ArgMinMax, BitShift, PrecisionL), %Done twice do once one call..
	set_i_conditions([Rule|Rules], ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, IndexedPred).
	%set_i_boundries([Rule|Rules], ArgMinMax, NoArgs, MaxBitIndex, BitShift, IndexedPred).

set_i_conditions([], _, _, _, _, _, []).
set_i_conditions([rule(Cond, ClassT)|Rules], ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, [rule_i(BitVector)-rule(Cond, NoC, ClassT)|RIs]):-
	length(Cond, NoC),
	get_i_conds(Cond, ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, 1, BitVector),
	%length(ArgMinMax, Args),
        %write(Cond),nl,
	%pp_bit_v(8, Args, BitVector),
	set_i_conditions(Rules, ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, RIs).

get_i_conds(_, _, 0, _, _, _, BitVector, BitVector).
get_i_conds(Conds, ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, TBitVector, BitVector):-
	member(index(NoArgs)-[Min, Max], ArgMinMax),
	(member(index(NoArgs)-[LCond, HCond], Conds) ->
	  member(index(NoArgs)-Precision, PrecisionL),
	  %trace,
	  calc_i(LCond, HCond, Min, Max, MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector),
	  %write('We do have conditions for this attribute'),nl,
	  %write('PartBit: '),write(NewTBitVector),nl,
	  NewNoArgs is NoArgs - 1,!,
	  get_i_conds(Conds, ArgMinMax, NewNoArgs, MaxBitIndex, PrecisionL, BitShift, NewTBitVector, BitVector)
	;
	  NewTBitVector is TBitVector << BitShift,
	  %write('No conditions for this attribute'),nl,
 	  %write('PartBit: '),write(NewTBitVector),nl,
	  NewNoArgs is NoArgs - 1,!,
	  get_i_conds(Conds, ArgMinMax, NewNoArgs, MaxBitIndex, PrecisionL, BitShift, NewTBitVector, BitVector)
	).
%No UB
calc_i(LCond, non, Min, Max, MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector):-
	number(LCond),
	val_to_index(LCond, Min, Max, BitShift, Precision, PartIndex),
	BitIndex is lsb(PartIndex) + 1,     %as we will have leading 1 to deal with
        create_to_bit_vector(BitShift, BitIndex, 1, PartBitVector),
	%remove leading 1 from PartBitVector
	%pp_bit_v(8,1,PartBitVector),
	PartBitV is PartBitVector xor (MaxBitIndex + 1),
	%From cond to Max
	MovedBitVector is TBitVector << BitShift,
	NewTBitVector is MovedBitVector \/ PartBitV.	
%No LB
calc_i(non, UCond, Min, Max, MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector):-
	number(UCond),
	val_to_index(UCond, Min, Max, BitShift, Precision, PartIndex),
	%pp_bit_v(8,1,PartIndex),
	BitIndex is lsb(PartIndex),
        create_from_bit_vector(BitShift, BitIndex, 1, PartBitVector),
	%pp_bit_v(8,1,PartBitVector),
	%remove leading 1 from PartBitVector
	PartBitV is PartBitVector xor (MaxBitIndex + 1),
	%From Min to cond 
	MovedTBitVector is TBitVector << BitShift,
	NewTBitVector is MovedTBitVector \/ PartBitV.

calc_i(LCond, UCond, Min, Max, MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector):-
	number(LCond),
	number(UCond),
	val_to_index(LCond, Min, Max, BitShift, Precision, LPartIndex),
	val_to_index(UCond, Min, Max, BitShift, Precision, UPartIndex),
	LBitIndex is lsb(LPartIndex) + 1, %as first msb = 8 
        create_to_bit_vector(BitShift, LBitIndex, 1, LPartBitVector),
	UBitIndex is lsb(UPartIndex),
        create_from_bit_vector(BitShift, UBitIndex, 1, UPartBitVector),
	%pp_bit_v(8,1,LPartBitVector),
	%pp_bit_v(8,1,UPartBitVector),
	%remove leading 1 from PartBitVector = index 8 leaving 0-7 bits left
	LPartBitV is LPartBitVector xor (MaxBitIndex + 1),
	%remove leading 1 from PartBitVector
	UPartBitV is UPartBitVector xor (MaxBitIndex + 1),
	PartBitV is LPartBitV /\ UPartBitV,
	MovedTBitVector is TBitVector << BitShift,
	NewTBitVector is MovedTBitVector \/ PartBitV.


create_to_bit_vector(0, _, PartBitVector, PartBitVector).
create_to_bit_vector(BitShift, BitIndex, TPartBitVector, PartBitVector):-
	(BitIndex < BitShift ->                                                         %Should be included
	  NewTPartBitVector is TPartBitVector << 1 
	;
          NTPartBitVector is TPartBitVector << 1, %No 1	 
	  NewTPartBitVector is NTPartBitVector + 1 %Add 1 when after we pass Bit index 
	),
	 NewBitShift is BitShift - 1,
	create_to_bit_vector(NewBitShift, BitIndex, NewTPartBitVector, PartBitVector).

create_from_bit_vector(0, _, PartBitVector, PartBitVector).
create_from_bit_vector(BitShift, BitIndex, TPartBitVector, PartBitVector):-
	(BitIndex >= BitShift ->                                                        %should be included
	  NewTPartBitVector is TPartBitVector << 1 %No 1
	 ;
	  NTPartBitVector is TPartBitVector << 1,
	  NewTPartBitVector is NTPartBitVector + 1 %Add 1 when we passed Bit index to right
	),
	 NewBitShift is BitShift - 1,
	create_from_bit_vector(NewBitShift, BitIndex, NewTPartBitVector, PartBitVector).


set_i_boundries([], _, _, _, _, []).
set_i_boundries([rule(Cond, ClassT)|Rules], ArgMinMax, NoArgs, MaxBitIndex, BitShift,
		[rule_i(LBI,HBI)-rule(Cond, ClassT)|RIs]):-
	get_i_bounds(Cond, ArgMinMax, NoArgs, MaxBitIndex, BitShift, 1, 1, LBI, HBI),
	set_i_boundries(Rules, ArgMinMax, NoArgs, MaxBitIndex, BitShift, RIs).

get_i_bounds(_, _, 0, _, _, LBI, HBI, LBI, HBI).
get_i_bounds(Conds, ArgMinMax, NoArgs, MaxBitIndex, BitShift, TLI, THI, LBI, HBI):-
	member(index(NoArgs)-[Min, Max], ArgMinMax),
	(member(index(NoArgs)-[LCond, HCond], Conds) ->
	  calc_b(LCond, HCond, Min, Max, MaxBitIndex, BitShift, TLI, THI, NewTLI, NewTHI),
	  NewNoArgs is NoArgs - 1,!,
	  get_i_bounds(Conds, ArgMinMax, NewNoArgs, MaxBitIndex, BitShift, NewTLI, NewTHI, LBI, HBI)
	;
	 NewTLI is TLI << BitShift,
	  MovedTHI is THI << BitShift,
	  NewTHI is MovedTHI + MaxBitIndex,
	  NewNoArgs is NoArgs - 1,!,
	  get_i_bounds(Conds, ArgMinMax, NewNoArgs, MaxBitIndex, BitShift, NewTLI, NewTHI, LBI, HBI)
	).
%No UB
calc_b(LCond, non, Min, Max, MaxBIndex, BitShift, TLI, THI, NewTLI, NewTHI):-
	number(LCond),
	val_to_index(LCond, Min, Max, MaxBIndex, PartIndex),
	%LB
	MovedTLI is TLI << BitShift,
	NewTLI is MovedTLI + PartIndex,
	%UB
	MovedTHI is THI << BitShift,
	NewTHI is MovedTHI + MaxBIndex.
%No LB
calc_b(non, UCond, Min, Max, MaxBIndex, BitShift, TLI, THI, NewTLI, NewTHI):-
	number(UCond),
	val_to_index(UCond, Min, Max, MaxBIndex, PartIndex),
	%LB
	NewTLI is TLI << BitShift,
	%UB
	MovedTHI is THI << BitShift,
	NewTHI is MovedTHI + PartIndex.
calc_b(LCond, UCond, Min, Max, MaxBIndex, BitShift, TLI, THI, NewTLI, NewTHI):-
	number(LCond),
	number(UCond),
	val_to_index(LCond, Min, Max, MaxBIndex, LPartIndex),
	val_to_index(UCond, Min, Max, MaxBIndex, UPartIndex),
	%LB
	MovedTLI is TLI << BitShift,
	NewTLI is MovedTLI + LPartIndex,
	%UB
	MovedTHI is THI << BitShift,
	NewTHI is MovedTHI + UPartIndex.

/**
 index_example(+List, -List) is det.

 Each argument in the List of examples must be instantiated.
*/
index_examples([Ex|Exs], BitSize, ArgMinMax, ClassI, IndexedPred):-
	Ex =.. [_|Args],
	length(Args, NoArgs),
	%get_maximum_bitvector(MaxBitIndex),
	get_bit_index(BitSize, MaxBitIndex),
	%MaxBitIndex = 0xFF, %8 bits
	BitShift is msb(MaxBitIndex + 1), %add one t onto 8 bits
	build_precision_list(ArgMinMax, BitShift, PrecisionL),
	set_indexes([Ex|Exs], ArgMinMax, NoArgs, ClassI, MaxBitIndex, BitShift, PrecisionL, IndexedPred).

build_precision_list([], _, []).
build_precision_list([index(I)-[Min, Max]|Rest], BitShift, [index(I)-Precision|PrecL]):-
	Span is Max - Min,
	Precision is Span / BitShift, 
	build_precision_list(Rest, BitShift, PrecL).

get_bit_index(4, 0xF).
get_bit_index(8, 0xFF).
get_bit_index(12, 0xFFF).
get_bit_index(16, 0xFFFF).
get_bit_index(20, 0xFFFFF).
get_bit_index(24, 0xFFFFFF).
get_bit_index(28, 0xFFFFFFF).
get_bit_index(32, 0xFFFFFFFF).
get_bit_index(36, 0xFFFFFFFFF).
get_bit_index(40, 0xFFFFFFFFFF).
get_bit_index(44, 0xFFFFFFFFFFF).
get_bit_index(48, 0xFFFFFFFFFFFF).
get_bit_index(52, 0xFFFFFFFFFFFFF).
get_bit_index(56, 0xFFFFFFFFFFFFFF).
get_bit_index(60, 0xFFFFFFFFFFFFFFF).
get_bit_index(64, 0xFFFFFFFFFFFFFFFFF).
get_bit_index(68, 0xFFFFFFFFFFFFFFFFFF).
get_bit_index(72, 0xFFFFFFFFFFFFFFFFFFF).
get_bit_index(76, 0xFFFFFFFFFFFFFFFFFFFF).
get_bit_index(80, 0xFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(84, 0xFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(88, 0xFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(92, 0xFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(96, 0xFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(100, 0xFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(102, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(104, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(108, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(112, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(116, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(120, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(124, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(128, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(132, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(136, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(140, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(144, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(148, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(152, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(156, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(160, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(164, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(168, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(172, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(176, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(180, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(184, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(188, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(192, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(196, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(200, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(204, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(208, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(212, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(216, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(220, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(224, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(228, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(232, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(236, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(240, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(244, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(248, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(252, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(256, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(512, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
get_bit_index(1024, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).
% set_indexes(+List, +NoArgs:int, +MaxIndex:int, -List) is det.
set_indexes([], _, _, _, _, _, _, []).
set_indexes([Ex|Exs], ArgMinMax, NoArgs, ClassI, MaxBIndex, BitShift, PrecisionL, [ExIndex|ExsIndex]):-
	set_index(Ex, ArgMinMax, NoArgs, ClassI, MaxBIndex, 1, BitShift, PrecisionL, ExIndex),!,
	set_indexes(Exs, ArgMinMax, NoArgs, ClassI, MaxBIndex, BitShift, PrecisionL, ExsIndex).

/**
 set_index(+Term, +NoArgs:int, +MaxIndex:int, +TempI:int, -ExIndex) is
 det.

 Term must be fully instantiated, eg. all argument must not contain
 variabels
*/
set_index(Ex, _ArgMinMax, 0, _, _, Index, _, _, ex_i(Index)-Ex). %:-
	%length(ArgMinMax, NoArgs),
	%pp_bit_v(8, NoArgs, Index).
	%format('Hex Index: ~16r~n',[Index]).
set_index(Ex, ArgMinMax, ClassI, ClassI, MaxBIndex, TempBitVector, BitShift, PrecisionL, ExIndex):-
	NewNoArgs is ClassI - 1,!,
	set_index(Ex, ArgMinMax, NewNoArgs, ClassI, MaxBIndex, TempBitVector, BitShift, PrecisionL, ExIndex).
set_index(Ex, ArgMinMax, NoArgs, ClassI, MaxBIndex, TempBitVector, BitShift, PrecisionL, ExIndex):-
	arg(NoArgs, Ex, Val),
	member(index(NoArgs)-[Min, Max], ArgMinMax),
	member(index(NoArgs)-Precision, PrecisionL),
	val_to_index(Val, Min, Max, BitShift, Precision, PartIndex),
	MovedTBitVector is TempBitVector << BitShift,
	ExParI is PartIndex xor (MaxBIndex + 1), 
	%write(ExParI),nl,
	NewTempBitVector is MovedTBitVector + ExParI,
	NewNoArgs is NoArgs - 1,!,
	set_index(Ex, ArgMinMax, NewNoArgs, ClassI, MaxBIndex, NewTempBitVector, BitShift, PrecisionL, ExIndex).

/**
 val_to_index(+Val:int, +Min:int, +Max:int,
 +MaxBIndex:int,-PartIndex:int) is det.

*/
val_to_index(Val, Min, Max, BitShift, Precision, PartIndex):-
	%Span is Max - Min, %Pre-calculate once for all in PrecisionL, same for all...
	%NVal is (Val - Min) / Span,
	UB is Precision + Min,
	set_index_1(BitShift, Val, 1, Precision, UB, Min, Min, Max, PartIndex).
	%pp_bit_v(8, 1, PartIndex).

% sets a 1 to the value corresponding bit...
set_index_1(0, _, PartIndex, _, _, _, _, _, PartIndex).
set_index_1(BitShift, NVal, TempBitV, Precision, UPrecision, LPrecision, LPrecision, Max, PartIndex):- %For Min Extreme value
	NTBitV is TempBitV << 1,
	NewBitShift is BitShift - 1,
	NUPrecision is Precision +  UPrecision,
	NLPrecision is Precision +  LPrecision,!,
	((NVal >= LPrecision, NVal =< UPrecision) ->
	   NTempBitV is NTBitV + 1,
	   set_index_1(NewBitShift, NVal, NTempBitV, Precision, NUPrecision, NLPrecision, LPrecision, Max, PartIndex)
	;
	   set_index_1(NewBitShift, NVal, NTBitV, Precision, NUPrecision, NLPrecision, LPrecision, Max, PartIndex)
	).
set_index_1(BitShift, NVal, TempBitV, Precision, UPrecision, LPrecision, Min, Max, PartIndex):-
        NTBitV is TempBitV << 1,
	NewBitShift is BitShift - 1,
	NUPrecision is Precision +  UPrecision,
	NLPrecision is Precision +  LPrecision,!,
	((NVal > LPrecision, NVal =< UPrecision) ->
	   NTempBitV is NTBitV + 1,
	   set_index_1(NewBitShift, NVal, NTempBitV, Precision, NUPrecision, NLPrecision, Min, Max, PartIndex)
	;
	   set_index_1(NewBitShift, NVal, NTBitV, Precision, NUPrecision, NLPrecision, Min, Max, PartIndex)
	).

