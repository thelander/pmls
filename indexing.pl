%%%
% Module for indexing rules and examples
%
% Author: Tony Lindgren
%
% version 0.0.1
% This module takes either rules or examples and create and index for them.
% Example calls,
% index_examples([(1,ex(0.5,0.5pos)),(2, ex(0.7,0.5pos)),(3,ex(0,0.5neg)),(4,ex(1,0.5neg))], 4, [index(1)-[0,1],index(2)-[0,1]], 2, R), util:pp_i_ex(R, 4, 1).
% index_rules([rule_set([rule([index(1)-[non,0.5]],[2-pos, 0-neg]),rule([index(1)-[0.5,non]],[2-neg,0-pos])])], 4, [index(1)-[0,1]], R), util:pp_i_rule_set(R, 4, 1). 

:- module(indexing, [index_examples/5, index_rules/4]).
:- use_module([library(pldoc), library(plunit), library(clpb), util]).

/**
 index_rules(+List, -List) is det.
*/
%Note that msb and lsb returns index from 0
index_rules([], _, _, []).
index_rules([tree(TreeRule)|Rules], BitSize, ArgMinMax, Result):-
	index_r(TreeRule, BitSize, ArgMinMax, IndexedPred),
	index_rules(Rules, BitSize, ArgMinMax, RestPred),
	append([tree(IndexedPred)], RestPred, Result).
index_rules([rule_set(RuleSet)|Rules], BitSize, ArgMinMax, Result):-
	index_r(RuleSet, BitSize, ArgMinMax, IndexedPred),
	index_rules(Rules, BitSize, ArgMinMax, RestPred),
	append([rule_set(IndexedPred)], RestPred, Result).

index_r([Rule|Rules], BitSize, ArgMinMax, IndexedPred):-
	%ArgMinMax =.. [_|Args],
	length(ArgMinMax, NoArgs),
	%get_maximum_bitvector(MaxBitIndex),
	%MaxBitIndex = 0xFF, %8 bits, should be converteed to parameter?!
	get_bit_index(BitSize, MaxBitIndex),
	BitShift is msb(MaxBitIndex + 1), %add one onto 8 bits
	build_precision_list(ArgMinMax, BitShift, PrecisionL), %Done twice do once one call..
	set_i_conditions([Rule|Rules], ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, IndexedPred).
	%set_i_boundries([Rule|Rules], ArgMinMax, NoArgs, MaxBitIndex, BitShift, IndexedPred).

set_i_conditions([], _, _, _, _, _, []).
set_i_conditions([rule(ID, Cond, ClassT)|Rules], ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, [rule(ID, rule_i(BitVector)-rule(Cond, NoC, ClassT))|RIs]):-
	length(Cond, NoC),
	%write(Cond),nl,
	%trace,
	get_i_conds(Cond, ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, 1, BitVector),
	%length(ArgMinMax, Args),
	%pp_bit_v(4, Args, BitVector),
	set_i_conditions(Rules, ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, RIs).

get_i_conds(_, _, 0, _, _, _, BitVector, BitVector).
get_i_conds(Conds, ArgMinMax, NoArgs, MaxBitIndex, PrecisionL, BitShift, TBitVector, BitVector):-
	member(index(NoArgs)-[Min, Max], ArgMinMax),
	(member(index(NoArgs)-[LCond, HCond], Conds) ->
	  member(index(NoArgs)-Precision, PrecisionL),
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
calc_i(LCond, non, Min, Max, _MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector):-
	number(LCond),
	val_to_index(LCond, >=, Min, Max, BitShift, Precision, PartIndex),
	%BitIndex is lsb(PartIndex) + 1,     %as we will have leading 1 to deal with
        %create_to_bit_vector(BitShift, BitIndex, 1, PartBitVector),
	%remove leading 1 from PartBitVector
%	write('PartBit: '),pp_bit_v(4,1,PartIndex),
%	write('TempBit: '),pp_bit_v(4,1,TBitVector),
	%PartBitV is PartBitVector xor (MaxBitIndex + 1),
	%From cond to Max
	MovedBitVector is TBitVector << BitShift,
	NewTBitVector is MovedBitVector \/ PartIndex.
				%PartBitV	
%No LB
calc_i(non, UCond, Min, Max, _MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector):-
	number(UCond),
	val_to_index(UCond, <, Min, Max, BitShift, Precision, PartIndex),
	%pp_bit_v(8,1,PartIndex),
	%BitIndex is lsb(PartIndex),
        %create_from_bit_vector(BitShift, BitIndex, 1, PartBitVector),
%	write('PartBit: '),pp_bit_v(4,1,PartIndex),
%	write('TempBit: '),pp_bit_v(4,1,TBitVector),
	%remove leading 1 from PartBitVector
	%PartBitV is PartBitVector xor (MaxBitIndex + 1),
	%From Min to cond 
	MovedTBitVector is TBitVector << BitShift,
	NewTBitVector is MovedTBitVector \/ PartIndex.   %PartBitV

calc_i(LCond, UCond, Min, Max, _MaxBitIndex, Precision, BitShift, TBitVector, NewTBitVector):-
	number(LCond),
	number(UCond),
	val_to_index(LCond, >=, Min, Max, BitShift, Precision, LPartIndex),
	val_to_index(UCond, <, Min, Max, BitShift, Precision, UPartIndex),
	%LBitIndex is lsb(LPartIndex) + 1, %as first msb = 8 
        %create_to_bit_vector(BitShift, LBitIndex, 1, LPartBitVector),
	%UBitIndex is lsb(UPartIndex),
        %create_from_bit_vector(BitShift, UBitIndex, 1, UPartBitVector),
%	write('LowerPartBit: '),pp_bit_v(4,1,LPartIndex),
%	write('UpperPartBit: '),pp_bit_v(4,1,UPartIndex),
%	write('TempBit: '),pp_bit_v(4,1,TBitVector),
	%remove leading 1 from PartBitVector = index 8 leaving 0-7 bits left
	%LPartBitV is LPartBitVector xor (MaxBitIndex + 1),
	%remove leading 1 from PartBitVector
	%UPartBitV is UPartBitVector xor (MaxBitIndex + 1),
	PartBitV is LPartIndex /\ UPartIndex,
	MovedTBitVector is TBitVector << BitShift,
	NewTBitVector is MovedTBitVector \/ PartBitV.

/**
 index_examle(+List, -List) is det.

 Each argument in the List of examples must be instantiated.
*/
index_examples([(Id, Ex)|Exs], BitSize, ArgMinMax, ClassI, IndexedPred):-
	Ex =.. [_|Args],
	length(Args, NoArgs),
	%get_maximum_bitvector(MaxBitIndex),
	get_bit_index(BitSize, MaxBitIndex),
	%MaxitIndex = 0xFF, %8 bits
	BitShift is msb(MaxBitIndex + 1), %add one t onto 8 bits
	build_precision_list(ArgMinMax, BitShift, PrecisionL),
	set_indexes([(Id, Ex)|Exs], ArgMinMax, NoArgs, ClassI, MaxBitIndex, BitShift, PrecisionL, IndexedPred).

build_precision_list([], _, []).
build_precision_list([index(I)-[Min, Max]|Rest], BitShift, [index(I)-Precision|PrecL]):-
	Span is Max - Min,
	Precision is Span / BitShift,!, 
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
set_indexes([(Id,Ex)|Exs], ArgMinMax, NoArgs, ClassI, MaxBIndex, BitShift, PrecisionL, [(Id,ExIndex)|ExsIndex]):-
	set_index(Ex, ArgMinMax, NoArgs, ClassI, MaxBIndex, 1, BitShift, PrecisionL, ExIndex),!,
	set_indexes(Exs, ArgMinMax, NoArgs, ClassI, MaxBIndex, BitShift, PrecisionL, ExsIndex).

/**
 set_index(+Term, +NoArgs:int, +MaxIndex:int, +TempI:int, -ExIndex) is
 det.

 Term must be fully instantiated, eg. all argument must not contain
 variabels
*/
set_index(Ex, _ArgMinMax, 0, _, _, Index, _, _, ex_i(Index)-Ex).%:-
	%length(ArgMinMax, NoArgs),
	%pp_bit_v(4, NoArgs, Index).
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

%% For rules
val_to_index(Val, Dir, Min, _Max, BitShift, Precision, PartIndex):-
	%Span is Max - Min, %Pre-calculate once for all in PrecisionL, same for all...
	%NVal is (Val - Min) / Span,
	Breakpoint is Precision + Min,
	set_index_1(BitShift, Dir, Val, 0, Precision, Breakpoint, PartIndex).
	%pp_bit_v(8, 1, PartIndex).

set_index_1(0, _, _, PartIndex, _, _, PartIndex).
set_index_1(BitShift, <, NVal, 0, Precision, BreakPoint, PartIndex):-      %first time!
	TempBitV is 0,
	NTBitV is TempBitV << 1,
	NewBitShift is BitShift - 1,
	NewBreakPoint is BreakPoint + Precision,!,
	(BreakPoint < NVal ->                    %Keep it to the left of the breakpoint
	   NTempBitV is NTBitV + 1,
	   set_index_1(NewBitShift, <, NVal, NTempBitV, Precision, NewBreakPoint, PartIndex)
	;
	   (NVal < BreakPoint ->
	     NTempBitV is NTBitV + 1, 
	     set_index_1(NewBitShift, <, NVal, NTempBitV, Precision, NewBreakPoint, PartIndex)
           ;
	     set_index_1(NewBitShift, <, NVal, NTBitV, Precision, NewBreakPoint, PartIndex)
	   )
	).
set_index_1(BitShift, <, NVal, TempBitV, Precision, BreakPoint, PartIndex):-
	NTBitV is TempBitV << 1,
	NewBitShift is BitShift - 1,
	NewBreakPoint is BreakPoint + Precision,!,
	(BreakPoint < NVal ->                    %Keep it to the left of the breakpoint
	   NTempBitV is NTBitV + 1,
	   set_index_1(NewBitShift, <, NVal, NTempBitV, Precision, NewBreakPoint, PartIndex)
	;
	   set_index_1(NewBitShift, <, NVal, NTBitV, Precision, NewBreakPoint, PartIndex)
	).
set_index_1(BitShift, >=, NVal, TempBitV, Precision, BreakPoint, PartIndex):-
        NTBitV is TempBitV << 1,
	NewBitShift is BitShift - 1,
	NewBreakPoint is BreakPoint + Precision,!,           
	(BreakPoint >= NVal ->                   %Keep it to the right of the breakpoint
	   NTempBitV is NTBitV + 1,
	   set_index_1(NewBitShift, >=, NVal, NTempBitV, Precision, NewBreakPoint, PartIndex)
	;
	   set_index_1(NewBitShift, >=, NVal, NTBitV, Precision, NewBreakPoint, PartIndex)
	).


%%For Examples
val_to_index(Val, Min, Max, BitShift, Precision, PartIndex):-
	%Span is Max - Min, %Pre-calculate once for all in PrecisionL, same for all...
	%NVal is (Val - Min) / Span,
	%trace,
	UB is Precision + Min,
	set_index_1(BitShift, Val, 1, Precision, UB, Min, Min, Max, PartIndex).

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

