%%%%
% Module resonsible for printing results to file
%
% Author: Tony Lindgren
%
% version 0.0.3
%

:- module(write_results,[write_results/3]).

write_results([], _, _).
write_results([result(DataSet, Method, MethodSize, _Key, ClassT, NoRules, _NoConds, Eval, EvalTime)|Rest], Stream, Folds):-
    %trace,
    find_and_remove(Rest, DataSet, Method, MethodSize, NoRulesL, _NoCondsL, EvalList, EvalTimeList, NewRest),
    (is_list(NoRules)->
      sumlist(NoRules, TempRules),
      sum_list_c(NoRulesL, TempRules, TotRules)     
    ;
      sum_list_c(NoRulesL, NoRules, TotRules)
    ),
    %(is_list(NoConds)->
    %  sumlist(NoConds, TempConds),
    %  sum_list_c(NoCondsL, TempConds, TotConds)     
    %;
    %  sum_list_c(NoCondsL, NoConds, TotConds)
    %),
    AverageRules is TotRules / Folds,
    %AverageConds is TotConds / Folds,
    summarize_evaluation(EvalList, Eval, EvalTimeList, EvalTime, AggregatedValues, AggregatedTime),
    nl(Stream),nl(Stream),
    nl(Stream),
    write(Stream,'================================'),nl(Stream),
    write(Stream,'Data set: '),
    write(Stream,DataSet),nl(Stream),
    write(Stream,'Method: '),
    write(Stream,Method),nl(Stream),
    write(Stream,'Size: '),
    write(Stream,MethodSize),nl(Stream),
    write(Stream,'--------------------------------'),nl(Stream),
    write(Stream,'Average results over all folds :'),nl(Stream),
    write(Stream,'--------------------------------'),nl(Stream),
    write(Stream,'Number of rules: '),
    write(Stream,AverageRules),nl(Stream),
    %write(Stream,'Number of conditions: '),
    %write(Stream, AverageConds),nl(Stream),
    get_statistics(AggregatedValues, Stream, ClassT, _Accuracy, _AUC, _ROC, _Precision, _Recall, _ConfusionMatrix),
    write(Stream,'Time to classify test set: '),
    write(Stream,AggregatedTime),nl(Stream),
    write_results(NewRest, Stream, Folds).

find_and_remove([], _, _, _, [], [], [], [], []).
find_and_remove([result(Name, Method, MethodSize, _Key, _ClassT, NoRules, NoConds, Eval, EvalTime)|Rest], Name, Method, MethodSize,
		[NoRules|NoRulesL], [NoConds|NoCondsL], [Eval|EvalList], [EvalTime|EvalTimeList], NewRest):-
    !, find_and_remove(Rest, Name, Method, MethodSize, NoRulesL, NoCondsL, EvalList, EvalTimeList, NewRest).
find_and_remove([Save|Rest], Name, Method, MethodSize,
		NoRulesL, NoCondsL, EvalList, EvalTimeList, [Save|NewRest]):-
    find_and_remove(Rest, Name, Method, MethodSize, NoRulesL, NoCondsL, EvalList, EvalTimeList, NewRest).

sum_list_c([], FVal, FVal).
sum_list_c([L|Ls], TVal, FVal):-
    number(L),
    NewTVal is TVal + L,!,
    sum_list_c(Ls, NewTVal, FVal).
sum_list_c([L|Ls], TVal, FVal):-
    is_list(L),
    sumlist(L,LVal),
    NewTVal is TVal + LVal,!,
    sum_list_c(Ls, NewTVal, FVal).

summarize_evaluation([], FSum, [], TimeSum, FSum, TimeSum).
summarize_evaluation([Fold|Folds], Cov_TC_FC_FN_F, [FTime|FTimes], Time, FinalResult, FinalTime):-
    summarize_evaluation1(Fold, Cov_TC_FC_FN_F, FSum),
    NewTime is Time + FTime,
    summarize_evaluation(Folds, FSum, FTimes, NewTime, FinalResult, FinalTime).

summarize_evaluation1([], FinalSum, FinalSum).
summarize_evaluation1([Class-[Cov,TP,FP,FN,False]|Rest], TempCov_TP_FP_FN_F, FSum):-
    select(Class-[TempCov,TempTP,TempFP,TempFN, TempF], TempCov_TP_FP_FN_F, RestTempCov_TP_FP_FN_F),
    NewTempCov is TempCov + Cov,
    NewTempTP is TempTP + TP,
    NewTempFP is TempFP + FP,
    NewTempFN is TempFN + FN,
    sum_fn(False, TempF, NewTempF),
    append([Class-[NewTempCov, NewTempTP, NewTempFP, NewTempFN, NewTempF]], RestTempCov_TP_FP_FN_F, NewTempCov_TP_FP_FN_F),
    summarize_evaluation1(Rest,  NewTempCov_TP_FP_FN_F, FSum).

%Loops many times, not so efficent...
get_statistics(AggValues, Stream, _ClassTuples, Accuracy, _AUC, _ROC, Precision, Recall, _ConfusionMatrix):-
    get_accuracy(AggValues, 0, 0, Accuracy),
    write(Stream,'Accuracy: '),
    write(Stream, Accuracy),nl(Stream), %We only do accuracy
    get_precision(AggValues, ClassTuples, Stream, Precision),
    get_recall(AggValues, ClassTuples, Stream, Recall),
    write(Stream,'Confusion matrix: '),nl(Stream).
    %format(Stream,'~w ~t ~w ~n',['Corr. class','Pred. class']),
    %length(ClassTuples, CTL),
    %write_f_row(ClassTuples, Stream, 1),
    %make_cost_mat(ClassTuples, ClassTuples, 1, CostMatrix),
    %get_confusion_mat(AggValues, ClassTuples, 0, CTL,  Stream, ConfusionMatrix, 0, Numerator, 0, Denominator, CostMatrix),
    %CostValue is Numerator / Denominator,
    %write(Stream, 'CostValue of ACC is: '),write(Stream, CostValue),nl(Stream).

%Not general at all...
make_cost_mat([_-C1,_-C2,_-C3], ClassTuples, 1, [[C1-8,C2-4,C3-8]|Rest]):-
	make_cost_mat(ClassTuples, ClassTuples, 2, Rest).
make_cost_mat([_-C1,_-C2,_-C3], ClassTuples, 2, [[C1-1,C2-1,C3-1]|Rest]):-
	make_cost_mat(ClassTuples, ClassTuples, 3, Rest).
make_cost_mat([_-C1,_-C2,_-C3], _, 3, [[C1-8,C2-4,C3-8]]).

write_f_row([], Stream, _):-
    nl(Stream).
write_f_row([_-Class|CT], Stream, I):-
    %Tab is I * 5,
    format(Stream,'~t ~t ~t ~t ~w',[Class]),
    write_f_row(CT, Stream,I).

get_confusion_mat(_, _, CTL, CTL, _, [], Numerator, Numerator, Denominator, Denominator, _).
get_confusion_mat(AggVals, ClassTuple, I, CTL, Stream, [Class-Row|Rec], TNum, Num, TDenom, Denom, CostM):-
    NewI is I + 1,
    nth1(NewI, ClassTuple, _-Class),
    nth1(NewI, CostM, RCostM),
    write_row(AggVals, Class, NewI, ClassTuple, Stream, Row, TNum, PNum, TDenom, PDenom, RCostM),
    nl(Stream),
    get_confusion_mat(AggVals, ClassTuple, NewI, CTL, Stream, Rec, PNum, Num, PDenom, Denom, CostM).

write_row(AggVals, Class, I, CT, Stream, Row, TNum, Num, TDenom, Denom, RCostM):-
    member(Class-[_, TP, _, _, Row], AggVals),
    get_row_denom(Row, RCostM, TDenom, PartDenom),
    write('Row: '), write(Row),nl,
    write(Stream, Class),
    write_before(I, CT, Stream, Row),
    format(Stream, '~t ~d ',[TP]),
    member(Class-Cost, RCostM),
    PartNum is (TP * Cost),
    Num is PartNum + TNum,
    Denom is PartNum + PartDenom,
    length(CT, ClassL),
    ClassL1 is ClassL + 1,
    NextI is I + 1,
    write_after(NextI, CT, ClassL1, Stream, Row).

get_row_denom([], _, Denom, Denom). 	   
get_row_denom([No-Class|Row], RCostM, TDenom, Denom):-
    member(Class-Cost, RCostM),
    NewTDenom is (No * Cost) + TDenom,
    get_row_denom(Row, RCostM, NewTDenom, Denom).
	
write_after(ClassL, _, ClassL, _, _).
write_after(I, CT, ClassL, Stream, Row):-
    nth1(I, CT, C),	
    write_rr(C, Stream, Row),
    NewI is I + 1,
    write_after(NewI, CT, ClassL, Stream, Row).

write_before(1, _, _, _).
write_before(I, [CT|CTs], Stream, Row):-
    write_rr(CT, Stream, Row),
    NewI is I - 1,
    write_before(NewI, CTs, Stream, Row).

%write_rr([],_):-
%    nl.
write_rr(_-Class, Stream, RR):-
    member(No-Class,RR),
    format(Stream, '~t ~d ',[No]).%,
    %write_rr(CTs,RR).

get_precision([],[],_,[]).
get_precision(AggVals,[_-Class|CT],Stream,[Class-Precision|Prec]):-
    select(Class-[_, TP, FP, _, _],AggVals,Rest),
    Denominator is TP + FP,
    (Denominator = 0 ->
       Precision = 0
    ;
       Precision is TP / Denominator
    ),
    write(Stream,'Precision for class: '),write(Stream,Class),write(Stream,' is: '),write(Stream,Precision),nl(Stream),
    get_precision(Rest,CT,Stream,Prec).

get_accuracy([], Tot, TP, Accuracy):-
    (Tot = 0 ->
      Accuracy = 0
    ;
      Accuracy is TP / Tot
    ).
get_accuracy([_-[No, TP, _, _, _]|R], TTot, TTP, Accuracy):-
    NewTot is TTot + No,
    NewTP is TTP + TP,
    get_accuracy(R, NewTot, NewTP, Accuracy).

get_recall([],[],_,[]).
get_recall(AggVals,[_-Class|CT],Stream,[Class-Recall|Rec]):-
    select(Class-[_, TP, _, FN, _],AggVals,Rest),
    Recall is TP / (TP + FN),
    write(Stream,'Recall for class: '),write(Stream,Class),write(Stream, ' is: '),write(Stream,Recall),nl(Stream),
    get_recall(Rest,CT,Stream,Rec).

sum_fn([], _, []):-!.
sum_fn([No-Class|Rest], TempFN, [NewNo-Class|Zz]):-
    member(TempNo-Class, TempFN),
    NewNo is No + TempNo,
    sum_fn(Rest, TempFN, Zz).

get_parts([],_,[]).
get_parts([Class-No|Yz],TempFN,[Class-NewNo|Zz]):-
    member(Class-TempNo, TempFN),
    NewNo is No + TempNo,
    get_parts(Yz,TempFN,Zz).

/*
get_mean_val([], _Folds, _).
get_mean_val([Class-[ClassCov,TP,FP,FN]|Zz], Folds, MeanFSum):-
    MeanCov is ClassCov / Folds,
    MeanTP is TP / Folds,
    MeanFP is FP / Folds,
    write('Class: '), write(Class),nl,
    write('Covers '), write(MeanCov), write(' of the example space'),nl,
    write('Accuracy: '), write(MeanTP),nl,
    write(': '), write(MeanFP), nl,
    get_mean_fn(FN, Folds),
    get_mean_val(Zz, Folds, MeanFSum).

get_mean_fn(Cov-FN, Folds):-
    MeanCov is Cov / Folds,
    write('% wrongly classified not belonging to the particular class classified as such are: '), write(MeanCov),nl,
    get_mean_fn2(FN, Folds).

get_mean_fn2([], _Folds).
get_mean_fn2([Class-AggNo|FNs], Folds):-
    Percent is AggNo / Folds,
    write('Class '), write(Class), write(' wrongly classified in (%, wrt. all classes): '), write(Percent),nl,
    get_mean_fn2(FNs, Folds).

get_tc_fc(Class-[TC,FC], TempTCFC, Class-[NewTempTC,NewTempFC]):-
    member(Class-[TempTC,TempFC],TempTCFC),
    NewTempTC is TC + TempTC,
    NewTempFC is FC + TempFC.
*/