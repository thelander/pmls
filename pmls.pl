%%%%
% Machine learning system for experimental purposes
% PLMS = Prolog Machine Learning System
%
% Author: Tony Lindgren
%
% version 0.0.3
%
%  

:- module(plms,[run_experiment/3, start/0]).
%tree, util, model_based_sampling, rnd_uniform, munge, indexing]).
:- use_module([library(csv), library(lists), library(random), library(codesio), write_results,
	       tree, rule_set, util, model_based_sampling, rnd_uniform, munge, indexing]).
/*
	       'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/tree.pl',
	       'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/util.pl',
	       'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/model_based_sampling.pl',
	       'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/rnd_uniform.pl',
	       'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/munge.pl',
               'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/indexing.pl']).
*/

%%increase memory
:- set_prolog_stack(global, limit(9 000 000 000 000 000 000)).
:- set_prolog_stack(trail,  limit(9 000 000 000 000 000 000)).
:- set_prolog_stack(local,  limit(9 000 000 000 000 000 000)).

%:- volatile rule_no/1.
%:- dynamic rule_no/1.
:- volatile numerical/1.
:- dynamic numerical/1.

%rule_no(_).
%put_rule_no:-
%    retract(rule_no(T)),
%    NewT is T + 1,
%    assert(rule_no(NewT)).

setup:-
    %current_directory(_,'C:/Users/tony/Box Sync/jobb/dsv/programmering/Indexing/data').              %SICStus prolog
    working_directory(L,L).
    %working_directory(L,'/Users/jimmy/Documents/src/pmls/data_f').   %SU
    %working_directory(L,'C:/Users/sssldy/Box Sync/jobb/dsv/programmering/Indexing/data').   %SCANIA

experiment([validation_size(0.0), fold_x_v(10)]).

method([single_model([error_est(sq_err)], linear_regression)]). %,
        %single_model([classification, list_classification, transform(no), rnd_feature(no), min_cov(5), min_margin(0.90)], sac)]). %,
        %ensemble_model(1000, [classification, bagging, list_classification, %generate_ex([mbs(30), rnd_uniform(30), rnd_uniform(50), munge(30, 0.25, 4)]),
	%bit_size([4, 8, 16, 32, 64, 128, 256, 512, 1024])
	%index([bit_size([4,8,16,32,64,128,256])]),rnd_feature(no), min_cov(0), min_margin(0.90)], sac)]).

%new file meta information and new data input, csv
%also altered old prolog format...
%all csv create a tuple of Name(Xinput, Yval/Class)
data([set(hubble, csv, [filename('regression_data/hubble.csv'), convert(true), functor(hubble), y_cols([3]), x_cols([4])])
       %set(breast_cancer_wisconsin, prolog, [background_head(relation/10),   background_file('classification_data/breast_cancer_wisconsin_cx'), example_file('classification_data/breast_cancer_wisconsin_ex_mod')]),
       %set(bupa,                    prolog, [background_head(bupa/7),        background_file('classification_data/bupa'),                       example_file('classification_data/bupa_ex')]),
       %set(cleveland_heart_disease, prolog, [background_head(relation/14),   background_file('classification_data/cleveland_heart_disease_cx'), example_file('classification_data/cleveland_heart_disease_ex_mod')]),   
       %set(glass,                   prolog, [background_head(glass/10),      background_file('classification_data/glass'),                      example_file('classification_data/glass_ex')]),
       %set(haberman,                prolog, [background_head(haberman/4),    background_file('classification_data/haberman'),                   example_file('classification_data/haberman_data')]),
       %set(iris,                    prolog, [background_head(iris/5),        background_file('classification_data/iris'),                       example_file('classification_data/iris_data')])
       %set(thyroid,                 prolog, [background_head(thyroid/6),     background_file('classification_data/new_thyroid'),                example_file('classification_data/new_thyroid_ex')]),
       %set(wine,                    prolog, [background_head(relation/14),   background_file('classification_data/wine_cx'),                    example_file('classification_data/wine_ex')]),
       %set(image_segmentation,      prolog, [background_head(relation/20),   background_file('classification_data/image_segmentation_cx'),      example_file('classification_data/image_segmentation_ex')]),
       %set(ionosphere,              prolog, [background_head(good_radar/35), background_file('classification_data/ionosphere'),                 example_file('classification_data/ionosphere_ex')]),
       %set(pendigitis,              prolog, [background_head(rel/17),        background_file('classification_data/pendigits_names'),            example_file('classification_data/pendigits_ex')]),
       %set(pima_indians,            prolog, [background_head(diabetes/9),    background_file('classification_data/pima_indians'),               example_file('classification_data/pima_indians_ex')]),
       %set(sonar,                   prolog, [background_head(mine/63),       background_file('classification_data/sonar'),                      example_file('classification_data/sonar_ex')]),
       %set(spectf,                  prolog, [background_head(relation/47),   background_file('classification_data/spectf_cx'),                  example_file('classification_data/spectf_ex')]),       
    ]).

start:-	
    experiment(E),
    method(M),
    data(D),
    write('Starting experiment'),nl,
    run_experiment(experiment(E), method(M), data(D)).

run_experiment(experiment(Parameters), method(Methods), data(DataSets)):-
    process_data(DataSets, Parameters, Methods).

process_data([], _, _).
process_data([set(Name, prolog, InfoList)|Sets], Parameters, Methods):-
    interpret_info(InfoList, Head, BackgroundKnowledge, Examples),	
    read_from_file(BackgroundKnowledge, BKList),
    read_from_file(Examples, ExList),
    random_permutation(ExList, RndExL),	                        %re-order examples
    member(fold_x_v(Folds), Parameters),
    member(validation_size(VSize),Parameters),
    pre_process_data(Folds, VSize, RndExL, ExLists),
    use_methods(Methods, Folds, Name, Head, Parameters, ExLists, BKList),!,
    process_data(Sets, Parameters, Methods).
process_data([set(Name, csv, CsvParameters)|Sets], Parameters, Methods):-
    member(filename(FileName), CsvParameters),
    %use internal csv library..
    %read_csv_file_to_list(FileName, Name, CsvParameters, ExList),
    %trace,
    csv_read_file(FileName, [First|ExList], CsvParameters),     %Firs hold meta info about data-set  	    
    random_permutation(ExList, RndExL),	                        %re-order examples
    member(fold_x_v(Folds), Parameters),
    member(validation_size(VSize),Parameters),
    pre_process_data(Folds, VSize, RndExL, ExLists),
    member(y_cols(YCols), CsvParameters),
    member(x_cols(XCols), CsvParameters),
    use_methods(Methods, Folds, Name, First, [y_cols(YCols),x_cols(XCols)|Parameters], ExLists, no_bk),!,
    process_data(Sets, Parameters, Methods).

interpret_info(InfoList, Head, BackgroundKnowledge, Examples):-
   member(background_head(Name/Arity), InfoList),
   functor(Head, Name, Arity),
   member(background_file(BackgroundKnowledge), InfoList),
   member(example_file(Examples), InfoList).

use_methods([], _, _, _, _, _, _).
use_methods([Method|Methods], Folds, Name, Head, Parameters, ExLists, BKList):-
    do_experiment(Folds, Name, Head, Method, ExLists, BKList, Results),
    open('Result', append, Stream),
    write_results(Results, Stream, Folds),
    close(Stream),
    retract_loop(Results, Head, BKList),!,
    use_methods(Methods, Folds, Name, Head, Parameters, ExLists, BKList).

retract_loop([], _, _).
retract_loop([result(_, _, _, Key, _, _, _, _, _)|Keys], Head, BKList):-
    retract_inner_loop(Key, Head, BKList),
    retract_loop(Keys, Head, BKList).

retract_inner_loop(Key, Head, BKList):-
    atom(Key),
    retractall(Key:Head),
    retract_all(BKList, Key).
retract_inner_loop([], _, _).
retract_inner_loop([Key|Keys], Head, BKList):-
    retractall(Key:Head),
    retract_all(BKList, Key),
    retract_inner_loop(Keys, Head, BKList).
retract_inner_loop(_, _, _). %Fail silently

do_experiment(0, _, _, _, _, _, []).
do_experiment(Folds, Name, Head, Parameters, ExLists, BKList, Eval):-
    do_one_fold(Folds, Name, Head, Parameters, ExLists, BKList, PartEval),
    NewFolds is Folds - 1,!,
    do_experiment(NewFolds, Name, Head, Parameters, ExLists, BKList, RestEval),
    append(PartEval, RestEval, Eval).
do_experiment(Folds, Name, Head, Parameters, ExLists, BKList, Eval):-
    write(Folds), write(Name), write(Head), write(Parameters), write(ExLists), write(BKList), write(Eval),
    do_experiment(Folds, Name, Head, Parameters, ExLists, BKList, Eval).

do_one_fold(TestFold, Name, Head, Method, ExLists, no_bk, Result):-
    TestFold \== 0,
    nth1(TestFold, ExLists, TestExList, TrainExList), 
    do_regression(Method, TrainExList, Model).
    %Write these later
    %clean_conditions(Rules, CleanCondRulesL),
    %add_rule_id(CleanCondRulesL, RulesWId, NoConds, NoRules),
    %post_process_regression(Name, Head, TestFold, Method, TestExList, TrainExList, BKList, ClassIndex, Keys, NoRules, NoConds, ClassT, RulesWId, Result).

do_one_fold(TestFold, Name, Head, Method, ExLists, BKList, Result):-
    TestFold \== 0,
    nth1(TestFold, ExLists, TestExList, TrainExList), 
    do_induction(Method, Head, TrainExList, BKList, TestFold, ClassIndex, Keys, _RawInduction, _NoRules, ClassT, Rules),!,
    clean_conditions(Rules, CleanCondRulesL),
    add_rule_id(CleanCondRulesL, RulesWId, NoConds, NoRules),
    post_process_rules(Name, Head, TestFold, Method, TestExList, TrainExList, BKList, ClassIndex, Keys, NoRules, NoConds, ClassT, RulesWId, Result).

do_one_fold(TestFold, Name, Head, Method, ExLists, no_bk, Result):-
    TestFold \== 0,
    nth1(TestFold, ExLists, TestExList, TrainExList), 
    do_regression(Method, Head, TrainExList, BKList, TestFold, ClassIndex, Keys, _RawInduction, _NoRules, ClassT, Rules),
    clean_conditions(Rules, CleanCondRulesL),
    add_rule_id(CleanCondRulesL, RulesWId, NoConds, NoRules),
    post_process_rules(Name, Head, TestFold, Method, TestExList, TrainExList, BKList, ClassIndex, Keys, NoRules, NoConds, ClassT, RulesWId, Result).

post_process_rules(DataName, Head, TestFold, Method, TestExList, TrainExList, BKList, ClassIndex, Keys, NoRules, NoConds, ClassT, Rules, Result):-
     get_parameters(Method, Parameters),
     member(generate_ex(List), Parameters),!,       % use sampling methods
     ensemble_model(_, _, Type) = Method,           % must be an ensemble or else no use in sampling!!
     copy_term(ClassT, CClassTuples),
     join_folds(TrainExList, NewTrainExList),
     use_sampling_methods(List, Rules, DataName, NewTrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, CClassTuples, SampRes),
     classify_rules(DataName, Method, TestExList, ClassIndex, CClassTuples, Keys, NoRules, NoConds, Rules, PartResult),     
     append(PartResult, SampRes, Result).
post_process_rules(DataName, _Head, TestFold, Method, TestExList, TrainExList, _BKList, ClassIndex, Keys, NoRules, NoConds, ClassT, Rules, Result):-
     %trace,
     get_parameters(Method, Parameters),
     member(index(IParams), Parameters),!,          % use indexing
     copy_term(ClassT, CClassTuples),
     join_folds(TrainExList, NewTrainExList),
     [Ex|_] = NewTrainExList,
     functor(Ex, _, Arity),
     att_min_max(NewTrainExList, Arity, ClassIndex, AttMinMaxList),
     member(bit_size(BitSizeList), IParams),
     add_ex_id(TestExList, TestExWIdList),         %for debugging as of now    
%trace,
     use_indexing1(BitSizeList, TestFold, Rules, AttMinMaxList, TestExWIdList, DataName, IParams, ClassIndex, CClassTuples, IndexRes),
     classify_rules(DataName, Method, TestExWIdList, ClassIndex, CClassTuples, Keys, NoRules, NoConds, Rules, PartResult),
     append(PartResult, IndexRes, Result).
post_process_rules(DataName, _Head, _TestFold, Method,  TestExList, _TrainExList, _BKList, ClassIndex, Keys, NoRules, NoConds, ClassT, Rules, Result):-    
     write('No post-processing'),nl,	
     copy_term(ClassT, CClassTuples),	
     classify_rules(DataName, Method, TestExList, ClassIndex, CClassTuples,  Keys, NoRules, NoConds, Rules, Result).     	

classify_rules(DataName, Method, TestExList, ClassIndex, ClassTuples, Keys, NoRules, NoConds, Rules,
	       [result(DataName, MethodName, Size, Keys, ClassTuples, NoRules, NoConds, PartEval, EnsEvalTime)]):-
       ((Method = ensemble_model(Size, Settings, dac),
	 MethodName = ensemble_dac_list_classification
	)
       ;
       (Method = single_model(Settings, dac),
	 MethodName = single_dac_list_classification,
	 Size = 1
        )
       ),
       member(list_classification, Settings),!,
       collect_conditions(Rules, ClassIndex, RulesList),
       statistics(runtime, _),
       classify_rules_list(dac, TestExList, RulesList, ClassIndex, ClassTuples, ClassTuples, ClassDist, Res),
       statistics(runtime, [_,EnsEvalTime]),
       length(Res, ResL),
       evaluate(ClassDist, ClassTuples, Res, ResL, PartEval).

classify_rules(DataName, Method, TestExList, ClassIndex, ClassTuples, Keys, NoRules, NoConds, _Rules,
	       [result(DataName, MethodName, Size, Keys, ClassTuples, NoRules, NoConds, PartEval, EnsEvalTime)]):-
       ((Method = ensemble_model(Size, _, dac),
	 MethodName = ensemble_dac_bb_classification
	)
       ;
       (Method = single_model(_, dac),
	 MethodName = single_dac_bb_classification,
	 Size = 1
        )
       ),!,	
       %collect_conditions(Rules, ClassIndex, RulesList),
       statistics(runtime, _),
       classify_test_set(dac, TestExList, ClassIndex, ClassTuples, ClassTuples, ClassDist, Keys, Res),
       statistics(runtime, [_,EnsEvalTime]),
       %check_ties(Res, NoTies),
       %write('We found this amount of ties: ' ), write(NoTies),nl,
       length(Res, ResL),
       evaluate(ClassDist, ClassTuples, Res, ResL, PartEval).
classify_rules(DataName, Method, TestExList, ClassIndex, ClassTuples, Keys, NoRules, NoConds, Rules,
	       [result(DataName, MethodName, Size, Keys, ClassTuples1, NoRules, NoConds, PartEval, EnsEvalTime)]):-
       ((Method = ensemble_model(Size, Settings, sac),
	 MethodName = ensemble_sac_list_classification
	)
       ;
	(Method = single_model(Settings, sac),
	 MethodName = single_sac_list_classification,
	 Size = 1
        )
       ),
       member(list_classification, Settings),!,
       %collect_conditions(Rules, ClassIndex, RulesList),
       copy_term(ClassTuples, ClassTuples1),
       copy_term(ClassTuples, ClassTuples2),
       copy_term(ClassTuples, ClassTuples3),
       copy_term(ClassTuples, ClassTuples4),
       statistics(runtime, _),
       %trace,
       classify_rules_list(sac, TestExList, Rules, ClassIndex, ClassTuples2, ClassTuples3, ClassDist, Res),
       statistics(runtime, [_,EnsEvalTime]),
       %check_ties(Res, NoTies),
       %write('We found this amount of ties: ' ), write(NoTies),nl,
       length(Res, ResL),
       evaluate(ClassDist, ClassTuples4, Res, ResL, PartEval).
classify_rules(DataName, Method, TestExList, ClassIndex, ClassTuples, Keys, NoRules, NoConds, _Rules,
	       [result(DataName, MethodName, Size, Keys, ClassTuples, NoRules, NoConds, PartEval, EnsEvalTime)]):-
       ((Method = ensemble_model(Size, _, sac),
	 MethodName = ensemble_sac_bb_classification
	)
       ;
       (Method = single_model(_, sac),
	 MethodName = single_sac_bb_classification,
	 Size = 1
        )
       ),!,	
       %collect_conditions(Rules, ClassIndex, RulesList),
       statistics(runtime, _),
       classify_test_set(sac, TestExList, ClassIndex, ClassTuples, ClassTuples, ClassDist, Keys, Res),
       statistics(runtime, [_,EnsEvalTime]),
       %check_ties(Res, NoTies),
       %write('We found this amount of ties: ' ), write(NoTies),nl,
       length(Res, ResL),
       %trace,
       evaluate(ClassDist, ClassTuples, Res, ResL, PartEval).              

%%    
% classify rules list
%
%classify_rules_list(sac, [TestEx|TestExs], Rules, ClassIndex, ClassTuples, TempCTup, FCTup, [PartRes|Res])
classify_rules_list(_, [], _, _, _, ClassDist, ClassDist, []).
classify_rules_list(Method, [(_ExId,TestEx)|TestExs], Rules, ClassIndex, ClassTuples, TempCTup, FCTup, [PartRes|Res]):-
    TestEx =..[F|Args],
    nth1(ClassIndex, Args, Class, Rest),
    nth1(ClassIndex, NewArgs, _, Rest),
    select(No-Class, TempCTup, TCTup),
    NewNo is No + 1,
    append([NewNo-Class], TCTup, NewTempCTup),    
    NTestEx =..[F|NewArgs],
    get_rules_votes1(Rules, Method, NTestEx, ClassIndex, Class-ClassTuples, PartRes, _RulesFiredList),!,
    %write('Classifying Ex id: '), write(ExId), nl,
    %write('Class become is: '), write(PartRes),nl,
    %write('These rules fired: '),write(RulesFiredList), nl, 
    %nl,
    classify_rules_list(Method, TestExs, Rules, ClassIndex, ClassTuples, NewTempCTup, FCTup, Res).
/*
get_rules_votes1([], dac, _, _, C-CT, C-CT).
get_rules_votes1([tree(TreeRules)|RestRules], dac, NTestEx, ClassIndex, Class-FClassTuples, Class-RSRes):-
    get_rules_votes(TreeRules, dac, NTestEx, ClassIndex, Class-FClassTuples, _-PartRes),       %One rule per tree! 
    get_rules_votes1(RestRules, dac, NTestEx, ClassIndex, Class-FClassTuples, _-RestRes, RulesFiredList),
    joint_dist(RestRes, PartRes, Res),
    keysort(Res, SRes),
    reverse(SRes, RSRes).
*/
get_rules_votes1([], sac, _, _, C-CT, C-CT, []).
get_rules_votes1([rule_set(Rules)|Rest], sac, NTestEx, ClassIndex, Class-ClassTuples, Class-RSRes, FRulesFiredList):-
    get_rules_votes(Rules, sac, NTestEx, ClassIndex, Class-ClassTuples, _-PartRes, RuleIDL),
/*
    (is_zero(PartRes) ->
      %write('we are zero for this example'),nl,
      member(rule([],RuleClassDist), Rules),
      keysort(RuleClassDist, SRuleClasses),                                                     %...until one fires!!
      reverse(SRuleClasses,[_-RClass|_]),
      select(No-RClass, PartRes, RestClasses),
      NewNo is No + 1,
      append([NewNo-RClass], RestClasses, NewClassTuples),
      keysort(NewClassTuples, SNewClassTuples),
      reverse(SNewClassTuples, RSNewClassTuples),
      NPartRes = RSNewClassTuples
      
    ;
      NPartRes = PartRes
    ),
  */
    NPartRes = PartRes,
    get_rules_votes1(Rest, sac, NTestEx, ClassIndex, Class-ClassTuples, _-RestRes, RestRulesFired),
    append(RuleIDL, RestRulesFired, FRulesFiredList),
    joint_dist(RestRes, NPartRes, Res),
    keysort(Res, SRes),
    reverse(SRes, RSRes).

is_zero([]).
is_zero([0-_|R]):-
	is_zero(R).

get_rules_votes([], _, _NTestEx, _ClassIndex, Class-ClassTuples, Class-RSClassT, []):-
    sort(ClassTuples, SClassT),
    reverse(SClassT, RSClassT).
%Not fixed below for ID..
/*
get_rules_votes([Rule|Rules], dac, NTestEx, ClassIndex, TClass-ClassTuples, CTuples):-
    copy_term(ClassTuples, CClassTuples),	
    get_rule_vote(Rule, NTestEx, CClassTuples, ClassDist),
    (ClassDist = CClassTuples ->
      get_rules_votes(Rules, dac, NTestEx, ClassIndex, TClass-ClassTuples, CTuples)             %continue trying rules
    ;
      keysort(ClassDist, Classes),                                                               %...until one fires!!
      reverse(Classes,[_-Class|_]),
      select(No-Class, ClassTuples, Rest),
      NewNo is No + 1,
      append([NewNo-Class], Rest, NewClassTuples),
      keysort(NewClassTuples, SNewClassTuples),
      reverse(SNewClassTuples, RSNewClassTuples),
      CTuples = TClass-RSNewClassTuples
    ).
*/ 
get_rules_votes([rule(Id, Conds, RDist)|Rules], sac, NTestEx, ClassIndex, TClass-ClassTuples, CTuples, FRulesIds):-          %try all rules!
    copy_term(ClassTuples, CClassTuples),	
    get_rule_vote(rule(Id, Conds, RDist), NTestEx, CClassTuples, ClassDist),
    (ClassDist = CClassTuples ->
      get_rules_votes(Rules, sac, NTestEx, ClassIndex, TClass-ClassTuples, CTuples, FRulesIds)
    ;
      keysort(ClassDist,Classes),
      reverse(Classes,[_-Class|_]),
      select(No-Class, ClassTuples, Rest),
      NewNo is No + 1,
      append([NewNo-Class], Rest, NewClassTuples),!,
      keysort(NewClassTuples, SNewClassTuples),
      reverse(SNewClassTuples, RSNewClassTuples),
      get_rules_votes(Rules, sac, NTestEx, ClassIndex, TClass-RSNewClassTuples, CTuples, RuleIds),
      append([Id],RuleIds,FRulesIds)
    ).

get_rule_vote(rule(_, [], _), _, TClassTuple, TClassTuple).                       %Ignore default rule
get_rule_vote(rule(Id, Conds, Dist), Ex, TClassTuple, NewTClassTuple):-
    update_dist(Conds, Id, Ex, Dist, TClassTuple, NewTClassTuple).

update_dist([], _Id, _Ex, Dist, TClassTuple, RSNewClassTuple):-
    %write('Rule with id: '),write(Id), write(' fired!'),nl,	
    keysort(Dist, Classes),
    reverse(Classes,[_-Class|_]),
    select(No-Class, TClassTuple, Rest),
    NewNo is No + 1,
    append([NewNo-Class], Rest, NewClassTuple),
    keysort(NewClassTuple, SNewClassTuple),
    reverse(SNewClassTuple, RSNewClassTuple).
update_dist([index(I)<CVal|Conds], Id, Ex, Dist, TClassTuple, NewTClassTuple):-  
    arg(I, Ex, ExVal),
    (ExVal < CVal ->
      update_dist(Conds, Id, Ex, Dist, TClassTuple, NewTClassTuple)        %okay, so continue
    ;
      TClassTuple = NewTClassTuple                                    %Failed so stop asap
    ).
update_dist([index(I)>=CVal|Conds], Id, Ex, Dist, TClassTuple, NewTClassTuple):-  
    arg(I, Ex, ExVal),
    (ExVal >= CVal ->
      update_dist(Conds, Id, Ex, Dist, TClassTuple, NewTClassTuple)        %okay, so continue
    ;
      TClassTuple = NewTClassTuple                                     %Failed so stop asap
    ).


%%
% use indexing for classifying  
%
use_indexing1([], _, _, _, _, _, _, _, _, []).
use_indexing1([BitSize|BitSizeL], TestFold, RulesL, AttMinMaxList, TestExList, DataName, Parameters, ClassIndex, ClassTuples, Result):-
    write_to_codes(index_rules-BitSize, IndexCodes),
    atom_codes(IndexKey, IndexCodes),
    %length(RulesL, NoRules),
    %write('Total number of rules is: '), write(NoRules),nl,
    use_indexing(RulesL, IndexKey, AttMinMaxList, TestExList, DataName, BitSize, Parameters, ClassIndex, ClassTuples, PartResult),
    use_indexing1(BitSizeL, TestFold, RulesL, AttMinMaxList, TestExList, DataName, Parameters, ClassIndex, ClassTuples, RestResult),
    append(PartResult, RestResult, Result).
    %write(Result).

use_indexing(RulesL, IndexKey, AttMinMaxList, TestExList, DataName, BitSize, _Parameters, ClassIndex, ClassTuples,
	     [result(DataName, IndexKey, 1, non, ClassTuples1, NoRules, 0, IndexEval, IndexEvalTime)]):- %0=NoConds use that of the ensemble! 
    %length(RulesL, NoRules2),
    %write('Total number of rules after cleaning is: '), write(NoRules2),nl,	
    rule_to_index_form(RulesL, CorrectRuleForm),
    index_rules(CorrectRuleForm, BitSize, AttMinMaxList, IndexRules),    
    get_i_rules(IndexRules, NoRules),
    %write('Total number of indexed rules is: '), write(NoRules),nl,
    copy_term(ClassTuples, ClassTuples1),
    copy_term(ClassTuples, ClassTuples2),
    copy_term(ClassTuples, ClassTuples3),
    copy_term(ClassTuples, ClassTuples4),
    statistics(runtime, _),
    %trace,
    index_examples(TestExList, BitSize, AttMinMaxList, ClassIndex, ITestExList),
    %write('Classifying with bit size: '),write(BitSize),nl,
    evaluate_index_rules(ITestExList, IndexRules, ClassIndex, ClassTuples2, ClassTuples3, ClassDist, IndexRes),
    statistics(runtime, [_,IndexEvalTime]), 
    length(IndexRes, IndexResL),
    evaluate(ClassDist, ClassTuples4, IndexRes, IndexResL, IndexEval),!.

get_i_rules([], 0).
get_i_rules([tree(IRules)|RestR], FNoRules):-
    get_i_rules(RestR, RestNoRules),
    length(IRules, PartNoRules),
    FNoRules is RestNoRules + PartNoRules.
get_i_rules([rule_set(IRules)|RestR], FNoRules):-
    get_i_rules(RestR, RestNoRules),
    length(IRules, PartNoRules),
    FNoRules is RestNoRules + PartNoRules.
    
evaluate_index_rules([], _, _, _, ClassDist, ClassDist, []).
evaluate_index_rules([(_Id,IEx)|IExs], IndexRules, ClassIndex, ClassTuple, TClassD, FClassD, [ExClass-EvalClass|PartEval]):-
    %trace,	
    copy_term(ClassTuple, CClassTuple),
    IEx = ex_i(_)-Head,
    %write('Example with id: '),write(Id),write(' and head: '),write(Head),write(' get class dist: '),
    arg(ClassIndex, Head, ExClass),
    get_class_dist_w_index(IndexRules, IEx, ClassIndex, CClassTuple, EvalClass, _RulesFiredList),
    %write(EvalClass),nl,
    %write('These rules fired: '),write(RulesFiredList),nl,
    %get_class_dist(IndexRules, IEx, ClassIndex, CClassTuple, EvalClass),
    select(No-ExClass, TClassD, TCTup),
    NewNo is No + 1,
    append([NewNo-ExClass], TCTup, NewTClassD),!,
    evaluate_index_rules(IExs, IndexRules, ClassIndex, ClassTuple, NewTClassD, FClassD, PartEval).

%%Fix from here... use tree and rule set differently for classification
get_class_dist_w_index([], _IEx, _ClassIndex, _CClassTuple, [], []).
/*
get_class_dist_w_index([tree(IndexRules)|Trees], IEx, ClassIndex, CClassTuple, RSEvalClass):-
    get_class_part_dist(IndexRules, IEx, ClassIndex, CClassTuple, PartClass),
    get_class_dist_w_index(Trees, IEx, ClassIndex, CClassTuple, RestClass),
    joint_dist(RestClass, PartClass, EvalClass),
    keysort(EvalClass, SEvalClass),
    reverse(SEvalClass, RSEvalClass).
*/
get_class_dist_w_index([rule_set(IndexRules)|RuleSets], IEx, ClassIndex, CClassTuple, RSEvalClass, FRulesFList):-
    get_class_part_dist(IndexRules, IEx, ClassIndex, CClassTuple, PartClass, PartFiredList),!,
    get_class_dist_w_index(RuleSets, IEx, ClassIndex, CClassTuple, RestClass, RestFiredList),
    append(PartFiredList, RestFiredList, FRulesFList),
    joint_dist(RestClass, PartClass, EvalClass),
    keysort(EvalClass, SEvalClass),
    reverse(SEvalClass, RSEvalClass).

%Move to util later
joint_dist([],  ClassT, ClassT).
joint_dist([No-Class|Rest], TempClassT, FClassT):-
    select(OtherNo-Class, TempClassT, SelRest),
    NewNo is No + OtherNo,
    select(NewNo-Class, NewTempClassT, SelRest),
    joint_dist(Rest, NewTempClassT, FClassT).

%%
% Check parameter settings to call correct sampling method, then use it for ruleinduction and evaluate the result...
%
use_sampling_methods([], _, _, _, _, _, _, _, _, _, _, _, []).
use_sampling_methods([no], _, _, _, _, _, _, _, _, _, _, _, []).
use_sampling_methods([no|R], RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, CClassTuples, SampRes):-
    !,use_sampling_methods(R, RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, CClassTuples, SampRes).
use_sampling_methods([rnd_uniform(SampleSize)|R], RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, ClassT, [PartResult|RestRes]):-
    write('starting rnd_uniform with size: '), write(SampleSize),nl,
    write_to_codes(rnd_uniform-TestFold-SampleSize, TreeCodes),    %remove testFold here else the evaluation is wrong..?
    atom_codes(TreeKey,TreeCodes),
    rnd_uniform(TrainExList, RulesL, SampleSize, Head, ClassIndex, ClassT, GeneratedEx),
    append(TrainExList, GeneratedEx, CMMTrainEx),
    tree(CMMTrainEx, BKList, Parameters, Head, TreeKey, ClassIndex, _, Tree, _DefaultFact),
    evaluate_tree(Type, TestExList, Name, rnd_uniform, SampleSize, Tree, BKList, Head, TreeKey, ClassIndex, ClassT, PartResult),!,
    use_sampling_methods(R, RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, ClassT, RestRes).
use_sampling_methods([munge(SampleSize, Prob, Variance)|R], RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, ClassT, [PartResult|RestRes]):-	
    write('starting munge with size: '), write(SampleSize),nl,
    write_to_codes(munge-TestFold-SampleSize, TreeCodes),
    atom_codes(TreeKey,TreeCodes),
    munge(TrainExList, RulesL, SampleSize, Prob, Variance, Head, ClassIndex, ClassT, GeneratedEx),
    append(TrainExList, GeneratedEx, CMMTrainEx),
    tree(CMMTrainEx, BKList, Parameters, Head, TreeKey, ClassIndex, _, Tree, _DefaultFact),
    evaluate_tree(Type, TestExList, Name, munge, SampleSize, Tree, BKList, Head, TreeKey, ClassIndex, ClassT, PartResult),!,
    use_sampling_methods(R, RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, ClassT, RestRes).
use_sampling_methods([mbs(SampleSize)|R], RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, ClassT, [PartResult|RestRes]):-
    write('starting mbs with size: '), write(SampleSize),nl,
    write_to_codes(mbs-TestFold-SampleSize, TreeCodes),
    atom_codes(TreeKey,TreeCodes),
    mbs(RulesL, SampleSize, TrainExList, Head, ClassIndex, ClassT, GeneratedEx),!,
    garbage_collect,
    append(TrainExList, GeneratedEx, CMMTrainEx),
    %trace,
    tree(CMMTrainEx, BKList, Parameters, Head, TreeKey, ClassIndex, _, Tree, _DefaultFact),
    evaluate_tree(Type, TestExList, Name, mbs, SampleSize, Tree, BKList, Head, TreeKey, ClassIndex, ClassT, PartResult),!,
    use_sampling_methods(R, RulesL, Name, TrainExList, BKList, TestExList, TestFold, Type, Parameters, Head, ClassIndex, ClassT, RestRes).

%Test classification speed of the indexed tree
/*
evaluate_indexed_tree(ITestExList, ITree, DataName, NoRules, ClassIndex, ClassT, result(DataName, indexed_rules, 0, none, ClassT, NoRules, TreeEval, TreeEvalTime)):-
    statistics(runtime, _),
    classify_indexed_test_set(ITestExList, ITree, ClassIndex, ClassT, ClassT, ClassDist, TreeRes),
    statistics(runtime, [_,TreeEvalTime]),
    length(TreeRes, TreeResL),
    copy_term(ClassT, CClassT),
    evaluate(ClassDist, CClassT, TreeRes, TreeResL, TreeEval),!.

classify_indexed_test_set([], _, _, _, FinalCTup, FinalCTup, []).
classify_indexed_test_set([Ind-TestEx|TestExL], node([],[], NoRules,ILeftTree,IRightTree), ClassIndex, ClassTuples, TempCTup, FCTup, [PartRes|Res]):-
    TestEx =..[F|Args],
    nth1(ClassIndex, Args, Class, Rest),
    nth1(ClassIndex, NewArgs, _, Rest),
    select(No-Class, TempCTup, TCTup),
    NewNo is No + 1,
    append([NewNo-Class], TCTup, NewTempCTup),
    NTestEx =..[F|NewArgs],
    get_indexed_votes(ILeftTree, Ind-NTestEx, ClassIndex, Class-ClassTuples, LPartRes),
    get_indexed_votes(IRightTree,Ind-NTestEx, ClassIndex, Class-ClassTuples, RPartRes),
    add_votes(LPartRes, RPartRes, PartRes),!,
    write('IndexPartRes: '),write(PartRes),nl,nl,
    classify_indexed_test_set(TestExL, node([],[],NoRules,ILeftTree,IRightTree), ClassIndex, ClassTuples, NewTempCTup, FCTup, Res).

add_votes(Class-[], _, Class-[]).
add_votes(Class-[LNo-C|R], Class-RParts, Class-[No-C|PartsR]):-
    member(RNo-C, RParts),
    No is LNo + RNo,
    add_votes(Class-R, Class-RParts, Class-PartsR).
	
get_indexed_votes([], _TestEx, _ClassIndex, TClass-ClassTuples, TClass-RSClassT):-	
    sort(ClassTuples, SClassT),
    reverse(SClassT, RSClassT).
%Crap dist that could not be covered in the induction, not use att all?!
get_indexed_votes([node(leaf-_, IndexedRules, _NoRules, [], [])], TestEx, ClassIndex, TClass-ClassTuples, TClass-NewClassTuples):-
    get_class_dist(IndexedRules, TestEx, ClassIndex, ClassTuples, NewClassTuples).	
%    get_indexed_votes(Rest, TestEx, ClassIndex, TClass-NewClassTuples, CTuples).	
get_indexed_votes([node(SplitV, [], _NoRules, LeftT, RightT)], ex_i(TestIEx)-Ex, ClassIndex, TClass-ClassTuples, CTuples):-
    %write('SplitVec: '), pp_bit_v(8, NoArgs, SplitV),
    %write('ExampleV: '), pp_bit_v(8, NoArgs, TestIEx),nl,	
    Result is TestIEx /\ SplitV,
    NoPop is popcount(Result),
    %write('Pop count: '), write(NoPop),nl,
    (NoPop > 1 ->
      %write('Going left'),nl,
      get_indexed_votes(LeftT, ex_i(TestIEx)-Ex, ClassIndex, TClass-ClassTuples, CTuples)
    ;
      %write('Going right'),nl,
      get_indexed_votes(RightT, ex_i(TestIEx)-Ex, ClassIndex, TClass-ClassTuples, CTuples)
    ).
    %get_indexed_votes(Rest, TestEx, ClassIndex, TClass-NewClassTuples, CTuples).	
*/
get_class_part_dist([], _-_, _, ClassTuples, ClassTuples, []).
get_class_part_dist([rule(_,rule_i(_)-rule([], _, _))|Rest], ex_i(Index)-Ex, ClassIndex, TClassTuple, FClassTuple, FRulesFired):- %ignore def. rule
    get_class_part_dist(Rest, ex_i(Index)-Ex, ClassIndex, TClassTuple, FClassTuple, FRulesFired).	
get_class_part_dist([rule(RId,rule_i(IndexV)-rule(_Cond, NoC, ClassD))|Rest], ex_i(Index)-Ex, ClassIndex, TClassTuple, FClassTuple, FRulesFired):-
   % nl,write('NoC: '),write(NoC),write(' Cond: '),write(Cond),nl,
   % write(' Rule Vec: '), pp_bit_v(4, 3, IndexV), 
   % write('ExampleV: '), pp_bit_v(4, 3, Index),nl,
    CompatibleV is IndexV /\ Index,
    PopC is popcount(CompatibleV) - 1,  %remove leading 1
   % write('POP  count: '), write(PopC),nl,
   % write('Cond count: '), write(NoC),nl,
    %trace,
    ((PopC =:= NoC) ->
   %     write('Rule with Id: '),write(RId),write(' is fired'),nl,
        keysort(ClassD, Classes),
        reverse(Classes, [_-Class|_]),
	select(No-Class, TClassTuple, RestClasses),
	NewNo is No + 1,
        append([NewNo-Class], RestClasses, NewTClassTuple),
        get_class_part_dist(Rest, ex_i(Index)-Ex, ClassIndex, NewTClassTuple, FClassTuple, RestRulesFired),
        append([RId], RestRulesFired, FRulesFired)
     ;
        %write('Rule no compatible skip'),nl,
        get_class_part_dist(Rest, ex_i(Index)-Ex, ClassIndex, TClassTuple, FClassTuple, FRulesFired)
    ).
/*
get_class_dist([], _-_, _, ClassTuples, ClassTuples).	
get_class_dist([rule_i(IndexV)-rule(_, NoC, ClassD)|Rest], ex_i(Index)-Ex, ClassIndex, TClassTuple, FClassTuple):-
    %write('Rule Vec: '), pp_bit_v(8, 6, IndexV),
    %write('ExampleV: '), pp_bit_v(8, 6, Index),nl,
    CompatibleV is IndexV /\ Index,
    PopC is popcount(CompatibleV) - 1,  %remove leading 1
    %write('POP  count: '), write(PopC),nl,
    %write('Cond count: '), write(NoC),nl,
    (PopC =:= NoC ->
        %write('Updating classtuple'),nl,
        keysort(ClassD,Classes),
        reverse(Classes, [_-Class|_]),
	select(No-Class, TClassTuple, RestClasses),
	NewNo is No + 1,
        append([NewNo-Class], RestClasses, NewTClassTuple),!,
        get_class_dist(Rest, ex_i(Index)-Ex, ClassIndex, NewTClassTuple, FClassTuple)
     ;
        %write('Rule no compatible skip'),nl,
        get_class_dist(Rest, ex_i(Index)-Ex, ClassIndex, TClassTuple, FClassTuple)
    ).
*/    
evaluate_tree(Type, TestExList, DataName, SampMethod, SampleSize, Tree, BKList, Head, TreeKey, ClassIndex, ClassT,
	      result(DataName, SampMethod, SampleSize, TreeKey, ClassT, NoRules, TreeEval, TreeEvalTime)):-
    assert_tree_to_bb(Tree, TreeKey, NoRules, _TreeRules),
    statistics(runtime, _),
    classify_test_set(Type, TestExList, ClassIndex, ClassT, ClassT, ClassDist, [TreeKey], TreeRes),
    statistics(runtime, [_,TreeEvalTime]),
    retract_inner_loop(TreeKey, Head, BKList),
    length(TreeRes, TreeResL),
    copy_term(ClassT, CClassT),
    evaluate(ClassDist, CClassT, TreeRes, TreeResL, TreeEval),!.

%Hur hantera NC?! bara låta växa?! eller mellan regler endast...
remove_all_maj_class_rules([], _, _, _, []).
remove_all_maj_class_rules([rule(_,Conds,_,ClassT)|Rest], true, _, MajClass, NewRevTransRules):-
     get_majority_class(ClassT, MajClass),!,
     remove_all_maj_class_rules(Rest, false, [Conds], MajClass, NewRevTransRules).
remove_all_maj_class_rules([rule(_,Conds,_,ClassT)|Rest], false, PrevNConds, MajClass, NewRevTransRules):-
     get_majority_class(ClassT, MajClass),!,
     remove_all_maj_class_rules(Rest, false, [Conds|PrevNConds], MajClass, NewRevTransRules).
remove_all_maj_class_rules([rule(H,C,NC,CT)|Rest], _, PrevNC, MajClass, [rule(H,C,NNC,CT)|NewRevTransRules]):-
     append(PrevNC,NC,NNC),
     remove_all_maj_class_rules(Rest, true, PrevNC, MajClass, NewRevTransRules).

check_ties(Res, NoTies):-
    check_ties(Res, 0, NoTies).
check_ties([], NoTies, NoTies).
check_ties([_-[No-_,No-_|_]|Rest], TempNoT, NoTies):-
    NewTempNoT is TempNoT + 1,!,
    check_ties(Rest, NewTempNoT, NoTies).
check_ties([_|Rest], TempNoT, NoTies):-
    check_ties(Rest, TempNoT, NoTies).

get_parameters(ensemble_model(_, Parameters,_), Parameters).
get_parameters(single_model(Parameters), Parameters).
get_forrest_size(ensemble_model(Size, _, _), Size).
get_learning_type(ensemble_model(_, _, Type), Type).


create_rule_id([], _, []).
create_rule_id([Tree|Trees], ID, FinalRulesWithIDs):-
    create_rule_id1(Tree, ID, NewID, RulesWithIDs1),
    create_rule_id(Trees, NewID, RulesWithIDs2),
    append(RulesWithIDs1, RulesWithIDs2, FinalRulesWithIDs).

create_rule_id1([], NewID, NewID, []).
create_rule_id1([Rule|Rules], ID, FinalID, [ID-Rule|RuleWithIDs]):-
    NewID is ID + 1,
    create_rule_id1(Rules, NewID, FinalID, RuleWithIDs).

evaluate([], _, [], _, []).
evaluate([No-Class|CTuples], ClassTuple, Res, ResL, [Class-[No, TrueP, FalseP, FalseN, FalseTup]|Results]):-
    evaluate_class(Res, Class, RestL, 0, 0, 0, ClassTuple, TrueP, FalseP, FalseN, FalseTup),
    %PercentTrueP is (TrueP / No) * 100,   %ResL
    %PercentFalseP is (FalseP / ResL) * 100, %ResL
    %ClassCov is (No / ResL) * 100 ,
    %nl,
    %write('Class: '), write(Class),nl,
    %write('Covers '), write(ClassCov), write(' of the example space'),nl,
    %write('Accuracy: '), write(PercentTrueP),nl,
    %write('% of False Classifications(wrt. to all classes): '), write(PercentFalseP),nl,
    %write_false_neg1(FalseNTup, ResL, FNegL),!,
    evaluate(CTuples, ClassTuple, RestL, ResL, Results).

write_false_neg1(FalseNTup, TotClassified, Tot-FNegL):-
    sum_false(FalseNTup, 0, Tot),
    %Cov is (Tot / TotClassified) * 100,
    %write('% wrongly classified not belonging to the particular class classified as such are: '), write(Cov),nl,
    write_false_neg(FalseNTup, TotClassified, FNegL).

write_false_neg([], _, []).
write_false_neg(_, 0, 0). %:-
    %write('No False negatives classifications'),nl,!.
write_false_neg([No-Class|Rest], Tot, [Class-Percent|FNegL]):-
    Percent is No/Tot * 100,
    %write('Class '), write(Class), write(' wrongly classified in (%, wrt. all classes): '), write(Percent),nl,
    write_false_neg(Rest, Tot, FNegL).

sum_false([], Tot, Tot).
sum_false([No-_|Rest], TTot, Tot):-
    NewTTot is TTot + No,
    sum_false(Rest, NewTTot, Tot).

evaluate_class([], _Class, [], TrueP, FalseP, FalseN, False, TrueP, FalseP, FalseN, False).
evaluate_class([Class-[_-Class|_]|Rest], Class, RestL, TTrueP, TFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False):-
    NewTTrueP is TTrueP + 1,!,
    evaluate_class(Rest, Class, RestL, NewTTrueP, TFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False).
evaluate_class([Class2-[No-Class|R]|Rest], Class, [Class2-[No-Class|R]|RestL], TTrueP, TFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False):-
    Class \== Class2,!,
    NewTFalseP is TFalseP + 1,
    update_false(TFalse, Class, _NewTFalse),
    evaluate_class(Rest, Class, RestL, TTrueP, NewTFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False).
evaluate_class([Class-[_-Class2|_]|Rest], Class, RestL, TTrueP, TFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False):-
    Class \== Class2,!,
    NewTFalseN is TFalseN + 1,
    update_false(TFalse, Class2, NewTFalse),
    evaluate_class(Rest, Class, RestL, TTrueP, TFalseP, NewTFalseN, NewTFalse, TrueP, FalseP, FalseN, False).
evaluate_class([Class2-[No-Class3|R]|Rest], Class, [Class2-[No-Class3|R]|RestL], TTrueP, TFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False):-
    evaluate_class(Rest, Class, RestL, TTrueP, TFalseP, TFalseN, TFalse, TrueP, FalseP, FalseN, False).

update_false(TFalseN, Class, NewTFalseN):-
    select(No-Class, TFalseN, RestTFalseN),
    NewNo is No + 1,!,
    append([NewNo-Class], RestTFalseN, NewTFalseN).

%prune - write later if needed

classify_test_set(_Type, [],_ClassIndex, _ClassTuples, FinalCTup, FinalCTup, _Keys, []).
classify_test_set(Type, [TestEx|TestExL], ClassIndex, ClassTuples, TempCTup, FCTup, Keys, [PartRes|Res]):-
    TestEx =..[F|Args],
    nth1(ClassIndex, Args, Class, Rest),
    nth1(ClassIndex, NewArgs, _, Rest),
    select(No-Class, TempCTup, TCTup),
    NewNo is No + 1,
    append([NewNo-Class], TCTup, NewTempCTup),
    NTestEx =..[F|NewArgs],
    get_votes(Type, Keys, NTestEx, ClassIndex, Class-ClassTuples, PartRes),!,
    classify_test_set(Type, TestExL, ClassIndex, ClassTuples, NewTempCTup, FCTup, Keys, Res).

%write sac later...
get_votes(dac, [], _TestEx, _ClassIndex, TClass-ClassTuples, TClass-RSClassT):-
    sort(ClassTuples, SClassT),
    reverse(SClassT, RSClassT).
get_votes(dac, [Key|Keys], TestEx, ClassIndex, TClass-ClassTuples, CTuples):-
    copy_term(TestEx,CTestEx),
    call(Key:CTestEx),                              %findall för sac!
    CTestEx =.. [_|Args],
    nth1(ClassIndex, Args, ClassDist),
    keysort(ClassDist,Classes),
    reverse(Classes,[_-Class|_]),
    select(No-Class, ClassTuples, Rest),
    NewNo is No + 1,
    append([NewNo-Class], Rest, NewClassTuples),!,
    get_votes(dac, Keys, TestEx, ClassIndex, TClass-NewClassTuples, CTuples).

%do_regression(Method, TrainExList, Model).
do_regression(single_model(Method, linear_regression), TrainExList, Model):-
    join_folds(TrainExLists, NewTrainExList),!,
    l_m(NewTrainExList, Method, Model).

%do_induction(Method, Head, TrainExList, BKList, TestFold, ClassIndex, Keys, _RawInduction, NoRules, ClassT, Rules),
do_induction(ensemble_model(Size, Parameters, dac), Head, TrainExLists, BKList, BBKey, ClassIndex, BBKeys, Trees, NoTrees, ClassT, Rules):-
    join_folds(TrainExLists, NewTrainExList),!,
    %create_bag(TrainExList, NewTrainExList),!,
    do_trees(Size, Parameters, Head, NewTrainExList, BKList, BBKey, ClassIndex, _, ClassT, BBKeys, Trees, NoTrees, Rules).

do_induction(single_model(Parameters, dac), Head, TrainExLists, BKList, BBKey, ClassIndex, BBKeys, Trees, NoTrees, ClassT, Rules):-
    join_folds(TrainExLists, NewTrainExList),!,
    do_trees(1, Parameters, Head, NewTrainExList, BKList, BBKey, ClassIndex, _, ClassT, BBKeys, Trees, NoTrees, Rules).

do_induction(ensemble_model(Size, Parameters, sac), Head, TrainExLists, BKList, BBKey, ClassIndex, BBKeys, Trees, NoTrees, ClassT, Rules):-
    join_folds(TrainExLists, NewTrainExList),!,
    do_rule_set(Size, Parameters, Head, NewTrainExList, BKList, BBKey, ClassIndex, _, ClassT, BBKeys, Trees, NoTrees, Rules).

do_induction(single_model(Parameters, sac), Head, TrainExLists, BKList, BBKey, ClassIndex, BBKeys, Trees, NoTrees, ClassT, Rules):-
    join_folds(TrainExLists, NewTrainExList),!,
    do_rule_set(1, Parameters, Head, NewTrainExList, BKList, BBKey, ClassIndex, _, ClassT, BBKeys, Trees, NoTrees, Rules).

%%
% join the different folds to one list
join_folds([], []).
join_folds([Fold|Folds], TrainExList):-
   join_folds(Folds, PartList),!,
   append(Fold, PartList, TrainExList).

%%
% random sampling for bagging
create_bag(TrainExList, Size, NewTrainExList):-
    length(TrainExList, NoExamples),!,
    create_bag(TrainExList, Size, NoExamples, NoExamples, NewTrainExList).
create_bag(TrainExList, Size, NewTrainExList):-
    write('Failed creating bag'),nl,
    length(TrainExList, NoExamples),
    create_bag(TrainExList, Size, NoExamples, NoExamples, NewTrainExList).
create_bag(_, _, 0, _, []).
create_bag(TrainExList, FSize, NoExLeft, Size, [Example|Bag]):-
    random(0, Size, Rand),
    nth0(Rand, TrainExList, Example),
    NewNoExLeft is NoExLeft - 1,!,
    create_bag(TrainExList, FSize, NewNoExLeft, Size, Bag).
create_bag(TrainExList, FSize, NoExLeft, Size, Bag):-
    write('Failed creating random bag ex'), %try again
    create_bag(TrainExList, FSize, NoExLeft, Size, Bag).

do_rule_set(0, _Parameters, _Head, _TrainExList, _BKList, _BKKey, _ClassIndex, ClassT, ClassT, [], [], [], []).
do_rule_set(Size, Parameters, Head, TrainExList, BKList, BBKey, ClassIndex, _ClassTuple, FCT, [RuleSetKey|Keys], [RuleSet|RuleSets], [NoRules|Rest], [rule_set(Rules)|RRules]):-
    (member(bagging, Parameters) ->
         %write('Creating bag...'),nl,
         create_bag(TrainExList, Size, NewTrainExList)
         %write('Bag complete...'),nl
    ;
         NewTrainExList = TrainExList
    ),
    write_to_codes(Size-BBKey, RuleSetCodes),
    atom_codes(RuleSetKey, RuleSetCodes),
    rule_set(NewTrainExList, BKList, Parameters, Head, RuleSetKey, ClassIndex, NewClassT, RuleSet, _DefaultFact),
    NewSize is Size - 1,!,    
    (member(list_classification, Parameters) ->
      Head =.. [_|HVarList],
      rough_rule_set_to_rules(RuleSet, HVarList, 0, NoRules, Rules),   
      do_rule_set(NewSize, Parameters, Head, TrainExList, BKList, BBKey, ClassIndex, NewClassT, FCT, Keys, RuleSets, Rest, RRules)
    ;
      assert_rule_set_to_bb(RuleSet, Head, RuleSetKey, 0, NoRules, Rules),  %%implement
      do_rule_set(NewSize, Parameters, Head, TrainExList, BKList, BBKey, ClassIndex, NewClassT, FCT, Keys, RuleSets, Rest, RRules)
    ).

%Skriv om denna så att vi enbart assertar till bb endast vid behov
do_trees(0, _Parameters, _Head, _TrainExList, _BKList, _BKKey, _ClassIndex, ClassT, ClassT, [], [], [], []).
do_trees(Size, Parameters, Head, TrainExList, BKList, BBKey, ClassIndex, _ClassTuple, FCT, [TreeKey|Keys], [Tree|Trees], [NoTrees|Rest], [tree(Rules)|RRules]):-
    (member(bagging, Parameters) ->
         %write('Creating bag...'),nl,
         create_bag(TrainExList, Size, NewTrainExList)
         %write('Bag complete...'),nl
    ;
         NewTrainExList = TrainExList
    ),
    write_to_codes(Size-BBKey, TreeCodes),
    atom_codes(TreeKey,TreeCodes),
    tree(NewTrainExList, BKList, Parameters, Head, TreeKey, ClassIndex, NewClassT, Tree, _DefaultFact),
    NewSize is Size - 1,!,    
    (member(list_classification, Parameters) ->
      tree_to_rules(Tree, NoTrees, Rules),
      do_trees(NewSize, Parameters, Head, TrainExList, BKList, BBKey, ClassIndex, NewClassT, FCT, Keys, Trees, Rest, RRules)
    ;
      assert_tree_to_bb(Tree, TreeKey, NoTrees, Rules),
      do_trees(NewSize, Parameters, Head, TrainExList, BKList, BBKey, ClassIndex, NewClassT, FCT, Keys, Trees, Rest, RRules)
    ).

rough_rule_set_to_rules([], _, NoRules, NoRules, []).
rough_rule_set_to_rules([rule(RoughCondList, Dist)|Rules], HVarList, TNoRules, FNoRules, [rule(IndexedCondList, Dist)|Rest]):-
    collect_rough_conditions(RoughCondList, HVarList, IndexedCondList),
    NewTNoRules is TNoRules + 1,
    rough_rule_set_to_rules(Rules, HVarList, NewTNoRules, FNoRules, Rest).

collect_rough_conditions([], _, []).   	
collect_rough_conditions([numeric(Var)<Val|RoughCondList], HVarList, [index(Index) < Val|Rest]):-   	
    nth1(Index, HVarList, CondVar),
    CondVar == Var,!,
    collect_rough_conditions(RoughCondList, HVarList, Rest).
collect_rough_conditions([numeric(Var)>=Val|RoughCondList], HVarList, [index(Index) >= Val|Rest]):-   	
    nth1(Index, HVarList, CondVar),
    CondVar == Var,!,
    collect_rough_conditions(RoughCondList, HVarList, Rest).

tree_to_rules(dac_node(Head,Features,ClassDist,clean_enough), 1, [CHead]):-
    copy_term((Head, Features), (CHead, CFeatures)),
    member(class(Var),CFeatures),
    Var = ClassDist.
tree_to_rules(dac_node(Head,_Features,_ClassDist,Body), NoRules, Rules):-
    tree_body_to_rules(Head,_, Body, 0, NoRules, Rules).

tree_body_to_rules(_Head,_Conds, [], NoRules, NoRules, []).
tree_body_to_rules(_Head,_Conds, clean_enough, NoR, NoR, []).
tree_body_to_rules(Head,Conds,[dac_node(Cond,FList,ClassDist,no_examples_left)|R], TNoRules, NoRules, [:-(CHead,CCond)|Rules]):-
    var(Conds),!, %ignore it
    %write('No examples left 1'),nl,
    copy_term((Head,FList,Cond), (CHead,CFList,CCond)),
    member(class(Var), CFList),
    %keysort(ClassDist,Classes),
    %reverse(Classes,[_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %BBKey:assertz(:-(CHead,CCond)),
    %get_rule_no(RuleNo),
    %portray_clause(:-(CHead,(CCond,write(RuleNo)))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R, NewTNoRules, NoRules, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,FList,ClassDist,min_cov)|R], TNoRules, NoRules, [:-(CHead,CCond)|Rules]):-
    var(Conds),!, %ignore it
    %write('No examples left 1'),nl,
    copy_term((Head,FList,Cond), (CHead,CFList,CCond)),
    member(class(Var), CFList),
    %keysort(ClassDist,Classes),
    %reverse(Classes,[_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %BBKey:assertz(:-(CHead,CCond)),
    %get_rule_no(RuleNo),
    %portray_clause(:-(CHead,(CCond,write(RuleNo)))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    tree_body_to_rules(Head,Conds,R, NewTNoRules, NoRules, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,FList,ClassDist,clean_enough)|R], TNoRules, NoRules, [:-(CHead,CCond)|Rules]):-
    var(Conds),!, %ignore it
    copy_term((Head,FList,Cond), (CHead,CFList,CCond)),
    member(class(Var), CFList),
    %keysort(ClassDist,Classes),
    %reverse(Classes,[_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %BBKey:assertz(:-(CHead, CCond)),
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead, (CCond,write(RuleNo)))),
    %portray_clause(:-(CHead,(CCond,write(RuleNo)))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    tree_body_to_rules(Head,Conds,R, NewTNoRules, NoRules, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,FList,ClassDist,no_examples_left)|R], TNoRules, NoRules, [:-(CHead,(CCond, CConds))|Rules]):-
    %write('No examples left 2'),nl,
    copy_term((Head, FList, Cond, Conds), (CHead, CFList, CCond, CConds)),
    member(class(Var), CFList),
    %keysort(ClassDist, Classes),
    %reverse(Classes, [_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead, (CCond, CConds))),
    %BBKey:assertz(:-(CHead,(CCond, (CConds, write(RuleNo))))),
    %portray_clause(:-(CHead,(CCond,(CConds, write(RuleNo))))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    tree_body_to_rules(Head,Conds,R, NewTNoRules, NoRules, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,FList,ClassDist,min_cov)|R], TNoRules, NoRules, [:-(CHead,(CCond, CConds))|Rules]):-
    %write('No examples left 2'),nl,
    copy_term((Head, FList, Cond, Conds), (CHead, CFList, CCond, CConds)),
    member(class(Var), CFList),
    %keysort(ClassDist, Classes),
    %reverse(Classes, [_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %BBKey:assertz(:-(CHead, (CCond, CConds))),
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead,(CCond, (CConds, write(RuleNo))))),
    %portray_clause(:-(CHead,(CCond,(CConds, write(RuleNo))))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    tree_body_to_rules(Head,Conds,R, NewTNoRules, NoRules, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,FList,ClassDist,clean_enough)|R], TNoRules, NoRules, [:-(CHead,(CCond, CConds))|Rules]):-
    copy_term((Head, FList, Cond, Conds), (CHead, CFList, CCond, CConds)),
    member(class(Var), CFList),
    %keysort(ClassDist, Classes),
    %reverse(Classes, [_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %BBKey:assertz(:-(CHead, (CCond, CConds))),
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead,(CCond, (CConds, write(RuleNo))))),
    %portray_clause(:-(CHead,(CCond,(CConds, write(RuleNo))))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    tree_body_to_rules(Head,Conds,R, NewTNoRules, NoRules, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,_FList,_ClassDist,List)|R], TNoRules, NoRules, Rules):-
    var(Conds),!,
    tree_body_to_rules(Head, Cond, List, 0, NoRules1, Rules1),
    tree_body_to_rules(Head, Conds, R, 0, NoRules2, Rules2),
    NoRules is NoRules1 + NoRules2 + TNoRules,
    append(Rules1, Rules2, Rules).
tree_body_to_rules(Head,Conds,[dac_node(Cond,_FList,_ClassDist,List)|R], TNoRules, NoRules, Rules):-
    tree_body_to_rules(Head,(Cond,Conds),List, 0, NoRules1, Rules1),
    tree_body_to_rules(Head,Conds,R, 0, NoRules2, Rules2),
    NoRules is NoRules1 + NoRules2 + TNoRules,
    append(Rules1, Rules2, Rules).


% we only have a default rule, no more rules... lägg till denna i andra kod baser..
assert_tree_to_bb(dac_node(Head,Features,ClassDist,clean_enough), BBKey, 1, [CHead]):-
    copy_term((Head, Features), (CHead, CFeatures)),
    member(class(Var),CFeatures),
    Var = ClassDist,
    BBKey:assertz(CHead).   
assert_tree_to_bb(dac_node(Head,_Features,_ClassDist,Body),BBKey, NoRules, Rules):-
    assert_numerical(BBKey),
    %append(ensemble_rules_home),
    assert_body_to_bb(Head,_,Body,BBKey, 0, NoRules, Rules).
    %write('%One Fold'),nl,
    %told.
assert_tree_to_bb(dac_node(Head,_Features,_ClassDist,Body),BBKey, NoRules, Rules):-
     write('ASSERT_TREE_TO_BB FAILED!'),nl,
     write('Head: '),write(Head),nl,
     write('Body: '),write(Body),nl,
     write('BBKey: '),write(BBKey),nl,
     write('NoRules '),write(NoRules),nl,
     write('Rules '),write(Rules),nl,
     assert_body_to_bb(Head,_,Body,BBKey, 0, NoRules, Rules).

%Add more for categorical from Background knowledge etc...
assert_numerical(BBKey):-
    BBKey:asserta(:-(numerical(A),number(A))).

assert_body_to_bb(_Head,_Conds,[],_BBKey, NoRules, NoRules, []).
assert_body_to_bb(_Head,_Conds,clean_enough,_BBKey, NoR, NoR, []).
assert_body_to_bb(Head,Conds,[dac_node(Cond,FList,ClassDist,no_examples_left)|R],BBKey, TNoRules, NoRules, [:-(CHead,CCond)|Rules]):-
    var(Conds),!, %ignore it
    %write('No examples left 1'),nl,
    copy_term((Head,FList,Cond), (CHead,CFList,CCond)),
    member(class(Var), CFList),
    %keysort(ClassDist,Classes),
    %reverse(Classes,[_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    BBKey:assertz(:-(CHead,CCond)),
    %get_rule_no(RuleNo),
    %portray_clause(:-(CHead,(CCond,write(RuleNo)))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R,BBKey, NewTNoRules, NoRules, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,FList,ClassDist,min_cov)|R],BBKey, TNoRules, NoRules, [:-(CHead,CCond)|Rules]):-
    var(Conds),!, %ignore it
    %write('No examples left 1'),nl,
    copy_term((Head,FList,Cond), (CHead,CFList,CCond)),
    member(class(Var), CFList),
    %keysort(ClassDist,Classes),
    %reverse(Classes,[_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    BBKey:assertz(:-(CHead,CCond)),
    %get_rule_no(RuleNo),
    %portray_clause(:-(CHead,(CCond,write(RuleNo)))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R,BBKey, NewTNoRules, NoRules, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,FList,ClassDist,clean_enough)|R],BBKey, TNoRules, NoRules, [:-(CHead,CCond)|Rules]):-
    var(Conds),!, %ignore it
    copy_term((Head,FList,Cond), (CHead,CFList,CCond)),
    member(class(Var), CFList),
    %keysort(ClassDist,Classes),
    %reverse(Classes,[_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    BBKey:assertz(:-(CHead, CCond)),
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead, (CCond,write(RuleNo)))),
    %portray_clause(:-(CHead,(CCond,write(RuleNo)))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R,BBKey, NewTNoRules, NoRules, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,FList,ClassDist,no_examples_left)|R],BBKey, TNoRules, NoRules, [:-(CHead,(CCond, CConds))|Rules]):-
    %write('No examples left 2'),nl,
    copy_term((Head, FList, Cond, Conds), (CHead, CFList, CCond, CConds)),
    member(class(Var), CFList),
    %keysort(ClassDist, Classes),
    %reverse(Classes, [_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    %get_rule_no(RuleNo),
    BBKey:assertz(:-(CHead, (CCond, CConds))),
    %BBKey:assertz(:-(CHead,(CCond, (CConds, write(RuleNo))))),
    %portray_clause(:-(CHead,(CCond,(CConds, write(RuleNo))))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R,BBKey, NewTNoRules, NoRules, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,FList,ClassDist,min_cov)|R],BBKey, TNoRules, NoRules, [:-(CHead,(CCond, CConds))|Rules]):-
    %write('No examples left 2'),nl,
    copy_term((Head, FList, Cond, Conds), (CHead, CFList, CCond, CConds)),
    member(class(Var), CFList),
    %keysort(ClassDist, Classes),
    %reverse(Classes, [_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    BBKey:assertz(:-(CHead, (CCond, CConds))),
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead,(CCond, (CConds, write(RuleNo))))),
    %portray_clause(:-(CHead,(CCond,(CConds, write(RuleNo))))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R,BBKey, NewTNoRules, NoRules, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,FList,ClassDist,clean_enough)|R],BBKey, TNoRules, NoRules, [:-(CHead,(CCond, CConds))|Rules]):-
    copy_term((Head, FList, Cond, Conds), (CHead, CFList, CCond, CConds)),
    member(class(Var), CFList),
    %keysort(ClassDist, Classes),
    %reverse(Classes, [_-Class|_]),
    %Var = Class,
    Var = ClassDist,
    BBKey:assertz(:-(CHead, (CCond, CConds))),
    %get_rule_no(RuleNo),
    %BBKey:assertz(:-(CHead,(CCond, (CConds, write(RuleNo))))),
    %portray_clause(:-(CHead,(CCond,(CConds, write(RuleNo))))),
    %put_rule_no,
    NewTNoRules is TNoRules + 1,
    assert_body_to_bb(Head,Conds,R,BBKey, NewTNoRules, NoRules, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,_FList,_ClassDist,List)|R],BBKey, TNoRules, NoRules, Rules):-
    var(Conds),!,
    assert_body_to_bb(Head,Cond,List,BBKey, 0, NoRules1, Rules1),
    assert_body_to_bb(Head,Conds,R,BBKey, 0, NoRules2, Rules2),
    NoRules is NoRules1 + NoRules2 + TNoRules,
    append(Rules1, Rules2, Rules).
assert_body_to_bb(Head,Conds,[dac_node(Cond,_FList,_ClassDist,List)|R],BBKey, TNoRules, NoRules, Rules):-
    assert_body_to_bb(Head,(Cond,Conds),List,BBKey, 0, NoRules1, Rules1),
    assert_body_to_bb(Head,Conds,R,BBKey, 0, NoRules2, Rules2),
    NoRules is NoRules1 + NoRules2 + TNoRules,
    append(Rules1, Rules2, Rules).

loop_write(_,[]).
loop_write(N,[_F|R]):-
    %length(F,L),
    %write('Fold '),write(N), write(' has '), write(L),write(' examples'),nl,
    NewN is N + 1,
    loop_write(NewN,R).

pre_process_data(N, VSize, ExList, ExLists):- %, ValExList):-
    length(ExList, NoEx),
    ExPerFold is round(NoEx / N),
    NoValPerFold is round(ExPerFold * VSize),
    NewExPerFold is ExPerFold - NoValPerFold,
    rnd_fold_gen(N, ExList, NewExPerFold, NoValPerFold, NoEx, ExLists). %, ValExList).

rnd_fold_gen(0, [], _ExPerFold, _NoValPerFold, _NoEx, []). %, []).
rnd_fold_gen(0, _ExLeft, _, _, _, []). %, []). % throw example away!, fix better solution later
rnd_fold_gen(N, Examples, ExPerFold, NoValsPerFold, NoEx, [Fold|Folds]):- %, [FoldVal|FoldVals]):-
    %gen_one_fold(NoValsPerFold, NoEx, Examples, NewNoEx, NewExamples, FoldVal),
    gen_one_fold(ExPerFold, NoEx, Examples, NewNoEx, NewExamples, Fold),
    NewN is N - 1,
    rnd_fold_gen(NewN, NewExamples, ExPerFold, NoValsPerFold, NewNoEx, Folds). %, FoldVals).

gen_one_fold(0, NoEx, Examples, NoEx, Examples, []).
gen_one_fold(_, 0, Examples, 0, Examples, []).
gen_one_fold(ExPerFold, NoEx, Examples, RestNoEx, RestExamples, [Ex|Exs]):-
    %Pick training examples per fold randomly
    random(0, NoEx, Rand),
    nth0(Rand, Examples, Ex, NewExamples),
    NewExPerFold is ExPerFold - 1,
    NewNoEx is NoEx - 1,
    gen_one_fold(NewExPerFold, NewNoEx, NewExamples, RestNoEx, RestExamples, Exs).

% Read clauses from file
read_from_file(FileName,FileInformation):-
    exists_file(FileName),!,
    see(FileName),
    read(X),
    read_loop(X,FileInformation),
    seen.
read_from_file(_FileName,_FileInformation):-
    write('No such file exist'),nl.

read_loop(end_of_file,[]).
read_loop(X,[X|R]):-
    read(NewX),
    read_loop(NewX,R).

%Could/should be done in conjuntion w induction...
add_rule_id(RuleSets, RulesWId, NoConds, NoRules):-
    add_rule_id(RuleSets, 0, RulesWId, NoConds, NoRules).

add_rule_id([], _, [], 0, 0).
add_rule_id([rule_set(Rules)|RuleSets], RuleSetId, [rule_set(PartRulesWId)|RestRulesWId], FConds, FNoRules):-
    NRuleSetId is RuleSetId + 1,
    add_r_id(Rules, NRuleSetId, 1, PartRulesWId, PartConds, PartNoRules),	   
    add_rule_id(RuleSets, NRuleSetId, RestRulesWId, RestConds, RestNoRules),
    FConds is PartConds + RestConds,
    FNoRules is PartNoRules + RestNoRules.

add_r_id([], _, _, [], 0, 0).
add_r_id([rule(Cond, Dist)|Rules], NRuleSetId, RuleId, [rule(NRuleSetId-RuleId, Cond, Dist)|RulesWId], FAccConds, FNoRules):-
    NewRuleId is RuleId + 1,
    length(Cond, NoConds),
    add_r_id(Rules, NRuleSetId, NewRuleId, RulesWId, RestAccConds, RestNoRules),
    FAccConds is NoConds + RestAccConds,
    FNoRules is RestNoRules + 1.

add_ex_id(Ex, ExWId):-
   add_e_id(Ex, 1, ExWId).
add_e_id([], _, []).
add_e_id([Ex|Exs], Id, [(Id, Ex)|ExWId]):-
   NewId is Id + 1,	
   add_e_id(Exs, NewId, ExWId).

%%
% fill the predTuple list with values
read_csv_file_to_list(FileName, Name, CsvParameters, ExList):-
  open(FileName, read, Stream),
  trace,
  handle_input(Stream, 1, Name, CsvParameters, ExList),
  close(Stream).
  
handle_input(Stream, Line, Name, CsvParameters, PredTuple):-
  member(ignore_row(RowList), CsvParameters),
  member(Line, RowList),!,
  read_line_to_codes(Stream, LineOfCodes),
  (LineOfCodes == end_of_file ->
    !
  ;
    NewLine is Line + 1,
    handle_input(Stream, NewLine, Name, CsvParameters, PredTuple)
  ).
handle_input(Stream, Line, Name, CsvParameters, PredTuple):-
  read_line_to_codes(Stream, LineOfCodes),
  (LineOfCodes == end_of_file ->
    !
  ;
    NewLine is Line + 1,
    member(ignore_col(IgCols), CsvParameters),
    member(x_cols(XCols), CsvParameters),
    member(y_cols(YCols), CsvParameters),
    length(XCols, NoXs),
    length(YCols, NoYs),
    Args is NoXs + NoYs,
    functor(PredTuple, Name, Args), 
    handle_line(LineOfCodes, 1, IgCols, XCols, YCols, PredTuple),
    handle_input(Stream, NewLine, Name, CsvParameters, PredTuple)
  ).

%End of the line...
handle_line([], _, _, _, _, _).
%Ignore this column
handle_line(LineOfCodes, Col, IgCol, XCol, YCol, PredTuple):-
  member(Col, IgCol),!,
  get_col(LineOfCodes, _, RestOfCodes),
  NewCol is Col + 1,
  handle_line(RestOfCodes, NewCol, IgCol, XCol, YCol, PredTuple).
%Add this column as x (input)
handle_line(LineOfCodes, Col, IgCol, XCol, YCol, PredTuple):-
  member(Col, XCol),!,
  length(IgCol, RemovedCols),
  get_col(LineOfCodes, ColumnValue, RestOfCodes),
  Arg is Col - RemovedCols,  
  arg(Arg, PredTuple, ColumnValue),
  NewCol is Col + 1,
  handle_line(RestOfCodes, NewCol, IgCol, XCol, YCol, PredTuple).
%Add this column as y (output)
handle_line(LineOfCodes, Col, IgCol, XCol, YCol, PredTuple):-
  member(Col, YCol),!,
  length(IgCol, RemovedCols),
  get_col(LineOfCodes, ColumnValue, RestOfCodes),
  Arg is Col - RemovedCols,  
  arg(Arg, PredTuple, ColumnValue),
  NewCol is Col + 1,
  handle_line(RestOfCodes, NewCol, IgCol, XCol, YCol, PredTuple). 
  

get_col(Codes, Value, RestOfCodes):-
  get_number(Codes, RestOfCodes, Value).
get_col(Codes, Value, RestOfCodes):-
  get_atom(Codes, RestOfCodes, Value).	

%first try to get into a number
get_number(Codes, RestOfCodes, Value):-
  get_numberC(Codes, NumberCodes, RestOfCodes),
  number_codes(Value, NumberCodes).

get_numberC([Code|RestOfCodes], [], RestOfCodes):-
  char_code(',', Code),!.	%Stop at ','
get_numberC([Code|RestOfCodes], NumberCodes, RestOfCodes):-
  char_code(' ', Code),!,	%remove if we have blank junk
  get_numberC(RestOfCodes, NumberCodes, RestOfCodes).
get_numberC([Code|Codes], [Code|NumberCodes], RestOfCodes):-
  get_numberC(Codes, NumberCodes, RestOfCodes).

%else get it as an atom
get_atom(Codes, RestOfCodes, Value):-
  get_atomC(Codes, AtomCodes, RestOfCodes),
  atom_codes(Value, AtomCodes).

get_atomC([Code|RestOfCodes], [], RestOfCodes):-
  char_code(',', Code).
get_atomC([Code|Codes], [Code|AtomCodes], RestOfCodes):-
  get_atomC(Codes, AtomCodes, RestOfCodes).



:- setup.



















