%%%%%
%
% Module for making explicit
% matrix indexing easier 
%
%
% Author: Tony Lindgren
%%%%%
:- module(mat, [mat/2, mat/3, mat/4, pp_mat/1,
		mat_set_zero/1, mat_set_id/1,
		mat_set_upper_tri/1,mat_set_stri_upper_tri/1,mat_set_id_upper_tri/1,
		mat_set_lower_tri/1,mat_set_stri_lower_tri/1,mat_set_id_lower_tri/1,
		mat_set_upper_sym/1,mat_set_lower_sym/1,
		mat_no_cols/2, mat_no_rows/2,
		mat_col_arr/3, mat_row_arr/3,
		mat_col_list/3, mat_row_list/3,
		mat_create_id/3, mat_create_diag/4,
		mat_assign/2, mat_eq/2, mat_lin_comb/3,
		mat_axpy/3, mat_tr/1, mat_det/2, mat_co/2, mat_inv/2,
		mat_mult/2, mat_mult/3, mat_add/3, mat_sub/3]).

:- use_module([arr, library(assoc), library(lists), library(pairs)]).


%Create an Matrix of I=rows and J=Columns
%Defines a matrix
mat(IJVar1, Rows):-
  length(Rows, NoRows),
  Rows = [Cols|_],
  length(Cols, NoCols),
  mat(IJVar1, NoRows, NoCols, 0),
  add_m_vals(Rows, 1, 1, IJVar1),!.
mat(IJVar2, I, J):-
  mat(IJVar2, I, J, _),!.	
mat(IJVar3, I, J, IJValue):-
  \+attvar(IJVar3),!,
  (var(IJValue) ->
    create_mat_index(I,J,J,IJKeys),
    length(IJKeys, L),
    length(Values, L),
    pairs_keys_values(Pairs, IJKeys, Values),
    list_to_assoc(Pairs, Mat)
   ;
    create_mat_index(I,J,J,IJKeys),
    length(IJKeys, L),
    set_list(L, IJValue, Values),
    pairs_keys_values(Pairs, IJKeys, Values),
    list_to_assoc(Pairs, Mat)
  ),
  put_attr(IJVar3, mat, Mat),!.

%Get/put value
mat(IJVar4, I, J, Value):-
  attvar(IJVar4),
  integer(I),
  integer(J),
  (atomic(Value) ->                         %PUT, cannot store variables!
    get_attr(IJVar4, mat, Mat), 
    is_assoc(Mat),
    put_assoc(I-J, Mat, Value, NewMat),
    put_attr(IJVar4, mat, NewMat)
  ;
    get_attr(IJVar4, mat, Mat),
    is_assoc(Mat),
    get_assoc(I-J, Mat, Value)
  ),!.

%creates indentity matrix of size n x n
mat_create_id(N, N, Mat):- %must be square
  mat(Mat, N, N, 0),
  mat_set_id(Mat).

%creates diagonal matrix of size n x n
mat_create_diag(N, N, ScalarList, Mat):- %must be square
  mat(Mat, N, N, 0), 
  reverse(ScalarList, RevScalarList),
  do_diag_mat(N,N, Mat, RevScalarList).	

do_diag_mat(0, 0, _, []).
do_diag_mat(No, No, MatVar1, [Scalar|Ss]):-
  mat(MatVar1, No, No, Scalar),
  NewNo is No - 1, 
  do_diag_mat(NewNo, NewNo, MatVar1, Ss).

%Returns number of rows of a matrix
mat_no_rows(IJVar5, NoRows):-
  attvar(IJVar5),
  get_attr(IJVar5, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, Cells),
  last(Cells, NoRows-_-_),!.

%Returns number of columns of a matrix
mat_no_cols(IJVar6, NoCol):-
  attvar(IJVar6),
  get_attr(IJVar6, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, Cells),
  last(Cells, _-NoCol-_),!.

%Returns number of rows and columns of a matrix
mat_no_rows_cols(IJVar6, NoRows, NoCol):-
  attvar(IJVar6),
  get_attr(IJVar6, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, Cells),
  last(Cells, NoRows-NoCol-_),!.

add_m_vals([], _, _, _).
add_m_vals([[]|Cols], CurrRow, _, IJVar):-
  NewCurrRow is CurrRow + 1,
  add_m_vals(Cols, NewCurrRow, 1, IJVar).
add_m_vals([[V|Vs]|Cols], CurrRow, CurrCol, IJVar):-
  mat(IJVar, CurrRow, CurrCol, V),
  NewCurrCol is CurrCol + 1,
  add_m_vals([Vs|Cols], CurrRow, NewCurrCol, IJVar).

create_mat_index(1,1,_,[1-1]).
create_mat_index(I,1,JSize,[I-1|IJKeys]):-
  I > 0,!,
  NewI is I - 1,
  create_mat_index(NewI,JSize,JSize,IJKeys).
create_mat_index(I,J,JSize,[I-J|IJKeys]):-
  NewJ is J - 1,
  create_mat_index(I,NewJ,JSize,IJKeys).

%Matrix linear combination -> V_n Vectors and s_n scalars gives out put vector of w
mat_lin_comb(MatVar,ScalarList,W):-
  mat_no_rows_cols(MatVar, NoRows, NoCols),	
  %mat_no_rows(MatVar, NoRows),	
  length(ScalarList, NoRows),
  get_attr(MatVar, mat, Mat),
  is_assoc(Mat),
  assoc_to_list(Mat, MatList),
  reverse(ScalarList, ScalRevL),
  mat(Wtemp, 1, NoCols, 0),
  it_axpy(NoRows, MatList, ScalRevL, Wtemp, W).

it_axpy(0, _, [], W, W).
it_axpy(RowNo, V,[S|Ss],Wtemp, W):-
  get_row_mat(V, RowNo, RowMat),
  mat_axpy(S, RowMat, Wtemp),
  NewRowNo is RowNo - 1,
  it_axpy(NewRowNo, V, Ss, Wtemp, W).	  

%Matrix axpy -> y := alpha * x * y
mat_axpy(Alpha, XMat, YMat):-
  number(Alpha),	
  mat_no_rows_cols(XMat, NoRows, NoCols),	
  mat_no_rows_cols(YMat, NoRows, NoCols),!,
  mat_mult(XMat, Alpha),
  mat_add(XMat, YMat, TempMat),
  mat_assign(YMat, TempMat).

%sets-all elements of a mtrix to the value specified 
mat_assign(MatVar1, Value):-
  number(Value),	
  mat_no_rows_cols(MatVar1, NoRows, NoCols),!,
  do_mat_assign(NoRows, NoCols, NoCols, Value, MatVar1),!.

%Matrix assignement, first Matrix is assigened the values of the second Matrix
mat_assign(MatVar1, MatVar2):-
  mat_no_rows_cols(MatVar1, NoRows, NoCols),	
  mat_no_rows_cols(MatVar2, NoRows, NoCols),!,
  get_attr(MatVar2, mat, Mat2),
  is_assoc(Mat2),
  assoc_to_list(Mat2, MatList2),
  do_mat_assign(MatList2, MatVar1),!.

%set matric to a symetrical matric using upper, or lower,
mat_set_lower_sym(MatVar):- %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square	
  do_mat_assign_sym(No, No, No, <, MatVar). % > upper, >= strictly upper, < lower =< stricly lower

mat_set_upper_sym(MatVar):- %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square	
  do_mat_assign_sym(No, No, No, >, MatVar).

%assignment of symertrical matrix I, J, = J, I 
do_mat_assign_sym(0, _, _, _, _).
do_mat_assign_sym(I, 0, NoCols, Op, Mat1):-
  NewI is I - 1,
  do_mat_assign_sym(NewI, NoCols, NoCols, Op, Mat1),!.	
do_mat_assign_sym(I, J, NoCols, Op, Mat1):-
  E =..[Op, I, J],
  E, !,
  mat(Mat1, J, I, Val),
  mat(Mat1, I, J, Val),
  NewJ is J - 1,
  do_mat_assign_sym(I, NewJ, NoCols, Op, Mat1),!.
do_mat_assign_sym(I, J, NoCols, Op, Mat1):-
  NewJ is J - 1,
  do_mat_assign_sym(I, NewJ, NoCols, Op, Mat1),!.

%set_to triangular matrices, upper, stri_upper, id_upper, lower, stri_lower, id_lower
mat_set_lower_tri(MatVar):- %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square	
  do_mat_assign_tri(No, No, No, <, MatVar). % > upper, >= strictly upper, < lower =< stricly lower

mat_set_stri_lower_tri(MatVar):- %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square	
  do_mat_assign_tri(No, No, No, =<, MatVar).

mat_set_id_lower_tri(MatVar):-    %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square
  mat_set_id(MatVar),
  do_mat_assign_tri(No, No, No, <, MatVar).

mat_set_upper_tri(MatVar):- %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square	
  do_mat_assign_tri(No, No, No, >, MatVar). % > upper, >= strictly upper, < lower =< stricly lower

mat_set_stri_upper_tri(MatVar):- %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square	
  do_mat_assign_tri(No, No, No, >=, MatVar).

mat_set_id_upper_tri(MatVar):-    %set top half values to 0
  mat_no_rows_cols(MatVar, No, No),                 % must be square
  mat_set_id(MatVar),
  do_mat_assign_tri(No, No, No, >, MatVar).

%assignment of upper rectangualar matrix I, J, i.e. lower half = 0 
do_mat_assign_tri(0, 0, _, _, _). % only 0 first!?
do_mat_assign_tri(I, 0, NoCols, Op, Mat1):-
  NewI is I - 1,
  do_mat_assign_tri(NewI, NoCols, NoCols, Op, Mat1),!.	
do_mat_assign_tri(I, J, NoCols, Op, Mat1):-
  E =..[Op, I, J],
  E, !,
  mat(Mat1, I, J, 0),
  NewJ is J - 1,
  do_mat_assign_tri(I, NewJ, NoCols, Op, Mat1),!.
do_mat_assign_tri(I, J, NoCols, Op, Mat1):-
  NewJ is J - 1,
  do_mat_assign_tri(I, NewJ, NoCols, Op, Mat1),!.	
  
%sets-all elements of a matrix to zero (0)
mat_set_zero(MatVar1):-
  mat_no_rows_cols(MatVar1, NoRows, NoCols),!,
  do_mat_assign(NoRows, NoCols, NoCols, 0, MatVar1),!.

%sets-all matrix to identity matrix
mat_set_id(MatVar1):-
  mat_no_rows_cols(MatVar1, No, No),!, %must be a squre matrix
  do_id_mat(No, No, MatVar1),!.

do_id_mat(0, 0, _).
do_id_mat(No, No, MatVar1):-
  mat(MatVar1, No, No, 1),
  NewNo is No - 1, 
  do_id_mat(NewNo, NewNo, MatVar1).
  
%assignment loop using I, J 
do_mat_assign(0, 0, _, _, _).
do_mat_assign(I, 0, NoCols, Val, Mat1):-
  NewI is I - 1,
  do_mat_assign(NewI, NoCols, NoCols, Val, Mat1),!.	
do_mat_assign(I, J, NoCols, Val, Mat1):-
  mat(Mat1, I, J, Val),
  NewJ is J - 1,
  do_mat_assign(I, NewJ, NoCols, Val, Mat1),!.

%assignmnent loop using list
do_mat_assign([],_).
do_mat_assign([I-J-Val2|IJVals], Mat1):-
  mat(Mat1, I, J, Val2),
  do_mat_assign(IJVals, Mat1),!.

%Matrix equality
mat_eq(MatVar1, MatVar2):-
  mat_no_rows_cols(MatVar1, NoRows, NoCols),	
  mat_no_rows_cols(MatVar2, NoRows, NoCols),!,
  get_attr(MatVar1, mat, Mat1),
  is_assoc(Mat1),
  assoc_to_list(Mat1, MatList1),
  do_mat_eq(MatList1, MatVar2),!.

do_mat_eq([],_).
do_mat_eq([I-J-Val1|IJVals], Mat2):-
  mat(Mat2, I, J, Val2),
  (Val1 =\= Val2 ->
   !,false
   ;
   do_mat_eq(IJVals, Mat2)
  ),!.

%Matrix transpose 
mat_tr(IJVar7):-
  get_attr(IJVar7, mat, Mat),
  is_assoc(Mat),	
  assoc_to_list(Mat, List),
  %last(List, I-J-_),
  %create new assoc tree
  transpose_mat_from_list(List, IJKeys, Values),
  pairs_keys_values(Pairs, IJKeys, Values),
  list_to_assoc(Pairs, NewMat),
  put_attr(IJVar7, mat, NewMat).

transpose_mat_from_list([], [], []).
transpose_mat_from_list([I-J-V|IJVList], [J-I|IJKeys], [V|Values]):-
  transpose_mat_from_list(IJVList, IJKeys, Values).	


%Write cofactor of a matrix
mat_co(IJVar8,CoVar):-
  get_attr(IJVar8, mat, Mat),
  is_assoc(Mat),
  assoc_to_list(Mat, List),
  last(List, Last),
  ((Last = Same-Same-_) ->
    mat(CoVar, Same, Same, 0),
    End is Same - 1,
    get_co(List, List, End, CoVar)
   ;
    write('The matrix must be square')
  ).

get_co([], _, _, _).
get_co([I-J-_|IJVs], OrigL, End, CoVar):-
  sign(I, ISign),
  sign(J, JSign),
  create_sub_m(OrigL, I, J, 1, 1, End, End, SubMatTripple),
  list_to_assoc(SubMatTripple, SubMat),
  put_attr(SubMatVar, mat, SubMat),
  mat_det(SubMatVar, Det),
  CoValue is ISign * JSign * Det, 
  mat(CoVar, I, J, CoValue),
  get_co(IJVs, OrigL, End, CoVar).

% example for determinant should be -306
% mat(Mat1, 1, 1, 6), mat(Mat1, 1, 2, 1), mat(Mat1, 1, 3, 1), mat(Mat1, 2, 1, 4), mat(Mat1, 2, 2, -2), mat(Mat1, 2, 3, 5), mat(Mat1, 3, 1, 2), mat(Mat1, 3, 2, 8), mat(Mat1, 3, 3, 7), mat_det(Mat1, Det).
mat_det(Var, Det):-
  mat_duplicate_var(Var, CVar),   %don't touch original matrix	
  get_attr(CVar, mat, Mat),
  is_assoc(Mat),
  assoc_to_list(Mat, List),
  last(List, Last),
  ((Last = Same-Same-_) ->
    get_det(List, CVar, Det)
   ;
    write('The matrix must be square')
  ).

mat_duplicate_var(Var, CVar):-
  get_attr(Var, mat, Mat),
  is_assoc(Mat),
  assoc_to_list(Mat, List),
  list_to_assoc(List, CopyMat),
  put_attr(CVar, mat, CopyMat).

get_det([1-1-Det], _, Det).
get_det(L, Mat, Det):-
   length(L, 4),
   mat(Mat, 1, 1, V1),
   mat(Mat, 1, 2, V2),
   mat(Mat, 2, 1, V3),
   mat(Mat, 2, 2, V4),
   Det is (V1 * V4) - (V3 * V2).   
get_det(L, Mat, Det):-
   find_det(L, L, Mat, 0, Det).

find_det([2-_-_|_], _, _, Det, Det).
find_det([1-J-V|Rest], OrigL, Mat, TSum, Det):-
   last(Rest, End),
   End=IEnd-JEnd-_,
   NewIEnd is IEnd - 1,
   NewJEnd is JEnd - 1,
   create_sub_m(OrigL, 1, J, 1, 1, NewIEnd, NewJEnd, SubMatTripple),
   list_to_assoc(SubMatTripple, SubMat),
   put_attr(SubMatVar, mat, SubMat),
   get_det(SubMatTripple, SubMatVar, SubDet),
   sign(J, Sign),
   mat(Mat, J, 1, V),   
   NewTSum is TSum + (Sign * V * SubDet),
   find_det(Rest, OrigL, Mat, NewTSum, Det).    

sign(I, -1):-
  NewI is I / 2,
  integer(NewI),!.
sign(_, 1).

sign2(I, 1):-
  NewI is I / 2,
  integer(NewI),!.
sign2(_, -1).

create_sub_m(IJVar9, I, J, SubMatVar):-
  get_attr(IJVar9, mat, Mat),
  is_assoc(Mat),	
  assoc_to_list(Mat, List),
  last(List, End),
  End=IEnd-JEnd-_,
  NewIEnd is IEnd - 1,
  NewJEnd is JEnd - 1,
  create_sub_m(List, I, J, 1, 1, NewIEnd, NewJEnd, SubMatTripple),
  list_to_assoc(SubMatTripple, SubMat),
  put_attr(SubMatVar, mat, SubMat).

create_sub_m([], _, _, _, _, _, _, []).
create_sub_m([I-_-_|Rest], I, J, ICurr, JCurr, IEnd, JEnd, SubMat):-
   !,create_sub_m(Rest, I, J, ICurr, JCurr, IEnd, JEnd, SubMat).	
create_sub_m([_-J-_|Rest], I, J, ICurr, JCurr, IEnd, JEnd, SubMat):-
   !,create_sub_m(Rest, I, J, ICurr, JCurr, IEnd, JEnd, SubMat).
create_sub_m([I-J-V], Irow, Jcol, ICurr, JCurr, IEnd, JEnd, [ICurr-JCurr-V]):-
   %write('Try stopping cond'),nl,
   Irow =\= I,
   Jcol =\= J,!,
   ICurr = IEnd,
   JCurr = JEnd.
create_sub_m([I-J-V|Rest], Irow, Jcol, ICurr, JCurr, IEnd, JEnd, [ICurr-JCurr-V|SubMat]):-
   %write('Try new row'),nl,
   Irow =\= I,
   Jcol =\= J,
   JCurr = JEnd,!,
   NewICurr is ICurr + 1,
   create_sub_m(Rest, Irow, Jcol, NewICurr, 1, IEnd, JEnd, SubMat).
create_sub_m([I-J-V|Rest], Irow, Jcol,  ICurr, JCurr, IEnd, JEnd, [ICurr-JCurr-V|SubMat]):-
   %write('Try new col'),nl,
   Irow =\= I,
   Jcol =\= J,
   JCurr < JEnd,!,
   NewJCurr is JCurr + 1,
   create_sub_m(Rest, Irow, Jcol, ICurr, NewJCurr, IEnd, JEnd, SubMat).

%Inverse of matrix
mat_inv(MatVar,MatInv):-
  mat_det(MatVar, Det),
  mat_co(MatVar, MatCo),
  Const is 1.0 / Det,
  mat_mult(MatCo, Const),
  mat_tr(MatCo),
  MatInv = MatCo.

%add
mat_add(MatVar1, MatVar2, MatVar3):-
  get_attr(MatVar1, mat, Mat1),
  is_assoc(Mat1),
  assoc_to_list(Mat1, MatList1),
  get_attr(MatVar2, mat, Mat2),
  is_assoc(Mat2),
  assoc_to_list(Mat2, MatList2),
  last(MatList1, Last1),
  last(MatList2, Last2),
  Last1 = Rows-Columns-_,
  Last2 = Rows-Columns-_,!,
  mat(MatVar3, Rows, Columns, 0),
  add(MatList1, MatList2, 1, MatVar3).

add([], _, _, _).
add([I-J-Val1|IJVals], MatList2, CurrRow, MatVar3):-
  member(I-J-Val2, MatList2),
  Sum is Val1 + Val2,  
  mat(MatVar3, I, J, Sum),
  add(IJVals, MatList2, CurrRow, MatVar3).

%subtract
mat_sub(MatVar1, MatVar2, MatVar3):-
  get_attr(MatVar1, mat, Mat1),
  is_assoc(Mat1),
  assoc_to_list(Mat1, MatList1),
  get_attr(MatVar2, mat, Mat2),
  is_assoc(Mat2),
  assoc_to_list(Mat2, MatList2),
  last(MatList1, Last1),
  last(MatList2, Last2),
  Last1 = Rows-Columns-_,
  Last2 = Rows-Columns-_,!,
  mat(MatVar3, Rows, Columns, 0),
  sub(MatList1, MatList2, 1, MatVar3).

sub([], _, _, _).
sub([I-J-Val1|IJVals], MatList2, CurrRow, MatVar3):-
  member(I-J-Val2, MatList2),
  Difference is Val1 - Val2,  
  mat(MatVar3, I, J, Difference),
  sub(IJVals, MatList2, CurrRow, MatVar3).

%Square multiplication
mat_mult(MatVar1, MatVar2, MatVar3):-
  get_attr(MatVar1, mat, Mat1),
  is_assoc(Mat1),
  assoc_to_list(Mat1, MatList1),
  get_attr(MatVar2, mat, Mat2),
  is_assoc(Mat2),
  assoc_to_list(Mat2, MatList2),
  mat_no_rows(MatVar1, NoRows),
  mat_no_cols(MatVar2, NoCols),
  mat(MatVar3, NoRows, NoCols, 0),
  mult(MatList1, MatList2, 1, MatVar3).
	
mult([], _, _,  _).
mult(MatList1, MatList2, CurrRow, MatVar3):-
  get_row(CurrRow, MatList1, MatList1Row, MatList1Rest),
  mult_row_col(MatList2, MatList1Row, 1, MatVar3),
  NewCurrRow is CurrRow + 1,
  mult(MatList1Rest, MatList2, NewCurrRow, MatVar3).

mult_row_col([], _MatList1Row, _CurrCol, _MatVar3).
mult_row_col(MatList2, MatList1Row, CurrCol, MatVar3):-
  get_col(CurrCol, MatList2, MatList2Col, MatList2Rest),
  m_r_c(MatList1Row, MatList2Col, 0, MatVar3),  
  NewCurrCol is CurrCol + 1,
  mult_row_col(MatList2Rest, MatList1Row, NewCurrCol, MatVar3).

m_r_c([I-V1], [J-V2], Sum, MatVar3):-
  FSum is Sum + V1 * V2,
  mat(MatVar3, I, J, FSum).
m_r_c([_-V1|IVs], [_-V2|JVs], Sum, MatVar3):-
  NewSum is Sum + V1 * V2, 	 
  m_r_c(IVs, JVs, NewSum, MatVar3).

%get the column
mat_col_arr(IJVar10, J, ColVar):-
  attvar(IJVar10),
  get_attr(IJVar10, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, MatList),
  get_col(J, MatList, Col, _),
  pairs_values(Col, Values),
  arr(ColVar, Values).
mat_col_list(IJVar11, J, ColVals):-
  attvar(IJVar11),
  get_attr(IJVar11, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, MatList),
  get_col(J, MatList, Col, _),
  pairs_values(Col, ColVals).

%get the row
mat_row_arr(IJVar12, I, RowVar):-
  attvar(IJVar12),
  get_attr(IJVar12, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, MatList),
  get_row(I, MatList, Row, _),
  pairs_values(Row, Values),
  arr(RowVar, Values).
mat_row_list(IJVar13, I, RowVals):-
  attvar(IJVar13),
  get_attr(IJVar13, mat, Mat), 
  is_assoc(Mat),
  assoc_to_list(Mat, MatList),
  get_row(I, MatList, Row, _),
  pairs_values(Row, RowVals).  

get_col(_, [], [], []).
get_col(J, [I-OJ-V|IJVs], Col, [I-OJ-V|Rest]):-
  J =\= OJ,
  get_col(J, IJVs, Col, Rest).	
get_col(J, [_-J-V|IJVs], [J-V|LRow], MatList1Rest):-
  get_col(J, IJVs, LRow, MatList1Rest).

get_row_mat(V, RowNo, RowMat):-
  get_row_val(RowNo, V, Row),
  mat(RowMat, [Row]).

get_row_val(_, [], []).
get_row_val(I, [OI-_-_|IJVs], Row):- 
  I =\= OI,
  get_row_val(I, IJVs, Row).
get_row_val(I, [I-_-V|IJVs], [V|LRow]):-
	get_row_val(I, IJVs, LRow).

get_row(_, [], [], []).
get_row(I, [OI-J-V|IJVs], Row, [OI-J-V|Rest]):- 
  I =\= OI,
  get_row(I, IJVs, Row, Rest).
get_row(I, [I-_-V|IJVs], [I-V|LRow], MatList1Rest):-
  get_row(I, IJVs, LRow, MatList1Rest).

mat_mult(MatCoVar, Const):-
  number(Const),	
  get_attr(MatCoVar, mat, Mat),
  is_assoc(Mat),
  assoc_to_list(Mat, List),
  mat_m(List, Const, MatCoVar).

mat_m([], _, _).
mat_m([I-J-V|IJVs], Const, Mat):-
  NewVal is V * Const,
  mat(Mat, I, J, NewVal),
  mat_m(IJVs, Const, Mat).

%PrettyPrint Matrices 
pp_mat(Var):-
  get_attr(Var, mat, Mat),
  is_assoc(Mat),
  assoc_to_list(Mat, List),
  List = [I-_-_|_],
  nl,write('[['),
  pp_mat(List, I). 

pp_mat([], _):-write(']]'),nl.
pp_mat([I-_-V, I-J-NextV|List], I):-
  write(V),write(', '),
  pp_mat([I-J-NextV|List], I).
pp_mat([I-_-V|List], I):-
  write(V),
  pp_mat(List, I).
pp_mat([I-_-V, I-J-NextV|List], Iprev):-
  I > Iprev,!,	
  write(']'),nl,write('['),write(V),write(', '),
  Inext = I,
  pp_mat([I-J-NextV|List], Inext).
pp_mat([I-_-V|List], Iprev):-
  I > Iprev,!,	
  write(']'),nl,write('['),write(V),
  Inext = I,
  pp_mat(List, Inext).

set_list(0, _, []).
set_list(L, Value, [Value|Values]):-
  NewL is L - 1,	
  set_list(NewL, Value, Values).