:- begin_tests(mat).
:- use_module(mat).

%%
%Unit tests for matrices

%test-transpose1
test(transpose1):-
  mat(Mat, [[1,2],
	    [3,4]]),
  mat_tr(Mat),
  mat(Mat, 1, 1, First),
  mat(Mat, 1, 2, Second),
  mat(Mat, 2, 1, Third),
  mat(Mat, 2, 2, Fourth),
  First =:= 1,
  Second =:= 3,
  Third =:= 2,
  Fourth =:= 4.

%test-transpose2
test(transpose2):-
  mat(Mat,[[1,2]]),
  mat_tr(Mat),
  mat(Mat, 1, 1, First),
  mat(Mat, 2, 1, Second),
  First =:= 1,
  Second =:= 2.

%test-createSubMatrix
test(createSubMatrix):-
  mat(Mat, [[1,2,3],
	    [4,5,6],
	    [7,8,9]]),	
  mat:create_sub_m(Mat, 1, 1, SubM1),
  mat(SubM1, 1, 1, First1),
  mat(SubM1, 1, 2, Second1),
  mat(SubM1, 2, 1, Third1),
  mat(SubM1, 2, 2, Fourth1),
  First1 =:= 5,
  Second1 =:= 6,
  Third1 =:= 8,
  Fourth1 =:= 9,
  mat:create_sub_m(Mat, 1, 2, SubM2),
  mat(SubM2, 1, 1, First2),
  mat(SubM2, 1, 2, Second2),
  mat(SubM2, 2, 1, Third2),
  mat(SubM2, 2, 2, Fourth2),
  First2 =:= 4,
  Second2 =:= 6,
  Third2 =:= 7,
  Fourth2 =:= 9,
  mat:create_sub_m(Mat, 2, 2, SubM3),
  mat(SubM3, 1, 1, First3),
  mat(SubM3, 1, 2, Second3),
  mat(SubM3, 2, 1, Third3),
  mat(SubM3, 2, 2, Fourth3),
  First3 =:= 1,
  Second3 =:= 3,
  Third3 =:= 7,
  Fourth3 =:= 9.

%test-determinant1x1
test(determinant1x1):-
  mat(Mat, [[3]]),	
  mat_det(Mat, Det),
  Val is abs(Det - 3),
  Val < 0.0001.

%test-determinant3x3_1
test(determinant3x3_1):-
  mat(Mat, [[-2,2,3],
	    [-1,1,3],
	    [2,0,-1]]),	
  mat_det(Mat, Det),
  Val is abs(Det - 6),
  Val < 0.0001.

%test-determinant4x4_1
test(determinant4x4_1):-
  mat(Mat, [[3,2,0,1],
	    [4,0,1,2],
	    [3,0,2,1],
	    [9,2,3,1]]),	
  mat_det(Mat, Det),
  Val is abs(Det - 24),
  Val < 0.0001.

%test-determinant4x4_2
test(determinant4x4_2):-
  mat(Mat, [[1,2,3,4],
	    [5,6,7,8],
	    [2,6,4,8],
	    [3,1,1,2]]),	
  mat_det(Mat, Det),
  Val is abs(Det - 72),
  Val < 0.0001.

%test-cofactor
test(cofactor):-
  mat(Mat, [[1,2,3],
	    [0,4,5],
	    [1,0,6]]),	
  mat_co(Mat, MatCo),
  mat(MatCo, 1, 1, Co1),
  Val1 is abs(24 - Co1),
  Val1 < 0.0001,
  mat(MatCo, 1, 2, Co2),
  Val2 is abs(5 - Co2),
  Val2 < 0.0001,
  mat(MatCo, 1, 3, Co3),
  Val3 is abs(-4 - Co3),
  Val3 < 0.0001,
  mat(MatCo, 2, 1, Co4),
  Val4 is abs(-12 - Co4),
  Val4 < 0.0001,
  mat(MatCo, 2, 2, Co5),
  Val5 is abs(3 - Co5),
  Val5 < 0.0001,
  mat(MatCo, 2, 3, Co6),
  Val6 is abs(2 - Co6),
  Val6 < 0.0001,
  mat(MatCo, 3, 1, Co7),
  Val7 is abs(-2 - Co7),
  Val7 < 0.0001,
  mat(MatCo, 3, 2, Co8),
  Val8 is abs(-5 - Co8),
  Val8 < 0.0001,
  mat(MatCo, 3, 3, Co9),
  Val9 is abs(4 - Co9),
  Val9 < 0.0001.

%test-inverse1
test(inverse1):-
  mat(Mat, [[1,2,3],
	    [0,4,5],
	    [1,0,6]]),	
  mat_inv(Mat, MatInv),
  mat(MatInv, 1, 1, Inv1),
  Cell1 is 12.0/11.0,
  Val1 is abs(Cell1 - Inv1),
  Val1 < 0.0001,
  mat(MatInv, 1, 2, Inv2),
  Cell2 is -6.0/11.0,
  Val2 is abs(Cell2 - Inv2),
  Val2 < 0.0001,
  mat(MatInv, 1, 3, Inv3),
  Cell3 is -1.0/11.0,
  Val3 is abs(Cell3 - Inv3),
  Val3 < 0.0001,
  mat(MatInv, 2, 1, Inv4),
  Cell4 is 5.0/22.0,
  Val4 is abs(Cell4 - Inv4),
  Val4 < 0.0001,
  mat(MatInv, 2, 2, Inv5),
  Cell5 is 3.0/22.0,
  Val5 is abs(Cell5 - Inv5),
  Val5 < 0.0001,
  mat(MatInv, 2, 3, Inv6),
  Cell6 is -5.0/22.0,
  Val6 is abs(Cell6 - Inv6),
  Val6 < 0.0001,
  mat(MatInv, 3, 1, Inv7),
  Cell7 is -2.0/11.0,
  Val7 is abs(Cell7 - Inv7),
  Val7 < 0.0001,
  mat(MatInv, 3, 2, Inv8),
  Cell8 is 1.0/11.0,
  Val8 is abs(Cell8 - Inv8),
  Val8 < 0.0001,
  mat(MatInv, 3, 3, Inv9),
  Cell9 is 2.0/11.0,
  Val9 is abs(Cell9 - Inv9),
  Val9 < 0.0001.

%Note, we have to take care of this in the code later with catch...
test(inverse2):-
  mat(Mat, [[3,9,2],
	    [0,0,0],
	    [-4,-5,1]]),
  catch(mat_inv(Mat, _),Result,true),
  Result = error(evaluation_error(zero_divisor),context((/)/2,_)).

%
test(add):-
  mat(Mat1, [[1,3],
	    [1,0],
	    [1,2]]),
  mat(Mat2, [[0,0],
	    [7,5],
	    [2,1]]),
  mat_add(Mat1, Mat2, Mat3),
  mat(Mat3, 1, 1, Cell1),
  mat(Mat3, 1, 2, Cell2),
  mat(Mat3, 2, 1, Cell3),
  mat(Mat3, 2, 2, Cell4),
  mat(Mat3, 3, 1, Cell5),
  mat(Mat3, 3, 2, Cell6),
  Cell1 =:= 1,
  Cell2 =:= 3,
  Cell3 =:= 8,
  Cell4 =:= 5,
  Cell5 =:= 3,
  Cell6 =:= 3.

test(subtract):-
  mat(Mat1, [[1,3],
	    [1,0],
	    [1,2]]),
  mat(Mat2, [[0,0],
	    [7,5],
	    [2,1]]),
  mat_sub(Mat1, Mat2, Mat3),
  mat(Mat3, 1, 1, Cell1),
  mat(Mat3, 1, 2, Cell2),
  mat(Mat3, 2, 1, Cell3),
  mat(Mat3, 2, 2, Cell4),
  mat(Mat3, 3, 1, Cell5),
  mat(Mat3, 3, 2, Cell6),
  Cell1 =:= 1,
  Cell2 =:= 3,
  Cell3 =:= -6,
  Cell4 =:= -5,
  Cell5 =:= -1,
  Cell6 =:= 1.

test(multiply1):-
  mat(Mat1, [[1,0,-2],
	    [0,3,-1]]),
  mat(Mat2, [[0,3],
	    [-2,-1],
	    [0,4]]),
  mat_mult(Mat1, Mat2, Mat3),
  mat_no_cols(Mat3, NoCols),
  mat_no_rows(Mat3, NoRows),
  NoCols =:= 2,
  NoRows =:= 2,
  mat(Mat3, 1, 1, Cell1),
  mat(Mat3, 1, 2, Cell2),
  mat(Mat3, 2, 1, Cell3),
  mat(Mat3, 2, 2, Cell4),
  Val1 is abs(0.0 - Cell1),
  Val1 < 0.0001,
  Val2 is abs(-5.0 - Cell2),
  Val2 < 0.0001,
  Val3 is abs(-6.0 - Cell3),
  Val3 < 0.0001,
  Val4 is abs(-7.0 - Cell4),
  Val4 < 0.0001.

test(multiply2):-
  mat(Mat1, [[1,2,3],
	    [4,5,6]]),
  mat(Mat2, [[7,8],
	    [9,10],
	    [11,12]]),
  mat_mult(Mat1, Mat2, Mat3),
  mat_no_cols(Mat3, NoCols),
  mat_no_rows(Mat3, NoRows),
  NoCols =:= 2,
  NoRows =:= 2,
  mat(Mat3, 1, 1, Cell1),
  mat(Mat3, 1, 2, Cell2),
  mat(Mat3, 2, 1, Cell3),
  mat(Mat3, 2, 2, Cell4),
  Val1 is abs(58.0 - Cell1),
  Val1 < 0.0001,
  Val2 is abs(64.0 - Cell2),
  Val2 < 0.0001,
  Val3 is abs(139.0 - Cell3),
  Val3 < 0.0001,
  Val4 is abs(154.0 - Cell4),
  Val4 < 0.0001.

test(multiply3):-
  mat(Mat1, [[3,4,2]]),
  mat(Mat2, [[13,9,7,15],
	    [8,7,4,6],
	    [6,4,0,3]]),
  mat_mult(Mat1, Mat2, Mat3),
  mat_no_cols(Mat3, NoCols),
  mat_no_rows(Mat3, NoRows),
  NoCols =:= 4,
  NoRows =:= 1,
  mat(Mat3, 1, 1, Cell1),
  mat(Mat3, 1, 2, Cell2),
  mat(Mat3, 1, 3, Cell3),
  mat(Mat3, 1, 4, Cell4),
  Val1 is abs(83.0 - Cell1),
  Val1 < 0.0001,
  Val2 is abs(63.0 - Cell2),
  Val2 < 0.0001,
  Val3 is abs(37.0 - Cell3),
  Val3 < 0.0001,
  Val4 is abs(75.0 - Cell4),
  Val4 < 0.0001.

test(equality1):-
  mat(Mat1, [[3,4,2],
             [3,2,4]]),
  mat(Mat2, [[3,4,2],
	     [3,2,4]]),
  mat_eq(Mat1, Mat2).

test(assignment1):-
  mat(Mat1, [[3,4],
             [3,2]]),
  mat(Mat2, [[1,1],
	     [1,1]]),
  mat_assign(Mat1, Mat2),
  mat(Mat1, 1, 1, 1),
  mat(Mat1, 1, 2, 1),
  mat(Mat1, 2, 1, 1),
  mat(Mat1, 2, 2, 1).

test(assignment2):-
  mat(Mat1, [[3,4],
             [3,2]]),
  mat_set_zero(Mat1),
  mat(Mat1, 1, 1, 0),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 0).

test(assignment3):-
  mat(Mat1, [[3,4],
             [3,2]]),
  mat_assign(Mat1, 5),
  mat(Mat1, 1, 1, 5),
  mat(Mat1, 1, 2, 5),
  mat(Mat1, 2, 1, 5),
  mat(Mat1, 2, 2, 5).

test(identity1):-
  mat(Mat1, [[3,4],
             [3,2]]),
  mat_set_id(Mat1),
  mat(Mat1, 1, 1, 1),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 1).

test(identity2):-
  mat_create_id(2, 2, Mat1),
  mat(Mat1, 1, 1, 1),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 1).

test(diagonal1):-
  mat_create_diag(2, 2, [5,5], Mat1),
  mat(Mat1, 1, 1, 5),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 5).

test(triangular1):- %lower triangular
 mat(Mat1, [[3,4],
            [3,2]]),	
  mat_set_lower_tri(Mat1),
  mat(Mat1, 1, 1, 3),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 3),
  mat(Mat1, 2, 2, 2).

test(triangular2):- %strictly lower triangular
 mat(Mat1, [[3,4],
            [3,2]]),
  mat_set_stri_lower_tri(Mat1),
  mat(Mat1, 1, 1, 0),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 3),
  mat(Mat1, 2, 2, 0).

test(triangular3):- %id lower triangular
 mat(Mat1, [[3,4],
            [3,2]]),
  mat_set_id_lower_tri(Mat1),
  mat(Mat1, 1, 1, 1),
  mat(Mat1, 1, 2, 0),
  mat(Mat1, 2, 1, 3),
  mat(Mat1, 2, 2, 1).

test(triangular4):- %id lower triangular
  mat(Mat1, [[2,1,1],
             [5,1,3],
       	     [4,8,9]]),	
  mat_set_upper_tri(Mat1),
  mat(Mat1, 1, 1, 2),
  mat(Mat1, 1, 2, 1),
  mat(Mat1, 1, 3, 1),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 1),
  mat(Mat1, 2, 3, 3),
  mat(Mat1, 3, 1, 0),
  mat(Mat1, 3, 2, 0),
  mat(Mat1, 3, 3, 9).

test(triangular4):-		%upper triangular
 mat(Mat1, [[3,4],
            [3,2]]),	
  mat_set_upper_tri(Mat1),
  mat(Mat1, 1, 1, 3),
  mat(Mat1, 1, 2, 4),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 2).

test(triangular5):- %strictly upper triangular
 mat(Mat1, [[3,4],
            [3,2]]),	
  mat_set_stri_upper_tri(Mat1),
  mat(Mat1, 1, 1, 0),
  mat(Mat1, 1, 2, 4),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 0).

test(triangular6):- %id upper triangular
 mat(Mat1, [[3,4],
            [3,2]]),	
  mat_set_id_upper_tri(Mat1),
  mat(Mat1, 1, 1, 1),
  mat(Mat1, 1, 2, 4),
  mat(Mat1, 2, 1, 0),
  mat(Mat1, 2, 2, 1).

test(symetrical1):- %id upper triangular
 mat(Mat1, [[2,1,1],
            [5,1,3],
	    [4,8,9]]),	
  mat_set_upper_sym(Mat1),
  mat(Mat1, 1, 1, 2),
  mat(Mat1, 1, 2, 5),
  mat(Mat1, 1, 3, 4),
  mat(Mat1, 2, 1, 5),
  mat(Mat1, 2, 2, 1),
  mat(Mat1, 2, 3, 8),
  mat(Mat1, 3, 1, 4),
  mat(Mat1, 3, 2, 8),
  mat(Mat1, 3, 3, 9).

test(symetrical2):- %id upper triangular
 mat(Mat1, [[2,1,1],
            [5,1,3],
	    [4,8,9]]),	
  mat_set_upper_sym(Mat1),
  mat(Mat1, 1, 1, 2),
  mat(Mat1, 1, 2, 1),
  mat(Mat1, 1, 3, 1),
  mat(Mat1, 2, 1, 1),
  mat(Mat1, 2, 2, 1),
  mat(Mat1, 2, 3, 3),
  mat(Mat1, 3, 1, 1),
  mat(Mat1, 3, 2, 3),
  mat(Mat1, 3, 3, 9).

test(axpy1):-
	mat(MatX,[[-1,2,1]]),
	mat(MatY,[[-2,3,-3]]),
	mat_axpy(2,MatX,MatY),
	mat(MatY, 1, 1, -4),
	mat(MatY, 1, 2, -7),
	mat(MatY, 1, 3, -1).

test(lin_comb):-
	mat(Mat,[[2,4,-1,0],
		 [1,0,1,0]]),
	mat_lin_comb(Mat,[3,2],ResMat),
	mat(ResMat, 1, 1, 8),
	mat(ResMat, 1, 2, 12),
	mat(ResMat, 1, 3, -1),
	mat(ResMat, 1, 4, 0). 	
:- end_tests(mat).