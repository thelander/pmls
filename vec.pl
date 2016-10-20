%%%%%
%
% Module for handeling vectors
%  
%
% Author: Tony Lindgren
%%%%%

:- module(vec, [vec/2, vec/3, vec_eq/2, vec_assign/2, vec_add/3, vec_sub/3,
		vec_mult/2, vec_axpy/3, vec_lin_comb/3, vec_dot/3, vec_norm/3,
		pp_vec/1]).

:- use_module([library(assoc), library(lists), library(pairs)]).

%Define vector by Var and List
vec(Var, L):-
  is_list(L),!,
  length(L, NoElem),
  vec(Var, NoElem),
  add_vec_vals(L, 1, Var).
vec(Var, Index):-
  vec(Var, Index, _),!.
vec(Var, Index, Value):-
  \+ attvar(Var),!, 
  %Index is I - 1,!,	
  (var(Value) ->
    numlist(1, Index, Keys),
    length(Keys, L),
    length(Values, L),
    pairs_keys_values(Pairs, Keys, Values),
    list_to_assoc(Pairs, Vec)
  ;
    numlist(1, Index, Keys),
    length(Keys, L),
    set_list(L, Value, Values),
    pairs_keys_values(Pairs, Keys, Values),
    list_to_assoc(Pairs, Vec)
  ),
  put_attr(Var, vec, Vec).

%Get/put value
vec(Var, Index, Value):-
  attvar(Var),
  integer(Index),
  (atomic(Value) ->                         %PUT, cannot store variables!
    get_attr(Var, vec, Vec), 
    is_assoc(Vec),
    put_assoc(Index, Vec, Value, NewVec),
    put_attr(Var, vec, NewVec)
  ;
    get_attr(Var, vec, Vec),
    is_assoc(Vec),
    get_assoc(Index, Vec, Value)
  ).

%Add Values to diffrent indexes
add_vec_vals([], _, _).
add_vec_vals([Val|Vals], I, Var):-
 vec(Var, I, Val),
 NewI is I + 1,
 add_vec_vals(Vals, NewI, Var).

%Set values of the list 
set_list(0, _, []).
set_list(L, Value, [Value|Values]):-
  NewL is L - 1,	
  set_list(NewL, Value, Values).

%pretty_print
pp_vec(Var):-
  get_attr(Var, vec, Vec), 
  is_assoc(Vec),	
  assoc_to_values(Vec, List),
  write(List).

%Check if two vectors are equal
vec_eq(VecVar1, VecVar2):-
  vec_no_elements(VecVar1, NoElements),	
  vec_no_elements(VecVar2, NoElements),!,
  get_attr(VecVar1, vec, Vec1),
  is_assoc(Vec1),
  assoc_to_list(Vec1, VecList1),
  do_vec_eq(VecList1, VecVar2),!.

do_vec_eq([],_).
do_vec_eq([I-Val1|IVals], Vec2):-
  vec(Vec2, I, Val2),
  (Val1 =\= Val2 ->
   !,false
   ;
   do_vec_eq(IVals, Vec2)
  ),!.

%Vector assignement, first Vector is assigened the values of the second Vector
vec_assign(VecVar1, VecVar2):-
  vec_no_elements(VecVar1, NoElements),	
  vec_no_elements(VecVar2, NoElements),!,
  get_attr(VecVar2, vec, Vec2),
  is_assoc(Vec2),
  assoc_to_list(Vec2, VecList2),
  do_vec_assign(VecList2, VecVar1),!.

do_vec_assign([],_).
do_vec_assign([I-Val2|IVals], Vec1):-
  vec(Vec1, I, Val2),
  do_vec_assign(IVals, Vec1),!.

%Returns number of elemets of a vector
vec_no_elements(IVar6, NoElements):-
  attvar(IVar6),
  get_attr(IVar6, vec, Vec), 
  is_assoc(Vec),
  assoc_to_list(Vec, Cells),
  last(Cells, NoElements-_),!.

%vector addition
vec_add(VecVar1, VecVar2, VecVar3):-
  vec_no_elements(VecVar1, NoElements),	
  vec_no_elements(VecVar2, NoElements),!,
  get_attr(VecVar1, vec, Vec1),
  is_assoc(Vec1),
  assoc_to_list(Vec1, VecList1),
  get_attr(VecVar2, vec, Vec2),
  is_assoc(Vec2),
  assoc_to_list(Vec2, VecList2),
  do_vec_add(VecList1, VecList2, VecList3),
  vec(VecVar3, VecList3),!.

do_vec_add([],[],[]).
do_vec_add([I-Val1|Vec1s], [I-Val2|Vec2s], [Val3|Vec3s]):-
  Val3 is Val1 + Val2,
  do_vec_add(Vec1s, Vec2s, Vec3s),!.

%vector subtraction
vec_sub(VecVar1, VecVar2, VecVar3):-
  vec_no_elements(VecVar1, NoElements),	
  vec_no_elements(VecVar2, NoElements),!,
  get_attr(VecVar1, vec, Vec1),
  is_assoc(Vec1),
  assoc_to_list(Vec1, VecList1),
  get_attr(VecVar2, vec, Vec2),
  is_assoc(Vec2),
  assoc_to_list(Vec2, VecList2),
  do_vec_sub(VecList1, VecList2, VecList3),
  vec(VecVar3, VecList3),!.

do_vec_sub([],[],[]).
do_vec_sub([I-Val1|Vec1s], [I-Val2|Vec2s], [Val3|Vec3s]):-
  Val3 is Val1 - Val2,
  do_vec_sub(Vec1s, Vec2s, Vec3s),!.

%vector scaling: multiplies all elements of vec1 with scalar (const)
vec_mult(VecVar, Const):-	
  number(Const),	
  get_attr(VecVar, vec, Vec),
  is_assoc(Vec),
  assoc_to_list(Vec, VecList),
  vec_m(VecList, Const, VecVar).

vec_m([], _, _).
vec_m([I-V|IVs], Const, VecVar):-
  NewVal is V * Const,
  vec(VecVar, I, NewVal),
  vec_m(IVs, Const, VecVar).

%vector axpy: for each element of y and x,  yi := alpha * xi + yi
vec_axpy(Alpha, XVec, YVec):-
  number(Alpha),	
  vec_no_elements(XVec, NoElements),	
  vec_no_elements(YVec, NoElements),!,
  vec_mult(XVec, Alpha),
  vec_add(XVec, YVec, TempVec),
  vec_assign(YVec, TempVec).

%vector linear combination: for each element of vectors v1...vn and scalar s, wi = si*vi + wi, where w1 = the zero vector
vec_lin_comb(ListOfVectors, ListOfScalars, W):-
  length(ListOfVectors, Size),
  length(ListOfScalars, Size),
  check_all_vectors(ListOfVectors, NoDimensions),
  check_all_Scalars(ListOfScalars),!,
  vec(W, NoDimensions, 0),
  do_lin_comb(ListOfVectors,ListOfScalars, W). 

do_lin_comb([],[], _).
do_lin_comb([Vec|Vs],[Scalar|Ss], W):-
  vec_axpy(Scalar, Vec, W),!,
  do_lin_comb(Vs,Ss, W). 	
  
check_all_Scalars([]).
check_all_Scalars([Scalar|Ss]):-
  number(Scalar),!,
  check_all_Scalars(Ss).

%Check that all vectors are indeed vector and of same lenght
check_all_vectors([], _).
check_all_vectors([Vec|Vecs], NoDimensions):-
  get_attr(Vec, vec, VecAssoc),
  is_assoc(VecAssoc),
  assoc_to_list(VecAssoc, VecList),
  length(VecList, NoDimensions),!,
  check_all_vectors(Vecs, NoDimensions).

%vector linear combination: for each element of vectors v1...vn and scalar s, wi = si*vi + wi, where w1 = the zero vector
vec_dot(VecVar1, VecVar2, Dot):-
  vec_no_elements(VecVar1, NoElements),	
  vec_no_elements(VecVar2, NoElements),!,
  get_attr(VecVar1, vec, Vec1),
  is_assoc(Vec1),
  assoc_to_list(Vec1, VecList1),
  get_attr(VecVar2, vec, Vec2),
  is_assoc(Vec2),
  assoc_to_list(Vec2, VecList2),
  do_vec_dot(VecList1, VecList2, 0, Dot).

do_vec_dot([], [], Dot, Dot).
do_vec_dot([I-V1|Vec1s], [I-V2|Vec2s], TempDot, Dot):-
  NewTempDot is TempDot + (V1*V2),
  do_vec_dot(Vec1s, Vec2s, NewTempDot, Dot).

%vector norm (lenght): takes the vector and the (norm to use).
vec_norm(VecVar, NormValue, NormResult):-
  number(NormValue),	
  get_attr(VecVar, vec, Vec),
  is_assoc(Vec),
  assoc_to_list(Vec, VecList),
  vec_x_y(VecList, NormValue, ResList),
  sum_list(ResList, Sum),
  NormResult is Sum^(1/NormValue).

vec_x_y([], _, []).  
vec_x_y([_-V|IVs], NormValue, [VRes|Res]):-
  VRes is V^NormValue,
  vec_x_y(IVs, NormValue, Res).

