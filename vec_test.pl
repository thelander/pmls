:- begin_tests(vec).
:- use_module(vec).

%%
%Unit tests for vectors
%test equality: Vec1 and Vec2 must be equal for the predicate to succeed 
test(equality1):-
  vec(Vec1, [3,4,2]),
  vec(Vec2, [3,4,2]),
  vec_eq(Vec1, Vec2).

test(equality2):-
  vec(Vec1, [3,4,2]),
  vec(Vec2, [3,4,3]),
  \+vec_eq(Vec1, Vec2).

%test assignment: v1 is assigned values from v2
test(assignment1):-
  vec(V1,[1,2,3]),
  vec(V2, [0,0,0]),
  vec_assign(V1,V2),
  vec(V1, 1, I1),
  I1 =:= 0,
  vec(V1, 2, I2),
  I2 =:= 0,
  vec(V1, 3, I3),
  I3 =:= 0.

%test add: each element of V3 = V1 + V2 
test(add1):-
  vec(V1,[1,2,3]),
  vec(V2, [3,2,1]),
  vec_add(V1, V2, V3),
  vec(V3, 1, I1),
  I1 =:= 4,
  vec(V3, 2, I2),
  I2 =:= 4,
  vec(V3, 3, I3),
  I3 =:= 4.

%test subtraction: each element of V3 = V1 - V2 
test(sub1):-
  vec(V1,[1,2,3]),
  vec(V2, [3,2,1]),
  vec_sub(V1, V2, V3),
  vec(V3, 1, I1),
  I1 =:= -2,
  vec(V3, 2, I2),
  I2 =:= 0,
  vec(V3, 3, I3),
  I3 =:= 2.

%test mult: multiply each element of V1 with scalar (constant number)
test(mult1):-
  vec(V1,[1,2,3]),
  vec_mult(V1, 3),
  vec(V1, 1, I1),
  I1 =:= 3,
  vec(V1, 2, I2),
  I2 =:= 6,
  vec(V1, 3, I3),
  I3 =:= 9.

%test axpy: yi:= a*xi+yi
test(axpy1):-
	vec(VecX, [-1,2,1]),
	vec(VecY, [-2,3,-3]),
	vec_axpy(2,VecX,VecY),
	vec(VecY, 1, I1),
	I1 =:= -4,
	vec(VecY, 2, I2),
	I2 =:= 7,
	vec(VecY, 3, I3),
	I3 =:= -1.

%test vector linear combination: W := si * vi + si+1 * vi+1.. 
test(lin_comb1):-
  vec(V1, [1,0,0]),
  vec(V2, [0,1,0]),
  vec(V3, [0,0,1]),
  vec_lin_comb([V1,V2,V3], [2,-1,3], W),
  vec(W, 1, I1),
  I1 =:= 2,
  vec(W, 2, I2),
  I2 =:= -1,
  vec(W, 3, I3),
  I3 =:= 3.

%test dot product: Dot := v1i*v2i + v1i+1*v2i+1, for all i
test(dot1):-
  vec(V1,[2,5,-6,1]),
  vec(V2, [1,1,1,1]),
  vec_dot(V1,V2,Dot),
  Dot =:= 2.

%test norm (lenght of vector): Dot := v1i*v2i + v1i+1*v2i+1, for all i
test(norm1):-
  vec(V1,[1,2,3]),
  vec_norm(V1,1,Norm),
  Norm =:= 6.

test(norm2):-
  vec(V1,[1,2,3]),
  vec_norm(V1,2,Norm),
  Norm =:=  3.74165738677394136.

test(norm3):-
  vec(V1,[1,2,3]),
  vec_norm(V1,3,Norm),
  Norm =:=  3.3019272488946263.

test(norm4):-
  vec(V1,[1,2,3]),
  vec_norm(V1,5,Norm),
  Norm =:=  3.077384885394063.

:- end_tests(vec).