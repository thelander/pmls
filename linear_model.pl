%%%%
% Module for creating linear models
%
% Author: Tony Lindgren
%
% version 0.0.1
%
%

%Start writing code for creating linear models (lm)
l_m(Data, Parameters, Model):-
  member(error_est(sqr_err), Parameters),
  fit_l_sqr(Data, Parameters, Model).

%write this after reading chapter 1 in woods book
fit_l_sqr(Data, Parameters, Model).
  