(*This are the functions programmed in "An introduction to programming with Mathematica"
	by Paul Wellin, Richard Gaylord and Samuel Kamin *)
(*Last update: 2010-11-09*)
(*Johan Rhodin*)

(*-----------------------------------------------------*)
(*Chapter 1 - An introduction to Mathematica*)
(*-----------------------------------------------------*)
(*Find all Perfect numbers in a range. *)
PerfectQ[n_] := Apply[Plus, Divisors[n]] == 2 n
PerfectSearch[n_] := Select[Range[n], PerfectQ]

(*Newton method for finding roots*)
NewtonZero[f_, xi_] := NestWhile[(# - f[#]/f'[#]) &, xi, Unequal, 2]
g[x_] := x^3 - 2 x^2 + 1

(*-----------------------------------------------------*)
(*Chapter 2 - the Mathematica language*)
(*-----------------------------------------------------*)

(*A predicate function that returns a value of True if its argument is between -1 and 1*)
absValueLessThanOneQ[x_] := x < 1 && x > -1

(*A predicate function NaturalQ[n] that returns True if n is an element of
 N (the natural numbers)*)
NaturalQ[n_] := ( n >= 1) \[And] IntegerQ[n]

(*A predicate function SubsetQ[Subscript[lis, 1],Subscript[lis, 2]] that returns
 True if Subscript[lis, 1]is a subset of Subscript[lis, 2].*)
subSetQ[lis1_List, lis2_List] := Intersection[lis1, lis2] == lis1
subSetQ[{}, lis2_List] := True

(*-----------------------------------------------------*)
(*Chapter 3 - Lists*)
(*-----------------------------------------------------*)

