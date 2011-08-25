(*This are the functions programmed in "An introduction to programming with Mathematica"
	by Paul Wellin, Richard Gaylord and Samuel Kamin *)
(*All page references are from the above book unless stated otherwise*)
(*Last update: 2010-11-22*)
(*Johan Rhodin, www.johanrhodin.se*)

(*-----------------------------------------------------*)
(*Chapter 1 - An introduction to Mathematica           *)
(*-----------------------------------------------------*)
(*Find all Perfect numbers in a range. *)
PerfectQ[n_] := Apply[Plus, Divisors[n]] == 2 n
PerfectSearch[n_] := Select[Range[n], PerfectQ]

(*Newton method for finding roots*)
NewtonZero[f_, xi_] := NestWhile[(# - f[#]/f'[#]) &, xi, Unequal, 2]
g[x_] := x^3 - 2 x^2 + 1

dfdf

(*-----------------------------------------------------*)
(*Chapter 2 - the Mathematica language                 *)
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
(*Chapter 3 - Lists                                    *)
(*-----------------------------------------------------*)

(*tList*)

(*list S of length n, list P containing n different numbers between 1 and n 
(that is, P is a permutation of Range[n]). Compute the list T such that for 
all k between 1 and n, T[[k]]=S[[P[[k]]].  

Example: if S={a,b,c,d} and P={3,2,4,1}, then T={c,b,d,a} 
cf. Exercise 4 p. 68 
*)
tList[lis_List,pexpr_List]:= Block[{texpr},
	texpr = Table[lis[[pexpr[[i]]]], {i, 1, Length[lis]}]
	]

(*uList*)

(* For All k between 1 and n, U[[P[[k]]]=S[[k]]. 

Example: S={a,b,c,d} P={3,2,4,1}, U={d,b,a,c} =>T={c,b,d,a} 
Hint: think of it as moving S[[1]] to position P[[1]] and so on.
cf. Exercise 5 p. 68 *)

uList[sE4p68_List,pE4p68_List]:=Transpose[
  Sort[Table[{sE4p68[[i]], pE4p68[[i]]}, {i, 1, 
     Length[sE4p68]}]]][[2]]

(*unCommonList*)

(*Given two lists, find all those elements that are not common 
to the two lists. 
Ex: {a,b,c,d},{a,b,e,f} => {c,d,e,f} 
cf. Exercise 3 p. 70 *)

unCommonList[list1_List,list2_List]:=Join[
	Complement[list1, list2], Complement[list2, list1]
	]

(*OrderedWordQ*)
(*A boolean function that returns True or False depending upon whether
it's argument is in alphabetic order. Cf. Exercise 3. p. 73*)

OrderedWordQ[str_String] := 
  Block[{res, res1}, 
    For[i = 1, i < Length[ToCharacterCode[str]], i++, 
   	If[ToCharacterCode[str][[i]] < ToCharacterCode[str][[i + 1]], 
    res = True, Return[False]]];	
    Return[res]
    ]

(*PalindromQ*)
(*A function that returns True or False depending upon whether it's argument 
(string) is in alphabetic order or not. Cf. Exercise 6. p. 73*)

PalindromeQ[str_String] := StringReverse[str] == str

(*-----------------------------------------------------*)
(*Chapter 4 - Functional programming                   *)
(*-----------------------------------------------------*)