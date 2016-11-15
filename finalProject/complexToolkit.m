(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Input::Initialization:: *)
makeImage[pts_,expr_,pltRange1_,PltRange2_]:=Module[{},
{
Graphics[{White, Thick, Line[#&/@pts]}, PlotRange -> {{-pltRange1,pltRange1},{-pltRange1,pltRange1}}, Axes -> True, Background -> GrayLevel[.6], ImageSize -> {300,300}, AxesLabel -> {Style["x",Italic], Style["y",Italic]},ImagePadding->20],

Graphics[{White, Thick, Line[{Re[expr /. z -> #[[1]] + I #[[2]]], Im[expr /. z -> #[[1]] + I #[[2]]]}& /@ #&/@pts]}, PlotRange -> {{-PltRange2,PltRange2},{-PltRange2,PltRange2}}, Axes -> True, Background -> RGBColor[.7, .5, .5], ImageSize -> {300,300}, AxesLabel -> {Style["u",Italic], Style["v",Italic]},ImagePadding->20]
}
]


makeCirclePoints[smallRadius_,largeRadius_,stepSize_]:=Module[
{ang, lists, pts},
ang=Range[0Pi,2Pi,.001];
lists=Table[{r Cos[ang],r Sin[ang]},{r,Range[smallRadius,largeRadius,stepSize]}];
pts=Transpose[#]&/@lists;
Return[pts]
]


