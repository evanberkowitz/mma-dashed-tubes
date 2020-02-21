(* ::Package:: *)

BeginPackage["dashedTube`"];


dashedTube


Begin["`Private`"];


(* The user can just pass a list:*)
dashedTube[on_, off_, OptionsPattern[Tube]][func_List, radius_: 0.01, each___] := 
    dashedTube[on, off][BSplineFunction[func, SplineDegree -> 1], radius, each]

(* or a BSpline/BezierFunction: *)
dashedTube[on_, off_, OptionsPattern[Tube]][func_, radius_: 0.01,  each___] := 
    Tube[#[[All, 2]], radius, each,
        If[OptionValue[VertexColors] === Automatic, 
           VertexColors -> Automatic, 
           VertexColors -> (Blend[OptionValue[VertexColors], #] & /@ #[[All,1]])]
    ] & /@ Partition[{#, func[#]} & /@ Union@Clip[
  FoldList[Plus, 0, {on, off}[[1 + 
      Mod[Range[0, Ceiling[2/(on + off)]], 2]]]], {0, 1}], 2]


(* ::Section:: *)
(*Wrap Up*)


End[];


EndPackage[];
