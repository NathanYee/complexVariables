(* ::Package:: *)

(* ::Chapter:: *)
(*This code was adapted from Eric Miller, HALtheWise*)


(* ::Chapter:: *)
(*General Mathematica helpers*)


exportNotebookPDF[]:=
Module[{name},
name=("FileName"/.NotebookInformation[])[[2]];
name=StringReplace[name,".nb"->".pdf"];
Export[FileNameJoin[{NotebookDirectory[],name}],EvaluationNotebook[]]
]


(* ::Chapter:: *)
(*Frequency Analysis*)


(* ::Input::Initialization:: *)
fftShift[data_]:=Module[{},RotateRight[Fourier[data],Floor[Length[data]/2]]]
plotFFT[data_]:=
Module[{fft,N},
fft = fftShift[data];
N = Length[data];
Rasterize@ListLinePlot[Transpose@{Subdivide[-Pi,Pi,N-1],Abs@fft},PlotRange->Full,ImageSize->Large,AxesLabel->{"Frequency (rad/sample)","Amplitude"}]
]


(* ::Title:: *)
(*Misc*)


(* ::Input::Initialization:: *)
playSound[data_]:=Module[{path=StringReplace["/tmp/sound{rand}.wav","{rand}"-> ToString@RandomInteger[999]]},Export[path,data];SystemOpen[path]]
playSound[data_,fs_]:=playSound[Sound@SampledSoundList[data,fs]]


(* ::Subsection:: *)
(*Template cell*)


makeTemplate[]:=makeTemplate["Template"]


makeTemplate[name_] :=
Module[{namespace},
namespace = ToLowerCase@StringReplace[name,Whitespace->""];
makeTemplate[name, namespace]
]


makeTemplate[name_, namespace_]:=
Module[{cell, replacerules},
replacerules = <| "Template"->name, 
	"\"\<xxx\>\""->StringReplace["\"\<xxx`\>\"","xxx"->namespace] |>;


cell=
Cell[CellGroupData[{Cell["Template", "Section"],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{
  RowBox[{"With", "[", 
   RowBox[{
    RowBox[{"{", 
     RowBox[{"context", "=", "\"\<xxx\>\""}], "}"}], ",", 
    RowBox[{"If", "[", 
     RowBox[{
      RowBox[{
       RowBox[{"Context", "[", "]"}], " ", "\[NotEqual]", " ", 
       "context"}], ",", 
      RowBox[{"Begin", "[", "context", "]"}]}], "]"}]}], "]"}], ";", 
  RowBox[{"Dynamic", "[", 
   RowBox[{"Refresh", "[", 
    RowBox[{
     RowBox[{"Context", "[", "]"}], ",", 
     RowBox[{"UpdateInterval", "\[Rule]", "1"}]}], "]"}], 
   "]"}]}]], "Input"],

Cell[BoxData[
 DynamicBox[ToBoxes[
   Refresh[
    Context[], UpdateInterval -> 1], StandardForm],
  ImageSizeCache->{57., {0., 12.}}]], "Output"]
}, Open  ]],

Cell[BoxData[""], "Input"],

Cell[BoxData[""], "Input"],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{"With", "[", 
  RowBox[{
   RowBox[{"{", 
    RowBox[{"context", "=", "\"\<xxx\>\""}], "}"}], ",", 
   RowBox[{"If", "[", 
    RowBox[{
     RowBox[{
      RowBox[{"Context", "[", "]"}], "\[Equal]", "context"}], ",", 
     RowBox[{"End", "[", "]"}], ",", "\"\<Not in context\>\""}], 
    "]"}]}], "]"}]], "Input"],

Cell[BoxData["\<\"Not in context\"\>"], "Output"]
}, Open  ]]
}, Open  ]];

CellPrint[cell/.replacerules]
]
