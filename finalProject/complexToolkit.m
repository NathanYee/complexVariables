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
fSpace[min_,max_,numPts_,f_: Log]:=Module[{},N[InverseFunction[f]/@Range[f@min,f@max,(f@max-f@min)/(numPts-1)]]]
fSpace::usage="fSpace[min, max, steps, Log] gives inverse f (default Log) spaced points from min to max over a given number of points";


(* ::Input::Initialization:: *)
makeRiemannRealAxis[numPts_]:=Module[
{angles, pts},
angles=N@Range[0Pi,2Pi,(2 Pi - 0Pi)/(numPts - 1)];
pts=Table[{Cos[ang],0,Sin[ang]},{ang,angles}];
Return[pts]
]
makeRiemannRealAxis::usage="makeRiemannRealAxis[numPts_] makeRiemannRealAxis returns equally spaced points around the positive real axis on the Riemann Sphere";


riemannPointToComplexPlane[{X_,Y_,Z_}]:=Module[{sol,pair},
sol=Solve[{x+I y==(X+I Y)/(1-Z)},{x\[Element]Reals,y\[Element]Reals}];
pair=({x,y}/.sol)[[1]];
Return[pair]
]
riemannPointToComplexPlane::usage="riemannPointToComplexPlane[{X_,Y_,Z_}] riemannPointToComplexPlane returns the inverse stereographic projection of a point distance 1 from the origin in 3D space";


(* ::Input::Initialization:: *)
makeSphereSpacedPoints[numPts_]:=Module[{pts,complexPts,realPts},
pts=makeRiemannRealAxis[numPts];
complexPts=riemannPointToComplexPlane[#]&/@pts;
realPts=complexPts[[All,1]];
Return[realPts]
]
makeSphereSpacedPoints::usage="makeSphereSpacedPoints[numPts_] makeSphereSpacedPoints returns the inverse stereographic projection of points around the positive real axis on the Riemann Sphere. It gets the positive real axis of the Riemann Sphere by calling makeRiemannRealAxis. It finds the inverse stereographic projection of the points by mapping riemannPointToComplexPlane onto each point";


makeCirclePoints[smallRadius_,largeRadius_,numCircles_,ptsPerCircle_]:=Module[{ang, lists, pts},
ang=Range[0 Pi,2 Pi,(2\[Pi]-0)/(ptsPerCircle-1)];
lists=Table[{r Cos[ang],r Sin[ang]},{r,Range[smallRadius,largeRadius,(largeRadius-smallRadius)/(numCircles-1)]}];
pts=Transpose[#]&/@lists;
Return[pts]
]
makeCirclePoints::usage="makeCirclePoints[smallRadius_,largeRadius_,numCircles_,ptsPerCircle_] makeCirclePoints returns expanding circles given smallest radius, largest radius, step size, and number of points per circle. The constant step size dictates that the radius of the circles increases by a constant step size per circle";


(* ::Input::Initialization:: *)
makeExponentialSpacedCirclePoints[smallRadius_,largeRadius_,numCircles_,ptsPerCircle_]:=Module[{ang, lists, pts},
ang=Range[0 Pi,2 Pi,(2\[Pi]-0)/(ptsPerCircle-1)];
lists=Table[{r Cos[ang],r Sin[ang]},{r,fSpace[smallRadius,largeRadius,numCircles]}];
pts=Transpose[#]&/@lists;
Return[pts]
]
makeExponentialSpacedCirclePoints::usage="makeExponentialSpacedCirclePoints[smallRadius_,largeRadius_,numCircles_,ptsPerCircle_] makeExponentialSpacedCirclePoints returns expanding circles given smallest radius, largest radius, number of circles, and number of points per circle. The number of circles dictates that the circles be spaced exponentially within the largestRadius - smallestRadius range";


(* ::Input::Initialization:: *)
makeSphereSpacedCirclePoints[numCircles_,ptsPerCircle_]:=Module[{ang, lists, pts},
ang=Range[0 Pi,2 Pi,(2\[Pi]-0)/(ptsPerCircle-1)];
lists=Table[{r Cos[ang],r Sin[ang]},{r,makeSphereSpacedPoints[numCircles]}];
pts=Transpose[#]&/@lists;
Return[pts]
]
makeSphereSpacedCirclePoints::usage="makeSphereSpacedCirclePoints[numCircles_,ptsPerCircle_] makeSphereSpacedCirclePoints returns circles based on inverse stereographic projection given a number of circles, and the number of points per circle";


(* ::Input::Initialization:: *)
getImagePts[expr_,pts_]:=Module[{imgPts},
imgPts={Re[expr /. z -> #[[1]] + I #[[2]]], Im[expr /. z -> #[[1]] + I #[[2]]]}&/@#&/@pts;
Return[imgPts]]
getImagePts::usage="getImagePts[expr[z],pts] returns the image points given takes in an expression of z and a list of lists of lists of points";


(* ::Input::Initialization:: *)
plotImage[pts_,expr_,pltRange1_: Automatic,PltRange2_: Automatic,colors_: 1]:=Module[{},
If[colors==1,colors=RGBColor[1,1,1],colors=colors];
{
Graphics[MapThread[{#1,Thick,Line[#2]}&,{colors,pts}],PlotRange->pltRange1,Axes->True,Background->White,ImageSize->{300,300},AxesLabel->{Style["x",Italic],Style["y",Italic]},ImagePadding->20],Graphics[MapThread[{#1,Thick,Line[{Re[expr/.z->#[[1]]+I #[[2]]],Im[expr/.z->#[[1]]+I #[[2]]]}&/@#2]}&,{colors,pts}],PlotRange->PltRange2,Axes->True,Background->White,ImageSize->{300,300},AxesLabel->{Style["u",Italic],Style["v",Italic]},ImagePadding->20]
}
]
plotImage::usage="plotImage[pts,expr,pltRange1:Automatic,PltRange2:Automatic,colors:1] returns a list containing the complex and image plots given a list of lists of lists of points, an expression of z, two plot ranges (can be Automatic), and a list of colors. Note that plotImage doesn't call getImagePts so we can MapThread with color gradients";


(* ::Input::Initialization:: *)
makeVerticalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[{horiPts},
horiPts=Table[Table[{x,y},{y,Range[minY,maxY,(maxY-minY)/(ptsPerLine-1)]}],{x,minX,maxX,(maxX-minX)/(numLines-1)}];
Return[horiPts]
]
makeVerticalPts::usage="makeVerticalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of vertical lines of equally spaced points starting from minX to maxX with length maxY-minY with ptsPerLine number of points per line";


(* ::Input::Initialization:: *)
makeHorizontalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[{horiPts},
horiPts=Table[Table[{x,y},{x,Range[minX,maxX,(maxX-minX)/(ptsPerLine-1)]}],{y,minY,maxY,(maxY-minY)/(numLines-1)}];
Return[horiPts]
]
makeHorizontalPts::usage="makeHorizontalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of horizontal lines of equally spaced points starting from minY to maxY with length maxX-minX with ptsPerLine number of points per line";


makeGridPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[{vertPts,horiPts,pts},
vertPts=makeVerticalPts[minY,maxY,minX,maxX,ptsPerLine,numLines];
horiPts=makeHorizontalPts[minX,maxX,minY,maxY,ptsPerLine,numLines];
pts=Join[horiPts,vertPts];
Return[pts]
]
makeGridPts::usage="makeGridPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of vertical of equally spaced points lines starting from minX to maxX with length maxY-minY with ptsPerLine number of points per line and 
returns numLines number of horizontal of equally spaced points lines starting from minY to maxY with length maxX-minX with ptsPerLine number of points per line.
NOTE: enter args as if doing horizontal line";


(* ::Input::Initialization:: *)
makeLogVerticalPts2[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[{vertPts},
vertPts=Table[Table[{x,y},{y,fSpace[minY,maxY,ptsPerLine]}],{x,minX,maxX,(maxX-minX)/(numLines-1)}];
Return[Re[vertPts]]
]
makeLogVerticalPts2::usage="makeLogVerticalPts2[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of vertical lines of exponentially spaced points starting from minX to maxX with length maxY-minY with ptsPerLine number of points per line.
NOTE: makeLogVerticalPts2 is depreciated due to not handling negative minY to positive maxY ranges";


makeLogHorizontalPts2[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[{horiPts},
horiPts=Table[Table[{x,y},{x,fSpace[minX,maxX,ptsPerLine]}],{y,minY,maxY,(maxY-minY)/(numLines-1)}];
Return[Re[horiPts]]
]
makeLogHorizontalPts2::usage="makeLogHorizontalPts2[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of horizontal lines of exponentially spaced points starting from minY to maxY with length maxX-minX with ptsPerLine number of points per line
NOTE: makeLogHorizontalPts2 is depreciated due to not handling negative minX to positive maxX ranges";


(* ::Input::Initialization:: *)
makeLogVerticalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[
{posVertPts,negVertPts,pts},
posVertPts=Table[Table[{x,y},{y,fSpace[.01,maxY,Floor[ptsPerLine/2]]}],{x,minX,maxX,(maxX-minX)/(numLines-1)}];
negVertPts=Table[Table[{x,y},{y,fSpace[minY,-.01,Floor[ptsPerLine/2]]}],{x,minX,maxX,(maxX-minX)/(numLines-1)}];
pts=MapThread[Join,{negVertPts,posVertPts}];
Return[Re[pts]]
]
makeLogVerticalPts::usage="makeLogVerticalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of vertical lines of exponentially spaced points starting from minX to maxX with length maxY-minY with ptsPerLine number of points per line.";


(* ::Input::Initialization:: *)
makeLogHorizontalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[
{posHoriPts,negHoriPts,pts},
posHoriPts=Table[Table[{x,y},{x,fSpace[.01,maxX,Floor[ptsPerLine/2]]}],{y,minY,maxY,(maxY-minY)/(numLines-1)}];
negHoriPts=Table[Table[{x,y},{x,fSpace[minX,-.01,Floor[ptsPerLine/2]]}],{y,minY,maxY,(maxY-minY)/(numLines-1)}];
pts=MapThread[Join,{negHoriPts,posHoriPts}];
Return[Re[pts]]
]
makeLogHorizontalPts::usage="makeLogHorizontalPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of horizontal lines of exponentially spaced points starting from minY to maxY with length maxX-minX with ptsPerLine number of points per line";


makeLogGridPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_]:=Module[
{vertLogPts,horiLogPts,pts},
vertLogPts=makeLogVerticalPts[minY,maxY,minX,maxX,ptsPerLine,numLines];
horiLogPts=makeLogHorizontalPts[minX,maxX,minY,maxY,ptsPerLine,numLines];
pts=Join[horiLogPts,vertLogPts];
Return[pts]
]
makeLogGridPts::usage="makeLogGridPts[minX_,maxX_,minY_,maxY_,ptsPerLine_,numLines_] 
returns numLines number of vertical of exponentially spaced points lines starting from minX to maxX 
with length maxY-minY with ptsPerLine number of points per line and 
returns numLines number of horizontal of exponentially spaced points lines starting from minY to maxY 
with length maxX-minX with ptsPerLine number of points per line.
NOTE: enter args as if doing horizontal line";


(* ::Code::Initialization:: *)
makeSphereVerticalLines[min_,max_,ptsPerLine_,numLines_]:=Module[{horiPts,spherePts},
spherePts=makeSphereSpacedPoints[ptsPerLine];
horiPts=Table[Table[{x,y},{y,spherePts}],{x,min,max,(max-min)/(numLines-1)}];
Return[horiPts]
]
makeSphereVerticalLines::usage=
	"makeSphereVerticalLines[min_,max_,ptsPerLine_,numLines_] makes vertical lines of 
	sphere spaced points given line bounds of min, max, ptsPerLine, and numLines";


(* ::Input::Initialization:: *)
makeSphereHorizontalLines[min_,max_,ptsPerLine_,numLines_]:=Module[{horiPts,spherePts},
spherePts=makeSphereSpacedPoints[ptsPerLine];
horiPts=Table[Table[{x,y},{x,spherePts}],{y,min,max,(max-min)/(numLines-1)}];
Return[horiPts]
]
makeSphereHorizontalLines::usage="makeSphereHorizontalLines[min_,max_,ptsPerLine_,numLines_] makes horizontal lines of sphere spaced points given line bounds of min, max, ptsPerLine, and numLines";


(* ::Input::Initialization:: *)
makeSphereGrid[min_,max_,ptsPerLine_,numLines_]:=Module[{vertPts,horiPts,pts},
vertPts=makeSphereVerticalLines[min,max,ptsPerLine,numLines];
horiPts=makeSphereHorizontalLines[min,max,ptsPerLine,numLines];
pts=Join[horiPts,vertPts];
Return[pts]
]
makeSphereGrid::usage="makeSphereGrid[min,max,ptsPerLine,numLines] makes a grid of sphere spaced points given line bounds of min, max, ptsPerLine, and numLines";


makeRandomColors[pts_]:=Module[{colors},
colors=RandomColor[Length[pts]];
Return[colors]]
makeRandomColors::usage="makeRandomColors[pts_] returns length of pts number of random RBG colors";


(* ::Input::Initialization:: *)
makeColorGradient[pts_]:=Module[{colors},
colors=Table[RGBColor[.5,x,.7],{x,.1,.9,(.9-.1)/(Length[pts]-1)}];
Return[colors]]
makeColorGradient::usage="makeColorGradient[pts_] returns length of pts number of colors in a purple -> teal gradient";


complexTo3D[point_]:=Module[{x,y,z,mix,X,Y,Z},
x=point[[1]];
y=point[[2]];
mix=(2x+I 2y)/(1+x^2+y^2);
X=Re[mix];
Y=Im[mix];
z=x+I y;
Z=(Abs[z]^2-1)/(Abs[z]^2+1);
Return[{X,Y,Z}]
]
complexTo3D::usage="complexTo3D[point_] returns the stereographic projection of a point on the complex plane";


complexPtsTo3D[points_]:=Module[{spherePts3D},
spherePts3D=complexTo3D[#]&/@#&/@points;
Return[spherePts3D]
]
complexPtsTo3D::usage="complexPtsTo3D[points_] returns the stereographic projection of a list
of points on the complex plane";


takingPts[takes_,pts_]:=Module[{newPts},
newPts=Take[pts,#]&/@takes;
Return[newPts]]
takingPts::usage="takingPts[takes_,pts_] returns a list of taken points 
given a list of takes and a list of points";


cylinderPts[pts_]:=Module[{takes,returnPts},
takes=Table[{x,x+1},{x,1,Dimensions[pts][[2]]-1}];
returnPts=takingPts[takes,#]&/@pts
]
cylinderPts::usage="cylinderPts[pts_] returns a list of taken points ready to";


(* ::Input::Initialization:: *)
cylinders[pts_,radius_]:=Module[{},
Return[Cylinder[#,radius]&/@#&/@cylinderPts[pts]]]


(* ::Input::Initialization:: *)
tubes[pts_,radius_]:=Module[{},Tube[#,radius]&/@complexPtsTo3D[pts]]


(* ::Input::Initialization:: *)
makeNewtonMethodAnimation[plotRange_,map_,depth_,points_]:=Module[{localPts},
localPts=points;
plots=Flatten[{
{ListPlot[localPts,PlotRange->{{-plotRange,plotRange}, {-plotRange,plotRange}},PlotStyle->PointSize[.02],AspectRatio->1,ImageSize->Large]},
Table[
(*Apply map to grid pts*) localPts=getImagePts[map,localPts];
ListPlot[localPts,PlotRange->{{-plotRange,plotRange}, {-plotRange,plotRange}},PlotStyle->PointSize[.02],AspectRatio->1,ImageSize->Large]
,{n,1,depth}]
}];
Return[Manipulate[plots[[n]],{n,1,Length[plots],1}]]]
makeNewtonMethodAnimation::usage="makeNewtonMethodAnimation[plotRange,map,depth,points] makes animation of successive mappings";



