(* ::Package:: *)

BeginPackage["BezierCurveApproximation`"]

BezierControlPoints::usage = "Returns coordinates of control points.";
PlotBezier::usage = "Plot Bezier curve.";
ParametricPlotBezier::usage = "Parametric Plot Bezier curve.";
ShowCurve::usage = "Option for showing Bezier curve.";
ShowLine::usage = "Option for showing polygonal line which connects control points.";
ShowPoints::usage = "Option for showing control points.";
ShowKnots::usage = "Option for showing knots of piecewise Bezier curve.";

Begin["`Private`"]

BezierControlPoints[p_,knots_]:=Module[
		{knot,t0,t1,p0,p1,Dp0,Dp1,cos,k,pts,i},
		knot=Union[knots];
		t1=knot[[1]];
		p1=Limit[p[x],x->t1,Direction->-1];
		pts={p1};

		Do[t0=knot[[i]];
			t1=knot[[i+1]];

			p0=Limit[p[x],x->t0,Direction->-1]; If[p0!=p1,pts=Join[pts,{(2p1+p0)/3,(p1+2p0)/3,p0}]];
			p1=Limit[p[x],x->t1,Direction->1];
			Dp0=Limit[D[p[x],x]/Norm[D[p[x],x]],x->t0,Direction->-1];
			Dp1=Limit[D[p[x],x]/Norm[D[p[x],x]],x->t1,Direction->1];
			cos=Dp0.Dp1;

			Which[Det[({{1, cos},{cos, 1}})]==0, pts=Join[pts,{(2p0+p1)/3,(p0+2p1)/3,p1}];,
				True, k=4/3 Inverse[({{1, cos},{cos, 1}})].{Dp0,Dp1}.(p0+p1-2p[(t0+t1)/2]);
				pts=Join[pts,{p0-Dp0 k[[1]],p1-Dp1 k[[2]],p1}];
			];,
			{i,Length[knot]-1}
		];
		pts
	]

Options[PlotBezier]={ShowCurve->True,ShowLine->False,ShowPoints->False,ShowKnots->False}
Options[ParametricPlotBezier]={ShowCurve->True,ShowLine->False,ShowPoints->False,ShowKnots->False}

PlotBezier[f_,{t_,knots_},OptionsPattern[]]:=Module[
		{fn,pts,grp,i},
		fn[s_]:=Flatten[{f}]/.t->s;
		grp={};
		Do[
			pts=BezierControlPoints[{#,fn[#][[i]]}&,knots];
			If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
			If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
			If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
			If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
			{i,Length[Flatten[{f}]]}
		];
		Graphics[{grp}]
	]

ParametricPlotBezier[p_,{t_,knots_},OptionsPattern[]]:=Module[
		{pn,pts,grp,i},
		pn[s_]:=Partition[Flatten[{p}],2]/.t->s;
		grp={};
		Do[
			pts=BezierControlPoints[{pn[#][[i]][[1]],pn[#][[i]][[2]]}&,knots];
			If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
			If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
			If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
			If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
			{i,Length[pn[0]]}
		];
		Graphics[{grp}]
	]

(*Protect[BezierControlPoints, PlotBezier, ParametricPlotBezier, ShowCurve, ShowLine, ShowPoints, ShowKnots]*)

End[]
EndPackage[]









