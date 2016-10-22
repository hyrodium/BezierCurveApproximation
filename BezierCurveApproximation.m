(* ::Package:: *)

BeginPackage["BezierCurveApproximation`"]

BezierControlPoints::usage = "Returns coordinates of control points.";
PlotBezier::usage = "Plot Bezier curve.";
ParametricPlotBezier::usage = "Parametric Plot Bezier curve.";
Plot3DBezier::usage = "Plot Bezier curve.";
ParametricPlot3DBezier::usage = "Parametric Plot Bezier curve.";
ParametricPlot3DBezierA::usage = "Parametric Plot Bezier curve.";

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
Options[Plot3DBezier]={ShowCurve->True,ShowLine->False,ShowPoints->False,ShowKnots->False}
Options[ParametricPlot3DBezier]={ShowCurve->True,ShowLine->False,ShowPoints->False,ShowKnots->False}

PlotBezier[f_,{x_,knots_},OptionsPattern[]]:=Module[
		{fn,pts,grp,i},
		fn[s_]:=Flatten[{f}]/.x->s;
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

ParametricPlotBezier[p_,{u_,uknots_},{v_,vknots_},n_:{10,10},OptionsPattern[]]:=Module[
		{pn,pts,grp,i,j,w,umax,umin,vmax,vmin},
		umax=Max[uknots];
		umin=Min[uknots];
		vmax=Max[vknots];
		vmin=Min[vknots];
		pn[s_,t_]:=Partition[Flatten[{p}],2]/.u->s /.v->t;
		grp={};
		Do[
			w=vmin+j (vmax-vmin)/n[[2]];
			Do[
				pts=BezierControlPoints[{pn[#,w][[i]][[1]],pn[#,w][[i]][[2]]}&,uknots];
				If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
				If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
				If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
				If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
				{i,Length[pn[0,0]]}
			];,{j,0,n[[2]]}
		];
		Do[
			w=umin+j (umax-umin)/n[[1]];
			Do[
				pts=BezierControlPoints[{pn[w,#][[i]][[1]],pn[w,#][[i]][[2]]}&,vknots];
				If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
				If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
				If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
				If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
				{i,Length[pn[0,0]]}
			];,{j,0,n[[1]]}
		];
		Graphics[{grp}]
	]

ParametricPlot3DBezier[p_,{u_,uknots_},{v_,vknots_},n_:{10,10},OptionsPattern[]]:=Module[
		{pn,pts,grp,i,j,w,umax,umin,vmax,vmin},
		umax=Max[uknots];
		umin=Min[uknots];
		vmax=Max[vknots];
		vmin=Min[vknots];
		pn[s_,t_]:=Partition[Flatten[{p}],3]/.u->s /.v->t;
		grp={};
		Do[
			w=vmin+j (vmax-vmin)/n[[2]];
			Do[
				pts=BezierControlPoints[pn[#,w][[i]]&,uknots];
				If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
				If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
				If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
				If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
				{i,Length[pn[0,0]]}
			];,{j,0,n[[2]]}
		];
		Do[
			w=umin+j (umax-umin)/n[[1]];
			Do[
				pts=BezierControlPoints[pn[w,#][[i]]&,vknots];
				If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
				If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
				If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
				If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
				{i,Length[pn[0,0]]}
			];,{j,0,n[[1]]}
		];
		Graphics3D[{grp}]
	]

Plot3DBezier[f_,{x_,xknots_},{y_,yknots_},n_:{10,10},OptionsPattern[]]:=Module[
		{fn,pts,grp,i,j,w,xmax,xmin,ymax,ymin},
		xmax=Max[xknots];
		xmin=Min[xknots];
		ymax=Max[yknots];
		ymin=Min[yknots];
		fn[s_,t_]:=Flatten[{f}]/.x->s /.y->t;
		grp={};
		Do[
			w=ymin+j (ymax-ymin)/n[[2]];
			Do[
				pts=BezierControlPoints[{#,w,fn[#,w][[i]]}&,xknots];
				If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
				If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
				If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
				If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
				{i,Length[fn[0,0]]}
			];,{j,0,n[[2]]}
		];
		Do[
			w=xmin+j (xmax-xmin)/n[[1]];
			Do[
				pts=BezierControlPoints[{w,#,fn[w,#][[i]]}&,yknots];
				If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
				If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
				If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
				If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];,
				{i,Length[fn[0,0]]}
			];,{j,0,n[[1]]}
		];
		Graphics3D[{grp}]
	]

Lng=-(\[Pi]/4);
Lat=\[Pi]/4;

f[x_]:=({
 {0, 1, 0},
 {0, 0, 1}
}).RotationMatrix[Lat,{0,1,0}].RotationMatrix[-Lng,{0,0,1}].x;

ParametricPlot3DBezierA[p_,{u_,uknots_},{v_,vknots_},n_:{10,10}]:=Module[
		{pm,pn,pts,grp,i,j,w,umax,umin,vmax,vmin},
		umax=Max[uknots];
		umin=Min[uknots];
		vmax=Max[vknots];
		vmin=Min[vknots];
		pm[s_,t_]:=Partition[Flatten[{p}],3]/.u->s /.v->t;
		pn[s_,t_]=Map[f,pm[s,t]];
		Print["HOGE"];
		grp={};
		Do[
			w=vmin+j (vmax-vmin)/n[[2]];
			Print["fuga1"];
			Do[
				pts=BezierControlPoints[pn[#,w][[i]]&,uknots];
				grp=Join[grp,{BezierCurve[pts]}];,
				{i,Length[pn[0,0]]}
			];,{j,0,n[[2]]}
		];
		Do[
			w=umin+j (umax-umin)/n[[1]];
			Print["fuga2"];
			Do[
				pts=BezierControlPoints[pn[w,#][[i]]&,vknots];
				grp=Join[grp,{BezierCurve[pts]}];,
				{i,Length[pn[0,0]]}
			];,{j,0,n[[1]]}
		];
		Graphics[{grp}]
	]

(*Protect[BezierControlPoints, PlotBezier, ParametricPlotBezier, ShowCurve, ShowLine, ShowPoints, ShowKnots]*)

End[]
EndPackage[]












