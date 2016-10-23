(* ::Package:: *)

BeginPackage["BezierCurveApproximation`"]

(*BezierControlPoints::usage = "Return coordinates of control points.";*)
(*BezierDRAW::usage = "Draw Bezier curve.";*)
PlotBezier::usage = "Plot Bezier curve in 2D.";
Plot3DBezier::usage = "Plot Bezier curve in 3D.";
ParametricPlotBezier::usage = "Parametric Plot Bezier curve in 2D.";
ParametricPlot3DBezier::usage = "Parametric Plot Bezier curve in 3D.";

(*
ShowCurve::usage = "Option for showing Bezier curve.";
ShowLine::usage = "Option for showing polygonal line which connects control points.";
ShowPoints::usage = "Option for showing control points.";
ShowKnots::usage = "Option for showing knots of piecewise Bezier curve.";
*)

BoundingBox::usage = "Option for showing bounding box.";
LngLat::usage = "Option for setging longtitude and latitude.";

Begin["`Private`"]

Options[BezierDRAW]={ShowCurve->True,ShowLine->False,ShowPoints->False,ShowKnots->False}
Options[PlotBezier]={BoundingBox->{}}
Options[Plot3DBezier]={BoundingBox->{},Mesh->{10,10},LngLat->{\[Pi]/6,\[Pi]/6}}
Options[ParametricPlotBezier]={BoundingBox->{},Mesh->{10,10}}
Options[ParametricPlot3DBezier]={BoundingBox->{},Mesh->{10,10},LngLat->{\[Pi]/6,\[Pi]/6}}

proj[x_,Ln_,La_]:=({{0, 1, 0},{0, 0, 1}}).RotationMatrix[La,{0,1,0}].RotationMatrix[-Ln,{0,0,1}].x;

box[{}]=Graphics[];
box[{a_,b_}]:=Graphics[Line[{a,{a[[1]],b[[2]]},b,{b[[1]],a[[2]]},a}]];
box[{o_,a_,b_,c_}]:=Graphics[{Line[{o,a}],Line[{o,b}],Line[{o,c}],Line[{a,a+b-o}],Line[{a,a+c-o}],Line[{b,b+c-o}],Line[{b,b+a-o}],Line[{c,c+a-o}],Line[{c,c+b-o}],Line[{a+b+c-2o,a+b-o}],Line[{a+b+c-2o,b+c-o}],Line[{a+b+c-2o,c+a-o}]}];
box[f_,{}]:=box[{}];
box[f_,{a_,b_}]:=box[{f[a],f[a{0,1,1}+b{1,0,0}],f[a{1,0,1}+b{0,1,0}],f[a{1,1,0}+b{0,0,1}]}];

BezierControlPoints[p_,knots_]:=Module[
		{knot,t0,t1,p0,p1,Dp0,Dp1,k},
		knot=DeleteDuplicates[Sort[knots,Less]];
		Prepend[Apply[Join,Riffle[
			Table[
				t0=knot[[i]];
				t1=knot[[i+1]];

				p0=Limit[p[x],x->t0,Direction->-1];
				p1=Limit[p[x],x->t1,Direction->1];
				Dp0=Limit[D[p[x],x],x->t0,Direction->-1];
				Dp1=Limit[D[p[x],x],x->t1,Direction->1];

				Which[Dp0-Dp1=={0,0},{(2p0+p1)/3,(p0+2p1)/3,p1},
					True,k=-(4/3) (Dp0-Dp1).(p0+p1-2p[(t0+t1)/2])/(Dp0-Dp1).(Dp0-Dp1);{p0+k Dp0,p1-k Dp1,p1}],
				{i,Length[knot]-1}
			],
			Table[
				p0=Limit[p[x],x->knot[[i]],Direction->1];
				p1=Limit[p[x],x->knot[[i]],Direction->-1];
				k=Limit[p[knot[[i]]+h]-p[knot[[i]]-h],h->0];
				If[PossibleZeroQ[k.k],{},{(2p0+p1)/3,(p0+2p1)/3,p1}],
				{i,2,Length[knot]-1}
			]
		]],Limit[p[x],x->knot[[1]],Direction->-1]]
	]

BezierDRAW[p_,{t_,knots_},OptionsPattern[]]:=Module[
		{pn,pts,grp},
		pn[s_]:=p/.t->s;
		grp={};
		pts=BezierControlPoints[pn[#]&,knots];
		If[OptionValue[ShowCurve],grp=Join[grp,{BezierCurve[pts]}];];
		If[OptionValue[ShowLine],grp=Join[grp,{Line[pts]}];];
		If[OptionValue[ShowPoints],grp=Join[grp,{Point[pts]}];];
		If[OptionValue[ShowKnots],grp=Join[grp,{Point[Part[pts,Table[3i+1,{i,0,(Length[pts]-1)/3}]]]}];];
		Graphics[grp]
	]

ParametricPlotBezier[p_,{t_,knots_},OptionsPattern[]]:=Module[
		{pn,n},
		pn[s_]:=Partition[Flatten[{p}],2]/.t->s;
		n=Length[Partition[Flatten[{p}],2]];
		Show[Table[BezierDRAW[pn[s][[i]],{s,knots}],{i,n}],box[OptionValue[BoundingBox]]]
	];

ParametricPlotBezier[p_,{u_,uknots_},{v_,vknots_},OptionsPattern[]]:=Module[
		{pn,umin,umax,vmin,vmax},
		umin=Min[uknots];
		umax=Max[uknots];
		vmin=Min[vknots];
		vmax=Max[vknots];
		pn[s_,t_]:=Partition[Flatten[{p}],2]/.u->s /.v->t;
		Show[
			ParametricPlotBezier[Table[pn[umin+i (umax-umin)/OptionValue[Mesh][[1]],t],{i,0,OptionValue[Mesh][[1]]}],{t,vknots}],
			ParametricPlotBezier[Table[pn[s,vmin+i (vmax-vmin)/OptionValue[Mesh][[2]]],{i,0,OptionValue[Mesh][[2]]}],{s,uknots}],
			box[OptionValue[BoundingBox]]
		]
	]

PlotBezier[f_,{x_,knots_},OptionsPattern[]]:=Module[
		{pn},
		pn[t_]:=Map[{t,#}&,Flatten[{f/.x->t}]];
		Show[ParametricPlotBezier[pn[t],{t,knots}],box[OptionValue[BoundingBox]]]
	]

ParametricPlot3DBezier[p_,{t_,knots_},OptionsPattern[]]:=Module[
		{pn,pm},
		pm[s_]:=Partition[Flatten[{p}],3]/.t->s;
		pn[s_]:=Map[proj[#,OptionValue[LngLat][[1]],OptionValue[LngLat][[2]]]&,pm[s]];
		Show[ParametricPlotBezier[pn[s],{s,knots}],box[proj[#,OptionValue[LngLat][[1]],OptionValue[LngLat][[2]]]&,OptionValue[BoundingBox]]]
	]

ParametricPlot3DBezier[p_,{u_,uknots_},{v_,vknots_},OptionsPattern[]]:=Module[
		{pn,pm},
		pm[s_,t_]:=Partition[Flatten[{p}],3]/.u->s /.v->t;
		(*pn[s_,t_]=Simplify[Map[f[#,0,0]&,pm[s,t]]];*)
		pn[s_,t_]:=Map[proj[#,OptionValue[LngLat][[1]],OptionValue[LngLat][[2]]]&,pm[s,t]];
		(*ParametricPlotBezier[pn[s,t],{s,uknots},{t,vknots},Mesh->OptionValue[Mesh]]*)
		Show[ParametricPlotBezier[pn[s,t],{s,uknots},{t,vknots},Mesh->OptionValue[Mesh]],box[proj[#,OptionValue[LngLat][[1]],OptionValue[LngLat][[2]]]&,OptionValue[BoundingBox]]]
	]

Plot3DBezier[f_,{x_,xknots_},{y_,yknots_},OptionsPattern[]]:=Module[
		{pn},
		pn[s_,t_]:=Map[{s,t,#}&,Flatten[{f/.x->s /.y->t}]];
		ParametricPlot3DBezier[pn[s,t],{s,xknots},{t,yknots},Mesh->OptionValue[Mesh],LngLat->OptionValue[LngLat],BoundingBox->OptionValue[BoundingBox]]
	]

Protect[PlotBezier, ParametricPlotBezier, Plot3DBezier, ParametricPlot3DBezier]

End[]
EndPackage[]




