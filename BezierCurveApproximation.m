(* ::Package:: *)

BeginPackage["BezierCurveApproximation`"]

BezierControlPoints::usage = "Returns control points.";
ShowCurve::usage = "Returns approximated Bezier curve.";

Begin["`Private`"]

BezierControlPoints[p_,knots_]:=Module[
		{knot,t0,t1,p0,p1,Dp0,Dp1,cos,k,pts},
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

ShowCurve[p_,knots_]:=Module[
		{pts},
		pts=BezierControlPoints[p,knots];
		Which[
			Length[p[knots[[1]]]]==2,
			Graphics[{BezierCurve[pts],Green,Line[pts],Red,Point[pts]}],
			Length[p[knots[[1]]]]==3,
			Graphics3D[{BezierCurve[pts],Green,Line[pts],Red,Point[pts]}]
		]
	]

End[]
EndPackage[]




