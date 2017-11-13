# BezierCurveApproximation
Mathematica package for Bezier curve approximation.

Using this package, you can reduce control points in plots like this.

```mathematica:
Plot[Sin[t],{t,-5,5}]
```
![](https://i.imgur.com/ScdmxSq.png)

```mathematica:
PlotBezier[Sin[t], {t, {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5}}]
```
![](https://i.imgur.com/aBi6qlh.png)

More info (Japanese):
https://qiita.com/Hyrodium/items/1a81efd11849fb0695d0
