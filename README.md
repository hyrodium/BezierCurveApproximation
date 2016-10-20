# BezierCurveApproximation
Mathematica package for Bezier curve approximation.

## これは何
Mathematicaで描いたグラフ, 例えば
Plot[Sin[t],{t,0,1}]
をsvgやepsで出力したものを拡大すれば, 折れ線で近似されている事が分かる.

このパッケージを使えば, 曲線をBezier曲線で近似し, ノード数を減らす事が可能となる.



## インストール
BezierCurveApproximation.mをダウンロードし, 適切なディレクトリに配置する.
配置場所を知るには
FileNameJoin[{$UserBaseDirectory, "Applications"}
をMathematica上で実行すれば良い.


## 使い方
このパッケージで定義される関数は次の5つである:
- BezierControlPoints
- ShowCurve
- ShowLine
- BezierControlPoints

全ての関数の引数はp, kである.
オプションは存在しない.

## 例



