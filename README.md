# BezierCurveApproximation
Mathematica package for Bezier curve approximation.

## これは何
Mathematicaで描いたグラフ, 例えば
Plot[Sin[t],{t,0,1}]
をsvgやepsで出力したものを拡大すれば, 折れ線で近似されている事が分かる.

このパッケージを使えば, 曲線をBezier曲線で近似してノード数を減らす事が可能となる.



## インストール
`BezierCurveApproximation.m`をダウンロードし, 適切なディレクトリに配置する.
配置場所を知るには
`FileNameJoin[{$UserBaseDirectory, "Applications"}]`
をMathematica上で実行すれば良い.


## 使い方
このパッケージで定義される関数は次の5つである:
- BezierControlPoints
- ShowCurve
- ShowLine
- ShowPoints
- ShowKnotss

全ての関数の引数はp, kである.
オプションは存在しない.
何れの関数もProtectedされており, 例えば`Clear[ShowLine]`などとすればエラーが発生する.


## 例
このパッケージでの描画の例を次に示す:
- 普通の関数
- 微分不能
- 不連続
- 3次までの多項式曲線
- 2変数関数


## 近似の理論
BezierCurveApproximation.pdfにBezier曲線で近似するための理論を記述した.


## 注意
Pointを使う関数(つまりShowPointsとShowKnots)では点の位置が正確に配置されない.
これはMathematicaのPointの仕様であって, このパッケージの問題ではない.
正確さを要求するなら例えば

とすれば良いだろう.

