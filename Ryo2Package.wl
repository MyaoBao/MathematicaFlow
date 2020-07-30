(* ::Package:: *)

(* ::Title:: *)
(*\:65c5\:5ba2\:8f38\:9001\:4eba\:54e1\:30c7\:30fc\:30bf\:89e3\:6790*)


(* ::Text:: *)
(*Copyright (c) 2020 Naoto Agawa, Yoshihiro Mizoguchi, Kyushu University.*)
(**)
(*Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell*)
(*copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:*)
(*The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.*)
(*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.*)


(* ::Text:: *)
(*\:6700\:7d42\:66f4\:65b0\:65e5:2020/07/23 *)
(*\:4f5c\:6210\:65e5:2020/07/15*)


(* ::Text:: *)
(*\:540c\:540d\:306e\:30b7\:30f3\:30dc\:30eb\:306e\:7af6\:5408\:3092\:907f\:3051\:308b\:305f\:3081\:30b3\:30f3\:30c6\:30ad\:30b9\:30c8\:3092\:5b9a\:3081\:308b*)


BeginPackage["Ryo2Package`"];


(* ::Text:: *)
(*\:30d1\:30c3\:30b1\:30fc\:30b8\:5185\:306e\:307f\:3067\:5229\:7528\:3059\:308b\:95a2\:6570\:3092\:30b3\:30f3\:30c6\:30ad\:30b9\:30c8 `Private` \:5185\:3067\:5b9a\:7fa9\:3059\:308b.*)


Begin["`Private`"];

End[];


(* ::Text:: *)
(*\:56fd\:571f\:4ea4\:901a\:7701:\:4ea4\:901a\:95a2\:4fc2\:7d71\:8a08\:8cc7\:6599:\:9244\:9053\:8eca\:4e21\:7b49\:52d5\:614b\:7d71\:8a08\:8abf\:67fb*)
(*\:8ca8\:7269\:5730\:57df\:6d41\:52d5\:8abf\:67fb\:30fb\:65c5\:5ba2\:5730\:57df\:6d41\:52d5\:8abf\:67fb(2017\:5e74\:5ea6) (\:7d71\:8a08\:88682) \:5e9c\:770c\:76f8\:4e92\:9593\:8f38\:9001\:4eba\:54e1\:8868*)
(*e-Stat: \:8ca8\:7269\:5730\:57df\:6d41\:52d5\:8abf\:67fb\:30fb\:65c5\:5ba2\:5730\:57df\:6d41\:52d5\:8abf\:67fb / \:65c5\:5ba2\:5730\:57df\:6d41\:52d5\:8abf\:67fb / \:65c5\:5ba2\:5730\:57df\:6d41\:52d5\:8abf\:67fb*)
(*https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00600460&kikan=00600&tstat=000001016696&cycle=8&year=20171&month=0&tclass1=000001067591&stat_infid=000031817433*)


ryo2="https://www.e-stat.go.jp/stat-search/file-download?statInfId=000031817433&fileKind=0";
excel=Import[URL[ryo2]];
index={7,66,124,183};
width={13,13,13,12};
sheet1=excel[[1]];
trimr=sheet1[[#;;(#-1)+51]]&/@index;
trimc=Function[x,#[[3;;(3-1)+x[[1]]]]&/@x[[2]]]/@ Transpose[{width,trimr}];

array1::usage="\:79fb\:52d5\:8005\:6570\:306e\:884c\:5217";
array1=Transpose[Flatten/@ Transpose[trimc]];

label1::usage="\:5730\:57df\:540d\:306e\:30ea\:30b9\:30c8";
label1=Transpose[Transpose[sheet1][[{1,2}]]][[index[[1]];;(index[[1]]-1)+51]];

label3::usage="\:5730\:57df\:540d\:3092\:90fd\:5e02\:540d\:306b\:5dee\:3057\:66ff\:3048\:305f\:30e9\:30d9\:30eb\:30ea\:30b9\:30c8";
label3={{1.`,"\:7a1a\:5185"},{2.`,"\:91e7\:8def"},{3.`,"\:65ed\:5ddd"},{4.`,"\:51fd\:9928"},{5.`,"\:672d\:5e4c"},{6.`,"\:9752\:68ee"},
{7.`,"\:76db\:5ca1"},{8.`,"\:4ed9\:53f0"},{9.`,"\:79cb\:7530"},{10.`,"\:5c71\:5f62"},{11.`,"\:798f\:5cf6"},{12.`,"\:6c34\:6238"},
{13.`,"\:5b87\:90fd\:5bae"},{14.`,"\:524d\:6a4b"},{15.`,"\:3055\:3044\:305f\:307e"},{16.`,"\:5343\:8449"},{17.`,"\:6771\:4eac"},{18.`,"\:6a2a\:6d5c"},
{19.`,"\:65b0\:6f5f"},{20.`,"\:5bcc\:5c71"},{21.`,"\:91d1\:6ca2"},{22.`,"\:798f\:4e95"},{23.`,"\:7532\:5e9c"},{24.`,"\:9577\:91ce"},
{25.`,"\:5c90\:961c"},{26.`,"\:9759\:5ca1"},{27.`,"\:540d\:53e4\:5c4b"},{28.`,"\:6d25"},{29.`,"Otsu"},{30.`,"\:4eac\:90fd"},
{31.`,"\:5927\:962a"},{32.`,"\:795e\:6238"},{33.`,"\:5948\:826f"},{34.`,"\:548c\:6b4c\:5c71"},{35.`,"\:9ce5\:53d6"},{36.`,"\:677e\:6c5f"},
{37.`,"\:5ca1\:5c71"},{38.`,"\:5e83\:5cf6"},{39.`,"\:5c71\:53e3"},{40.`,"\:5fb3\:5cf6"},{41.`,"\:9999\:5ddd"},{42.`,"\:677e\:5c71"},
{43.`,"\:9ad8\:77e5"},{44.`,"\:798f\:5ca1"},{45.`,"\:4f50\:8cc0"},{46.`,"\:9577\:5d0e"},{47.`,"\:718a\:672c"},{48.`,"\:5927\:5206"},
{49.`,"\:5bae\:5d0e"},{50.`,"\:9e7f\:5150\:5cf6"},{51.`,"\:90a3\:8987"}};


(* ::Text:: *)
(*\:5fc5\:8981\:306a\:90fd\:5e02\:9593\:306e\:30c7\:30fc\:30bf\:3092\:62bd\:51fa\:3057\:305f\:8868\:306e\:4f5c\:6210\:3002*)


CityTable::usage="CityTable[array,label,city]\:3000\:5fc5\:8981\:306a\:90fd\:5e02\:9593\:306e\:30c7\:30fc\:30bf\:3092\:62bd\:51fa\:3057\:305f\:8868\:306e\:4f5c\:6210
 (array:\:30c7\:30fc\:30bf\:8868, label:\:90fd\:5e02\:540d\:30ea\:30b9\:30c8, city:\:90fd\:5e02\:756a\:53f7\:30ea\:30b9\:30c8)";
CityTable[array_,label_,city_]:=Module[{label11,array11},
label11=label[[city]];
array11=Transpose[Transpose[array[[city]]][[city]]];
TableForm[array11,TableHeadings->{Transpose[label11][[2]],Transpose[label11][[2]]}]]


(* ::Text:: *)
(*\:77e2\:5370\:306e\:5411\:304d\:306b\:79fb\:52d5\:3057\:305f\:4eba\:6570\:3092\:8fba\:306e\:4e0a\:306b\:8a18\:3057\:3066\:3001\:90fd\:5e02\:540d\:3092\:70b9\:30e9\:30d9\:30eb\:306b\:3057\:305f\:53ef\:8996\:5316\:3002*)


CityGraph::usage="CityGraph[array,label,city]\:3000\:77e2\:5370\:306e\:5411\:304d\:306b\:79fb\:52d5\:3057\:305f\:4eba\:6570\:3092\:8fba\:306e\:4e0a\:306b\:8a18\:3057\:3066\:3001
\:90fd\:5e02\:540d\:3092\:70b9\:30e9\:30d9\:30eb\:306b\:3057\:305f\:53ef\:8996\:5316
 (array:\:30c7\:30fc\:30bf\:8868, label:\:90fd\:5e02\:540d\:30ea\:30b9\:30c8, city:\:90fd\:5e02\:756a\:53f7\:30ea\:30b9\:30c8)";
CityGraph[array_,label_,city_]:=Module[{label11,array11},
label11=label[[city]];
array11=Transpose[Transpose[array[[city]]][[city]]];
WeightedAdjacencyGraph[array11,EdgeLabels-> Flatten[Table[i\[DirectedEdge]j->array11[[i,j]],
{i,1,Length[city]},{j,1,Length[city]}],1],VertexSize->0.25,
VertexLabels-> Table[i-> Placed[label11[[i,2]],Center],
{i,1,Length[city]}]]];


(* ::Text:: *)
(*\:30b0\:30e9\:30d5\:306e\:9802\:70b9\:3092\:5730\:56f3\:4e0a\:306b\:914d\:7f6e\:3059\:308b\:6e96\:5099\:3002*)


TrianglizationConnecter::usage="TrianglizationConnecter[city1,city2,city3]
\:30b0\:30e9\:30d5\:306e\:9802\:70b9\:3092\:5730\:56f3\:4e0a\:306b\:914d\:7f6e\:3059\:308b\:6e96\:5099
 (city1:\:4e00\:3064\:76ee\:306e\:90fd\:5e02\:540d, city2:\:4e8c\:3064\:76ee\:306e\:90fd\:5e02\:540d, city3:\:4e09\:3064\:76ee\:306e\:90fd\:5e02\:540d)";
TrianglizationConnecter[city1_,city2_,city3_]:=Module[ {city11,city12,city13},
city11=Interpreter["Location"][city1<>", Japan"];
city12=Interpreter["Location"][city2<>", Japan"];
city13=Interpreter["Location"][city3<>", Japan"];
GeoGraphics[{
{Red,Thickness[0.01],GeoPath[{city11,city12},"Geodesic"]},
{Blue,Thickness[0.02],GeoPath[{city12,city13},"Geodesic"]},
{Green,Thickness[0.03],GeoPath[{city13,city11},"Geodesic"]}
}]
];


(* ::Text:: *)
(*\:30b0\:30e9\:30d5\:3067\:306e\:53ef\:8996\:5316\:ff1a\:4e00\:90e8\:306e\:90fd\:5e02\:9593\:306e\:30c7\:30fc\:30bf\:3092\:62bd\:51fa\:3057\:30c7\:30fc\:30bf\:91cf\:306b\:3088\:308a\:8fba\:306e\:5e45\:3084\:8272\:3092\:5909\:5316\:3057\:3066\:63cf\:753b\:3059\:308b\:3002*)


CityGraph2[array_,label_,city_,n_]:=Module[{label11,array11,array12,x,x2,x3,el},
label11=label[[city]];
(* label11: \:9078\:629e\:3057\:305f\:90fd\:5e02\:306e\:540d\:524d\:3092\:62bd\:51fa\:3059\:308b *)
array11=Transpose[Transpose[array[[city]]][[city]]];
(* array11: \:9078\:629e\:3057\:305f\:90fd\:5e02\:9593\:306e\:30c7\:30fc\:30bf\:3092\:62bd\:51fa\:3059\:308b *)
array12=(array11+Transpose[array11])*(Table[1,{Length[city]},{Length[city]}]
-IdentityMatrix[Length[city]]);
(* array12: array11\:306e\:5bfe\:89d2\:6210\:5206\:30920\:306b\:3059\:308b *)
x=Normalize[Flatten[Table[array12[[i,j]],{i,1,Length[city]},{j,1,i}]]];
x2=Normalize[(1-(x-1)^n)^(1/n)];
(* Print[{Variance[x],Variance[x2]}]; *)
x3=x2/Max[x2];
(* x3: \:30c7\:30fc\:30bf\:7bc4\:56f2\:3092[0,1]\:3078\:5909\:63db\:3057,\:5206\:6563\:3092\:5927\:304d\:304f\:3059\:308b *)
el=Flatten[Table[i\[UndirectedEdge]j->{Thickness[x3[[(i(i-1))/2+j]]/50],
ColorData["TemperatureMap"][x3[[(i(i-1))/2+j]]]},{i,1,Length[city]},{j,1,i}],1];
(* \:8fba\:306e\:592a\:3055\:3068\:8272\:3092\:91cd\:307f\:306b\:3088\:308a\:5909\:5316\:3055\:305b\:308b. *)
CompleteGraph[Length[city],VertexSize->0.25,EdgeStyle-> el,
VertexLabels-> Table[i-> Placed[label11[[i,2]],Center],{i,1,Length[city]}]]
];


(* ::Text:: *)
(*3\:70b9\:9593\:306e\:79fb\:52d5\:8005\:6570\:30c7\:30fc\:30bf\:306e\:53ef\:8996\:5316\:95a2\:6570\:306e\:5b9a\:7fa9*)


ThreePointsView::usage="ThreePointView[array,label,city] 3\:90fd\:5e02\:9593\:79fb\:52d5\:8005\:6570\:30c7\:30fc\:30bf\:306e\:53ef\:8996\:5316
 (array:\:30c7\:30fc\:30bf\:8868, label:\:90fd\:5e02\:540d\:30ea\:30b9\:30c8, city:3\:90fd\:5e02\:756a\:53f7\:30ea\:30b9\:30c8)";
ThreePointsView[array_,label_,city_]:=
Module[ {array11,array12,array13,array14,label11,CityTable1,GeoGraphicsTable},
array11=Transpose[array[[city]]][[city]];
array12=(array11+Transpose[array11])*(Table[1,{Length[city]},{Length[city]}]
-IdentityMatrix[Length[city]]);
array13=array12/(10*Max[array12]);
array14=array12/Max[array12];
label11=label[[city]];
CityTable1=Table[Interpreter["Location"][label11[[i,2]]<>", Japan"],{i,1,3}];
GeoGraphicsTable=
Table[
Table[
{
Thickness[array13[[i,j]]],
ColorData["TemperatureMap"][array14[[i,j]]],
GeoPath[{CityTable1[[i]],CityTable1[[j]]},
"Geodesic"]
},
{j,i+1,Length[city]}
],
{i,1,Length[city]-1}
];
GeoGraphics[GeoGraphicsTable]
ColorData["TemperatureMap","Image"]
];

FourPointsView::usage="ThreePointView[array,label,city] 4\:90fd\:5e02\:9593\:79fb\:52d5\:8005\:6570\:30c7\:30fc\:30bf\:306e\:53ef\:8996\:5316
 (array:\:30c7\:30fc\:30bf\:8868, label:\:90fd\:5e02\:540d\:30ea\:30b9\:30c8, city:4\:90fd\:5e02\:756a\:53f7\:30ea\:30b9\:30c8)";
FourPointsView[array_,label_,city_]:=
Module[ {array11,array12,array13,array14,label11,CityTable1,GeoGraphicsTable},
array11=Transpose[array[[city]]][[city]];
array12=(array11+Transpose[array11])*(Table[1,{Length[city]},{Length[city]}]
-IdentityMatrix[Length[city]]);
array13=array12/(10*Max[array12]);
array14=array12/Max[array12];
label11=label[[city]];
CityTable1=Table[Interpreter["Location"][label11[[i,2]]<>", Japan"],{i,1,4}];
GeoGraphicsTable=
Table[
Table[
{
Thickness[array13[[i,j]]],
ColorData["TemperatureMap"][array14[[i,j]]],
GeoPath[{CityTable1[[i]],CityTable1[[j]]},
"Geodesic"]
},
{j,i+1,Length[city]}
],
{i,1,Length[city]-1}
];
GeoGraphics[GeoGraphicsTable]
ColorData["TemperatureMap","Image"]
];


EndPackage[];
