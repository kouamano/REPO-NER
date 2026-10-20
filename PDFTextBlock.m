BeginPackage["PDFTextBlock`"];
docrefine::usage = "docrefine[mupdftext] return structureed text.";
Begin["`Private`"];
repod = "/Users/kouamano/gitsrc/utility/text_op/";
repof = "mupdftotxblock.py";
repoImpt = Import[repod <> repof, "Text"];
ss = StringPosition[repoImpt, "# config start"][[1, 2]];
se = StringPosition[repoImpt, "# config end"][[1, 1]];
rlStr = StringSplit[StringTake[repoImpt, {ss + 2, se - 2}], "\n"];
exStr = StringReplace[rlStr, "'" -> "\""];
valStr = Map[StringSplit[#, " = "][[1]] &, exStr];
exStr // ToExpression;
pgRectSstr = "Rect(";
pgRectEstr = ") ";

refinePgRect[str_] := Module[{posS, posE, coordstr, coord},
  posS = StringPosition[str, pgRectSstr][[1, 2]] + 1;
  posE = StringPosition[str, pgRectEstr][[1, 1]] - 1;
  (*coordstr="{"<>StringTake[str,{posS,posE}]<>"}"*);
  coord = ToExpression["{" <> StringTake[str, {posS, posE}] <> "}"];
  coord
  ];

refinebbRect[str_] := Module[{posS, posE, coordstr, coord},
  posS = StringPosition[str, bbS][[1, 2]] + 3;
  posE = StringPosition[str, bbE][[1, 1]] - 3;
  (*coordstr="{"<>StringTake[str,{posS,posE}]<>"}"*);
  coord = ToExpression["{" <> StringTake[str, {posS, posE}] <> "}"];
  coord
  ];

refinebbText[str_] := 
 StringReplace[str, RegularExpression["\}\}\}\n*"] -> "\n"];

refinebb[bblist_] := {refinebbRect[bblist[[1]]], 
  refinebbText[bblist[[2 ;;]]]};

refinePg[pglist_] := {pglist[[1]], Map[refinebb, pglist[[2]]]};

docrefine[doc_] := Module[{imptPg, refine1},
  imptPg = Map[StringSplit[#, blDLM] &, StringSplit[doc, pgRECTS]];
  refine1 = 
   DeleteCases[
    Map[{refinePgRect[#[[1]]], 
       Map[StringSplit[#, txS] &, #[[2 ;;]]]} &, imptPg], {"\n"}, 
    Infinity];
  Map[refinePg, refine1]
  ];
End[];
EndPackage[];
