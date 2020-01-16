(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: LatexTable *)
(* :Context: LatexTable` *)
(* :Author: axtimhaus *)
(* :Date: 2020-01-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 axtimhaus *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["LatexTable`"];
(* Exported symbols added here with SymbolName::usage *)

LatexTable::usage = "LatexTable[list_] - Constructs a LaTeX tabu from the two-dimensional list 'list'. The first row is considered as header.";
LatexAlignmentQ::usage = "LatexAlignmentQ[s_] - Tests if s ist either \"l\", \"c\" oder \"r\".";
TableWidth::usage = "Width of the tabu. Give a LaTeX string.";
MathCells::usage = "Specification which cells should be enclosed in $$. Default is 'All'. Give All, None or like {{rowNr -> (True|False), ...}, {columnNr -> (True|False), ...}}.";
ColumnAlignment::usage = "Specifies column alignments. Default is \"l\". Give a single value for all columns to be equal. Possible values are \"l\", \"c\" and \"r\". Give a list of rules like {columnNr -> val, ...} to specify special values. A list like {def, {columnNr -> val, ...}} is interpreted like as 'def' is the default value an the second element gives the values for single columns.";
HeaderAlignment::usage = "Specifies the alignment of the header column. Default is \"c\". Possible values are \"l\", \"c\" and \"r\".";
ColumnWidthFraction::usage = "Specifies column widths fractions. Default is 1. Give a single value for all columns to be equal. Only numerical values allowed. Give a list of rules like {columnNr -> val, ...} to specify special values. A list like {def, {columnNr -> val, ...}} is interpreted like as 'def' is the default value an the second element gives the values for single columns.";
TableDividers::usage = "Specifies the dividers drawn. Syntax almost like Dividers like in Grid (see ref/Dividers), but no graphics directives allowed.";

LatexTable::inva = "Invalid argument or option `1`.";

Begin["`Private`"];

Options[LatexTable] = {
  MathCells -> All,
  TableWidth -> "\\linewidth",
  ColumnAlignment -> "l",
  HeaderAlignment -> "c",
  ColumnWidthFraction -> 1,
  TableDividers -> {{True, True, {False}, True}, None}
};


LatexAlignmentQ[s_] := s === "l" || s === "c" || s === "r";
LatexTable[list_, OptionsPattern[]] := ToString[
  Module[{Dim = Dimensions[list][[1 ;; 2]], columnWidthFraction, columnAlignment, enclosedTable, DividersInterpreter, ColumnDividers, RowDividers, PrintRowDivider, PrintColumnDivider},

    If[Length@Dim < 2, Message[LatexTable::inva, "list"];Abort[]];
    If[Not@LatexAlignmentQ@OptionValue@HeaderAlignment, Message[LatexTable::inva, HeaderAlignment];Abort[]];

    columnAlignment = Switch[
      OptionValue@ColumnAlignment,

      _String?LatexAlignmentQ,
      Table[OptionValue@ColumnAlignment, Dim[[2]]],

      {__?(MatchQ[#, _Integer -> _String?LatexAlignmentQ] &)},
      With[{asso = Association@OptionValue@ColumnAlignment}, Table[If[MissingQ[asso@i], "l", asso@i] , {i, Dim[[2]]}]],

      {_?LatexAlignmentQ, {__?(MatchQ[#, _Integer -> _String?LatexAlignmentQ] &)}},
      With[{def = OptionValue[ColumnAlignment][[1]], asso = Association@OptionValue[ColumnAlignment][[2]]}, Table[If[MissingQ[asso@i], def, asso@i ], {i, Dim[[2]]}]],

      _,
      Message[LatexTable::inva, ColumnAlignment];Abort[]
    ];
    columnWidthFraction = Switch[
      OptionValue@ColumnWidthFraction,

      _?NumericQ,
      Table[OptionValue@ColumnWidthFraction, Dim[[2]]],

      {__?(MatchQ[#, _Integer -> _?NumericQ] &)},
      With[{asso = Association@OptionValue@ColumnWidthFraction}, Table[If[MissingQ[asso@i], "l", asso@i] , {i, Dim[[2]]}]],

      {_?NumericQ, {__?(MatchQ[#, _Integer -> _?NumericQ] &)}},
      With[{def = OptionValue[ColumnWidthFraction][[1]], asso = Association@OptionValue[ColumnWidthFraction][[2]]}, Table[If[MissingQ[asso@i], def, asso@i ], {i, Dim[[2]]}]],

      _,
      Message[LatexTable::inva, ColumnWidthFraction];Abort[]
    ];
    enclosedTable = MapThread[
      If[#2, ToString@StringForm["$``$", #1], ToString@#1]&,
      {
        list,
        Switch[
          OptionValue[MathCells],

          All,
          Table[True, Evaluate[Sequence @@ Dim]],

          None,
          Table[False, Evaluate[Sequence @@ Dim]],

          {},
          Table[False, Evaluate[Sequence @@ Dim]],

          {_, _},
          With[
            {RowRules = Association@OptionValue[MathCells][[1]], ColumnRules = Association@OptionValue[MathCells][[2]]},
            Table[If[RowRules[i] === True || ColumnRules[j] === True, True, False], {i, Dim[[1]]}, {j, Dim[[2]]}]
          ],

          _,
          Message[LatexTable::inva, MathCells];Abort[]
        ]
      },
      2
    ];

    DividersInterpreter[spec_, n_] := Replace[
      spec,
      {
        None :> Table[False, n],
        False :> Table[False, n],
        {} :> Table[False, n],
        All :> Table[True, n],
        Center :> Block[{divs}, divs = Table[True, n]; divs[[1]] = False; divs[[-1]] = False; divs],
        True :> Block[{divs}, divs = Table[False, n]; divs[[1]] = True; divs[[-1]] = True; divs],
        {__?BooleanQ} :> spec ~ Join ~ Table[False, {i, n - Length@spec}],
        (*        {{s_?BooleanQ}} :> Table[s, n],
                {{s__?BooleanQ}} :> Flatten@Table[List[s], {i, 1, n, Length@spec[[1]]}],*)
        {first___?BooleanQ, {center___?BooleanQ}, last___?BooleanQ} :> With[{cLen = n - Length@List[first] - Length@List[last]},
          Join[
            List[first],
            If[
              Length@List[center] === 0,
              Table[False, cLen],
              Flatten[Table[List[center], {i, 1, cLen, Length@List[center]}]][[;; cLen]]
            ],
            List[last]
          ]
        ],
        {(_ -> _)..} :> Block[{divs},
          divs = Table[False, n];
          Do[divs[[r[[1]]]] = r[[2]], {r, spec}];
          divs
        ],
        {s : {__}, i_ -> r_} :> Block[{divs},
          divs = DividersInterpreter[s, n];
          divs[[i]] = r;
          divs
        ],
        {s : {__}, rules : {(_ -> _)..}} :> Block[{divs},
          divs = DividersInterpreter[s, n];
          Do[divs[[r[[1]]]] = r[[2]], {r, rules}];
          divs
        ],
        _ :> (Message[LatexTable::inva, TableDividers];Abort[])
      }
    ];

    RowDividers = DividersInterpreter[OptionValue[TableDividers][[1]], Dim[[1]] + 1];
    PrintRowDivider[i_] := If[RowDividers[[i]], "\t\\hline\n", ""];
    ColumnDividers = DividersInterpreter[OptionValue[TableDividers][[2]], Dim[[2]] + 1];
    PrintColumnDivider[i_] := If[ColumnDividers[[i]], "|", ""];

    StringForm[
      "\\begin{tabu} to `1` {`2`}\n`3``4`\\end{tabu}",

      OptionValue@TableWidth,

      StringJoin[
        Table[PrintColumnDivider[i] <> "X[" <> ToString@columnWidthFraction[[i]] <> "," <> columnAlignment[[i]] <> "]", {i, Dim[[2]]}]
      ] <> PrintColumnDivider[-1],

      PrintRowDivider[1] <> "\t\\rowfont[" <> OptionValue@HeaderAlignment <> "]{}" <> StringJoin[
        Table[
          e <> " & ",
          {e, enclosedTable[[1, 1 ;; -2]]}
        ]
      ] <> enclosedTable[[1, -1]] <> " \\\\\n",

      StringJoin[
        Table[
          PrintRowDivider[i] <> "\t" <> StringJoin[
            Table[
              e <> " & ",
              {e, enclosedTable[[i, 1 ;; -2]]}
            ]
          ] <> enclosedTable[[i, -1]] <> " \\\\\n",
          {i, 2, Length@enclosedTable}
        ]
      ] <> PrintRowDivider[-1]
    ]
  ],
  StandardForm
];

End[]; (* `Private` *)

EndPackage[]