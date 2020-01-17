(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: DumpFile *)
(* :Context: DumpFile` *)
(* :Author: maxwe *)
(* :Date: 2019-11-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 maxwe *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["DumpFile`"];
(* Exported symbols added here with SymbolName::usage *)

DumpFile::usage = "DumpFile[name_] - Gives the path to a dump file like \"NotebookDirectory/dmp/NotebookFileName/name\".";

Begin["`Private`"];
DumpFile[name_, subdir_:""] := Module[{Dir},
  Dir = FileNameJoin@{NotebookDirectory[], "dmp", Last@FileNameSplit@NotebookFileName[], subdir};
  If[Not@DirectoryQ@Dir, CreateDirectory@Dir];

  FileNameJoin@{Dir, name}
];

End[]; (* `Private` *)

EndPackage[]