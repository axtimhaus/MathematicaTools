#!/usr/bin/env wolframscript
(* ::Package:: *)

Print["Installing latest *.wl files to ", FileNameJoin[{$UserBaseDirectory, "Applications"}]];
Print["Current working directory is ", Directory[]];
files = FileNames["*.wl"];
Do[
  Print["Installing ", file];
  CopyFile[file, FileNameJoin[{$UserBaseDirectory, "Applications", file}], OverwriteTarget -> True],
  {file, files}
];
Print["Finished."]



