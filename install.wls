#!/usr/bin/env wolframscript
(* ::Package:: *)

Print["Installing latest *.wl files to ", FileNameJoin[{$UserBaseDirectory, "Applications"}]];
SetDirectory[NotebookDirectory[]];
files = FileNames["*.wl"];
Print @ TableForm @ files;
Do[CopyFile[file, FileNameJoin[{$UserBaseDirectory, "Applications", file}], OverwriteTarget->True], {file, files}];



