(* ::Package:: *)

(* ::Chapter:: *)
(*Split and Catenate*)


(* ::Subsection:: *)
(*Paclet Parts*)


PacletPartList[] := FileNames["*.paclet.*", "Paclets"]


(* ::Subsubsection:: *)
(*Parts regularization*)


(* Assertion for the First[{}] situation? *)

PartsRegularize[fileNameList_] := Sort /@ GroupBy[
	First@*StringCases[StartOfString~~fileBaseName__~~".paclet."~~__ :> fileBaseName~~".paclet"]
]@fileNameList //Normal
PartsRegularize[] := PartsRegularize@PacletPartList[]


(* ::Subsubsection:: *)
(*Catenate*)


CatenateParts[pacletFilePath_ -> {partFilePaths__}] := Replace[0 -> FileBaseName@pacletFilePath]@Run@StringRiffle@{"cat", partFilePaths, ">", pacletFilePath}
SetAttributes[CatenateParts, Listable]
CatenateParts[] := CatenateParts@PartsRegularize[]


(* ::Subsubsection:: *)
(*Split*)


(* ::Text:: *)
(*Size unit: Byte*)


$PartSize = 1024000;


getPartsNumber[file_] := FileByteCount@file/$PartSize //Floor

SplitPaclet[pacletFilePath_] := If[# > 0,
	Replace[0 -> FileBaseName@pacletFilePath]@Run@StringRiffle@{"split", "-b", IntegerString@$PartSize, pacletFilePath, "-d", "-a", ToString@IntegerLength@#, pacletFilePath<>"."},
	FileBaseName@pacletFilePath
]&@getPartsNumber@pacletFilePath
SetAttributes[SplitPaclet, Listable]
SplitPaclet[] := SplitPaclet@PacletList[]
