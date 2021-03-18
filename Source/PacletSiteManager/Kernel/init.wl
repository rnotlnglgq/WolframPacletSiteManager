(* ::Package:: *)

(* ::Title:: *)
(*PacletSiteManager*)


BeginPackage["PacletSiteManager`"]


PacletSiteManager`Private`$Test = False;


SetDirectory@If[PacletSiteManager`Private`$Test && $Notebooks,
	NotebookDirectory[],
	DirectoryName@$InputFileName
];

<< Declaration.wl;

Begin["`Private`"]
	SetDirectory@"Definitions";
	<< Utilities.wl
	<< Conversion.wl
	<< ImportAndExport.wl
	<< Download.wl
	<< Search.wl
	<< Requirement.wl
	<< SplitAndCatenate.wl
	ResetDirectory[]
End[]

ResetDirectory[];


EndPackage[]
