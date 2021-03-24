(* ::Package:: *)

(* ::Chapter:: *)
(*File Import*)


(* ::Subsection:: *)
(*Site Information*)


(* ::Text:: *)
(*Local: Load from Directory[]*)


GetSiteInfo[1] := PacletExpressionConvert[2] /@ withUniqueContext@ImportString[#, "Package"] & @ Import["PacletSite.mz", {"ZIP", "PacletSite.m", "String"}]


(* ::Text:: *)
(*Local: Fetch now*)


GetSiteInfo[2] := `PacletSite @@ PacletExpressionConvert[2]@*GetPacletInfo /@ PacletList[]


(* ::Text:: *)
(*Cloud: Load from cache*)


replaceSystemSymbol[h_@s___] := Switch[{Context@h, SymbolName@h},
	{"System`", "Paclet"},
		`Paclet@s,
	{"System`", "PacletObject"},
		`PacletObject@s,
	_,
		h@s
]


GetSiteInfo[3] := With[
	{
		info = SelectFirst[
			PacletManager`Services`Private`$pacletSiteData,
			StringContainsQ[$RemotePacletSite]@*First
		]
	},
	If[MissingQ@info,
		`PacletSite[],
		`PacletSite @@ PacletExpressionConvert[2]@*replaceSystemSymbol /@ Last@info
	]
]


(* ::Text:: *)
(*Cloud: Fetch now*)


GetSiteInfo[4] := PacletExpressionConvert[2] /@ withUniqueContext@ImportString[#, "Package"] &@ImportByteArray[
	ByteArray@URLRead[DownloadRequest@`PacletSite]["BodyBytes"]
, {"ZIP", "PacletSite.m", "String"}]


(* ::Subsection:: *)
(*Paclet Information*)


(* ::Subsubsection:: *)
(*File names*)


PacletList[] := FileNames["*.paclet", "Paclets"]


(* ::Subsubsection:: *)
(*Import PacletInfo in paclets*)


GetPacletInfo[filePath_] := withUniqueContext@ImportString[#, "Package"] &@First@Import[filePath, {StringRiffle[{"*", "PacletInfo.*"}, "/"], "String"}]
SetAttributes[GetPacletInfo, Listable]
GetPacletInfo[] := GetPacletInfo@PacletList[]


(* ::Chapter:: *)
(*File Export*)


(* ::Subsection:: *)
(*SiteInfo*)


PutSiteInfo[siteInfo_] := With[
	{
		infoString = withUniqueContext@ToString[#, InputForm] &[
			`PacletSite @@ PacletExpressionConvert[1] /@ siteInfo
		]
	},
	Export["PacletSite.m", infoString, "String"];
	CreateArchive["PacletSite.m", "PacletSite.mz", OverwriteTarget -> True];
	infoString
]
PutSiteInfo[i_Integer] := PutSiteInfo@SiteRegularize@GetSiteInfo@i
PutSiteInfo[] := PutSiteInfo@2
