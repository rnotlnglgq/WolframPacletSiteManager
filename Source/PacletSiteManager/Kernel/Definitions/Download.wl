(* ::Package:: *)

(* ::Chapter:: *)
(*Download*)


URLParse@"https://pacletserver2.wolfram.com"


URLBuild@{
	"pacletserver2.wolfram.com",
	"Paclets"
}


fileNameToURL[fileName_String] := URLBuild@<|
	"Scheme" -> "https",
	"User" -> None,
	"Domain" -> StringReplace["pacletserver.wolfram.com" :> "pacletserver2.wolfram.com"]@$RemotePacletSite,
	"Port" -> None,
	"Path" -> {"Paclets", URLEncode@fileName},
	"Query" -> {},
	"Fragment" -> None
|>
SetAttributes[fileNameToURL, Listable]


(* ::Subsubsection:: *)
(*Request*)


(* ::Text:: *)
(*Note: your $ActivationKey should be valid to access the server.*)


$userAgent = Replace[$ProgramName, Except@_String :> "MathematicaProgram"] <> "/" <> $VersionStringComplete <> " PM/5.0.0";


DownloadRequest[fileName_String] := HTTPRequest[
	fileNameToURL@fileName,
	<|
		"Headers" -> {
			"Mathematica-systemID" -> $SystemID,
			"Mathematica-license" -> $LicenseID,
			"Mathematica-mathID" -> $MachineID,
			"Mathematica-language" -> $Language,
			"Mathematica-activationKey" -> $ActivationKey
		},
		"UserAgent" -> $userAgent
	|>
];
DownloadRequest[`PacletSite] := HTTPRequest[
	URLBuild@<|
		"Scheme" -> "https",
		"User" -> None,
		"Domain" -> StringReplace["pacletserver.wolfram.com" :> "pacletserver2.wolfram.com"]@$RemotePacletSite,
		"Port" -> None,
		"Path" -> {"PacletSite.mz"},
		"Query" -> {},
		"Fragment" -> None
	|>,
	<|
		"Headers" -> {
			"Mathematica-systemID" -> $SystemID,
			"Mathematica-license" -> $LicenseID,
			"Mathematica-mathID" -> $MachineID,
			"Mathematica-language" -> $Language,
			"Mathematica-activationKey" -> $ActivationKey
		},
		"UserAgent" -> $userAgent
	|>
];
DownloadRequest[] := DownloadRequest[""]


(* ::Subsubsection:: *)
(*Export URL list*)


ExportURLList[pacletNames:{__String}, urlFileName_:"url.txt"] := Export[urlFileName, StringRiffle[fileNameToURL@pacletNames, "\n"], "Text"]
ExportURLList[paclets:_[__`Paclet], urlFileName_:"url.txt"] := ExportURLList[GetPacletValue["FileName"]/@paclets, urlFileName]
(* TODO(?): The url.txt can be in a temporary dir. *)


(* ::Subsubsection:: *)
(*Download command*)


$Downloader = "wget";


DownloadCommand["wget"] := StringTemplate["wget `` -i ``"][
	StringTemplate["--header=\"``: ``\""]@@@DownloadRequest[]["Headers"] //StringRiffle,
	"url.txt"
]


(* ::Subsubsection:: *)
(*Downloader*)


DownloadPaclet[paclets:_[__`Paclet]] := Switch[$Downloader,
	"wget",
		paclets //ExportURLList;
		"!"<>DownloadCommand[$Downloader] //Get,
	_, Throw@"NotImplemented"
]
DownloadPaclet[single_`Paclet] := DownloadPaclet@{single}
DownloadPaclet[partSpec:{___String}|_String, opts:OptionsPattern[]] := DownloadPaclet@PacletSearch[partSpec, opts]
DownloadPaclet[] := DownloadPaclet@{}


(* ::Subsection:: *)
(*Check Validity*)


ValidPacletQ[expr_] := Head@expr === `Paclet; (* Need to be better *)

ValidPacletFileQ[file_] := ValidPacletQ@GetPacletInfo@file;
