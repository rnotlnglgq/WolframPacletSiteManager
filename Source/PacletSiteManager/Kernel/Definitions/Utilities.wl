(* ::Package:: *)

$RemotePacletSite = "pacletserver.wolfram.com";


$VersionStringComplete = StringRiffle[#, "."]&@{
	If[StringMatchQ[#, "*."], # <> "0", #]&@ToString@$VersionNumber,
	ToString@$ReleaseNumber,
	ToString@$MinorReleaseNumber
};


SetAttributes[withContext, HoldAll]
withContext[expr_] := Block[{tmp},
	Begin@"PacletSiteManager`Private`";
	tmp = expr;
	End[];
	tmp	
]


SetAttributes[withUniqueContext, HoldAll]
withUniqueContext[expr_] := Block[{$Context = "PacletSiteManager`Private`", $ContextPath = {}},
	expr
]
