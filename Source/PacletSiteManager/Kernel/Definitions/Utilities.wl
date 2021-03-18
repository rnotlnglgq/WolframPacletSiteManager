(* ::Package:: *)

$RemotePacletSite = "http://pacletserver.wolfram.com";


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
