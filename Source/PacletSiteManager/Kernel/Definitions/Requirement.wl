(* ::Package:: *)

(* ::Chapter:: *)
(*Manage Requirement*)


(* ::Subsection:: *)
(*Manage requirement*)


(* ::Text:: *)
(*Note:*)
(*Requirement information won't include platform information as all paclets for different platforms will be processed together.*)
(*Several special paclets, which need to be separated to different repositories by platform information, should be processed manually.*)


(* https://www.wolfram.com/mathematica/quick-revision-history.html *)
$kernelVersionList = StringSplit@"12.2.0.0 12.1.1 12.1.0 12.0 11.3 11.2 11.1.1 11.1.0 11.0.1 11.0.0 10.4.1 10.4.0 10.3.1 10.3.0 10.2 10.1 10.0.2 10.0.1 10.0.0 9.0.1 9.0.0 8.0.4 8.0.3 8.0.2 8.0.1 8.0.0 7.0.1 7.0.0 6.0.3 6.0.2 6.0.1 6.0.0 5.2 5.1 5.0 4.2 4.1 4.0 3.0 2.2 2.1 2.0 1.2 1.0";


$RequirementFile = "Requirement.wl";


(* Not Implemented: Judge compatibility of 2 version specifications. . *)
GetRequirementInfo[_[paclets___`Paclet]] := DeleteDuplicates@ReplacePart[
	SelectFirst[$kernelVersionList, KernelVersionMatchQ[#], Nothing] & /@
		ReverseSortBy[
			#,
				GetPacletValue["VersionNumber"],
			OrderedQ@*PadRight@*List
		],
	1 -> All
] & /@ GroupByValue["Name"]@{paclets}

GetRequirementInfo[0] := Import[$RequirementFile, "Package"]

GetRequirementInfo[i_Integer] := GetRequirementInfo@GetSiteInfo@i

GetRequirementInfo[] := GetRequirementInfo@0


PutRequirementInfo[paclets_[__`Paclet]] := PutRequirementInfo@GetRequirementInfo@paclets

PutRequirementInfo[requirementInfo_Association] := (
	Export[$RequirementFile, requirementInfo, "Package"];
	requirementInfo
)

PutRequirementInfo[i_Integer] := PutRequirementInfo@GetRequirementInfo@i


(* ::Subsection:: *)
(*Select needed*)


(* ::Text:: *)
(*Unable to resolve non-crossplatform paclets yet.*)


selectNeeded[{paclets_, versions_}] := Function[kernelVer,
	kernelVer -> SelectFirst[paclets, KernelVersionMatchQ[#][kernelVer] &, Nothing]
] /@ versions


ListRequiredPaclet[requirementInfo_Association, cloudSiteInfo_`PacletSite] := {
	SortByVersion /@ GroupByValue["Name"]@cloudSiteInfo,
	requirementInfo
} //KeyIntersection //Merge[selectNeeded]

ListRequiredPaclet[requirementInfo_Association] := ListRequiredPaclet[requirementInfo, GetSiteInfo@3]

ListRequiredPaclet[] := ListRequiredPaclet@GetRequirementInfo[]


(* ::Subsection:: *)
(*List changes*)


pacletSameQ = SameQ @@ GetPacletValue[{"Name", "Version", "Qualifier"}] /@ {##} &;


(* Not implemented: a high-efficiency two-way complement algorithm. *)
compareDifference[{requiredPaclets_, localPaclets_}] := <|
	"Add" -> Complement[requiredPaclets, localPaclets, SameTest -> pacletSameQ],
	"Remove" -> Complement[localPaclets, requiredPaclets, SameTest -> pacletSameQ]
|>


ListPacletChanges[requiredPaclet_Association, localSiteInfo_`PacletSite] := {
	Values /@ requiredPaclet //DeleteDuplicates,
	GroupByValue["Name"]@localSiteInfo
} //KeyUnion[#, {}&]& //Merge[compareDifference] //Values //Merge[Catenate]

ListPacletChanges[needed_Association] := ListPacletChanges[needed, GetSiteInfo@2]

ListPacletChanges[] := ListPacletChanges@ListRequiredPaclet[]


(* ::Subsection:: *)
(*Update Specified Paclets*)


makePacletPath = FileNameJoin@{"Paclets", #} &;


ApplyPacletChanges::invalidfile = "Downloaded paclet file `FileName` is invalid, change-application stopped.";


ApplyPacletChanges::invalidfile = "File `` is not a valid paclet.";
ApplyPacletChanges[changes_Association] := Module[
	{
		add = Function[#Add]@changes,
		remove = Function[#Remove]@changes,
		valid = {}
	},
	Function[
		With[{fileName = GetPacletValue["FileName"]@#},
			DownloadPaclet@#;
			If[ValidPacletFileQ@fileName,
				CopyFile[fileName, makePacletPath@fileName, OverwriteTarget -> True];
				DeleteFile@fileName;
				AppendTo[valid, #]
				,
				DeleteFile@fileName;
				Message[ApplyPacletChanges::invalidfile, fileName];
				Return@Failure["InvalidFile",  <|
					"MessageTemplate" -> ApplyPacletChanges::invalidfile,
					"MessageParameters" -> <|"FileName" -> fileName, "ValidPaclets" -> StringRiffle@valid|>
				|>]
			]
		]
	] /@ add;
	
	DeleteFile@FileNames[GetPacletValue["FileName"]@#<>"*", "Paclets"] & /@ remove; (* clean both paclet and its parts *)
	changes
]


ApplyPacletChanges[paclet_`Paclet] := ApplyPacletChanges@<|"Add" -> {paclet}|>
