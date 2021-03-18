(* ::Package:: *)

(* ::Chapter:: *)
(*PacletSearch*)


(* ::Text:: *)
(*These functions will return regularized paclet expressions.*)


(* ::Subsection:: *)
(*Grouping*)


groupByDepth = KeySort@*GroupBy[Length@Keys@# - 1 &]@*Normal;

crossPlatformQ = GetPacletValue[{"SystemID", "Qualifier"}]@# === {All, ""} &;

splitSeriesName = KeyMap[StringSplit[#, "_"]&];

buildTreeByNameAndPlatform = MapIndexed[
	If[#2[[1,1]],
		GroupByValue["Name"]@#,
		Normal@*GroupByValue[{"SystemID", "Qualifier"}] /@ GroupByValue["Name"]@#
	]&,
	GroupBy[crossPlatformQ]@*List@@SiteRegularize@#
] &;

(* `GroupBy[Most@*Keys]` the deepest series once, and then `Merge[Catenate]` it into the shallower one (`newDepth = oldDepth - 1`). *)
groupBySeriesGradually = Nest[
	Merge[Catenate] @* MapAt[
		#[[1]]-1 -> Normal@GroupBy[Most@*Keys]@#[[2]] &
	, -1] @* Normal,
	#,
	Last@Keys@#
] &;


BuildTreeBySeries[siteInfo:_[__`Paclet]] := 
	Catenate@Values@groupBySeriesGradually@groupByDepth@splitSeriesName@# & /@ buildTreeByNameAndPlatform@siteInfo
BuildTreeBySeries[i_Integer] := BuildTreeBySeries@GetSiteInfo@i
BuildTreeBySeries[] := BuildTreeBySeries@3


(* ::Subsection:: *)
(*Version Matching*)


(* ::Text:: *)
(*Accept: Type-2 Paclet*)
(*Accept: Version Specification String*)
(*Accept: Version Number String*)


KernelVersionMatchQ[spec_String][version_String] := Which[
	StringMatchQ["*,*"]@spec,
		AnyTrue[
			StringSplit[spec, ","],
			KernelVersionMatchQ[#][version] &
		],
	StringMatchQ["*+"]@spec,
		FromDigits/@StringSplit[#, "."]&/@{StringDrop[spec, -1], version} //OrderedQ@*PadRight,
	StringMatchQ["*-"]@spec,
		FromDigits/@StringSplit[#, "."]&/@{version, StringDrop[spec, -1]} //OrderedQ@*PadRight,
	True,
		And@@Replace[
			Transpose@PadRight[StringSplit[#, "."]&/@{version, spec}, Automatic, "*"],
			{
				{_, "*"} -> True,
				{"*", y_} :> FromDigits@y === 0,
				{x_, y_} :> FromDigits@x === FromDigits@y
			},
			1
		]
]
KernelVersionMatchQ[paclet_`Paclet][version_String] := KernelVersionMatchQ[GetPacletValue["WolframVersion"]@paclet]@version
KernelVersionMatchQ[_String|_`Paclet][All] := True


(* ::Subsection:: *)
(*Search*)


PacletQuery[partSpec:{___String}:{}, OptionsPattern@{"SiteInfo" -> Hold@GetSiteInfo@3}] := Fold[
	Replace[_Missing :> {}]@Query[Key@#2]@<|#1|>&,(* Catenate Cases Rule *)
	#,
	Flatten@*List /@ FoldList[List, partSpec](* Map Take Range Length*)
]& /@ BuildTreeBySeries@ReleaseHold@OptionValue@"SiteInfo"
PacletQuery[partSpec_String, opts:OptionsPattern[]] := PacletQuery[StringSplit[partSpec, "_"], opts]


(* ::Text:: *)
(*Platform Compatibility Test Not Implemented !*)


getNewestCompatible[version_][_[paclets__`Paclet]] := SelectFirst[{paclets}, KernelVersionMatchQ[#][version] &, Nothing]


PacletSearch[partSpec:{___String}|_String:{}, OptionsPattern[]] := Cases[
	PacletQuery[partSpec, "SiteInfo" -> ReleaseHold@OptionValue@"SiteInfo"],
	paclets:{__`Paclet} :> getNewestCompatible[OptionValue@"KernelVer"]@paclets
, Infinity]
Options[PacletSearch] = {"SiteInfo" -> Hold@GetSiteInfo@3, "KernelVer" -> $VersionStringComplete};
