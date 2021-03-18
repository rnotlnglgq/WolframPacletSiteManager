(* ::Package:: *)

(* ::Chapter:: *)
(*Paclet Expressions*)


(* ::Subsection:: *)
(*Paclet*)


(* ::Subsubsection:: *)
(*Type convert*)


PacletExpressionConvert[2][paclet_`Paclet] := paclet /. $pacletInfoSymbolConversions


PacletExpressionConvert[2][pacletObject_`PacletObject] := `Paclet@@Normal@First@pacletObject


(* Cannot invert all rules applied above in 1 -> 2. As 2 -> 1 is rarely used, that won't be implemented. *)
PacletExpressionConvert[1][paclet_`Paclet] := paclet /. (key_String -> value_) :> (Symbol@key -> value)


PacletExpressionConvert[1][pacletObject_`PacletObject] := pacletObject //PacletExpressionConvert[2] //PacletExpressionConvert[1]


PacletExpressionConvert[3][paclet_`Paclet] := `PacletObject@*Association@@PacletExpressionConvert[2]@paclet


PacletExpressionConvert[3][pacletObject_`PacletObject] := paclet


$reservedSymbol = {
	`Name -> "Name",
	`Version -> "Version",
	`Extensions -> "Extensions",
	`Resources -> "Resources",
	`SystemID -> "SystemID",
	`MathematicaVersion -> "MathematicaVersion",
	`WolframVersion -> "WolframVersion",
	`Qualifier -> "Qualifier",
	`Internal -> "Internal",
	`Root -> "Root",
	`BackwardCompatible -> "BackwardCompatible",
	`BuildNumber -> "BuildNumber",
	`Description -> "Description", 
	`UUID -> "UUID",
	`Creator -> "Creator",
	`URL -> "URL",
	`Publisher -> "Publisher",
	`Support -> "Support",
	`Category -> "Category",
	`Keywords -> "Keywords",
	`Icon -> "Icon",
	`Thumbnail -> "Thumbnail",
	`Copyright -> "Copyright",
	`License -> "License",
	`Loading -> "Loading",
	`Language -> "Language",
	`Context -> "Context",
	`LinkBase -> "LinkBase",
	`MainPage -> "MainPage",
	`Prepend -> "Prepend",
	`Symbols -> "Symbols",
	`FunctionInformation -> "FunctionInformation",
	`HiddenImport -> "HiddenImport",
	`Alias -> "Alias",
	`ProductID -> "ProductID",
	`Updating -> "Updating",
	`AutoUpdating -> "AutoUpdating",
	`PlatformQualifier -> "Qualifier",
	`Contexts -> "Context"
};


$systemSymbol = {
	Version -> "Version",
	Root -> "Root",
	URL -> "URL",
	Thumbnail -> "Thumbnail",
	Context -> "Context",
	Language -> "Language",
	Pretend -> "Pretend",
	Contexts -> "Contexts",
	`Association -> System`Association,
	`List -> System`List,
	`Rule -> System`Rule,
	`True -> System`True,
	`False -> System`False,
	`Except -> System`Except,
	`Alternatives -> System`Alternatives,
	`All -> System`All,
	`None -> System`None,
	`Null -> System`Null,
	`Automatic -> System`Automatic,
	Association -> System`Association,
	List -> System`List,
	Rule -> System`Rule,
	True -> System`True,
	False -> System`False,
	Except -> System`Except,
	Alternatives -> System`Alternatives,
	All -> System`All,
	None -> System`None,
	Null -> System`Null,
	Automatic -> System`Automatic
};


$privateSymbol = {
	`Paclet -> `Paclet,
	`PacletObject -> `PacletObject, 
	`PacletGroup -> `PacletGroup, 
	`PacletSite -> `PacletSite
};


$pacletInfoSymbolConversions = Dispatch[{
	Sequence@@$reservedSymbol,
	Sequence@@$systemSymbol,
	Sequence@@$privateSymbol,
	v:_Real|_Integer :> ToString[v],
	s_Symbol :> SymbolName@s
}] 


(* ::Subsubsection:: *)
(*Get values*)


(* ::Text:: *)
(*Accept: Type-2 Paclet.*)


GetPacletValue[fields_][paclet_`Paclet] := GetPacletValue[paclet, fields]

GetPacletValue[paclet_`Paclet, field_String] := Replace[field, Join[List@@paclet, $defaultPacletValue]]

GetPacletValue[paclet_`Paclet, fields:{__String}] := GetPacletValue[paclet, #]& /@ fields

GetPacletValue[paclet_`Paclet, "QualifiedName"] := If[#2 == "",
	ExternalService`EncodeString[#1, "UTF-8"] <> "-" <> #3,
	ExternalService`EncodeString[#1, "UTF-8"] <> "-" <> #2 <> "-" <> #3
]& @@ GetPacletValue[paclet, {"Name", "Qualifier", "Version"}]

GetPacletValue[paclet_`Paclet, "MathematicaVersion"|"WolframVersion"] := List@@paclet //Query[{"MathematicaVersion", "WolframVersion"}] //Switch[#,
	{_Missing, _Missing}, "10+",
	{_Missing, _}, Last@#,
	{_, _Missing}, First@#,
	_, Null
]&

GetPacletValue[paclet_`Paclet, "FileName"] := GetPacletValue["QualifiedName"]@paclet <> ".paclet"

GetPacletValue[paclet_`Paclet, "VersionNumber"] := FromDigits /@ StringSplit[
	GetPacletValue["Version"]@paclet
, "."];


$defaultPacletValue = {
    "Extensions" -> {},
    "SystemID" -> All,
    "WolframVersion" -> "10+",
    "ProductName" -> All,
    "Qualifier" -> "",
    "Internal" -> False,
    "Root" -> ".",
    "BackwardCompatible" -> True,
    "BuildNumber" -> "",
    "Description" -> "",
    "InstallFromDocRequest" -> False,
    "ID" -> "",
    "Creator" -> "",
    "URL" -> "",
    "Publisher" -> "",
    "Support" -> "",
    "Category" -> "",
    "Thumbnail" -> "",
    "Copyright" -> "",
    "License" -> "",
    "Loading" -> Manual,
    "Updating" -> Manual,
    _ -> Null
}


(* ::Subsection:: *)
(*PacletSite*)


(* ::Subsubsection:: *)
(*Group*)


(* ::Text:: *)
(*Accept: Type-2 Paclet Collection.*)


GroupByValue[field_][_[paclets___`Paclet]] := GroupBy[GetPacletValue@field]@{paclets}


(* ::Subsubsection:: *)
(*Sort*)


(* ::Text:: *)
(*Accept: Type-2 Paclet Collection.*)
(*ReverseSort(>)*)


SortByVersion[_[paclets___`Paclet]] := ReverseSortBy[
	{paclets},
	GetPacletValue["VersionNumber"],
	OrderedQ@*PadRight@*List
]


(* ::Text:: *)
(*Accept: Paclet Collection.*)


SiteRegularize[_[paclets___`Paclet]] := `PacletSite @@ Catenate@Values@KeySort[
	SortByVersion /@ GroupByValue["Name"][PacletExpressionConvert[2] /@ {paclets}]
]
