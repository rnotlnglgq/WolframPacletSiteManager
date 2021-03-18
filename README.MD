# Paclet Site Manager - Wolfram Language

Provide tools for building paclet sites, especially on a remote git repository.

## What's a paclet site

A paclet site is a (local or cloud) directory, with two necessary components in it:

* Directory `"Paclets"`: includes all paclet files.
* File `"PacletSite.mz"`: ZIP compressed, contains only one file:
	* File `"PacletSite.m"`: Wolfram Language package file, contains only one expression:
		* Expression: an expression that matches pattern`PacletSite[Paclet[___Rule]...]`.
			* The inner `Paclet` expressions are **almost** the same to those in the `PacletInfo.m` of your paclets - you can convert them to each other with ```PacletSiteManager`PacletExpressionConvert``` provided by this package.

## Environment Requirement

* Any **Wolfram Kernel** of **version 11+**. Only 12.0.0.0, 12.2.0.0 are tested.
* `wget` for downloading paclets from internet. If you have nothing to do with a remote paclet site, this is not needed.
* `cat`  shell command available for catenate files, `split` command for split files. Only cygwin on Windows 10 and Linux tested. *Paclet splitting is **not** supported by the official paclet manager, so usually you won't need this feature.*

## Installation

### One-shot

```mathematica
PacletInstall["PacletSiteManager", "Site" -> "https://github.com/rnotlnglgq/WolframPacletSiteManager/raw/master"]
```

### Register this site for further update

Register site (only once needed)

```mathematica
PacletManager`PacletSiteAdd["https://github.com/rnotlnglgq/WolframPacletSiteManager/raw/master"]
```

Install or update:

```mathematica
PacletInstall["PacletSiteManager"]
```

## Basic Usage

This paclet will be automatically loaded when its symbols are evaluated. And **it assumes the `Directory[]` has been set to the root path of the paclet site.** 

* To generate `PacletSite.mz` file from `Paclets` directory, simply put paclet files in the directory `Paclets/` and then evaluate `PutSiteInfo[]`.

* To search paclets from a paclet site, try `PacletSearch` and `PacletQuery`.

* To make your paclet site synchronous with a remote paclet site, write your requirement to the `$RequirementFile` (you can use `GetRequirementInfo[expr_]` to see its format and generate it from paclets). And then try `ListRequiredPaclet` and `ListPacletChanges`.

## Main Features

*The documentation is far from finished. Feel free to contact me if you need help.*

### Read and generate `PacletSite` expression

* `GetSiteInfo[1]` generates `PacletSite` expression from `"Paclets/*.paclet" -> "PacletInfo.*"`
* `GetSiteInfo[2]` reads `PacletSite` expression from `"PacletSite.mz"`
* `GetSiteInfo[3]` reads `PacletSite` expression from `"PacletSite.mz"`
* `GetSiteInfo[4]` reads `PacletSite` expression from `$RemotePacletSite` (default as `"https://pacletserver2.wolfram.com/PacletSite.mz"`)
* `PutSiteInfo[expr_]` exports `expr` to `FileNameJoin@{Directory[], "Paclets"}`
* `PutSiteInfo[i_Integer]` is equivalent to `PutSiteInfo@GetSiteInfo[i]` 

### Convert `Paclet` expressions

* `PacletExpressionConvert[i_Integer]` convert `Paclet` expressions to type-i:

	* Type-1 is for `"PacletSite.mz"`
	* Type-2 is for `PacletInfo.m` . Type-2 will be required by most functions in this package
	* Type-3 is in the `PacletObject` form.
	
	**Note**: the head of all of these expressions will be in the context ```PacletSiteManager`Private` ```, instead of ```System` ```or ```PacletManager` ``` !

### Symbols in the root context

```mathematica
{
	$Downloader,
    $VersionStringComplete,
    $RemotePacletSite,
    $PartSize,
    $RequirementFile,
    ApplyPacletChanges,
    BlockedExport,
    BlockedImport,
    BuildTreeBySeries,
    CatenateParts,
    DownloadCommand,
    DownloadPaclet,
    DownloadRequest,
    ExportURLList,
    GetPacletInfo,
    GetPacletValue,
    GetRequirementInfo,
    GetSiteInfo,
    GroupByValue,
    KernelVersionMatchQ,
    ListPacletChanges,
    ListRequiredPaclet,
    PacletExpressionConvert,
    PacletList,
    PacletPartList,
    PacletQuery,
    PacletSearch,
    PartsRegularize,
    PutRequirementInfo,
    PutSiteInfo,
    SiteRegularize,
    SortByVersion,
    SplitPaclet,
    ValidPacletFileQ,
    ValidPacletQ
}
```

## Development

### Features in plan

* Write platform requirements in the requirement file and then use it for more useful functions.

### Bug

* The `Format` output of ````Private`Paclet``` expressions disturb the  output display in a command-line environment.

## Note

* This package do not require any function from the official paclet manger.
* This package is developed due to the requirement of `WolframPacletMirror` for China users. Original written by asdasd1dsadsa.

