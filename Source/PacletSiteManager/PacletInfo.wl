(* ::Package:: *)

Paclet[
	Name -> "PacletSiteManager",
	Version -> "0.0.1", (* Update Downloader Support *)
	WolframVersion -> "11+", (* Most versions are not tested. 12.0+ recommended. *)
	Description -> "Provide tools for building PacletSite, especially on a remote git repository.",
	Root -> ".",
	Loading -> Automatic,
	Extensions -> {
		{
			"Kernel",
			Root -> ".",
			Context -> "PacletSiteManager`",
			Symbols -> {
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
		(* Select[Names["PacletSiteManager`*"], Capitalize@# === # &@ StringTake[#, 1] &]//StringRiffle[#,"\",\n\t\t\t\t\""]& *)
		}
	}
]
