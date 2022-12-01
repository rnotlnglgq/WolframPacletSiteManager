#!/usr/bin/env wolframscript
(* ::Package:: *)

SetDirectory@If[$Notebooks, NotebookDirectory[], Directory[]]


paclet = CreatePacletArchive["Source/PacletSiteManager", "Paclets"]


PacletManager`BuildPacletSiteFiles@Directory[]


PacletInstall[paclet, ForceVersionInstall -> True]


<< PacletSiteManager`


ResetDirectory[]
