library('rgcam')
files <- list.files('../output/FinalRuns/IDB_RDM/05252021', pattern = "*.csv", full.names=TRUE)
#options("rgcam.saved_compressed" = FALSE)
#files <- list.files('/pic/projects/GCAM/TomWild/output/FinalRuns/IDB_RDM/04052021', pattern = "*.csv", full.names=TRUE)
#proj <- mergeProjects('/pic/projects/GCAM/TomWild/output/FinalRuns/IDB_RDM/04052021/QueryResults.proj', lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))
proj <- mergeProjects("../output/FinalRuns/IDB_RDM/05252021/QueryResults.proj", lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))
quit(save = "no")  # prevents pic from trying to save and reload workspaces, which sucks up time and memory