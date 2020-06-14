library('rgcam')
files <- list.files('../output/FinalRuns/IDB_RDM/05242020', pattern = "*.csv", full.names=TRUE)
files
proj <- mergeProjects("../output/FinalRuns/IDB_RDM/05242020/05242020.proj", lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))