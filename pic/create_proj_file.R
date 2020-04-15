#library('devtools')
#install_github('JGCRI/rgcam', build_vignettes=TRUE)
library('rgcam')
files <- list.files('../output/FinalRuns/IDB_RDM', pattern = "*.csv", full.names=TRUE)
files
# Note the "temp", saveProj = FALSE
# this is to avoid saving a project file for each individual .csv in addition to all.proj
proj <- mergeProjects("../output/FinalRuns/IDB_RDM/allResults.proj", lapply(files, addMIBatchCSV, "temp", saveProj = FALSE))
# rgcam::addMIBatchCSV('../output/FinalRuns/IDB_RDM/all_results.csv', '../output/FinalRuns/IDB_RDM/03202020.dat')