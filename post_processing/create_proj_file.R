library('rgcam')
prj <- loadProject('C:/users/twild/downloads/all.proj')
rgcam::addMIBatchCSV('../output/FinalRuns/IDB_RDM/all_results.csv', '../output/FinalRuns/IDB_RDM/test.proj')