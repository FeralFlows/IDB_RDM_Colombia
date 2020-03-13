#!/bin/bash
#SBATCH -A stranded
#SBATCH -t 180
#SBATCH -N 1 --cpus-per-task 3
#SBATCH --output=%A.%a.out
#SBATCH --error=%A.%a.err
#SBATCH --array=1-50
 
module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Library config:'
echo "My SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
ldd ./gcam.exe

FILES=(/qfs/people/wild566/IDB/Final/gcam-core/exe/UncertainFactorsXML/*)
FILE=${FILES[$SLURM_ARRAY_TASK_ID-1]}
 
date
cat xmldb_batch_template.xml | sed "s/__OUTPUT_NAME__/${SLURM_ARRAY_TASK_ID}.csv/" | ./gcam.exe -C$FILE -Llog_conf.xml
date
R CMD BATCH create_proj_file.R
## This creates a single CSV from all the individual queries. Not currently necessary.
#cat ../output/FinalRuns/IDB_RDM/*.csv > ../output/FinalRuns/IDB_RDM/all_results.csv
#Rscript create_proj_file.R
