#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -t 360
#SBATCH -p shared,slurm -N 1 --cpus-per-task 3
#SBATCH --output=./stdout/%A.%a.out
#SBATCH --error=./stdout/%A.%a.err

module purge
module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Library config:'
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
ldd ./gcam.exe

repo_path=$1
config_extension="relationships/gcam/config/output/xml/*.xml"
CONFIG_FILES_PATH="$repo_path$config_extension"
FILES=($CONFIG_FILES_PATH)
NEW_TASK_ID=$(($2+$SLURM_ARRAY_TASK_ID-1))
FILE_INDEX=$((NEW_TASK_ID-1))
FILE=${FILES[$FILE_INDEX]}
echo "Adjusted Task ID: $NEW_TASK_ID"
echo "GCAM Config. File Index: $FILE_INDEX"
echo "GCAM Config. File Name: $FILE"
outdir="$3/"
mkdir -p $4
scenario=$5
date
cd $6; pres=$(pwd); echo "$pres"
exe_extension="relationships/gcam/exe/xmldb_batch_template.xml"
xmldb_batch="$repo_path$exe_extension"
cat $xmldb_batch | sed "s#__OUTPUT_NAME__#${outdir}${scenario}_${NEW_TASK_ID}.csv#" | ./gcam.exe -C$FILE -Llog_conf.xml
