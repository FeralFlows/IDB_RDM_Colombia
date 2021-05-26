#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -t 360
#SBATCH -p shared,slurm -N 1 --cpus-per-task 3
#SBATCH --output=./stdio/RDM_DDP_XL/%A.%a.out
#SBATCH --error=./stdio/RDM_DDP_XL/%A.%a.err

module purge
module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Library config:'
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
ldd ./gcam.exe

FILES=(/qfs/people/wild566/IDB/Final/gcam-core/exe/RDM/UncertainFactorsXML/RDM_DDP_XL/*.csv)
NEW_TASK_ID=$(($1+$SLURM_ARRAY_TASK_ID-1))
FILE_INDEX=$((NEW_TASK_ID-1))
FILE=${FILES[$FILE_INDEX]}
echo "Adjusted Task ID: $NEW_TASK_ID"
echo "GCAM Config. File Index: $FILE_INDEX"
echo "GCAM Config. File Name: $FILE"
scenario=RDM_DDP_XL
mkdir -p $3
outdir="$2/"
date
cat xmldb_batch_template.xml | sed "s#__OUTPUT_NAME__#${outdir}${scenario}_${NEW_TASK_ID}.csv#" | ./gcam.exe -C$FILE -Llog_conf.xml
