#!/bin/bash
#SBATCH -A stranded
#SBATCH -t 180
#SBATCH -N 1
 
job=$SLURM_JOB_NAME

module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Library config:'
ldd ./gcam.exe
 
date
time ./gcam.exe -C$job.xml -Llog_conf.xml
date