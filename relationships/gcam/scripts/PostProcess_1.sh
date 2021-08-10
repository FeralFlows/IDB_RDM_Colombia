#!/bin/bash
#SBATCH -p slurm
#SBATCH -t 600
#SBATCH --output=./stdout/%A.out
#SBATCH -N 1
 
module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Running first Post processing script in serial'
f="create_proj_file.R"
fpath="$1$f"
raw_outpath=$2
post_proc_outpath=$3

echo "Rscript --vanilla $fpath --args $raw_outpath $post_proc_outpath"
Rscript --vanilla $fpath --args $raw_outpath $post_proc_outpath
echo "Ended at $(date)"