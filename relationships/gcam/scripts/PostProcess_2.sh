#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -p short,slurm,shared
#SBATCH -t 179
#SBATCH --output=./stdout/%A.%a.out
#SBATCH --array=1-27
#SBATCH -N 1
 
#module load git
#module load svn/1.8.13
#module load R/3.4.3
#module load java/1.8.0_31

module purge
module load gcc/8.1.0
module load R/3.4.3
module load intel
 
echo 'Running Post processing script in parallel for each query'
echo "Started at $(date)"
echo "nodes: $SLURM_JOB_NODELIST"
tid=$SLURM_ARRAY_TASK_ID
proj_function_arg=1
echo "My SLURM_ARRAY_TASK_ID: $tid"
f="create_query_proj_file.R"
PostProcFn=$1
fpath="$PostProcFn$f"
post_proc_outpath=$2
echo "Rscript --vanilla $fpath --args $tid $proj_function_arg $PostProcFn $post_proc_outpath"
Rscript --vanilla $fpath --args $tid $proj_function_arg $PostProcFn $post_proc_outpath
echo "Ended at $(date)"