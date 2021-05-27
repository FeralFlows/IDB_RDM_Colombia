#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -t 179
#SBATCH -p short,slurm,shared
#SBATCH --output=./stdio/%A.out
#SBATCH -N 1
 
#module load git
#module load svn/1.8.13
#module load R/3.4.3
#module load java/1.8.0_31

module purge
module load gcc/8.1.0
module load R/3.4.3
module load intel

echo 'Running Post processing script in serial for the requisite metis queries'
echo "Started at $(date)"
proj_function_arg=1
f="create_query_proj_file_metis.R"
PostProcFn=$1
fpath="$PostProcFn$f"
output_path=$2
echo "Rscript --vanilla $fpath --args $proj_function_arg $PostProcFn $output_path"
Rscript --vanilla $fpath --args $proj_function_arg $PostProcFn $output_path
echo "Ended at $(date)"