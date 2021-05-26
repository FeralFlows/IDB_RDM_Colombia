#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -t 179
#SBATCH --output=./stdio/RDM_DDP_XL/PostProc/%A.out
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
plot_runner=create_query_proj_file_metis.R
echo "Rscript --vanilla $plot_runner --args $proj_function_arg"
Rscript --vanilla $plot_runner --args $proj_function_arg
echo "Ended at $(date)"