#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -t 600
#SBATCH --output=./stdio/RDM_DDP_XL/PostProc/%A.out
#SBATCH -N 1
 
module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Running Post processing script in serial '
R CMD BATCH create_proj_file.R

