#!/bin/bash
#SBATCH -t 600
#SBATCH -J output_inventory

# README -----------------------------------------------------------------------
#
# This script will run a python script that inventories slurm *.out files to see
# what time periods in each gcam run failed, and dumps a list of those runs. 
# The process is very quick, so this is done in serial. This script is designed 
# to be called from launch_rdm_all_jobs.sh.
# ------------------------------------------------------------------------------

# load modules
module load anaconda/3

echo "Started at $(date)"

# Read in cmd line inputs from meta script
repo_path=$1
gcam_meta_scenario=$2
output_sub_dir=$3
total_jobs=$4

# Identify files
raw_outpath="${repo_path}scripts/stdout/${gcam_meta_scenario}/${output_sub_dir}/"
PYFILE_EXTENSION="relationships/gcam/output/code/output_inventory.py"
PYFILE="$repo_path$PYFILE_EXTENSION"

echo "python $PYFILE --output_dir $raw_outpath --total_jobs $total_jobs --inventory 2"
python $PYFILE --output_dir $raw_outpath --total_jobs $total_jobs --inventory 2

echo "job completed."
