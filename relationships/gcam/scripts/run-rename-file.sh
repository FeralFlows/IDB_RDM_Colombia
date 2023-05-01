#!/bin/bash
#SBATCH -t 600
#SBATCH -J output_inventory

# README -----------------------------------------------------------------------
#
# This script will run a python script that checks each gcam output csv file
# to see if the scenario name in the filename matches the scenario name listed
# in the contents of the file. If they don't match, the filename is renamed
# to match the scenario name mentioned in the contents of the file. 
# ------------------------------------------------------------------------------

# load modules
module load anaconda/3

echo "Started at $(date)"

# Read in cmd line inputs from meta script
repo_path=$1
gcam_meta_scenario=$2
output_sub_dir=$3

# Identify files
raw_outpath="${repo_path}relationships/gcam/output/raw/${gcam_meta_scenario}/${output_sub_dir}/"
PYFILE_EXTENSION="relationships/gcam/output/code/output_inventory.py"
PYFILE="$repo_path$PYFILE_EXTENSION"

echo "python $PYFILE --output_dir $raw_outpath --gcam_meta_scenario $gcam_meta_scenario --inventory 3"
python $PYFILE --output_dir $raw_outpath --gcam_meta_scenario $gcam_meta_scenario --inventory 3

echo "job completed."
