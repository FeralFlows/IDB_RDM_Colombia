#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -p short,slurm,shared
#SBATCH -t 179
#SBATCH -J config
#SBATCH --output=./stdout/%A.out

# README -----------------------------------------------------------------------
#
# This script will run a python script that generates GCAM config files. This
# process is very quick, so this is done in serial. This script is designed to
# be called from launch_rdm_all_jobs.sh.
# ------------------------------------------------------------------------------

source /etc/profile.d/modules.sh
module load python/anaconda3.6
source /share/apps/python/anaconda3.6/etc/profile.d/conda.sh

echo "Started at $(date)"
# Read in cmd line inputs from meta script
repo_path=$1
gcam_meta_scenario=$2
output_sub_dir=$3
gcam_input_dir=$4
base_config_file=$5
base_alt_xml_dir=$6

PYFILE_EXTENSION="relationships/gcam/config/code/gcam_config_generator.py"
PYFILE="$repo_path$PYFILE_EXTENSION"

output_dir="${repo_path}relationships/gcam/config/output"

echo "python $PYFILE --scenarios $gcam_meta_scenario --base_dir $repo_path --base_gcam_dir $gcam_input_dir --base_config_file $base_config_file --base_alt_xml_dir $base_alt_xml_dir --output_dir $output_dir --output_sub_dir $output_sub_dir"
python $PYFILE --scenarios $gcam_meta_scenario --base_dir $repo_path --base_gcam_dir $gcam_input_dir --base_config_file $base_config_file --base_alt_xml_dir $base_alt_xml_dir --output_dir $output_dir --output_sub_dir $output_sub_dir
echo "job completed."
