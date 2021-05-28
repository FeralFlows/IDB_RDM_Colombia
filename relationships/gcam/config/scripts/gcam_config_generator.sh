#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -p short,slurm,shared
#SBATCH -t 179
#SBATCH -J config
#SBATCH --output=./stdout/%A.out

# README -----------------------------------------------------------------------
#
# This script will run a python script that generates GCAM config files. This
# process is very quick, so this is done in serial.
# ------------------------------------------------------------------------------

source /etc/profile.d/modules.sh
module load python/anaconda3.6
source /share/apps/python/anaconda3.6/etc/profile.d/conda.sh

echo "Started at $(date)"
repo_path=$1
scenarios=$2
PYFILE_EXTENSION="relationships/gcam/config/code/gcam_config_generator.py"
PYFILE="$repo_path$PYFILE_EXTENSION"

base_dir=/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/
base_gcam_dir=/pic/projects/GCAM/TomWild/GCAM-LAC/gcam-LAC-stash/input
base_config_file=/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/config/input/gcam_config_base_nopolicy.xml
base_alt_xml_dir=/pic/projects/GCAM/TomWild/GCAM-LAC/gcam-LAC-stash/input/idb_5.3/rdm/XL_category_files
output_dir=/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/config/output

echo "python $PYFILE $scenarios $base_dir $base_gcam_dir $base_config_file $base_alt_xml_dir $output_dir"
python $PYFILE --scenarios $scenarios --base_dir $base_dir --base_gcam_dir $base_gcam_dir --base_config_file $base_config_file --base_alt_xml_dir $base_alt_xml_dir --output_dir $output_dir
echo "job completed."
