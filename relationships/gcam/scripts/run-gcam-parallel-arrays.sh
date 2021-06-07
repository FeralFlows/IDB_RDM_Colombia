#!/bin/bash
#SBATCH -A br21_wild566
#SBATCH -t 360
#SBATCH -p shared,slurm -N 1 --cpus-per-task 3
#SBATCH --output=./stdout/%A.%a.out
#SBATCH --error=./stdout/%A.%a.err

module purge
module load git
module load svn/1.8.13
module load R/3.4.3
module load java/1.8.0_31
module load gcc/8.1.0
 
echo 'Library config:'
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
ldd ./gcam.exe

# Identify GCAM config files for which GCAM runs will be performed, and where to place outputs
repo_path=$1
output_sub_dir=$3
gcam_meta_scenario=$4
config_extension="relationships/gcam/config/output/xml/$gcam_meta_scenario/$output_sub_dir/*.xml"
CONFIG_FILES_PATH="$repo_path$config_extension"
FILES=($CONFIG_FILES_PATH)
raw_outpath="${repo_path}relationships/gcam/output/raw/${gcam_meta_scenario}/${output_sub_dir}/"
# ensure output dir exists to avoid errors
mkdir -p $raw_outpath

# Assign task IDs for individual GCAM runs
NEW_TASK_ID=$(($2+$SLURM_ARRAY_TASK_ID-1))
FILE_INDEX=$((NEW_TASK_ID-1))
FILE=${FILES[$FILE_INDEX]}
echo "Adjusted Task ID: $NEW_TASK_ID"
echo "GCAM Config. File Index: $FILE_INDEX"
echo "GCAM Config. File Name: $FILE"

# Specify location of gcam executable and other relevant files
gcam_exe_fpath=$5  # path to gcam executable
date
cd $gcam_exe_fpath
exe_extension="relationships/gcam/exe/xmldb_batch_template.xml"
xmldb_batch="$repo_path$exe_extension"
xmldb_driver_extension="relationships/gcam/exe/XMLDBDriver.properties"
xmldb_driver_file="$repo_path$xmldb_driver_extension"
cp $xmldb_driver_file $gcam_exe_fpath 

# Run GCAM
cat $xmldb_batch | sed "s#__OUTPUT_NAME__#${raw_outpath}${gcam_meta_scenario}_${NEW_TASK_ID}.csv#" | ./gcam.exe -C$FILE -Llog_conf.xml
