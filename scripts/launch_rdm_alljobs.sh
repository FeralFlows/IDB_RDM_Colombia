#!/bin/bash

# ------------------------------------------------------------------------------
# README
#
# This file executes the following steps:
#
# 1. Generates GCAM configuration files.
#
# 2. Executes a script (run-gcam-parallel-arrays.sh) separately for each batch of gcam runs. 
# Users must specify size of each batch, which cannot individually exceed
# 1000 runs. 
#
# 3. Executes a post-processing launch script (launch_post_processing.sh) 
# that executes a series of post-processing scripts.
#
# Author: Thomas B. Wild (thomas.wild@pnnl.gov)
#
# USAGE:
#
# sh launch_rdm_alljobs.sh
#
#------------------------------------------------------------------------------

# User to specify total number of GCAM runs to perform, and output dir
#total_jobs=4096
total_jobs=3
echo "total number of GCAM runs to perform: $total_jobs"

# User specify output dirs and paths
output_dir='05272021'
output_path='/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/outputs/raw/05272021/'

# User specify path to this meta-repo
repo_path='/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/'

# User specify scenario name
scenario=RDM_NoPolicy

# User to specify GCAM executable location.
gcam_exe_fpath=/pic/projects/GCAM/TomWild/GCAM-LAC/gcam-LAC-stash/exe/

# ensure output dir exists to avoid errors
mkdir -p $output_path

# launch script to generate config files
config_extension="relationships/gcam/config/scripts/gcam_config_generator.sh"
config_generator_path="$repo_path$config_extension"
jid1=$(sbatch $config_generator_path $repo_path $scenario | sed 's/Submitted batch job //')

# PIC has a max number of job arrays, which is currently 1000
#max_job_arrays=1000
max_job_arrays=2
# Calculate the number of groups of 1000, and any remainder to be dealt with
separate_groups=$((total_jobs/max_job_arrays))
remainder=$((total_jobs%max_job_arrays))
echo "number of groups: $((separate_groups+remainder))"

# Declare arrays that will store run number for each batch job
declare -a job_array_upper
declare -a start_point

# Loop through and populate matrix that specifies number of runs in each job
# and the indexed staring point for each job (e.g., 2000, 3000, etc.)
count=0
while [ $count -le $separate_groups ]
do
    if [[ $count -ne $separate_groups ]]; then
        job_array_upper[$count]=$max_job_arrays
    else
		if [[ $remainder -gt 0 ]]; then
			job_array_upper[$count]=$remainder
		fi
    fi
    start_point[$count]=$((count*max_job_arrays+1))
    count=$((count + 1))
done

# launch individual batches of gcam runs
count=0
run_gcam_script="relationships/gcam/scripts/run-gcam-parallel-arrays.sh"
run_gcam_script_path="$repo_path$run_gcam_script"
jid_str=""
for arr in ${job_array_upper[@]}; do
    arr_lower="1-"
    arr_upper="$arr"
    arr_string="$arr_lower$arr_upper"
    echo "sbatch --array=$arr_string --dependency=afterany:$jid1 $run_gcam_script_path $repo_path ${start_point[count]} $output_dir $output_path $scenario $gcam_exe_fpath"
    jid_n=$(sbatch --array=$arr_string --dependency=afterany:$jid1 $run_gcam_script_path $repo_path ${start_point[count]} $output_dir $output_path $scenario $gcam_exe_fpath | sed 's/Submitted batch job //')
    jid_str="$jid_str:$jid_n"
    count=$((count+1))
done

# launch post processing
PostProcDir="/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/scripts/"
PostProcFile="launch_post_processing.sh"
PostProcFpath="$PostProcDir$PostProcFile"
PostProcFn="/pic/projects/GCAM/TomWild/IDB_RDM_Colombia/relationships/gcam/outputs/code/"
echo "sh $PostProcFpath $PostProcDir $PostProcFn $jid_str"
sh $PostProcFpath $PostProcDir $PostProcFn $jid_str
