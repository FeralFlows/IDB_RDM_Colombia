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
# Step 1: User specify main assumptions

# Total number of GCAM runs to perform
total_jobs=6
#total_jobs=10
echo "total number of GCAM runs to perform: $total_jobs"

# Which steps to perform--generate configs, perform gcam runs, and/or
# conduct post-processing. 0=No, 1=Yes
gen_config=0
run_gcam=1
post_proc=1
num_gcam_queries=29  # must be in xml query file. these will be parallelized over.

# Repo path, output dirs and paths, and scenario name
repo_path='/qfs/people/wild566/rdm_test/IDB_RDM_Colombia/'
gcam_meta_scenario='RDM_Policy'  # scenarios upon which variations will be done.
#gcam_meta_scenario='RDM_NoPolicy'  # scenarios upon which variations will be done.
# specify a sub-dir within the meta-scenario output dir. Change this when you update 
# runs on a new date, and want to save old runs, for example. Used for both raw and
# post-processed outputs.
output_sub_dir='08042021'
#output_sub_dir='06252021_no_cprice'
slurmoutname="./stdout/${gcam_meta_scenario}.${output_sub_dir}.%A.%a.out"

# GCAM executable, input files, and base configuration file paths
# Either of these can be located inside or outside of the repository.
gcam_exe_fpath=/qfs/people/wild566/rdm_test/gcam-LAC-stash/exe/
gcam_input_dir=/qfs/people/wild566/rdm_test/gcam-LAC-stash/input
base_config_file=/qfs/people/wild566/rdm_test/IDB_RDM_Colombia/relationships/gcam/config/input/gcam_config_base_FFI_LUC_policy.xml
base_alt_xml_dir=/qfs/people/wild566/rdm_test/IDB_RDM_Colombia/relationships/gcam/input

#-------------------------------------------------------------------------------
# Step 2: Generate GCAM configuration files
#
# The user does not need to specify assumptions below this point unless they wish
# to make more advanced customizations to their workflow.

if [[ $gen_config -eq 1 ]]; then
	# launch script to generate config files
	config_extension="relationships/gcam/config/scripts/gcam_config_generator.sh"
	config_generator_path="$repo_path$config_extension"
	jid1=$(sbatch $config_generator_path $repo_path $gcam_meta_scenario $output_sub_dir $gcam_input_dir $base_config_file $base_alt_xml_dir | sed 's/Submitted batch job //')
else
	echo "not generating GCAM configuration files per user specifications"
	jid1='None'
fi
#-------------------------------------------------------------------------------------
#
# Step 3: Run GCAM in Parallel
#
jid_str=""
if [[ $run_gcam -eq 1 ]]; then
	# PIC has a max number of job arrays, which is currently 1000
	max_job_arrays=1000
	# Calculate the number of groups of 1000, and any remainder to be dealt with
	separate_groups=$((total_jobs/max_job_arrays))
	remainder=$((total_jobs%max_job_arrays))
	echo "number of separate array-based jobs to be launched: $((separate_groups+1))"

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
	for arr in ${job_array_upper[@]}; do
		arr_lower="1-"
		arr_upper="$arr"
		arr_string="$arr_lower$arr_upper"
		if [[ $jid1 == "None" ]]; then
			echo "sbatch --array=$arr_string --output=$slurmoutname $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath"
			jid_n=$(sbatch --array=$arr_string --output=$slurmoutname $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath | sed 's/Submitted batch job //')
		else
			echo "sbatch --array=$arr_string --output=$slurmoutname --dependency=afterany:$jid1 $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath"
			jid_n=$(sbatch --array=$arr_string --output=$slurmoutname --dependency=afterany:$jid1 $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath | sed 's/Submitted batch job //')
		fi
		jid_str="$jid_str:$jid_n"
		count=$((count+1))
	done
else
    echo "not conducting gcam runs per user specifications"
    jid_str='None'
fi

#---------------------------------------------------------------------------------------------
# Step 4: launch post processing
if [[ $post_proc -eq 1 ]]; then
    PostProcDir="${repo_path}relationships/gcam/scripts/"
    PostProcFile="launch_post_processing.sh"
    PostProcFpath="$PostProcDir$PostProcFile"
    PostProcFn="${repo_path}relationships/gcam/output/code/"
    postproc_array="1-${num_gcam_queries}"
    echo "sh $PostProcFpath $PostProcDir $PostProcFn $jid_str $output_sub_dir $gcam_meta_scenario $repo_path $postproc_array"
    sh $PostProcFpath $PostProcDir $PostProcFn $jid_str $output_sub_dir $gcam_meta_scenario $repo_path $postproc_array
else
    echo "not conducting post-processing per user specifications"
fi