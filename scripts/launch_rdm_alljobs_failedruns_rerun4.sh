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
# 4. Launch CART Analytics
#
# 5. Conduct an inventory to see which runs completed
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
total_jobs=704  # 523
#total_jobs=10
echo "total number of GCAM runs to perform: $total_jobs"

# Which steps to perform--generate configs, perform gcam runs, and/or
# conduct post-processing. 0=No, 1=Yes
gen_config=0
run_gcam=1
post_proc=0
completion_check=0
cart_analysis=0
num_gcam_queries=29  # must be in xml query file. these will be parallelized over.
acct=llab
partition=llab

# Repo path, output dirs and paths, and scenario name
repo_path='/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/'
gcam_meta_scenario='RDM_Policy'  # scenarios upon which variations will be done.
GMS_Noscore='RDMPolicy'
#gcam_meta_scenario='RDM_NoPolicy'  # scenarios upon which variations will be done.
# specify a sub-dir within the meta-scenario output dir. Change this when you update 
# runs on a new date, and want to save old runs, for example. Used for both raw and
# post-processed outputs.
output_sub_dir='09242021-rerun4'
slurmoutname="./stdout/${gcam_meta_scenario}.${output_sub_dir}.%A.%a.out"
slurmoutname_serial="./stdout/${gcam_meta_scenario}.${output_sub_dir}.%A.out"

# PIC (and other clusters) have a max number of job arrays. On pic, it's 1000
max_job_arrays=1999
# Specify maximum number if simultaneously running tasks in the job array. 
# Place an empty string if you have no preference.
max_tasks=215
# Should job array groups (e.g., of 2000 each) proceed in serial? Only use if you
# need the max_tasks to hold as a hard cluster-wide requirement
dep_arrays=1

# GCAM executable, input files, and base configuration file paths
# Either of these can be located inside or outside of the repository.
gcam_exe_fpath=/cluster/tufts/lamontagnelab/byarla01/gcam-LAC-5.4/exe/
gcam_input_dir=/cluster/tufts/lamontagnelab/byarla01/gcam-LAC-5.4/input
base_config_file=/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/relationships/gcam/config/input/gcam_config_base_FFI_LUC_policy.xml
base_alt_xml_dir=/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/relationships/gcam/input
gcam_queries=/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/relationships/gcam/exe/rdm_queries_reduced.xml

#-------------------------------------------------------------------------------
# Step 2: Generate GCAM configuration files
#
# The user does not need to specify assumptions below this point unless they wish
# to make more advanced customizations to their workflow.

if [[ $gen_config -eq 1 ]]; then
	# launch script to generate config files
	config_extension="relationships/gcam/config/scripts/gcam_config_generator.sh"
	config_generator_path="$repo_path$config_extension"
	jid1=$(sbatch -A $acct $config_generator_path $repo_path $gcam_meta_scenario $output_sub_dir $gcam_input_dir $base_config_file $base_alt_xml_dir | sed 's/Submitted batch job //')
else
	echo "not generating GCAM configuration files per user specifications"
	jid1='None'
fi
#-------------------------------------------------------------------------------------
#
# Step 3: Run GCAM in Parallel
#
#jid_str=""
#if [[ $jid1 == "None" ]]; then
	
jid_n=$jid1
if [[ $run_gcam -eq 1 ]]; then
	# Calculate the number of groups of size max_job_arrays, and any remainder to be dealt with
	separate_groups=$((total_jobs/max_job_arrays))
	remainder=$((total_jobs%max_job_arrays))
	if [[ $remainder -gt 0 ]]; then
		separate_groups=$((separate_groups+1))
	fi
	echo "number of separate array-based jobs to be launched: $((separate_groups))"

	# Declare arrays that will store run number for each batch job
	declare -a job_array_upper
	declare -a start_point

	# Loop through and populate matrix that specifies number of runs in each job
	# and the indexed staring point for each job (e.g., 2000, 3000, etc.)
	count=0
	while [ $count -lt $separate_groups ]
	do
		if [[ $count -lt $((separate_groups-1)) ]]; then
			# not the last group!
			job_array_upper[$count]=$max_job_arrays
		else
			# you're in your last group		
			if [[ $remainder -gt 0 ]]; then
				# Create a job array to handle to remainder
				job_array_upper[$count]=$remainder
			else
				job_array_upper[$count]=$max_job_arrays
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
		arr_append="%$max_tasks"
		arr_string="$arr_lower$arr_upper$arr_append"
		if [[ $jid1 == "None" ]]; then
			echo "sbatch -A $acct --array=$arr_string --output=$slurmoutname $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath $gcam_queries"
			jid_n=$(sbatch -A $acct -p $partition --array=$arr_string --output=$slurmoutname $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath $gcam_queries | sed 's/Submitted batch job //')
		else
			jid2=$jid1:$jid_n
			echo "sbatch -A $acct --array=$arr_string --output=$slurmoutname --dependency=afterany:$jid_n $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath $gcam_queries"
			jid_n=$(sbatch -A $acct -p $partition --array=$arr_string --output=$slurmoutname --dependency=afterany:$jid_n $run_gcam_script_path $repo_path ${start_point[count]} $output_sub_dir $gcam_meta_scenario $gcam_exe_fpath $gcam_queries | sed 's/Submitted batch job //')
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
    echo "execute post-processing call"
    echo "sh $PostProcFpath $PostProcDir $PostProcFn $jid_str $output_sub_dir $gcam_meta_scenario $repo_path $postproc_array $acct $partition"
    sh $PostProcFpath $PostProcDir $PostProcFn $jid_str $output_sub_dir $gcam_meta_scenario $repo_path $postproc_array $acct $partition
else
    echo "not conducting post-processing per user specifications"
fi

#---------------------------------------------------------------------------------------------
# Step 5: launch CART Analysis
# TODO: insert dependency on completion of post-processing steps
if [[ $cart_analysis -eq 1 ]]; then
    MetricsDir="${repo_path}metrics"
    MetricsBatchDir="${repo_path}metrics/scripts/"
    MetricsBatch="launch_cart.sh"
    MetricsBatchFpath="$MetricsBatchDir$MetricsBatch"
    MetricsCodePath="${repo_path}metrics/code/"
    echo "execute metrics script"
    echo "sbatch -A $acct -p $partition $MetricsBatchFpath $MetricsCodePath $output_sub_dir $gcam_meta_scenario $MetricsDir $repo_path"
    jid_cart=$(sbatch -A $acct -p $partition $MetricsBatchFpath $MetricsCodePath $output_sub_dir $gcam_meta_scenario $MetricsDir $repo_path $GMS_Noscore | sed 's/Submitted batch job //')
else
    echo "not conducting CART analysis, per user specifications"
fi


#---------------------------------------------------------------------------------------------
# Step 6: See which runs completed (produced output files)
if [[ $completion_check -eq 1 ]]; then
    echo "execute script to check if any runs did not successfully finish"
    InventoryDir="${repo_path}relationships/gcam/scripts/"
    InventoryFile="run-output-inventory.sh"
    InventoryFpath="$InventoryDir$InventoryFile"	
    echo "sbatch -A $acct -p $partition --output=$slurmoutname_serial $InventoryFpath $repo_path $gcam_meta_scenario $output_sub_dir $total_jobs"
	jid_inventort=$(sbatch -A $acct -p $partition --output=$slurmoutname_serial $InventoryFpath $repo_path $gcam_meta_scenario $output_sub_dir $total_jobs | sed 's/Submitted batch job //')	
else
    echo "not evaluating iventory of outputs, per user specifications"
fi
