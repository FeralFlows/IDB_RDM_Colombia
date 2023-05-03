#!/bin/bash

# ------------------------------------------------------------------------------
# README
#
# This file executes three separate post-processing steps. Each of those three
# steps has its own associated shell script that this meta-script launches. Each
# successive script can only be launched when the previous script is completed,
# so we use dependencies to ensure each step is completed before the next step can
# proceed. This series of scripts is not designed to handle GCAM DBs, only .csv files.
# The user should not need to run this script by itself--ideally this script should
# be executed as part of a bigger pipeline being run from launch_rdm_alljobs.sh or
# similar.
#
# Author: Thomas B. Wild (thomas.wild@pnnl.gov)
#
# USAGE:
#
# sh launch_post_processing.sh
#
#------------------------------------------------------------------------------

module purge
module load java/1.8.0_60
module load R/3.4.3
module load git
module load gcc/9.3.0
module load intel-oneapi-tbb/2021.1.1-gcc-9.3.0

export CXX=g++
export BOOST_INCLUDE=/cluster/tufts/lamontagnelab/byarla01/libs/boost_1_67_0
export BOOST_LIB=/cluster/tufts/lamontagnelab/byarla01/libs/boost_1_67_0/stage/lib
export XERCES_INCLUDE=/cluster/tufts/lamontagnelab/byarla01/libs/xercesc/include
export XERCES_LIB=/cluster/tufts/lamontagnelab/byarla01/libs/xercesc/lib 
export JARS_LIB=/cluster/tufts/lamontagnelab/byarla01/libs/jars/*
export JAVA_INCLUDE=${JAVA_HOME}/include
export JAVA_LIB=${JAVA_HOME}/jre/lib/amd64/server

export EIGEN_INCLUDE=/cluster/tufts/lamontagnelab/byarla01/libs/eigen

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/shared/ansys_inc/v193/v195/tp/qt/5.9.6/linx64/lib/


echo 'Launching post-processing meta script'

# Identify input and output file locations and ensure output dirs exist
output_sub_dir=$4
gcam_meta_scenario=$5
repo_path=$6
postproc_array=$7
acct=$8
partition=$9
slurmoutname_serial=$10

raw_outpath="${repo_path}relationships/gcam/output/raw/${gcam_meta_scenario}/${output_sub_dir}/"
post_proc_outpath="${repo_path}relationships/gcam/output/post_processed/${gcam_meta_scenario}/${output_sub_dir}/"
# ensure post-processing output dir exists to avoid errors
mkdir -p $post_proc_outpath
PostProcFn=$2
gcam_dependencies=$3

# Launch series of three dependent post-processing scripts

f1="PostProcess_1.sh"
fpath1="$1$f1"
if [[ $gcam_dependencies == "None" ]]; then
    echo "no GCAM run dependencies exist, beginning post-processing"
    jid1=$(sbatch -A $acct -p $partition $fpath1 $PostProcFn $raw_outpath $post_proc_outpath | sed 's/Submitted batch job //')
else
    echo "GCAM run dependencies exist, waiting to conduct post-processing"
    echo "$gcam_dependencies"
    jid1=$(sbatch -A $acct -p $partition --output=$slurmoutname_serial --dependency=afterany$gcam_dependencies $fpath1 $PostProcFn $raw_outpath $post_proc_outpath | sed 's/Submitted batch job //')
fi

f2="PostProcess_2.sh"
fpath2="$1$f2"
jid2=$(sbatch -A $acct -p $partition --dependency=afterany:$jid1 --array=$postproc_array $fpath2 $PostProcFn $post_proc_outpath | sed 's/Submitted batch job //')
#jid2=$(sbatch -A $acct -p $partition --output=$slurmoutname_serial --array=$postproc_array $fpath2 $PostProcFn $post_proc_outpath | sed 's/Submitted batch job //')


f3="PostProcess_3.sh"
fpath3="$1$f3"
jid3=$(sbatch -A $acct -p $partition --dependency=afterany:$jid2 $fpath3 $PostProcFn $post_proc_outpath)
#jid3=$(sbatch -A $acct -p $partition $fpath3 $PostProcFn $post_proc_outpath)
