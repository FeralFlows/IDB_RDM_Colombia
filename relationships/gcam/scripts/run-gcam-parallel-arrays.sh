#!/bin/bash
#SBATCH -t 5000
#SBATCH --cpus-per-task 3
#SBATCH --mem=20000

module purge
module load java/1.8.0_60
module load R/4.0.0
module load git
module load gcc/9.3.0
module load intel-oneapi-tbb/2021.1.1-gcc-9.3.0

##SBATCH -n 1
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

echo 'Library config:'
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID

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
cd $gcam_exe_fpath
exe_extension="relationships/gcam/exe/xmldb_batch_template.xml"
xmldb_batch="$repo_path$exe_extension"
xmldb_driver_extension="relationships/gcam/exe/XMLDBDriver.properties"
xmldb_driver_file="$repo_path$xmldb_driver_extension"
gcam_queries=$6
# cp $xmldb_driver_file $gcam_exe_fpath 

# Run GCAM
echo "run gcam"
date
ldd ./gcam.exe
echo "$xmldb_batch | sed "s#__OUTPUT_NAME__#${raw_outpath}${gcam_meta_scenario}_${NEW_TASK_ID}.csv#" | sed "s#__GCAM_Queries__#${gcam_queries}#" | ./gcam.exe -C$FILE -Llog_conf_mod.xml"
cat $xmldb_batch | sed "s#__OUTPUT_NAME__#${raw_outpath}${gcam_meta_scenario}_${NEW_TASK_ID}.csv#" | sed "s#__GCAM_Queries__#${gcam_queries}#" | ./gcam.exe -C$FILE -Llog_conf_mod.xml
