#!/bin/bash
#SBATCH --mem=500000
#SBATCH -t 2000
#SBATCH --output=./stdout/%A.out

module purge
module load java/1.8.0_60
module load R/4.0.0
module load git
module load gcc/9.3.0
module load intel-oneapi-tbb/2021.1.1-gcc-9.3.0
module load curl/7.47.1
module load libxml2/2.9.10

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
 
echo 'Running CART Script'
echo "Started at $(date)"
echo "nodes: $SLURM_JOB_NODELIST"
f="metrics_paper.R"
MetricsCodePath=$1
fpath="$MetricsCodePath$f"
output_sub_dir=$2
gcam_meta_scenario=$3
MetricsDir=$4
repo_path=$5
GMS_Noscore=$6
gcam_path="${repo_path}relationships/gcam"
mkdir -p "${MetricsDir}/output/${gcam_meta_scenario}/${output_sub_dir}"
echo "Rscript --vanilla $fpath --args $gcam_meta_scenario $output_sub_dir $gcam_path $MetricsDir $GMS_Noscore"
Rscript --vanilla $fpath --args $gcam_meta_scenario $output_sub_dir $gcam_path $MetricsDir $GMS_Noscore
echo "Ended at $(date)"
