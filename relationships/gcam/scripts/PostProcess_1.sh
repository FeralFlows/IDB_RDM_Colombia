#!/bin/bash
#SBATCH -t 600
#SBATCH --output=./stdout/%A.out
#SBATCH --mem=500000
#SBATCH --cpus-per-task 72

module purge
module load java/1.8.0_60
module load R/4.0.0
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
 
echo 'Running first Post processing script in serial'
f="create_proj_file.R"
fpath="$1$f"
raw_outpath=$2
post_proc_outpath=$3

echo "Rscript --vanilla $fpath --args $raw_outpath $post_proc_outpath"
Rscript --vanilla $fpath --args $raw_outpath $post_proc_outpath
echo "Ended at $(date)"