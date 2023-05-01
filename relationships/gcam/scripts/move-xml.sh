#!/bin/bash
#SBATCH -t 600
#SBATCH -p llab
#SBATCH -J move_files
#SBATCH --output='./stdout/sbatchout.out'

echo "Started at $(date)"
while read line
destination_dir='/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/relationships/gcam/config/output/xml/RDM_Policy/09242021-rerun4'
do
   echo "Copying file: ${line}.xml"
   origin_file="/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/relationships/gcam/config/output/xml/RDM_Policy/09242021/${line}.xml"
   cp $origin_file $destination_dir
done < "/cluster/tufts/lamontagnelab/byarla01/IDB_RDM_Colombia/relationships/gcam/output/raw/RDM_Policy/09242021/inventory/inventory_failed_runs.csv"
echo "job completed."
