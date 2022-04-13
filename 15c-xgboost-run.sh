#!/bin/bash
export IFS=","

cat 4d-species-list.csv | while read a; do

job_file="species${a}_xgboost_run.job"


echo "#!/bin/bash

#SBATCH  -p normal
#SBATCH --job-name=spec${a}_xgboost_run
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --time=48:00:00
#SBATCH --mail-type=ALL
#SBATCH --output species${a}_xgboost_run.log


ml physics gdal udunits proj geos
ml R/4.0.2

Rscript ./15a-xgboost-model.R "$a" " > $job_file

    sbatch $job_file

done
