#!/bin/bash
export IFS=","

cat 4d-species-list.csv | while read a; do

job_file="species${a}_xgboost_run_dev.job"


echo "#!/bin/bash

#SBATCH  -p dev
#SBATCH --job-name=spec${a}_xgboost_run_dev
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=2
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL
#SBATCH --output species${a}_xgboost_run_dev.log


ml physics gdal udunits proj geos
ml R/4.0.2

Rscript ./15a-xgboost-model.R "$a" " > $job_file

    sbatch $job_file

done
