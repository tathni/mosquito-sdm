#!/bin/bash
export IFS=","

cat 5d-species-list.csv | while read a; do

job_file="species${a}_ecoregions_intersect_test.job"


echo "#!/bin/bash

#SBATCH  -p dev
#SBATCH --job-name=spec${a}_ecoregions_test
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL
#SBATCH --output species${a}_ecoregions_intersect_test.log


ml physics gdal udunits proj geos
ml R/4.0.2

Rscript ./5a-ecoregions.R "$a" " > $job_file

    sbatch $job_file

done
