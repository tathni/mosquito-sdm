#!/bin/bash
export IFS=","

cat 5c-species-list.csv | while read a; do

job_file="species${a}_ecoregions_intersect.job"


echo "#!/bin/bash

#SBATCH  -p normal
#SBATCH --job-name=spec${a}_ecoregions
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1      
#SBATCH --time=48:00:00
#SBATCH --mail-type=ALL
#SBATCH --output species${a}_ecoregions_intersect.log


ml R/4.0.2

Rscript ./5a-ecoregions.R "$a" " > $job_file

    sbatch $job_file

done
