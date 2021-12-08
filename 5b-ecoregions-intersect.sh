#!/bin/bash
export IFS=","

cat species_list.csv | while read a; do 

job_file="species${a}_ecogeions_intersect.job"


echo "#!/bin/bash

#SBATCH  -p dev
#SBATCH --job-name=spec${a}_ecoregions
#SBATCH --nodes=1              
#SBATCH --ntasks-per-node=1      
#SBATCH --time=2:00:00
#SBATCH --mail-type=ALL
#SBATCH --output species${a}_ecogeions_intersect.log


ml R/4.0.2

Rscript ./5.2-ecoregion-diagnostics.R "$a" " > $job_file

    sbatch $job_file

done



