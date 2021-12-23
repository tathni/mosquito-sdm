#!/bin/bash

job_file="bg_bias_mask.job"

echo "#!/bin/bash

#SBATCH  -p normal
#SBATCH --job-name=bg_bias_mask
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --time=48:00:00
#SBATCH --mail-type=ALL
#SBATCH --output bg_bias_mask.log


ml physics gdal udunits proj geos
ml R/4.0.2

Rscript ./3a-bg-data-clean.R " > $job_file

    sbatch $job_file
