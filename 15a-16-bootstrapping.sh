#!/bin/bash
#SBATCH --job-name=mos_sdm
#SBATCH --time=96:00:00
#SBATCH --qos long
#SBATCH -p normal
#SBATCH -c 16
#SBATCH --mem=128GB

ml math
ml gsl/2.7
ml system libressl/2.5.3 curl/7.54.0
ml physics gdal/2.2 udunits proj/4.9.3 geos
ml R/4.0

Rscript 15a-16-xgboost-model-figure-dfs.R
