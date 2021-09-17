#!/bin/bash
#SBATCH --job-name=portal_weekly_forecast
#SBATCH --mail-user=ethanwhite@ufl.edu
#SBATCH --mail-type=FAIL,END
#SBATCH --ntasks=1
#SBATCH --mem=2gb
#SBATCH --time=3:00:00
#SBATCH --partition=hpg2-compute
#SBATCH --output=portal_weekly_forecast_log.out

echo "INFO: [$(date "+%Y-%m-%d %H:%M:%S")] Starting Weekly Forecast on $(hostname) in $(pwd)"

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Loading required modules"
source /etc/profile.d/modules.sh
module load git R singularity

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Updating singularlity container"
singularity pull --force docker://weecology/portalcasting

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Updating forecasts repository"
cd forecasts
git fetch origin --prune --tags
git reset --hard origin/main

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Updating portalPredictions repository"
cd ../portalPredictions
git fetch origin --prune --tags
git reset --hard origin/main

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Running Portal Forecasts"
singularity run ../portalcasting_latest.sif Rscript PortalForecasts.R

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Checking if forecasts were successful"
# Redirect stderr(2) to stdout(1) if command fails, and exit script with 1
singularity run ../portalcasting_latest.sif Rscript tests/testthat/test-successful_forecasts.R > ../testthat.log 2>&1 || exit 1

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Archiving to GitHub and Zenodo"
singularity run ../portalcasting_latest.sif bash archive_hipergator.sh

echo "INFO [$(date "+%Y-%m-%d %H:%M:%S")] Checking if archiving to GitHub was successful"
singularity run ../portalcasting_latest.sif Rscript tests/testthat/test-forecasts_committed.R > ../testthat.log 2>&1 || exit 1
