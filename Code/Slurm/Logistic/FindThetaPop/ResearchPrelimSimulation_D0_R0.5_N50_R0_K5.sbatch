#!/bin/bash
#SBATCH --job-name ResearchPrelimSimulation_D0_R0.5_N50_R0_K5
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 1-00:00
#SBATCH --mem-per-cpu=300
#SBATCH -o ClusterMessages/out/myscript_ResearchPrelimSimulation_D0_R0.5_N50_R0_K5_%j.out
#SBATCH -e ClusterMessages/error/myscript_ResearchPrelimSimulation_D0_R0.5_N50_R0_K5_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/FindThetaPop.R \
    --Delta 0 \
    --Rho 0.5 \
    --N 50 \
    --Gamma 0 \
    --K 5 \
    --TypeSetting 'Logistic' \
    --Output data/Logistic/EstimatedThetaPop/D0_R0.5_N50_R0_K5.csv \
