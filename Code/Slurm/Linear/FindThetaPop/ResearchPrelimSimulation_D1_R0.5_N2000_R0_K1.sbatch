#!/bin/bash
#SBATCH --job-name ResearchPrelimSimulation_D1_R0.5_N2000_R0_K1
#SBATCH --partition medium
#SBATCH --ntasks 1
#SBATCH --time 1-00:00
#SBATCH --mem-per-cpu=3000
#SBATCH -o ClusterMessages/out/myscript_ResearchPrelimSimulation_D1_R0.5_N2000_R0_K1_%j.out
#SBATCH -e ClusterMessages/error/myscript_ResearchPrelimSimulation_D1_R0.5_N2000_R0_K1_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/FindThetaPopLinearNew.R \
    --Delta 1 \
    --Rho 0.5 \
    --N 2000 \
    --Gamma 0 \
    --K 1 \
    --TypeSetting 'Linear' \
    --Output data/EstimatedThetaPop/D1_R0.5_N2000_R0_K1.csv \
