#!/bin/bash
#SBATCH --job-name PrelimSim_Linear_D0_R0_N2000_G0.1_K5
#SBATCH --partition short
#SBATCH --ntasks 1
#SBATCH --time 0-12:00
#SBATCH --mem-per-cpu=3000
#SBATCH -o ClusterMessages/out/myscript_PrelimSim_Linear_D0_R0_N2000_G0.1_K5_%j.out
#SBATCH -e ClusterMessages/error/myscript_PrelimSim_Linear_D0_R0_N2000_G0.1_K5_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=simondn@uw.edu

cd ~/Stats572
module load R
Rscript Code/RunSimulationStudyNew.R \
    --Delta 0 \
    --Rho 0 \
    --N 2000 \
    --Gamma 0.1 \
    --K 5 \
    --TypeSetting 'Linear' \
    --Output Results/Linear/N500_2000/sim_D0_R0_N2000_G0.1_K5.rds \
