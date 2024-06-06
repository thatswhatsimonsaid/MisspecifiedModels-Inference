# PhD. Research Preliminary Exam (2024)

Implementation of Abadie, Imbens, Zheng (2014). Inference for Misspecified Models With Fixed Regressors.

This is the code for my Ph.D. Preliminary Examination at the Department of Statistics at University of Washington.

## Setup

All simulations were performed in R v4.3.1.

```R
install.packages("stats")
install.packages("tidyverse")
install.packages("StatMatch")
install.packages("xtable")
install.packages("lmtest")
install.packages("sandwich")
install.packages("mixtools")
install.packages("distr")
install.packages("ggplot2")
```

## Implementation
To see the results of the simulation, open and run the`SimulationResults.Rmd` file. The results are at the end. This section will describe the functions used to run the simulations.

The following list contains the scripts used in the simulations/application.
- `ConfidenceIntervalFunction.R`checks for containment of the population and conditional estimands based on the population and conditional variances.
- `OneIterationFunction.R`runs one iteration of the simulation.
- `RunSimulationStudy.R`runs the simulation function `SimulationFunction.R`with input from the terminal.
- `SimDataLinear.R`simulates the linear regression data in the simulation.
- `SimDataLogistic.R`simulates the logistic regression data in the simulation.
- `SimulationFunction.R`runs the `OneIterationFunction.R` $NSim$ many times.
- `ThetaCondFunction.R`calculates the conditional estimand from the data.
- `ThetaPopFunction.R`extracts the population estimand for the given case found numerically from `FindThetaPop`.
- `VHatCondFunction.R`calculates the conditional standard error estimate.
- `VHatPopFunction.R`calculates the population standard error estimate.
- `WhichMinFunction.R`finds the nearest-neighbor observation.
- `RunSimulationStudy.Rmd` runs the simulation locally (as opposed to the high-performance computing cluster).

The following list contains the scripts used to compare and analyze the results of the simulation/application.
- `SimulationResults.Rmd` analyzes the extension with large sample sizes and provides the results.
- `Application.Rmd` compares my results with those of the original manuscript for the Sachs and Warner (1997) data set.
- `CompareSimulationStudy.Rmd` compares my results with those of the original manuscript for the simulation study.
- `DiscrepancyFunction.R`checks for the discrepancy/differences between my results and those of the original manuscript.
- `MonteCarloResults.R`extracts and reformats the monte carlo results from the simulations.
- `ReformatApplicationData.R`reformats the application data into the format of the original manuscript.
- `SimulationReformatResultsFunction.R`reformats the results of the simulation into the format of the original manuscript.
- `SimulationResults.R` provides the results of my simulation in the same format of the original manuscript.

The following list contains the auxiliary scripts that are not directly used in the simulation but are necessary to set up the simulation.
- `AIZResults.R`extracts the results from the original manuscript. 
- `CreateParameterVectorFTP.R`creates the `.csv`file that contain the parameter vectors used in the simulation.
- `FindThetaPop.R`finds the population and conditional estimands numerically, as indicated by the original manuscript.
- `MakeSbatchFilesFindThetaPop.R`creates the SLURM `.sbatch` files to find the population and conditional estimands numerically.
- `MakeSbatchFilesSimulations.R`creates the SLURM `.sbatch` files to run the simulation.
- `MergeThetaPop.R` and `MergeThetaPop.Rmd` merges the results of `FindThetaPop` into one `.rds` file.

## Running

### Simulated data

The numerically calculated $\theta_{Pop}$ is already in the folder `~data/Linear/EstimatedThetaPop/EstimatedThetaPop.rds.` If you wish to recalculate $\theta_{Pop}$ for each simulation case, go to `~/Code/Slurm/Linear/FindThetaPop/` and type `./run.sh` in the terminal. Note that `FindThetaPop.R` is currently set to run over one million replications.

All simulations are seeded. They should be reproducible.

```shell
cd ~/Stats572
module load R
Rscript Code/RunSimulationStudy.R \
    --Delta 0 \
    --Rho 0 \
    --N 50 \
    --Gamma 0 \
    --K 1 \
    --TypeSetting 'Linear' \
    --Output Results/Linear/N50_200/_D0_R0_N50_G0_K1.rds \
```
The above command is an example of running the most basic simulation scenario in the terminal. To replicate all results in the original manuscript and my slight extension, go to `~/Code/SLURM/Linear/RunSimulation/` and type `./run.sh`. Note that `RunSimulationStudy.R` is currently set to run over fifty-thousand replications.

Arguments:
- `--Delta` - Misspecification rate {0,1}
- `--Rho` - High leverage rate {0,0.1}
- `--N` - Sample size for each replication {50,200,350,...,1850, 2000}
- `--Gamma` - Heteroscedasticity rate {0,0.5}
- `--K` - Number of covariates {1,5}
- `--TypeSetting` - Linear or logistic {"Linear", "Logistic"}
- `--Output` - File to store output

The same can be done in the logistic setting by changing `--TypeSetting` to `"Logistic"`, though there are are no large sample extensions in that setting. 

### Sachs and Warner (1997) dataset

The dataset was obtained from https://www.bristol.ac.uk/Depts/Economics/Growth/sachs.htm. This is locally available in the `data/SachsWarner` directory under `sachs.csv`. Note that the website refers the reader to a more recent data set at Harvard's Center for International Development (CID). However, when contacting CID and Harvard Dataverse, they were unaware of any data set from their 1997 paper. 

The data from Sachs and Warner (1995), available on Harvard Dataverse, should not be confused with the 1997 dataset used in this report.
