### Summary:
# A script to generate the .sbatch files used to numerically calculate the
# population estimand in the high-performance computing cluster.
### Inputs:
# dir: Directory
# typeSetting: Linear/Logistic
# ParameterVectorALL.csv: Vector of parameters to find the
#                         the population estimand for.
### Output:
# Generates one .sbatch file for each parameter combination. Each .sbatch file
# will be sent to the high-performance computing cluster to run a simulation
# for each of the parameter combinations.

# Read CSV file
rm(list=ls())
dir = "/Users/simondn/Documents/Stats572"
TypeSetting = "Logistic"
ParameterVector <- read.csv(paste0(dir,
                                   "/data/",
                                   TypeSetting,
                                   "/Parameters/ParameterVectorALL.csv"))

# Loop through each row
for (i in 1:nrow(ParameterVector)) {
  # Extract values from the current row
  job_name <- ParameterVector[i, "JobName"]
  misspec_vec <- ParameterVector[i, "MisspecVec"]
  homosked_vec <- ParameterVector[i, "HomoskedVec"]
  size_vec <- ParameterVector[i, "SizeVec"]
  leverage_vec <- ParameterVector[i, "LeverageVec"]
  k_vec <- ParameterVector[i, "KVec"]
  type_setting <- ParameterVector[i, "TypeSetting"]
  output <- ParameterVector[i, "Output"]
  
  # Create .sbatch file for the current simulation
  sbatch_file <- file(paste0(dir,
                             "/Code/Slurm/",
                             TypeSetting,
                             "/RunSimulation/",
                             job_name, 
                             ".sbatch"), "w")
  writeLines(
    c(
      "#!/bin/bash",
      paste("#SBATCH --job-name", job_name),
      "#SBATCH --partition medium",
      "#SBATCH --ntasks 1",
      "#SBATCH --time 7-00:00:00",
      "#SBATCH --mem-per-cpu=6000",
      paste("#SBATCH -o ClusterMessages/out/myscript_", 
            job_name, 
            "_%j.out", 
            sep=""),
      paste("#SBATCH -e ClusterMessages/error/myscript_", 
            job_name, 
            "_%j.err", 
            sep=""),
      "#SBATCH --mail-type=ALL",
      "#SBATCH --mail-user=simondn@uw.edu",
      "",
      "cd ~/Stats572",
      "module load R",
      "Rscript Code/RunSimulationStudy.R \\",
      paste("    --Delta ", misspec_vec, " \\", sep=""),
      paste("    --Rho ", leverage_vec, " \\", sep=""),
      paste("    --N ", size_vec, " \\", sep=""),
      paste("    --Gamma ", homosked_vec, " \\", sep=""),
      paste("    --K ", k_vec, " \\", sep=""),
      paste("    --TypeSetting '", type_setting, "' \\", sep=""),
      paste("    --Output ", output, " \\", sep="")
    ),
    con=sbatch_file
  )
  close(sbatch_file)
}

print("Sbatch files generated successfully.")


