#!/bin/sh

#SBATCH --job-name=h_1se_4
#SBATCH --time=24:00:00
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mem=12GB
#SBATCH --cpus-per-task=5
#SBATCH --array=1-100
#SBATCH --account=torch_pr_365_general

export OMP_NUM_THREADS=1
module purge
module load r/4.5.1
R CMD BATCH --no-save --no-restore h_1se_4.R script_$SLURM_ARRAY_TASK_ID