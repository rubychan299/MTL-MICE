# set seed for HPC array job
task.id <- Sys.getenv('SLURM_ARRAY_TASK_ID')
task.id <- as.numeric(task.id)
n.reps <- 1000
seeds <- matrix(rep(0, (n.reps * 7)), n.reps)
set.seed(1, kind = "L'Ecuyer-CMRG") # choose your fav seed (inital seed)
current.seed <- .Random.seed
for (i in 1:n.reps) {
  seeds[i,] <- current.seed
  current.seed <- nextRNGStream(current.seed)
}
.Random.seed <- seeds[task.id,]