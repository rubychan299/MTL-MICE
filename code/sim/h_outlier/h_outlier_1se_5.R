pkgs <- c("foreach","glmnet","caret","doParallel","parallel","glmnet", "tidyverse", "glmtrans")
lapply(pkgs, require, character.only = TRUE)

scripts.sources <-  list.files("/scratch/yc4178/MTL-MICE/R", 
                               pattern="*.R$", full.names=TRUE, 
                               ignore.case=TRUE)
sapply(scripts.sources,source,.GlobalEnv)

n <- 100    # Number of observations
p <- 150    # Total number of variables
mi <- 4    # Number of incomplete variables
s <- p-mi     # Number of complete variables
v <- 15

B0 <- matrix(rep(-2, v * mi), nrow = v, ncol = mi)  # Coefficient matrix for v
C0 <- matrix(rep(2, v * mi), nrow = v, ncol = mi)
baseline_params <- list(B0, C0)

ncores <- 5    # Number of cores
k <- 10
ka <- 9
h <- 1

dat <- simulate.scenario.shared(K = k, n = n, p = p, s = s, v = v, baseline_params = baseline_params, h = h, eta = 0, cluster_sizes = c(ka, k-ka), simulate_clusters = TRUE, complete_data_type = c(norm = 1, binom = 0), incomplete_data_type = c(norm = 0.5, binom = 0.5))

# Extract the data for each task and form a new list
data <- lapply(dat, function(x) x$data)

# simulate the missing at random
dat.mar <- mar.sim(data = data, covaraite = "c_x1", where.missing = c("ic_x1","ic_x2","ic_x3","ic_x4"), prop.missing = 0.2)

mtlmice.pool.imp <- mtlmice(data = dat.mar, m = 30, maxit = 10, transfer.id.mode = "all", cores = ncores, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                            correction = TRUE)
  
mtlmice.imp <- mtlmice(data = dat.mar, m = 30, maxit = 10, transfer.id.mode = "auto", cores = ncores, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                       correction = TRUE)

mtlmice0.imp<- mtlmice(data = dat.mar, m = 30, maxit = 10, transfer.id.mode = "auto", cores = ncores, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                       correction = FALSE)

mice.imp  <- mtlmice(data = dat.mar, m = 30, maxit = 10, transfer.id.mode = "none", cores = ncores, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                      correction = TRUE)

mice.pool.imp  <- mtlmice(data = dat.mar, m = 30, maxit = 10, transfer.id.mode = "none_all", cores = ncores, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                      correction = TRUE)

output_mtlmice_pool <- error.rate(mtlmice.pool.imp, dat)
output_mtlmice <- error.rate(mtlmice.imp, dat)
output_mtlmice0 <- error.rate(mtlmice0.imp, dat)
output_mice <- error.rate(mice.imp, dat)
output_mice_pool <- error.rate(mice.pool.imp, dat)

file.name <- paste0("/scratch/yc4178/MTL-MICE/sim/output/htrends_outlier/h_outlier_1se_h", h, "_", task.id, ".RData")
save(dat, mtlmice.pool.imp, mtlmice.imp, mtlmice0.imp, mice.imp, mice.pool.imp, output_mtlmice_pool, output_mtlmice, output_mtlmice0, output_mice, output_mice_pool, file = file.name)

