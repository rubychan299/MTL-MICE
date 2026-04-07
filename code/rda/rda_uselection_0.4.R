pkgs <- c("foreach","glmnet","caret","doParallel","parallel","glmnet", "tidyverse", "glmtrans")
lapply(pkgs, require, character.only = TRUE)

scripts.sources <-  list.files("/scratch/yc4178/MTL-MICE/R", 
                               pattern="*.R$", full.names=TRUE, 
                               ignore.case=TRUE)
sapply(scripts.sources,source,.GlobalEnv)

# Load the data
load(paste0("/scratch/yc4178/MTL-MICE/data/uselection/bystate_2020/mar_0.4/uselect_mar_",task.id, ".RData"))
load("/scratch/yc4178/MTL-MICE/data/uselection/bystate_2020/uselect_list2020.RData")

set.seed(3407)

uselect_mtlmice_pool <- mtlmice(data = uselect_mar, m = 30, maxit = 10, transfer.id.mode = "all", cores = 5, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                              correction = TRUE)

uselect_mtlmice <- mtlmice(data = uselect_mar, m = 30, maxit = 10, transfer.id.mode = "auto", cores = 5, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                         correction = TRUE)

uselect_mtlmice0 <- mtlmice(data = uselect_mar, m = 30, maxit = 10, transfer.id.mode = "auto", cores = 5,  lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                          correction = FALSE)

uselect_mice  <- mtlmice(data = uselect_mar, m = 30, maxit = 10, transfer.id.mode = "none", cores = 5, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                      correction = TRUE)

uselect_mice_pool <- mtlmice(data = uselect_mar, m = 30, maxit = 10, transfer.id.mode = "none_all", cores = 5, lambda.transfer = "lambda.1se", lambda.debias = "lambda.1se", lambda.detection = "lambda.1se",
                      correction = TRUE)


output_mtlmice_pool <- error.rate(uselect_mtlmice_pool, uselect_list)
output_mtlmice <- error.rate(uselect_mtlmice, uselect_list)
output_mtlmice0 <- error.rate(uselect_mtlmice0, uselect_list)
output_mice <- error.rate(uselect_mice, uselect_list)
output_mice_pool <- error.rate(uselect_mice_pool, uselect_list)

file.name <- paste0("/scratch/yc4178/MTL-MICE/rda/output_0.4/uselect_imp","_", task.id, ".RData")
save(uselect_mtlmice_pool,uselect_mtlmice,uselect_mtlmice0,uselect_mice, uselect_mice_pool, output_mtlmice_pool, output_mtlmice, output_mtlmice0, output_mice, output_mice_pool,file = file.name)
