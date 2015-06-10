source("rf-setup.r")

result_par1 <- evaluate(procedure = proc, x = x, y = y, resample = cv,
                        .cores = getOption("mc.cores"))

Sys.sleep(3)

