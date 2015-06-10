source("rf-setup.r")

result_seq <- evaluate(procedure = proc, x = x, y = y, resample = cv,
                       .cores = 1)

Sys.sleep(3)

