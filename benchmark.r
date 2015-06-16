#!/usr/bin/Rscript

#----------------------------------------------------------------------[ Setup ]

required.pkg <- c("caTools", "kernlab", "rpart", "pamr", "randomForest")
installed.pkg <- required.pkg %in% rownames(installed.packages())
if(!all(installed.pkg)){
    install.packages(required.pkg[!installed.pkg])
}
rm(required.pkg, installed.pkg)


basedir <- getwd()
library(dplyr)
library(ggplot2)
library(tidyr)


setwd(basedir)
jobs <- data.frame(input = dir(recursive=TRUE)) %>%
    dplyr::filter(grepl("^[^/]+/[^/]+.r$", input))
jobs <- expand.grid(input = jobs$input, replicate=1:5) %>%
    extract(input, c("task", "input", "method"),
            "^([^/]+)/(\\w+-([^/]+).r)$") %>%
    mutate(title = sprintf("%s-%s-%i", task, method, replicate)) %>%
    mutate(output = sprintf("output/%s.ps.log", title))

jobs <- jobs[jobs$task != "rf-parallelization",]
jobs <- jobs[jobs$method != "caret-custom",]
rownames(jobs) <- NULL

#--------------------------------------------------------------------[ Execute ]

for(i in order(jobs$task, jobs$replicate, sample(nrow(jobs)))){
    setwd(sprintf("%s/%s", basedir, jobs$task[i]))
    if(!file.exists(jobs$output[i])){
        system(sprintf("../benchmark.sh %s %s", jobs$input[i], jobs$title[i]))
        files <- dir(".", "*.log")
        dir.create("output", showWarnings=FALSE)
        file.rename(files, sprintf("%s/%s", dirname(jobs$output[i]), files))
    }
}

system("squeue | grep chrib | grep inter | cut -c 10-18 | xargs scancel")
stop("Done!")

#-----------------------------------------------------------------------[ Plot ]


setwd(basedir)

tab <- do.call(rbind, lapply(1:nrow(jobs), function(i){
    tryCatch({
        tab <- data.frame(task = jobs$task[i], method = jobs$method[i],
                          replicate = jobs$replicate[i],
            read.csv(sprintf("%s/%s", jobs$dir[i], jobs$output[i]),
                     colClasses=c("integer", rep("character", 3),
                                  rep("numeric", 2), rep("integer", 2)))
        )
        tab <- tab[complete.cases(tab),]
        tab$ELAPSED %<>% strsplit(":") %>%
            sapply(function(x) sum(c(3600, 60,1)*tail(c(0, as.integer(x)), 3)))
        tab <- tab %>%
            mutate(DATETIME = as.POSIXct(paste(DATE, TIME))) %>%
            select(-DATE, -TIME)
        tab_order <- setdiff(colnames(tab), "PID")
        tab_start <- tab %>%
            dplyr::filter(DATETIME == DATETIME[1]) %>%
            dplyr::filter(ELAPSED > min(ELAPSED))
        if(nrow(tab_start) > 0){
            # This is a multicore run, sum up all relevant processes
            tab <- tab[!tab$PID %in% tab_start$PID,] %>%
                group_by(DATETIME) %>%
                summarize(task = unique(task),
                          method = unique(method),
                          replicate = unique(replicate),
                          ELAPSED = max(ELAPSED),
                          CPU = sum(CPU),
                          MEM = sum(MEM),
                          RSS = sum(RSS),
                          VSIZE = sum(VSIZE))
        }
        tab[tab_order]
    }, error = function(...) NULL)
}))

tab_setup <- tab %>%
    dplyr::filter(method == "setup") %>%
    group_by(task, replicate) %>%
    summarize(MaxRSS = max(RSS)) %>%
    group_by(task) %>%
    summarize(MeanMaxRSS = mean(MaxRSS))

tab2 <- mutate(tab, replicate = sprintf("r%i", replicate))
tab2 <- select(tab2, task, method, replicate, ELAPSED, RSS)
tab2 <- spread(tab2, replicate, RSS)
tab2 <- gather_(tab2, "replicate", "RSS", grep("^r\\d$", colnames(tab2), value=TRUE))
tab2 <- mutate(tab2, title = sprintf("%s-%s-%i", task, method, replicate))

done <- tab2 %>%
    group_by(title) %>%
    summarize(MaxRSS = max(RSS, na.rm=TRUE)) %>%
    dplyr::filter(MaxRSS > 0)
tab2 <- dplyr::filter(tab2, title %in% done$title)

last <- 1
for(i in 1:nrow(tab2)){
    if(!is.na(tab2$RSS[i])){
        last <- sign(tab2$RSS[i])
    } else if(last == 0){
        tab2$RSS[i] <- 0
    }
}

tab3 <- tab2 %>% dplyr::filter(task != "rf") %>%
    mutate(method = factor(method, c("caret", "caret-custom", "emil", "emil-caret"))) %>%
    group_by(ELAPSED, task, method) %>%
    summarize(RSS = mean(RSS))
g <- ggplot(tab3[complete.cases(tab3),], aes(x = ELAPSED, y = RSS/1024, color=method)) +
    geom_hline(data = tab_setup, aes(yintercept = MeanMaxRSS/1024), color="grey80") +
    geom_line() +
    ylab("Memory (RSS) / MB") + xlab("Time / s") + 
    facet_wrap(~task, scales="free") + theme_bw(base_size=9) +
    scale_colour_manual(values=c("#ff7f2a", "#00aa88", "#8d5fd3", "#444444")) +
    theme(legend.position="right")

cairo_pdf("benchmark.pdf", 14/cm(1), 8/cm(1))
print(g)
dev.off()


