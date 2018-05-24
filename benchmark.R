#!/usr/bin/Rscript


#----------------------------------------------------------------------[ Setup ]

basedir <- getwd()
library("dplyr")
library("ggplot2")
library("tidyr")


setwd(basedir)
jobs <- data.frame(input = dir(recursive = TRUE)) %>%
    dplyr::filter(grepl("^[^/]+/[^/]+.R$", input))
jobs <- expand.grid(input = jobs$input, replicate = 1:5) %>%
    tidyr::extract(input, c("task", "input", "method"),
            "^([^/]+)/(\\w+-([^/]+).R)$") %>%
    mutate(title = sprintf("%s-%s-%i", task, method, replicate)) %>%
    mutate(output = sprintf("output/%s.ps.log", title))
jobs <- jobs[jobs$task != "rf-parallelization", ]
rownames(jobs) <- NULL


#--------------------------------------------------------------------[ Execute ]

for (i in order(jobs$replicate, jobs$task, sample(nrow(jobs)))) {
    setwd(file.path(basedir, jobs$task[i]))
    if (!file.exists(jobs$output[i])) {
        cat(format(Sys.time()), "\n")
        cmd <- paste(file.path("..", "benchmark.sh"), jobs$input[i], jobs$title[i])
        system(cmd)
        files <- dir(".", "*.log")
        dir.create("output", showWarnings = FALSE)
        file.rename(files, file.path(dirname(jobs$output[i]), files))
        cat("---\n")
    }
}


#---------------------------------------------------------[ Summarize the jobs ]

setwd(basedir)

tab <- do.call(rbind, lapply(1:nrow(jobs), function(i) {
    tryCatch({
        tab <- data.frame(task = jobs$task[i], method = jobs$method[i],
                          replicate = jobs$replicate[i],
            read.csv(sprintf("%s/%s", jobs$task[i], jobs$output[i]),
                     colClasses = c("integer", rep("character", 3),
                                  rep("numeric", 2), rep("integer", 2)))
        )
        tab <- tab[complete.cases(tab), ]
        tab$ELAPSED %<>% strsplit(":") %>%
            sapply(function(x) sum(c(3600, 60, 1)*tail(c(0, as.integer(x)), 3)))
        tab <- tab %>%
            mutate(DATETIME = as.POSIXct(paste(DATE, TIME))) %>%
            select(-DATE, -TIME)
        tab_order <- setdiff(colnames(tab), "PID")
        tab_start <- tab %>%
            dplyr::filter(DATETIME == DATETIME[1]) %>%
            dplyr::filter(ELAPSED > min(ELAPSED))
        if (nrow(tab_start) > 0) {
            # This is a multicore run, sum up all relevant processes
            tab <- tab[!tab$PID %in% tab_start$PID, ] %>%
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


# Visual inspection
tab_summary <- tab %>%
    group_by(task, method, replicate) %>%
    summarize(Duration = max(ELAPSED),
              MeanCPU = mean(CPU),
              MaxMEM = max(MEM),
              MaxRSS = max(RSS),
              MaxVSIZE = max(VSIZE))
ggplot(tab_summary, aes(x = method, y = Duration)) +
    geom_boxplot() + facet_wrap(~task, scales = "free_y")
ggplot(tab_summary, aes(x = method, y = MaxRSS)) +
    geom_boxplot() + facet_wrap(~task, scales = "free_y")


#--------------------------------------------------------------[ Make the plot ]

# To overcome the problem that ggplot2 cannot have different axis breaks for
# different panels we shift all values so that the X and Y intervals never
# overlap between tasks.

shift_rss <- 1e12
shift_elapsed <- 1e6

format_rss <- function(x) {
  # Based on http://www.moeding.net/archives/32-Metric-prefixes-for-ggplot2-scales.html
  x <- x %% shift_rss
  i <- sum(max(x, na.rm = TRUE) >= 1000^(1:3))
  prefix <- c("MB", "GB")[i]
  divisor <- 1000^i

  paste(format(round(x/divisor, 1), trim = TRUE, scientific = FALSE), prefix)
}

format_elapsed <- function(x) {
  format(ISOdate(2000, 1, 1, 0) + x %% shift_elapsed, "%k:%M:%S")
}

tab_setup <- tab %>%
    dplyr::filter(method == "setup") %>%
    group_by(task, replicate) %>%
    summarize(MaxRSS = max(RSS)) %>%
    group_by(task) %>%
    summarize(TickRSS = mean(MaxRSS))

mean_tab <- tab %>%
  group_by(task, method, ELAPSED) %>%
  summarise(MeanRSS = mean(RSS))

shift <- mean_tab %>%
  group_by(task) %>%
  summarise(TickRSS = max(MeanRSS), TickELAPSED = max(ELAPSED)) %>%
  mutate(
    ShiftRSS = shift_rss * seq_along(TickRSS),
    ShiftELAPSED = shift_elapsed * seq_along(TickELAPSED)
  )

axis_breaks <- shift %>%
  bind_rows(tab_setup) %>%
  bind_rows(mutate(shift, TickRSS = 0, TickELAPSED = 0)) %>%
  inner_join(shift, by = "task") %>%
  mutate(
    TickRSS.y = TickRSS.x + ShiftRSS.y,
    TickELAPSED.y = TickELAPSED.x + ShiftELAPSED.y
  ) %>%
  transmute(
    task = task,
    TickRSS = TickRSS.y,
    TickELAPSED = TickELAPSED.y,
  )

plot_data <- mean_tab %>%
  dplyr::filter(method != "setup") %>%
  inner_join(shift, by = "task") %>%
  mutate(
    MeanRSS = MeanRSS + ShiftRSS,
    ELAPSED = ELAPSED + ShiftELAPSED
  )

setup_hlines <- axis_breaks %>%
  dplyr::filter(is.na(TickELAPSED))

g <- plot_data %>%
  ggplot(aes(x = ELAPSED, y = MeanRSS, colour = method)) +
    facet_wrap(~task, scales = "free") +
    geom_hline(data = setup_hlines, aes(yintercept = TickRSS), color = "grey80") +
    geom_line() +
    ylab("Memory (RSS)") + xlab("Time (h:mm:ss)") +
    scale_colour_manual("Method", values = c("#ff7f2a", "#8d5fd3", "grey80")) +
    scale_x_continuous(
        labels = format_elapsed,
        breaks = axis_breaks$TickELAPSED
    ) +
    scale_y_continuous(
      labels = format_rss,
      breaks = axis_breaks$TickRSS
    ) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = c(5/6, 1/4)
    )

cairo_pdf("benchmark.pdf", 16/cm(1), 8/cm(1))
print(g)
dev.off()
