library(dplyr)
library(ggplot2)
library(here)

usage_tab <- read.table(here("server", "usage.txt"), header = TRUE)

#filter out rows with extra headers 
#(happeneed because of the way the bash script was written)
#as well as all uninformative rows at the end (job already failed)
usage_tab <- usage_tab |>
  dplyr::filter(total != "total")|>
  dplyr::mutate(time = as.ITime(time),
                date = as.Date(date),
                total = as.numeric(total),
                used = as.numeric(used),
                free = as.numeric(free),
                buff.cache = as.numeric(buff.cache),
                available = as.numeric(available)) |>
  dplyr::mutate(out = ifelse(date == "2022-07-19" & time > as.ITime("02:40:09") |
                               date == "2022-07-18" & time < as.ITime("21:00:00"),
                             1, 0)) |>
  dplyr::filter(out == 0) |>
  dplyr::mutate(minutes_since_start = (1:n())/6) |>
  dplyr::select(minutes_since_start, total, used, buff.cache) |>
  data.table() |>
  melt(id.vars = c("minutes_since_start"), variable.name = "memory") |>
  dplyr::rename(mb = value)
  

pdf(here("server", "memory_usage.pdf"), width = 12, height = 8)
ggplot(usage_tab, aes(x = minutes_since_start, y = mb, color = memory)) +
  geom_line() 
dev.off()
