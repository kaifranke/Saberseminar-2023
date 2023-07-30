library(tidyverse)


t = read.csv("block2023.csv")

t = t %>%
  mutate(oe = blocked - pred2023)

leaderboard = catcherResults %>%
  rename(Name = name, Ops = n, Blocks = blocks) %>%
  select(Name, Ops, Blocks, BAX) %>%
  mutate(across(c('BAX'), round, 2))

library(gt)
library(mlbplotR)

headshots = mlbplotR::load_headshots()


leaderboard = left_join(leaderboard,headshots, by = c("Name" = "player_name")) %>%
  select(Name, Ops, Blocks, BAX, espn_headshot) %>%
  rename(headshot = espn_headshot)


library(htmltools)
library(ggplot2)
library(gtExtras)
library(scales)

t5 <- leaderboard %>% 
  head(10) %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = "BAX Leaders 2023")

# add image column
t5 <- t5 %>% 
  gt_img_rows(
    column = headshot,
  ) %>% 
  cols_label(headshot = " ") %>%
  data_color(columns = BAX, colors = col_numeric(domain=NULL, palette = "Reds"))




b5 <- leaderboard %>% 
  tail(10) %>% 
  arrange(BAX) %>%
  ungroup() %>% 
  gt() %>% 
  tab_header(title = "BAX Stragglers 2023")

# add image column
b5 <- b5 %>% 
  gt_img_rows(
    column = headshot,
  ) %>% 
  cols_label(headshot = " ") %>%
  data_color(columns = BAX, colors = col_numeric(domain=NULL, palette = rev(RColorBrewer::brewer.pal(9, "Blues"))))

