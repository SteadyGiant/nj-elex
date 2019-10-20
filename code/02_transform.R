#!/usr/bin/env Rscript

DATA_INPUT_PATH = 'data/output/2017-official-general-election-results-general-assembly.csv'
DATA_OUTPUT_PATH = 'data/output/assembly_general_2017.csv'

library(dplyr)
library(readr)

options(scipen = 999)

data_1raw = read_csv(DATA_INPUT_PATH)

data_2cln = data_1raw %>%
  group_by(district_num, district) %>%
  mutate(low_winner = if_else(tally == min(tally[winner == 1]),
                              1, 0),
         high_loser = if_else(tally == max(tally[winner == 0]),
                              1, 0)) %>%
  filter(low_winner == 1
         | high_loser == 1)

data_3agg = data_2cln %>%
  summarize(winning_parties = paste(unique(party[winner == 1]),
                                    collapse = ', '),
            low_win_mov = pct[winner == 1] - pct[winner == 0],
            competitive = if_else(low_win_mov <= 0.05,
                                  1, 0),
            semi_competitive = if_else(low_win_mov > 0.05
                                       & low_win_mov <= 0.10,
                                       1, 0))

write_csv(data_3agg,
          path = DATA_OUTPUT_PATH)
