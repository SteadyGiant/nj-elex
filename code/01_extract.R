#!/usr/bin/env Rscript


##%######################################################%##
#                                                          #
####                      Summary                       ####
#                                                          #
##%######################################################%##

# This code rescues the data I want from the PDF containing results for the 2017
# NJ Assembly general election.

# TODO
# Eventually, this code will be used to scrape multiple PDFs.

# For now, this is the PDF I want scraped:
# https://www.state.nj.us/state/elections/election-information-2017.shtml#general
# https://www.state.nj.us/state/elections/assets/pdf/election-results/2017/2017-official-general-election-results-general-assembly.pdf
PDF_PATH = 'data/input/2017-official-general-election-results-general-assembly.pdf'


##%######################################################%##
#                                                          #
####                       Setup                        ####
#                                                          #
##%######################################################%##

library(dplyr)
library(pdftools) # requires the 'poppler' system library
library(purrr)
library(readr)
library(stringr)
library(tidyr)

# The last page contains a list of party names. That will come in handy.
# This pattern appears in that page only.
party_pg_patt = 'Candidate Totals for Party'

# These patterns appear in table headers. We'll remove them to get the data
# tables from each page.
bad_patts =
  c('[0-9][0-9]\\/[0-9][0-9]\\/[0-9][0-9][0-9][0-9]',
    'Candidates',
    'Legislative') %>%
  paste(collapse = '|')


##%######################################################%##
#                                                          #
####                      Extract                       ####
#                                                          #
##%######################################################%##

pages =
  # extract text from PDF
  pdf_text(PDF_PATH) %>%
  # split strings separated by newlines to create a vector w/ tons of rows
  # https://stackoverflow.com/a/49611563/8605348
  str_split('[\\r\\n]+')

parties_patt = pages %>%
  keep(~ any(str_detect(.x, party_pg_patt))) %>%
  unlist() %>%
  # remove rows that aren't part of the tables
  str_subset(paste(bad_patts, party_pg_patt, '^$',
                   sep = '|'),
             negate = TRUE) %>%
  # split strings separated by 2+ spaces to create a matrix w/ 6 columns
  str_split_fixed(pattern = '\\s{2,}', n = 6) %>%
  .[, 1] %>%
  paste(collapse = '|')

results = pages %>%
  # remove last table, which just counts candidates by party by county
  discard(~ any(str_detect(.x, party_pg_patt)))

# get "names" of districts
districts = results %>%
  map(~
        .x %>%
        str_extract('.+(?=Legislative District)') %>%
        .[!is.na(.)] %>%
        str_trim()
  )

u_districts = districts %>%
  unlist() %>%
  unique()

# want district numbers, as numeric
n_grp = length(u_districts)

district_xwalk = tibble(district = u_districts,
                        district_num = 1:n_grp)

# finally, get the actual data tables from each page
tables = results %>%
  set_names(districts) %>%
  map(~
        .x %>%
        # remove rows that aren't part of the tables
        str_subset(bad_patts, negate = TRUE) %>%
        # split strings separated by 2+ spaces to create a matrix w/ 6 columns
        str_split_fixed(pattern = '\\s{2,}', n = 6) %>%
        as_tibble(.name_repair = 'universal') %>%
        set_names(c('name', 'address', 'party', 'county', 'slogan',
                    'tally')) %>%
        slice(-1)
  )


##%######################################################%##
#                                                          #
####                     Transform                      ####
#                                                          #
##%######################################################%##

tbl = tables %>%
  # combine tables into one tibble
  bind_rows(.id = 'district') %>%
  # add district numbers
  left_join(district_xwalk,
            by = 'district') %>%
  mutate_if(.predicate = ~ is.character(.),
            .funs = ~ str_trim(.)) %>%
  mutate(
    # get some parties out of `address` column
    party_fix = if_else(str_detect(address, parties_patt),
                        str_extract(address, parties_patt),
                        NA_character_),
    # get rest of parties out of `party` column
    party_fix = if_else(str_detect(party, parties_patt),
                        str_extract(party, parties_patt),
                        party_fix),

    # get some tallies out of `party` column
    tally_fix = if_else(str_detect(party, '[0-9]'),
                        party, NA_character_),
    # get rest of tallies out of `county` column
    tally_fix = if_else(str_detect(county, '[0-9]'),
                        county, tally_fix)
  ) %>%
  filter(address != 'Total') %>%
  # throw out unnecessary columns
  select(district_num, district, name,
         party = party_fix,
         tally = tally_fix) %>%
  mutate(
    # remove everything from `names` column that isn't a name or a
    # winner/incumbent flag
    name = case_when(str_detect(name, '\\(w\\)|\\*') ~ name,
                     is.na(party) | !is.na(tally) ~ '',
                     TRUE ~ name),
    name = if_else(str_detect(name, '[0-9]'),
                   str_remove(name, '[0-9]+.*|(?i)P\\.*O\\.* BOX.*') %>%
                     str_trim(),
                   name),
    # Need to group by candidates.
    # Extract actual names from `names` column to create grouping variable.
    # (Leave behind winner/incumbent flags, if any.)
    name_grp = if_else(!is.na(party)
                       & is.na(tally),
                       name, NA_character_),
    tally = tally %>%
      str_remove_all(',') %>%
      as.numeric()
  ) %>%
  fill(name_grp, party, .direction = 'down')

tbl_cln = tbl %>%
  # for each candidate, combine names w/ winner/incumbent flags
  group_by(district, name_grp) %>%
  mutate(name = paste(unique(name),
                      collapse = ' ')) %>%
  filter(!is.na(tally)) %>%
  # for each candidate, keep record w/ total votes only
  group_by_at(.vars = vars(-c(tally, name_grp))) %>%
  summarize(tally = sum(tally,
                        na.rm = TRUE)) %>%
  group_by(district) %>%
  mutate(district_total_tally = sum(tally),
         pct = tally / district_total_tally) %>%
  ungroup() %>%
  mutate(
    # create dummies from text
    winner = if_else(str_detect(name, '\\(w\\)'),
                     1, 0),
    incumbent = if_else(str_detect(name, '\\*'),
                        1, 0),
    major_party = if_else(str_detect(party, '(?i)Democratic|(?i)Republican'),
                          1, 0),
    name = name %>%
      str_remove_all('\\*|\\(w\\)') %>%
      str_trim()
  ) %>%
  mutate_at(.vars = vars(name, party),
            .funs = ~ str_to_title(.)) %>%
  arrange(district_num, desc(tally))


##%######################################################%##
#                                                          #
####                        Load                        ####
#                                                          #
##%######################################################%##

output_path = PDF_PATH %>%
  basename() %>%
  str_replace(pattern = 'pdf', replacement = 'csv') %>%
  paste0('data/output/', .)

write_csv(tbl_cln,
          path = output_path)
