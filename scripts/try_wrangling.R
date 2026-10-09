library(tidyverse)
# install.packages('rtry')
library(rtry)

# try_raw <- read_csv("data/traits/Seedlings_TRY_08042026.csv")

try_raw <- rtry_import("data/try/09122026/52369.txt")

# try_raw <- rtry_import("data/traits/Seedlings_TRY_08042026.csv",
#                        separator = ",")

try_traits <- rtry_select_row(try_raw,
                              complete.cases(TraitID) & complete.cases(StdValue))

try_sel <- rtry_select_col(try_traits,
                           ObservationID, AccSpeciesID, AccSpeciesName, TraitID, TraitName,
                           StdValue, UnitName, Comment)