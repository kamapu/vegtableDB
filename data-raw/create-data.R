# TODO:   Script creating data from data-raw
#
# Author: Miguel Alvarez
################################################################################

# 1: Importing database catalogue

library(readODS)

db_catalog <- list()

for (i in c("taxonomy", "databases", "relations")) {
  db_catalog[[i]] <- read_ods("data-raw/db_catalog.ods", i)
}

# Final: Export data
save(db_catalog, file = "R/sysdata.rda")
