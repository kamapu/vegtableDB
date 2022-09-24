# TODO:   Script creating data from data-raw
#
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

.tax_sql <- read_sql("data-raw/new-taxonomy-schema.sql")

save(.tax_sql, file = "R/sysdata.rda")
