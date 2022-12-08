# TODO:   Script creating data from data-raw
#
# Author: Miguel Alvarez
################################################################################

library(divDB)

new_taxonomy_sql <- list()

for (i in c(
  "taxonomies", "taxon_names", "taxon_levels", "taxon_concepts",
  "names2concepts", "taxon_attributes"
)) {
  new_taxonomy_sql[[i]] <- read_sql(paste0("data-raw/", i, ".sql"))
}

new_taxonomy_sql <- do.call(c, new_taxonomy_sql)
class(new_taxonomy_sql) <- c("sql", "character")

save(new_taxonomy_sql, file = "R/sysdata.rda")
