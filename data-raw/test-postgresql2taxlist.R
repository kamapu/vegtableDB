# TODO:   Neede to re-write function
# 
# Author: Miguel Alvarez
################################################################################

library(dbaccess)
library(vegtable2)
library(RPostgreSQL)

# Arguments to parameters
conn <- connect_db2(dbname = "veg_databases", user = "miguel")

Test <- import_swea(conn)

header = c("swea_dataveg", "header")
samples = c("swea_dataveg", "samples")
relations = list(community_type = c("commons","community_type"))
layers = list(spec_miguel = c("specimens","specimens_miguel"))
coverconvert = list(
		br_bl = c("commons","br_bl"),
		b_bbds = c("commons","b_bbds"),
		ordinal = c("commons","ordinal"))
geometry = "plot_centroid"
as_list = FALSE




taxon_names = c("tax_commons", "taxonNames")
taxon_relations = c("swea_dataveg", "taxonRelations")
taxon_traits = c("swea_dataveg", "taxonTraits")
taxon_views = "bib_references"
taxon_levels = c("tax_commons", "taxonLevels")
names2concepts = c("swea_dataveg", "names2concepts")
subset_levels = TRUE
#subset_views = TRUE
as_list = FALSE
#verbose = TRUE



Test <- postgres2taxlist(conn,
		taxon_names = c("tax_commons", "taxonNames"),
		taxon_relations = c("swea_dataveg", "taxonRelations"),
		taxon_traits = c("swea_dataveg", "taxonTraits"),
		taxon_views = "bib_references",
		taxon_levels = c("tax_commons", "taxonLevels"),
		names2concepts = c("swea_dataveg", "names2concepts"))

summary(Test)
