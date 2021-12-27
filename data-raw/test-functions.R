# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtable2")
install_gitlab("kamapu/dbaccess")

library(vegtable2)
library(RPostgreSQL)
library(biblioDB)
library(dbaccess)

# Selection of sources
Sel <- c("Alvarez2012c", "Alvarez2017", "Alvarez2018a", "Ayichedehou2000",
		"Bronner1990", "deBock2009", "deFoucault1999", "Furness1980",
		"Germain1951", "Guyot1994", "Lebrun1947", "Lejoly2000", "Masens2000",
		"Mullenders1954", "Schmitt1991", "Schmitz1971", "Szafranski1983")
Query <- paste0("SELECT *\n",
		"FROM swea_dataveg.header\n",
		"WHERE bibtexkey IN (\'", paste0(Sel, collapse = "','"), "');\n")

# Connect to database
conn <- connect_db2(dbname = "veg_databases", user = "miguel")

swea <- import_swea(conn, sql_header = Query)

dbDisconnect(conn)



swea2 <- subset(swea, bibtexkey %in% Sel, slot = "relations", relation = "data_source")
summary(swea2)


### Debu db2vegtable
header = c("swea_dataveg","header")
samples = c("swea_dataveg","samples")
relations = list(
		globe_plots = c("swea_dataveg","globe_plots"),
		swea1_code = c("swea_dataveg","swea1_code"),
		soil_moisture = c("swea_dataveg","soil_moisture"),
		soil_texture = c("swea_dataveg","soil_texture"),
		community_type = c("commons","community_type"),
		naturalness = c("swea_dataveg","naturalness"),
		record_type = c("swea_dataveg","record_type")
)
layers = list(
		veg_layer = c("swea_dataveg","veg_layer"),
		spec_miguel = c("specimens","specimens_miguel")
)
coverconvert = list(
		br_bl = c("commons","br_bl"),
		b_bbds = c("commons","b_bbds"),
		ordinal = c("commons","ordinal")
)
geometry = "plot_centroid"
get_countries = TRUE
get_data_sources = TRUE
bib_args = list()
taxon_names = c("tax_commons","taxonNames")
taxon_relations = c("swea_dataveg","taxonRelations")
taxon_traits = c("swea_dataveg","taxonTraits")
taxon_views = "bib_references"
taxon_levels = c("tax_commons","taxonLevels")
names2concepts = c("swea_dataveg","names2concepts")



