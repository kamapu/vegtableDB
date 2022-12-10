# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

DB <- "vegetation_v3"

do_restore(
    dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB)
)

conn <- connect_db(DB, user = "miguel")

spp <- db2taxlist(conn, taxonomy = "ea_splist")

summary(spp, "Heliotropium")

insert_synonyms(conn, df = data.frame(
        taxon_concept_id = c(57034, 57032),
        usage_name = c("Heliotropium sp1", "Heliotropium somalense"),
        author_name = c("Me", "Vatke")))

insert_synonyms(conn, df = data.frame(
        taxon_concept_id = c(57034, 57032),
        usage_name = c("Heliotropium sp1", "Heliotropium sp2"),
        author_name = c("Me", "Me")))

spp <- db2taxlist(conn, taxonomy = "ea_splist")

summary(spp, "Heliotropium")

do_restore(
    dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB)
)
