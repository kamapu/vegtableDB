# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

# Restore PostgreSQL database and connect
DB <- "vegetation_v3"

do_restore(dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB),
    path_psql = "/usr/bin")

conn <- connect_db(DB, user = "miguel")

# Import species list and check name
spp <- db2taxlist(conn, taxonomy = "wfo")
summary(spp, "Ulex argenteus", secundum = "bibtexkey")

# New subspecies
new_concepts = data.frame(
    usage_name = "Ulex argenteus subsp. subsericans",
    author_name = "(Cout.) Rothm.",
    rank = "subspecies",
    parent_id = 155195,
    view_key = "WFOTeam2021")
new_concepts <- clean_strings(new_concepts)

insert_concepts(conn, taxonomy = "wfo", df = new_concepts)

# Check again
spp <- db2taxlist(conn, taxonomy = "wfo")
summary(spp, "Ulex argenteus", secundum = "bibtexkey")

# Restore to original database
do_restore(dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB),
    path_psql = "/usr/bin")
