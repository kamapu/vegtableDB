# TODO:   Response if not matched names
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

# Restore PostgreSQL database and connect
DB <- "vegetation_v3"

conn <- connect_db(DB, user = "miguel")

query_names(conn, "Ulex subsericeus", concepts = TRUE)
