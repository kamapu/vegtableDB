# TODO:   Test function merging names
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/vegtableDB", "devel")

# Load libraries
## library(dbaccess)
library(RPostgreSQL)
library(vegtableDB)

# Restore database
DB <- "vegetation_v3"

do_restore(
    dbname = DB,
    user = "miguel",
    filepath = file.path("../../db-dumps/00_dumps", DB)
)

# Connect database
conn <- connect_db(DB, user = "miguel")

# Use the check
check_tax(conn)
