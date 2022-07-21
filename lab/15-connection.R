# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(vegtableDB)

conn1 <- connect_db(dbname = "geomiguel", user = "miguel")
conn2 <- connect_db(dbname = "geomiguel", user = "miguel", pkg = "RPostgres")

conn3 <- connect_db(dbname = "geomiguel", user = "miguel", pkg = "rpostgres")
