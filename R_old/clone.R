#' @name clone
#' 
#' @title Clone Turboveg databases
#' 
#' @description 
#' Packing respective files belonging to a Turboveg database into a zip file.
#' 
#' This function attempts to substitute the backup options from **Turboveg**,
#' generating a backup of the whole database at once.
#' 
#' For `stamp=TRUE`, the system date will be included in the name of the
#' resulting zip file.
#' 
#' If `overwrite=FALSE` overwriting homonymous zip files will be avoided
#' by inserting a numerical suffix to the resulting zip file.
#' 
#' @param db_name Name of database as character value.
#' @param tv_home Character value indicating the path to Turboveg's home
#'     directory.
#' @param stamp Logical value indicating whether the date should be included in
#'     the zip file's name or not.
#' @param overwrite Logical value indicating whether existing files should be
#'     overwritten or not.
#' @param extras Logical value indicating the occurrence of extra files (see
#'     Details).
#' @param ... Additional arguments passed to function [zip()].
#' 
#' @author Miguel Alvarez \email{malvarez@@uni-bonn.de}
#' 
#' @examples
#' ## Add example for this function
#' ## unzip(file, exdir=tv.home())
#' 
#' @export clone
#' 
clone <- function(db_name, tv_home=tv.home(), stamp=TRUE, overwrite=FALSE,
        extras=FALSE, ...) {
    oldWD <- getwd()
    setwd(tv_home)
    db_description <- unlist(c(db_name, read.dbf(file.path(tv_home, "Data",
                                    db_name,"tvwin.dbf"), as.is=TRUE)[,
                            c("FLORA","DICTIONARY"), drop=FALSE]))
    # Species list
    path_species <- file.path("species", db_description["FLORA"])
    Files <- file.path(path_species, list.files(path_species, recursive=TRUE))
    # Observations
    path_observs <- file.path("Data", db_name)
    Files <- c(Files, file.path(path_observs, list.files(path_observs,
                            recursive=TRUE)))
    # Popups
    if(is.na(db_description["DICTIONARY"])) path_popups <- "popup" else
        path_popups <- file.path("popup", db_description["DICTIONARY"])
    Files <- c(Files, file.path(path_popups, list.files(path_popups,
                            recursive=TRUE)))
    # Extras
    if(extras) {
        path_extras <- file.path("extras", db_name)
        Files <- c(Files, file.path(path_extras, list.files(path_extras,
                                recursive=TRUE)))
    }
    # using stamp and don't overwrite
    if(stamp) db_name <- paste(db_name, Sys.Date(), sep="_")
    if(paste0(db_name, ".zip") %in% list.files(oldWD) & !overwrite) {
        i <- 0
        repeat{
            i <- i + 1
            if(!paste0(db_name, "_", i, ".zip") %in% list.files(oldWD)) break
        }
        db_name <- paste0(db_name, "_", i)
    }
    paste0(db_name, ".zip")
    # The clone
    zip(file.path(oldWD, db_name), Files, ...)
    setwd(oldWD)
}
