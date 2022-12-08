#' Internal function to create LETTERCODE
#' 
#' @keywords internal
#' 
get_code <- function(x) {
    x <- strsplit(x, " ", fixed=TRUE)
    x <- sapply(x, "[", c(1,2,4))
    x[2,!is.na(x[3,])] <- paste(substr(x[2,], 1, 1), substr(x[3,], 1, 1),
            sep=".")[!is.na(x[3,])]
    x[2,x[2,] %in% c("species","spec.",".sp")] <- ".SP"
    x <- toupper(paste0(substr(x[1,], 1, 4), substr(x[2,], 1, 3)))
    return(x)
}

#' @name taxlist2tvlist
#' 
#' @title Convert taxlist objects to Turboveg format
#' 
#' @description 
#' This is a function converting [taxlist-class] objects to a **Turboveg** like
#' format.
#' It may be used as prototype for an eventual export function.
#' 
#' This function basically export the slots `taxonNames` and `taxonTraits`,
#' adapts the respective column names to **Turboveg**, and generates
#' automatically some fields that are usually not requested in [taxlist-class]
#' objects.
#' 
#' Names belonging to a same concept get a common `LETTERCODE`.
#' 
#' @param taxlist A [taxlist-class] object.
#' @param ecodbase A logical value, whether the slot \code{'taxonTraits'} may
#'     be exported or not.
#' @author Miguel Alvarez \email{malvarez@@uni-bonn.de}
#' 
#' @seealso [taxlist-class] [tv2taxlist()]
#' 
#' @export taxlist2tvlist
#' 
taxlist2tvlist <- function(taxlist, ecodbase=TRUE) {
    if(class(taxlist) != "taxlist")
        stop("'taxlist' have to be of class 'taxlist'", call.=FALSE)
    if(ecodbase & ncol(taxlist@taxonTraits) > 1)
        Traits <- taxlist@taxonTraits else Traits <- NULL
    # Add synonyms
    taxlist@taxonNames$SYNONYM <- !taxlist@taxonNames$TaxonUsageID %in%
            taxlist@taxonRelations$AcceptedName
    taxlist@taxonNames$SHORTNAME <- substr(taxlist@taxonNames$TaxonName, 1, 22)
    taxlist@taxonNames$NATIVENAME <- ""
    # LETTERCODE in many steps
    LETTERCODE <- taxlist@taxonRelations[,c("TaxonConceptID","AcceptedName")]
    LETTERCODE$AcceptedName <- taxlist@taxonNames[match(LETTERCODE$AcceptedName,
                    taxlist@taxonNames$TaxonUsageID),"TaxonName"]
    LETTERCODE$LETTERCODE <- get_code(LETTERCODE$AcceptedName)
    taxlist@taxonNames$LETTERCODE <- LETTERCODE[
            match(taxlist@taxonNames$TaxonConceptID, LETTERCODE$TaxonConceptID),
            "LETTERCODE"]
    # Final version
    taxlist <- taxlist@taxonNames
    colnames(taxlist) <- replace_x(colnames(taxlist),
			old=c("TaxonUsageID","TaxonName","AuthorName","TaxonConceptID"),
			new=c("SPECIES_NR","ABBREVIAT","AUTHOR","VALID_NR"))
	Head <- c("SPECIES_NR","LETTERCODE","SHORTNAME","ABBREVIAT","NATIVENAME",
            "AUTHOR","SYNONYM","VALID_NR")
    Head <- c(Head, colnames(taxlist)[!colnames(taxlist) %in% Head])
    taxlist <- list(species=taxlist[,Head])
    if(!is.null(Traits)) {
        colnames(Traits)[1] <- "SPECIES_NR"
        taxlist[["ecodbase"]] <- Traits
    }
    return(taxlist)
}
