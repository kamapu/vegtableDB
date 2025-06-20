% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_taxonomy.R
\name{new_taxonomy}
\alias{new_taxonomy}
\alias{new_taxonomy,PostgreSQLConnection,missing-method}
\alias{new_taxonomy,PostgreSQLConnection,taxlist-method}
\title{Create new database from a taxlist object}
\usage{
new_taxonomy(conn, obj, ...)

\S4method{new_taxonomy}{PostgreSQLConnection,missing}(
  conn,
  taxonomy,
  schema,
  schema_refs,
  add_attributes,
  add_to_names,
  ...
)

\S4method{new_taxonomy}{PostgreSQLConnection,taxlist}(conn, obj, ...)
}
\arguments{
\item{conn}{A \link[RPostgreSQL:PostgreSQLConnection-class]{RPostgreSQL::PostgreSQLConnection} connecting to a
database, where the new list will be stored.}

\item{obj}{A \link[taxlist:taxlist-class]{taxlist::taxlist} object containing the new database.}

\item{...}{Further arguments passed among methods (not in use).}

\item{taxonomy}{A character value indicating the name (ID) of the new
taxonomy in the database.}

\item{schema}{A character value with the name of the new schema containing
the taxonomic list.}

\item{schema_refs}{A character value with the name of the new schema
containing the references used as taxon views. It is recommended to have
a separated schema for the references, especially if the views are
formated as a BibTeX database (\link[biblio:lib_df-class]{biblio::lib_df} object).}

\item{add_attributes}{A data frame used to add further columns into the table
\strong{taxon_attributes} in the database. The data frame has three mandatory
columns, \strong{name}, \strong{type} (type of variable, which may include
constraints) and \strong{comment}. See \code{\link[divDB:add_columns]{divDB::add_columns()}} for more
details.}

\item{add_to_names}{A data frame used to add further columns into the table
\strong{taxon_names} in the database. See details for argument
\code{'add_attributes'} and in \code{\link[divDB:add_columns]{divDB::add_columns()}}.}
}
\description{
Create a new schema in a connected database containing a taxonomic list from
a taxlist object.

If 'obj' is missing an empty database will be created. You can than use
\code{\link[=append_taxonomy]{append_taxonomy()}} to insert a taxonomy in the empty database.
}
