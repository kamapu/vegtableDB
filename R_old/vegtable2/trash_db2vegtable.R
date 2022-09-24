# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

## This code may still be of use

#' @rdname db2vegtable
#'
#' @aliases import_swea
#'
#' @export
import_swea <- function(conn,
    header = c("swea_dataveg", "header"),
    samples = c("swea_dataveg", "samples"),
    relations = list(
        globe_plots = c("swea_dataveg", "globe_plots"),
        swea1_code = c("swea_dataveg", "swea1_code"),
        soil_moisture = c("swea_dataveg", "soil_moisture"),
        soil_texture = c("swea_dataveg", "soil_texture"),
        community_type = c("commons", "community_type"),
        naturalness = c("swea_dataveg", "naturalness"),
        record_type = c("swea_dataveg", "record_type")
    ),
    layers = list(
        veg_layer = c("swea_dataveg", "veg_layer"),
        spec_miguel = c("specimens", "specimens_miguel")
    ),
    coverconvert = list(
        br_bl = c("coverconvert", "br_bl"),
        b_bbds = c("coverconvert", "b_bbds"),
        ordinal = c("coverconvert", "ordinal")
    ),
    geometry = "plot_centroid",
    get_countries = TRUE,
    get_data_sources = TRUE,
    bib_args = list(),
    taxon_names = c("tax_commons", "taxon_names"),
    taxon_relations = c("swea_dataveg", "taxon_concepts"),
    taxon_traits = c("swea_dataveg", "taxon_attributes"),
    taxon_views = c("bib_references", "main_table"),
    taxon_levels = c("tax_commons", "taxon_levels"),
    names2concepts = c("swea_dataveg", "names2concepts"),
    ...) {
  # Final object
  message("Importing vegtable body...")
  suppressMessages(veg_obj <- db2vegtable(
          conn = conn, header = header,
          samples = samples, relations = relations, layers = layers,
          coverconvert = coverconvert, geometry = geometry,
          taxon_names = taxon_names,
          taxon_relations = taxon_relations,
          taxon_traits = taxon_traits, taxon_views = taxon_views,
          taxon_levels = taxon_levels,
          names2concepts = names2concepts, ...
      ))
  # Adding Country codes
  message("Importing country codes...")
  if (get_countries) {
    Query <- paste0(
        "SELECT releve_id,adm0_a3\n",
        "FROM \"", paste0(header, collapse = "\".\""),
        "\",commons.countries_map\n",
        "WHERE ST_Intersects(commons.countries_map.unit,\"",
        paste0(header, collapse = "\".\""), "\".plot_centroid);\n"
    )
    Countries <- dbGetQuery(conn, Query)
    veg_obj@header$country_code <- with(
        Countries,
        adm0_a3[match(veg_obj@header$ReleveID, releve_id)]
    )
    Countries <- dbGetQuery(conn, "SELECT * FROM commons.countries;")
    colnames(Countries) <- c(
        "country_code", "name_short", "name_long",
        "population", "sov_code_1", "sov_code_2", "sov_state", "continent"
    )
    veg_obj@relations$country_code <- Countries
  }
  # Adding Data sources
  message("Importing data sources...")
  if (get_data_sources) {
    data_source <- do.call(read_pg, c(
            conn = conn, name = "bib_references",
            bib_args
        ))
    data_source <- data_source[data_source$bibtexkey %in%
            veg_obj$bibtexkey, ]
    data_source$data_source <- seq_along(data_source$bibtexkey)
    veg_obj$data_source <- with(
        veg_obj@header,
        data_source$data_source[match(
                bibtexkey,
                data_source$bibtexkey
            )]
    )
    # Delete bibtexkey from header
    veg_obj@header <- veg_obj@header[, colnames(veg_obj@header) !=
            "bibtexkey"]
    # Delete empty columns
    data_source <- data_source[, apply(
            data_source, 2,
            function(x) !all(is.na(x))
        )]
    # Insert to relations
    veg_obj@relations$data_source <- data_source[, c(
            "data_source",
            colnames(data_source)[colnames(data_source) !=
                    "data_source"]
        )]
  }
  message("DONE!\n")
  return(veg_obj)
}

#' @rdname db2vegtable
#'
#' @aliases import_sam
#'
#' @export
import_sam <- function(conn,
    header = c("sudamerica", "header"),
    samples = c("sudamerica", "samples"),
    relations = list(
        community_type = c("commons", "community_type")
    ),
    layers = list(
        spec_miguel = c("specimens", "specimens_miguel")
    ),
    coverconvert = list(
        br_bl = c("coverconvert", "br_bl"),
        b_bbds = c("coverconvert", "b_bbds"),
        ordinal = c("coverconvert", "ordinal")
    ),
    geometry = "plot_centroid",
    get_countries = TRUE,
    get_data_sources = TRUE,
    bib_args = list(),
    taxon_names = c("tax_commons", "taxon_names"),
    taxon_relations = c("sudamerica", "taxon_concepts"),
    taxon_traits = c("sudamerica", "taxon_attributes"),
    taxon_views = c("bib_references", "main_table"),
    taxon_levels = c("tax_commons", "taxon_levels"),
    names2concepts = c("sudamerica", "names2concepts"),
    ...) {
  # Final object
  veg_obj <- import_swea(
      conn = conn, header = header, samples = samples,
      relations = relations, layers = layers,
      coverconvert = coverconvert, geometry = geometry,
      taxon_names = taxon_names,
      taxon_relations = taxon_relations,
      taxon_traits = taxon_traits, taxon_views = taxon_views,
      taxon_levels = taxon_levels,
      names2concepts = names2concepts, get_countries = get_countries,
      get_data_sources = get_data_sources, bib_args = bib_args, ...
  )
}

#' @rdname db2vegtable
#'
#' @aliases import_bernice
#'
#' @export
import_bernice <- function(conn,
    description = c(
        Title = "Parthenium survey in Lake Baringo",
        Author = "Bernice Mereina Sainepo",
        Source = "SWEA-Dataveg (GIVD-AF-00-006)",
        Version = Sys.Date()
    ),
    head_cols = c(
        "releve_id", "code_trr228", "original_number",
        "record_date", "plot_size", "data_source",
        "elevation"
    ),
    samples_cols = c(
        "record_id", "releve_id", "quadrant",
        "taxon_usage_id", "misspelled_name",
        "cover_percentage", "frequency"
    ),
    ...) {
  # header
  sql_header <- paste0(
      "SELECT \"", paste(head_cols, collapse = "\", \""),
      "\", ST_X(plot_centroid) longitude, ST_Y(plot_centroid) latitude\n",
      "FROM swea_dataveg.header\n", "WHERE data_source  =  98;\n"
  )
  veg_obj <- import_swea(
      conn = conn, description = description,
      sql_header = sql_header, ...
  )
  veg_obj@samples <- veg_obj@samples[, colnames(veg_obj@samples) %in%
          samples_cols]
  return(veg_obj)
}
