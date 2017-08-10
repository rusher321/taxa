#' Taxon ID class
#'
#' Used to store taxon IDs, either arbitrary or from a taxonomy database. This
#' is typically used to store taxon IDs in [taxon()] objects.
#'
#' @export
#' @param id (character/integer/numeric) a taxonomic id, required
#' @param url (character) url/uri for the taxon, should be associated with the
#' database given to the `database` parameter
#' @param database (database) database class object, optional
#'
#' @return An `R6Class` object of class `TaxonId`
#' @family classes
#'
#' @examples
#' (x <- taxon_id(12345))
#' x$id
#' x$database
#'
#' (x <- taxon_id(
#'   12345,
#'   "https://www.ncbi.nlm.nih.gov/taxonomy/12345",
#'   database_list$ncbi
#' ))
#' x$id
#' x$database
taxon_id <- function(id, url = NULL, database = NULL) {
  TaxonId$new(
    id = id,
    url = url,
    database = database
  )
}

TaxonId <- R6::R6Class(
  "TaxonId",
  public = list(
    id = NULL,
    url = NULL,
    database = NULL,

    initialize = function(id = NULL, url = NULL, database = NULL) {
      assert(id, c("character", "integer", "numeric"))
      assert(url, "character")
      assert(database, c("character", "TaxonDatabase"))

      # Convert characters to appropriate classes
      if (is.character(database)) {
        database <- taxon_database(database)
      }

      self$id <- id
      self$url <- url
      self$database <- database
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<TaxonId> %s\n", self$id)))
      cat(paste0(indent, paste0("  url: ", self$url, "\n")))
      cat(paste0(indent, paste0("  database: ",
                                get_database_name(self$database) %||% "none",
                                "\n")))
      invisible(self)
    }
  )
)
