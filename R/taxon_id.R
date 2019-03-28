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
#'
#' # a null taxon_name object
#' taxon_name(NULL)
#'
taxon_id <- function(id, database = NULL, url = NULL) {
  database <- clone_if_r6(database)
  TaxonId$new(id = id, database = database, url = url)
}

#' @export
TaxonId <- R6::R6Class(
  "TaxonId",
  public = list(

    initialize = function(id = NULL, database = NULL, url = NULL) {
      self$database <- database
      self$url <- url
      self$id <- id
    },

    print = function(indent = "") {
      cat(paste0(indent, sprintf("<TaxonId> %s\n", char_or_placeholder(self$id))))
      cat(paste0(indent, paste0("  url: ", char_or_placeholder(self$url), "\n")))
      cat(paste0(indent, paste0("  database: ", char_or_placeholder(self$database), "\n")))
      invisible(self)
    }
  ),

  active = list(

    id = function(value) {
      if (missing(value)) { # GET
        return(private$my_id)
      }
      else { # SET
        if (is.null(value)) {
          private$my_id <- NULL
        } else {
          check_arg_class(value, c("character", "TaxonId", "numeric", "factor", "integer"), "taxon id")
          if (! is.null(self$database)) {
            invalid_ids <- value[! self$database$is_valid_id(value)]
            if (length(invalid_ids) > 0) {
              stop(call. = FALSE, 'Taxon IDs must follow the database ID conventions if a database with a defined ID regex is specified. ',
                   'The following IDs do not match the regex "', self$database$id_regex, '" for database "', self$database$name, '":\n',
                   limited_print(invalid_ids, type = "silent", prefix = "  "))
            }
          }
          private$my_id <- as.character(value)
        }
      }
    },

    database = function(value) {
      if (missing(value)) { # GET
        return(private$my_database)
      }
      else { # SET
        if (is.null(value)) {
          private$my_database <- NULL
        } else {
          private$my_database <- as_TaxonDatabase(value)
        }
      }
    },

    url = function(value) {
      if (missing(value)) { # GET
        return(private$my_url)
      } else { # SET
        if (is.null(value)) {
          private$my_url <- NULL
        } else {
          private$my_url <- value
        }
      }
    }

  ),

  private = list(
    my_id = NULL,
    my_database = NULL,
    my_url = NULL
  )
)


#' @export
as.character.TaxonId <- function(obj) {
  as.character(obj$id)
}


#' @export
as.TaxonId <- function(input) {
  if ("TaxonId" %in% class(input)) {
    return(input)
  } else {
    return(taxon_id(input))
  }
}

#' @export
as_TaxonId <- as.TaxonId
