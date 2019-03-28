#' Taxon class
#'
#' A class used to define a single taxon. Most other classes in the taxa package
#' include one or more objects of this class.
#'
#' @export
#' @param name a TaxonName object [taxon_name()] or character string. if
#' character passed in, we'll coerce to a TaxonName object internally, required
#' @param rank a TaxonRank object [taxon_rank()] or character string. if
#' character passed in, we'll coerce to a TaxonRank object internally, required
#' @param id a TaxonId object [taxon_id()], numeric/integer, or character
#' string. if numeric/integer/character passed in, we'll coerce to a
#' TaxonId object internally, required
#' @param authority (character) a character string, optional
#' @param attributes (list) a named list of arbitrary attributes. if given,
#' all attributes must be named
#'
#' @details Note that there is a special use case of this function - you can
#' pass `NULL` as the first parameter to get an empty `taxon` object. It makes
#' sense to retain the original behavior where nothing passed in to the first
#' parameter leads to an error, and thus creating a `NULL` taxon is done very
#' explicitly.
#'
#' @return An `R6Class` object of class `Taxon`
#' @family classes
#'
#' @examples
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036)
#' ))
#' x$name
#' x$rank
#' x$id
#' 
#' # optionally add taxon authority
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036),
#'   authority = "L."
#' ))
#'
#' # add arbitrary attributes
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036),
#'   attributes = list(
#'     foo = "bar",
#'     hello = "world"
#'   )
#' ))
#' x$attributes
#'
#' # include a URL in your taxon_id() call - used in print method
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036, database_list$ncbi, "https://www.ncbi.nlm.nih.gov/taxonomy/93036")
#' ))
#' ## open the URL in default browser
#' x$id$url
#' x$browse()
taxon <- function(name, rank = NULL, id = NULL, authority = NULL,
                  attributes = NULL) {
  Taxon$new(
    name = clone_if_r6(name),
    rank = clone_if_r6(rank),
    id = clone_if_r6(id),
    authority = clone_if_r6(authority),
    attributes = clone_if_r6(attributes)
  )
}

#' @export
Taxon <- R6::R6Class(
  "Taxon",

  public = list(

    initialize = function(name = NULL, rank = NULL, id = NULL, 
      authority = NULL, attributes = NULL) {

      self$name <- name
      self$rank <- rank
      self$id <- id
      self$authority <- authority
      
      assert(attributes, 'list')
      if (!is.null(attributes)) {
        if (is.null(names(attributes))) {
          stop("`attributes` must be a named list")
        }
      }
      self$attributes <- attributes
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxon>\n"))
      cat(paste0(indent, paste0("  name: ", char_or_placeholder(self$name), "\n")))
      cat(paste0(indent, paste0("  rank: ", char_or_placeholder(self$rank), "\n")))
      cat(paste0(indent, paste0("  id: ", char_or_placeholder(self$id), "\n")))
      cat(paste0(indent, paste0("  authority: ", char_or_placeholder(self$authority), "\n")))
      
      cat(paste0(indent, "  attributes:\n"))
      if (!is.null(self$attributes) && length(self$attributes) > 0) {
        for (i in seq_along(self$attributes)) {
          cat(paste0(indent, paste0("   ", sprintf("%s: %s", names(self$attributes)[i], self$attributes[[i]]) %||% "none", "\n")))
        }
      }
      invisible(self)
    },

    browse = function() {
      if (!is.null(self$id$url)) browseURL(self$id$url) else message("no taxon URL")
    },

    is_empty = function(x) {
      is.null(self$name) && is.null(self$rank) && is.null(self$id)
    },

    get_url = function() {
      if ("TaxonId" %in% class(self$id)) {
        self$id$url
      } else {
        ""
      }
    }
  ),

  active = list(
    name = function(value) {
      if (missing(value)) { # GET
        return(private$my_name)
      }
      else { # SET
        if (is.null(value)) {
          private$my_name <- NULL
        } else {
          private$my_name <- as_TaxonName(value)
        }
      }
    },

    rank = function(value) {
      if (missing(value)) { # GET
        return(private$my_rank)
      }
      else { # SET
        if (is.null(value)) {
          private$my_rank <- NULL
        } else {
          private$my_rank <- as_TaxonRank(value)
        }
      }
    },

    id = function(value) {
      if (missing(value)) { # GET
        return(private$my_id)
      }
      else { # SET
        if (is.null(value)) {
          private$my_id <- NULL
        } else {
          private$my_id <- as_TaxonId(value)
        }
      }
    },

    authority = function(value) {
      if (missing(value)) { # GET
        return(private$my_authority)
      }
      else { # SET
        if (is.null(value)) {
          private$my_authority <- NULL
        } else {
          check_arg_class(value, c("character", "numeric", "factor", "integer"), "authority")
          private$my_authority <- as.character(value)
        }
      }
    },

    attributes = function(value) {
      if (missing(value)) { # GET
        return(private$my_attributes)
      }
      else { # SET
        if (is.null(value)) {
          private$my_attributes <- NULL
        } else {
          private$my_attributes <- value
        }
      }
    }

  ),

  private = list(
    my_name = NULL,
    my_rank = NULL,
    my_id = NULL,
    my_authority = NULL,
    my_attributes = NULL
  )
)


#' @export
as.character.Taxon <- function(obj) {
  as.character(obj$name)
}

#' @export
as.Taxon <- function(input) {
  if ("Taxon" %in% class(input)) {
    return(input)
  } else {
    return(taxon(input))
  }
}

#' @export
as_Taxon <- as.Taxon
