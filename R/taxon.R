#' Taxon class
#'
#' A class used to define a taxon.
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
#' @param attributes (list) a named list of arbitrary attributes
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
#'
#' # include a URL in your taxon_id() call - used in print method
#' (x <- taxon(
#'   name = taxon_name("Poa annua"),
#'   rank = taxon_rank("species"),
#'   id = taxon_id(93036, "https://www.ncbi.nlm.nih.gov/taxonomy/93036", database_list$ncbi)
#' ))
#' ## open the URL in default browser
#' x$browse()
taxon <- function(name, rank = NULL, id = NULL, authority = NULL,
                  attributes = NULL) {
  Taxon$new(
    name = name,
    rank = rank,
    id = id,
    authority = authority,
    attributes = attributes
  )
}

Taxon <- R6::R6Class(
  "Taxon",
  public = list(
    name = NULL,
    rank = NULL,
    id = NULL,
    authority = NULL,
    attributes = list(),

    initialize = function(
      name = NULL, rank = NULL, id = NULL, authority = NULL,
      attributes = NULL
    ) {
      assert(name, c('TaxonName', 'character'))
      assert(rank, c('TaxonRank', 'character'))
      assert(id, c('TaxonId', 'character', 'numeric', 'integer'))
      assert(authority, 'character')
      assert(attributes, 'list')
      if (!is.null(attributes)) {
        if (is.null(names(attributes))) {
          stop("`attributes` must be a named list")
        }
      }

      # Convert characters to appropriate classes
      if (is.character(name)) {
        name <- taxon_name(name)
      }
      if (is.character(rank)) {
        rank <- taxon_rank(rank)
      }
      if (is.character(id)) {
        id <- taxon_id(id)
      }

      self$name <- name
      self$rank <- rank
      self$id <- id
      self$authority <- authority
      self$attributes <- attributes
    },

    print = function(indent = "") {
      cat(paste0(indent, "<Taxon>\n"))
      cat(paste0(indent, paste0("  name: ",
                                private$get_name() %||% "none", "\n")))
      cat(paste0(indent, paste0("  rank: ",
                                private$get_rank() %||% "none", "\n")))
      cat(paste0(indent, paste0("  id: ",
                                private$get_id() %||% "none", "\n")))
      cat(paste0(indent, paste0("  url: ",
                                private$get_url() %||% "none", "\n")))
      cat(paste0(indent, paste0("  authority: ",
                                self$authority %||% "none", "\n")))
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
    }
  ),

  private = list(
    get_name = function() {
      if ("TaxonName" %in% class(self$name)) {
        output <- self$name$name
      } else {
        output <- self$name
      }
      return(output)
    },

    get_rank = function() {
      if ("TaxonRank" %in% class(self$rank)) {
        output <- self$rank$name
      } else {
        output <- self$rank
      }
      return(output)
    },

    get_id = function() {
      if ("TaxonId" %in% class(self$id)) {
        output <- self$id$id
      } else {
        output <- self$id
      }
      return(output)
    },

    get_url = function() {
      if ("TaxonId" %in% class(self$id)) {
        self$id$url
      } else {
        ""
      }
    }
  )
)
