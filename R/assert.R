#' Assert Endpoint
#'
#' Check that endpoint is available.
#'
#' @family assert
#'
#' @param endpoint A string defining the endpoint of the Warp 10 database.
#'
assert_endpoint <- function(endpoint) {
  if (is.null(endpoint)) {
    stop(
      paste(
        "Endpoint is required to fetch data from Warp 10 database.",
        "Endpoint can be set as an R option with `options(\"warp10\" = c(endpoint = \"...\"))`.",
        "Or it can be provided directly."
      )
    )
  }
}

#' Assert Token
#'
#' Check that token is available.
#'
#' @family assert
#'
#' @param token A string defining the token used to fetch or update data.
#'
assert_token <- function(token) {
  if (is.null(token)) {
    stop(
      paste(
        "Token is required to fetch data from endpoint.",
        "Token can be set as an R option with `options(\"warp10\" = c(token = \"...\"))`.",
      )
    )
  }
}


#' Assert Labels
#'
#' Check that labels are constructed correctly.
#'
#' @family assert
#'
#' @param labels A list of labels to be used in the query.
#'
#' @return A list of labels or NULL.
#'
assert_labels <- function(labels) {
  if (is.null(labels)) {
    return(NULL)
  }
  if (!is.list(labels) && !is.character(labels)) {
    stop(
      paste(
        "Labels must be a list of strings, not a ", typeof(labels)
      )
    )
  }
  if (length(nchar(names(labels))) == 0 || any(nchar(names(labels)) == 0)) {
    stop(
      paste(
        "Labels must be a named list."
      )
    )
  }
  if (any(sapply(labels, length) != 1)) {
    stop(
      paste(
        "Labels must be a list of strings with length 1."
      )
    )
  }
  return(lapply(labels, as.character))
}