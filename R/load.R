#' Load file
#'
#' Load a warpscript from a file.
#'
#' @inheritParams documentation
#' @param file Path to the file
#' @param ... other parameters that will be injected in the warpscript
#' @param return_object Type of object returned by the macro
#'
#' @export
#'
load_file <- function(wrp_con, file, ...,
                      return_object = c("none", "list", "map", "string", "numeric", "gts", "lgts")) {
  params <- rlang::list2(...)
  return_object <- match.arg(return_object, several.ok = TRUE)
  if (return_object[1] == "none") {
    return_object <- list()
  }
  if (length(params) > 0) {
    for (param in names(params)) {
      wrp_store(wrp_con, .value = sanitize(params[[param]]), .symbol = param)
    }
  }
  script <- paste(readLines(file), collapse = "\n")
  add_stack(wrp_con, script, return_object)
}