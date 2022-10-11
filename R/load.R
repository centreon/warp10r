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
                      return_object = list(NULL, "list", "map", "character", "numeric", "gts", "lgts")) {
  params <- rlang::list2(...)
  return_object <- match.arg(return_object, several.ok = TRUE)
  if (length(params) > 0) {
    for (param in names(params)) {
      wrp_con %>%
        set_script(params[[param]], add = "character") %>%
        wrp_store(param)
    }
  }
  script <- readLines(file)
  add_stack(wrp_con, script, return_object)
}