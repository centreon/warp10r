#' Set Script
#'
#' Add command to a Warp 10 script.
#'
#' @inheritParams documentation
#' @param script A script to be set as a string.
#' @param consume What object(s) the warpscript is consuming from the stack.
#' @param add What object the warpscript is returning in the stack. If NULL,
#'   consuming object is automatically determined by the object.
#'
#' @export
#' @examples
#' wrp_connect() %>%
#'   set_script(list(a = 1)) %>%
#'   set_script("toto")
#'
set_script <- function(wrp_con, script = "", consume = NULL, add = "auto") {
  # For compatibility reasons
  if (is_auto(add)) {
    cl <- class(script)
    if (is_not_list(script, cl)) {
      if (cl == "character") {
        add <- "string"
      } else if (is_numeric(cl)) {
        add <- "numeric"
      }
    } else {
      script <- as.list(script)
      if (is_named_list(script)) {
        add <- "map"
      } else {
        add <- "list"
      }
    }
    script <- sanitize(script)
  }
  wrp_con$set_script(script)
  wrp_con$add_stack(add, consume)
  wrp_con
}

is_auto <- function(add) {
  !is.null(add) && length(add) == 1 && add == "auto"
}

is_not_list <- function(script, cl) {
  length(script) == 1 && cl != "list"
}

is_numeric <- function(cl) {
  cl == "numeric" || cl == "integer"
}

is_named_list <- function(script) {
  !is.null(names(script)) && all(names(script) != "")
}