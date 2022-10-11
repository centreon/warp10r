#' Store
#'
#' The STORE function stores a value in a symbol.
#'
#' @inheritParams documentation
#' @param symbol Name of the symbol to modify.
#' @param value Value to store under symbol.
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("42", add = "long") %>%
#'   wrp_store("foo") %>%
#'   wrp_exec()
#' @export
#'
wrp_store <- function(wrp_con, symbol = NULL, value = NULL) {
  script <- "STORE"
  stack <- get_stack(wrp_con)
  if (is.null(symbol) && is.null(value)) {
    consume <- list(stack[[length(stack)]], "string")
  } else if (is.null(value)) {
    consume <- stack[length(stack)]
    script  <- glue::glue("{sanitize(symbol)} STORE")
  } else if (is.null(symbol)) {
    consume <- list("string")
    script <- glue::glue("{sanitize(value)} SWAP STORE")
  } else {
    consume <- list()
    script <- glue::glue("{sanitize(value)} {sanitize(symbol)} STORE")
  }
  set_script(wrp_con, script = script, consume = consume, add = list())

  return(wrp_con)
}
