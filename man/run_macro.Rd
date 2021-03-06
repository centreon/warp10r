% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_macro.R
\name{run_macro}
\alias{run_macro}
\title{Run Macro}
\usage{
run_macro(wrp_con, macro, return_object = NULL, ..., .eval = FALSE)
}
\arguments{
\item{wrp_con}{\link{wrp_connect} object.}

\item{macro}{Name of the macro}

\item{return_object}{Type of object returned by the macro}

\item{...}{other parameters passed to macro}

\item{.eval}{Should be TRUE if macro built within warpscript}
}
\description{
Run a macro either stored in warp10 repository (accessible via \verb{@macro_name}), or a macro built with warpscript.
}
