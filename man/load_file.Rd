% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_file}
\alias{load_file}
\title{Load file}
\usage{
load_file(
  wrp_con,
  file,
  ...,
  return_object = c("none", "list", "map", "string", "numeric", "gts", "lgts")
)
}
\arguments{
\item{wrp_con}{\link{wrp_connect} object.}

\item{file}{Path to the file}

\item{...}{other parameters that will be injected in the warpscript}

\item{return_object}{Type of object returned by the macro}
}
\description{
Load a warpscript from a file.
}
