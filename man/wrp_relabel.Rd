% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrp_relabel.R
\name{wrp_relabel}
\alias{wrp_relabel}
\title{Relabel}
\usage{
wrp_relabel(wrp_con, ..., .l = NULL)
}
\arguments{
\item{wrp_con}{\link{wrp_connect} object.}

\item{...}{Named parameters of label names to values.}

\item{.l}{Named parameters passed as a list.}
}
\description{
Modifies the labels of a Geo Time Series™.
The RELABEL function expects a parameter MAP whose entries are labels to set or alter.
}
\details{
If the parameter MAP has an entry with a NULL key, the RELABEL function will use the rest of the MAP
as the actual labels to set for the GTS.
If no NULL key exist, then the other entries of the MAP will alter the existing labels.

An entry with an empty STRING value or a NULL value will have the effect of removing the label from the GTS' labels.
}
\examples{

wrp_connect() \%>\%
  wrp_new_gts() \%>\%
  wrp_clone() \%>\%
  wrp_relabel(foo = "bar", bar = "foo") \%>\%
  wrp_clone() \%>\%
  wrp_relabel("NULL" = NULL, "star" = "treck") \%>\%
  wrp_clone() \%>\%
  wrp_relabel("next" = "generation", heckle = "jeckle") \%>\%
  wrp_clone() \%>\%
  wrp_relabel(heckle =NULL) \%>\%
  wrp_clone() \%>\%
  wrp_relabel("next" = "") \%>\%
  wrp_exec()
}
\seealso{
\code{\link[=wrp_set_attributes]{wrp_set_attributes()}}, \code{\link[=wrp_rename]{wrp_rename()}}
}
