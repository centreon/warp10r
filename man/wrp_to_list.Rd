% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrp_to_list.R
\name{wrp_to_list}
\alias{wrp_to_list}
\title{To List}
\usage{
wrp_to_list(wrp_con, n)
}
\arguments{
\item{wrp_con}{\link{wrp_connect} object.}

\item{n}{The number of elements to take on the top of the stack to build the list.}
}
\description{
The \code{wrp_to_list} function create a list from N elements on the stack.
N and the N next elements on the stack are consumed.
}
\details{
If N is greater than current stack depth, the function raises an error.
}
\examples{
wrp_connect() \%>\%
  set_script("el1") \%>\%
  set_script(TRUE) \%>\%
  set_script("el4") \%>\%
  set_script(4) \%>\%
  wrp_to_list(4) \%>\%
  wrp_exec()
}
\keyword{conversion}
\keyword{lists,}
