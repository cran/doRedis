% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doRedis.R
\name{setFtinterval}
\alias{setFtinterval}
\title{Set the fault tolerance check interval in seconds.}
\usage{
setFtinterval(value = 30)
}
\arguments{
\item{value}{positive integer number of seconds}
}
\value{
\code{value} is invisibly returned.
}
\description{
Set the fault tolerance check interval in seconds.
}
\examples{
\dontrun{
setFtinterval(5)
foreach(j=1:10) \%dopar\% j
}

}
