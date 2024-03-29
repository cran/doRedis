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
\code{value} is invisibly returned (this function is used for its side effect).
}
\description{
Failed tasks are automatically re-submitted to the work queue.
The \code{setFtinterval} sets an upper bound on how frequently
the system checks for failure. See the package vignette for
discussion and examples.
}
