% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redux.R
\name{redisGet}
\alias{redisGet}
\title{A convenience function to return an R value from a Redis key.}
\usage{
redisGet(key)
}
\arguments{
\item{key}{(character or raw) Redis key name}
}
\value{
Unserialized R value.
}
\description{
This function assumes the value associated with the Redis key
is a serialized (binary) R value and unserializes it on return.
}
\seealso{
\code{\link{hiredis}}
}
