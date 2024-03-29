% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doRedis.R
\name{setChunkSize}
\alias{setChunkSize}
\title{Set the default granularity of distributed tasks.}
\usage{
setChunkSize(value = 1)
}
\arguments{
\item{value}{positive integer chunk size setting}
}
\value{
\code{value} is invisibly returned; this value is called for its side effect.
}
\description{
A job is the collection of all tasks in a foreach loop.
A task is a collection of loop iterations of at most size \code{chunkSize}.
R workers are assigned work by task in blocks of at most
\code{chunkSize} loop iterations per task.
The default value is one iteration per task.
Setting the default chunk size larger for shorter-running jobs can
substantially improve performance. Setting this value too high can
negatively impact load-balancing across workers, however.
}
\examples{
# Only run if a Redis server is running
if (redux::redis_available()) {

# Start a single local R worker process
startLocalWorkers(n=1, queue="jobs", linger=1)

# Register the work queue with the coordinator R process
registerDoRedis("jobs")

# Compare verbose task submission output from...
setChunkSize(1)
foreach(j=1:4, .combine=c, .verbose=TRUE) \%dopar\% j

# with the verbose task submission output from:
setChunkSize(2)
foreach(j=1:4, .combine=c, .verbose=TRUE) \%dopar\% j

# Clean up
removeQueue("jobs")
}

}
