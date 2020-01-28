### R code from vignette source 'Debug.Rnw'

###################################################
### code chunk number 1: Debug.Rnw:39-58
###################################################
cat('
library(foreach)
registerDoSEQ()

foreach(j=1:3, .errorhandling="pass") %dopar%
{
  if(j==2) j + undefined_variable   else j
}

# [[1]]
# [1] 1
# 
# [[2]]
# <simpleError in eval(expr, envir, enclos): 
#          object \'undefined_variable\' not found>
# 
# [[3]]
# [1] 3
')


###################################################
### code chunk number 2: Debug.Rnw:75-103
###################################################
cat('
library(doRedis)
registerDoRedis("cazart")

startLocalWorkers(n=1, timeout=1, queue="cazart",
                  log="/tmp/cazart.log", loglevel=1)

x <- foreach(j=1:3) %dopar%
{
  logger(paste("hello from loop iteration ", j))
  j
}

# Remove the work queue (terminating the worker process)
removeQueue("cazart")
Sys.sleep(2) # wait for the worker to terminate

# Display the log file
file.show("/tmp/cazart.log")

# Processing task(s) 1...1 from queue cazart ID 9ba32925bee...
# @ 2016-04-23 20:17:26 hostname 3406   hello from loop iteration  1 
# Processing task(s) 2...2 from queue cazart ID 9ba32925bee...
# @ 2016-04-23 20:17:26 hostname 3406   hello from loop iteration  2 
# Processing task(s) 3...3 from queue cazart ID 9ba32925bee...
# @ 2016-04-23 20:17:26 hostname 3406   hello from loop iteration  3 
# Normal worker exit.
')


###################################################
### code chunk number 3: Debug.Rnw:144-154
###################################################
cat('
library(doRedis)
registerDoRedis("cazart")
x <- foreach(j=1:3) %dopar%
{
  logger(paste("  log message from iteration", j))
  j
}
removeQueue("cazart")  # remove the work queue
')


###################################################
### code chunk number 4: Debug.Rnw:157-170
###################################################
cat('
library(doRedis)
redisWorker(queue="cazart", loglevel=1, timeout=1)

# Waiting for doRedis jobs.
# Processing task(s) 1...1 from queue cazart ID 9ba77ba191c...
# @ 2016-04-23 21:09:04 homer 3882   log message from iteration 1
# Processing task(s) 2...2 from queue cazart ID 9ba77ba191c...
# @ 2016-04-23 21:09:04 homer 3882   log message from iteration 2
# Processing task(s) 3...3 from queue cazart ID 9ba77ba191c...
# @ 2016-04-23 21:09:04 homer 3882   log message from iteration 3
# Normal worker exit.
')


