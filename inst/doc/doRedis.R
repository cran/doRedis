### R code from vignette source 'doRedis.Rnw'

###################################################
### code chunk number 1: doRedis.Rnw:202-210 (eval = FALSE)
###################################################
## library("doRedis")
## registerDoRedis("RJOBS")
## startLocalWorkers(n=2, queue="RJOBS")
## foreach(icount(10), .combine=sum,
##           .multicombine=TRUE, .inorder=FALSE) %dopar%
##           4 * sum((runif(1000000) ^ 2 + runif(1000000) ^ 2) < 1) / 10000000
## 
## # [1] 3.144212


###################################################
### code chunk number 2: doRedis.Rnw:295-306 (eval = FALSE)
###################################################
## require("doRedis")
## registerDoRedis("RJOBS", ftinterval=5, chunkSize=2)
## startLocalWorkers(n=4, queue="RJOBS", linger=1)
## cat("Workers started.\n")
## start <- Sys.time()
## x <- foreach(j=1:4, .combine=sum) %dopar%
##      {
##        if(difftime(Sys.time(),start) < 5) quit(save="no") else j
##      }
## 
## removeQueue("RJOBS")


###################################################
### code chunk number 3: doRedis.Rnw:343-386 (eval = FALSE)
###################################################
## bootForEach <- function (data, statistic, R, sim="ordinary",
##                  stype="i", strata=rep(1, n), L=NULL, m=0,
##                  weights=NULL, ran.gen=function(d, p) d,
##                  mle=NULL, simple=FALSE, chunks=1,
##                  verbose=FALSE, ...)
## {
##   thisCall <- match.call()
##   n <- if (length(dim(data)) == 2) nrow(data)
##   else length(data)
##   if(R < 2) stop("R must be greater than 1")
##   Rm1 <- R - 1
##   RB <- floor(Rm1 / chunks)
## 
##   combo <- function(...)
##   {
##     al <- list(...)
##     out <- al[[1]]
##     t <- lapply(al, "[[", "t")
##     out$t <- do.call("rbind", t)
##     out$R <- R
##     out$call <- thisCall
##     class(out) <- "boot"
##     out
##   }
##   
## # We define an initial bootstrap replicate locally. We use this
## # to set up all the components of the bootstrap output object
## # that don't vary from run to run. This is more efficient for
## # large data sets than letting the workers return this information.
##   binit <- boot(data, statistic, 1, sim = sim, stype = stype, 
##                 strata = strata, L = L, m = m, weights = weights,
##                 ran.gen = ran.gen, mle=mle, ...)
##   
##   foreach(j=icount(chunks), .inorder=FALSE, .combine=combo,
##            .init=binit, .packages=c("boot","foreach"),
##            .multicombine=TRUE, .verbose=verbose)  %dopar% {
##    if(j == chunks) RB <- RB + Rm1 %% chunks
##    res <- boot(data, statistic, RB, sim = sim, stype = stype,
##                strata = strata, L = L, m = m, weights = weights,
##                ran.gen = ran.gen, mle=mle, ...)
##    list(t=res$t)
##  }
## }


###################################################
### code chunk number 4: doRedis.Rnw:589-601 (eval = FALSE)
###################################################
## startLocalWorkers(n=5, queue="jobs")
## registerDoRedis("jobs")
## 
## # Make all the workers use the same random seed initialization and the old
## # "Super-Duper" RNG:
## set.seed.worker <- function(n) {
##    RNGkind("Super-Duper")
##    set.seed(55)
## }
## foreach(j=1:5, .combine="c", .export="set.seed.worker") %dopar% runif(1)
## 
## #[1] 0.2115148 0.2115148 0.2115148 0.2115148 0.2115148


###################################################
### code chunk number 5: doRedis.Rnw:637-663 (eval = FALSE)
###################################################
## registerDoRedis("jobs")
## 
## set.seed(1)
## A <- matrix(rnorm(100), nrow=10)   # (Imagine that A is really big.)
## 
## # Partition the matrix into parts small enough to fit into Redis values
## # (less than 512 MB). We partition our example matrix into two parts:
## 
## A1 <- A[1:5,]      # First five rows
## A2 <- A[6:10,]     # Last five rows
## 
## # Let's explicitly assign these sub-matrices as Redis values. Manually breaking
## # up the data like this helps avoid putting too much data in the R environment
## # exported by foreach.
## 
## redisSet("A1", A1)
## redisSet("A2", A2)
## 
## ans <- foreach(j=1:2, .combine=c) %dopar% {
##   chunk <- sprintf("A%d", j)
##   mychunk <- redisGet(chunk)
##   sum(mychunk)
## }
## print(ans)
## 
## # [1] 6.216482 4.672254


