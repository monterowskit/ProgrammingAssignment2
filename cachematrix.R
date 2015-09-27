## Put comments here that give an overall description of what your
## functions do

## The first function, makeVector creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the iverse matrix
# get  the iverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(NA,nrow=nrow(x),ncol=ncol(x))
  set <- function(y) {
    x <<- y
    m <<- matrix(NA,nrow=nrow(x),ncol=ncol(x))
  }
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Write a short comment describing this function
#function calculates the mean of the special "matrix" 
# created with the above function.
#it first checks to see if the inverse of a matrix has 
# already been calculated. If so, it gets iverted matrix 
# from the cache and skips the computation. 
# Otherwise, it calculates the verted matrix of the 
# data and sets the iverse matrix in the 
# cache via the setmean function.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(any(!is.na(m))) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
