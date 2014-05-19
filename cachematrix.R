## maekeCachematrix takes a matrix and creates  special a "matrix" object that can cache its inverse
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## maheCacheMatrix takes a matrix x and returns four functions. The first function set stores the matrix in a different environment.
## the function get when called gets the matrix x. The function setinv stores the inverse of x in a different environment and the call to the 
## function getinv fetches the inverse from this environment.
makeCacheMatrix <- function(x = matrix()) {
 invX <- NULL
  set <- function(y) {
    X <<- y
    invX <<- NULL
  }
  get <- function() X
  setinv <- function(inverse) invX <<- inverse
  getinv <- function() invX
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes the special matrix created by the makeCacheMatrix. If its inverse is already stored then it fetches the inverse, 
## else it computes the inverse of x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- X$getinv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- X$get()
  invX <- solve(data, ...)
  X$setinv(invX)
  invX
}
