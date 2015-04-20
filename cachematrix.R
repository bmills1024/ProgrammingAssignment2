## This pair of functions will cache the inverse of the provided matrix.
## When the matrix changes, the inverse will be recomputed.
## NOTE:  matrix "x" must be a square invertible matrix
## NOTE2:  Solution is a virtual cut and paste of example provided by rdpeng

## Example to test functions
##
## > a <- makeCacheMatrix(matrix(1:4, 2, 2))
## > i <- cacheSolve(a)
## > i <- cacheSolve(a)
## getting cached data
##
## End Example

## This function creates a list containing the original matrix, its inverse, 
##  and two functions to set cached values

makeCacheMatrix <- function(x = matrix()) {
  ## Clear solution/inverse
  s <- NULL
  
  ## function will cache new matrix and clear old solution
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## Returns matrix
  get <- function() x
  ## Compute and cache solution/inverse
  setsolve <- function(solve) s <<- solve
  ## Return cached solution/inverse
  getsolve <- function() s
  ## return a list containing the four previous objects
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Function returns the inverse of matrix "x" using cached value if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Try to retrieve solution/inverse from cache
  s <- x$getsolve()
  ## Return cached value if available
  ## Check for cached value
  if(!is.null(s)) {
    ## Let user know cached value is used
    message("getting cached data")
    ## Return cached value
    return(s)
  }
  ## Compute and cache solution/inverse if not available
  ## Get original matrix
  data <- x$get()
  ## Compute inverse using R function 
  s <- solve(data, ...)
  ## Cache computed solution/inverse
  x$setsolve(s)
  ## Return computed solution/inverse
  s
}
