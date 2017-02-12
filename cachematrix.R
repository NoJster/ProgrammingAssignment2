## These two functions allow for caching the inverse of a matrix, to save
## compuation time for this CPU intensive operation
## For this, two functions are defined:
##  - makeCacheMatrix: allows for caching a matrix and its inverse
##  - cacheSolve: computing and storing the inverse

## allows for caching a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## making sure that if a new matrix is set, we store its value
  ## and reset our inverse (since we haven't calculated it yet)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  ## return the original base matrix
  get <- function() x
  
  ## set new value for the inverse
  setinv <- function(mat) i <<- mat
  
  ## return value of the inverse
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This is the function to which we pass the function (which is really  a list)
## "makeCacheMatrix" and compute and store the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  
  ## in case we already know the inverse, we skip recomputing it and return the cached value
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  ## getting our base matrix ...
  data <- x$get()
  
  ## ... passing it to the R solver (making sure to catch any errors) ...
  i <- try(solve(data, ...))
  
  ## ... and storing and displaying the result
  x$setinv(i)
  i
}
