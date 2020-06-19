## makeCacheMatrix function creates a special matrix object whose inverse can be cached.
## cacheSolve function computes inverse of matrix and if it's already calculated then function retrieves the value from cache.  

## Creates special matrix object 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes inverse of matrix if not found earlier else takes its value from cache.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}


