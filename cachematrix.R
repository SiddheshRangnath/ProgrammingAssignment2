## Below functions are used to create a special object that 
## stores a matrix and caches its inverse


##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

##cacheSolve:## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  invr <- x$getInverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setInverse(invr)
  invr
}
