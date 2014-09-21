## makeCacheMatrix creates a special matrix object.

makeCacheMatrix <- function(x = matrix()) {
  inv_of_x <- NULL
  set <- function(y) {
    x <<- y
    inv_of_x <<- NULL
  }
  get <- function() x
  makeinverse <- function(inverse) inv_of_x <<- inverse
  getinverse <- function() inv_of_x
  list(set, get = get,
       makeinverse = makeinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_of_x <- x$makeinverse()
  if (!is.null(inv_of_x)) {
    message("Here is the cached inverse matrix: ")
    return(inv_of_x)
  }
  else {
    inv_of_x <-solve(x$get())
    x$makeinverse(inv_of_x)
    return(inv_of_x)
  }
}
