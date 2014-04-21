## The following pair of functions implement a special matrix that can cache
## its own inverse. 
##
## makeCacheMatrix creates the special matrix, which is actually a list of
## functions that can get/set the matrix and get/set the inverse of the matrix.
##
## cacheSolve computes the inverse of a special matrix created by makeCacheMatrix.
## If the inverse of the matrix has already been computed, it returns the 
## cached copy of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a list of getter/setter functions for both the matrix and its 
  # inverse. This function could be thought of as a "factory" for special 
  # matrices that can cache their inverse.
  #
  # Args: 
  #   x: The matrix for which the special (inverse caching) matrix is to 
  #      be created.
  #
  # Returns:
  #   A list of functions: set: sets the value of the matrix
  #                        get: returns the value of the matrix
  #                        setinverse: sets the inverse of the matrix
  #                        getinverse: returns the inverse of the matrix
  #                        
  if (!is.matrix(x)) { # error handling
    stop("x must be a matrix")
  }
  inv <- NULL # initalize the inverse
  # define the matrix setter function
  set <- function(y) {
    if (!is.matrix(y)) { # error handling
      stop("The argument for set must be a matrix.")
    }
    x   <<- y
    inv <<- NULL # re-initialize the inverse
  }  
  # define the matrix getter function
  get <- function() x
  # define the inverse setter function
  setinverse <- function(inverse) inv <<- inverse
  # define the inverse getter function
  getinverse <- function() inv
  # create and return the list of functions that comprise the special matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

cacheSolve <- function(x, ...) {
  # Returns a matrix that is the inverse of the special matrix 'x'. If x already
  # contains a cached copy of its inverse, the cached copy is returned. Otherwise, 
  # the inverse is computed and cached before it is returned.
  #
  # Args:
  #   x: The special matrix for which the inverse is desired. The matrix must
  #      be a square invertible matrix.
  #
  # Returns:
  #   A matrix that is the inverse of x.
  inv <- x$getinverse()
  # if the special matrix already contains its inverse, return the cached copy
  if(!is.null(inv)) {
    message("getting cached inverse...")
    return(inv)
  }
  # otherwise, compute the inverse
  inv <- solve(x$get())
  # cache the result, so subsequent calls do not need to recompute the inverse
  x$setinverse(inv)
  # and return the resulting inverse
  inv
}
