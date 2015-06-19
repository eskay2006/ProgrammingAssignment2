## These two functions help in minimizing the expensive operation of inverting a matrix.
## We need to calculate inverse of a matrix only when the contents of the matrix changes. Otherwise we could use the cached 
## inverted matrix 


## makeCacheMatrix returns a special vector that contains functions to get and set the value of the matrix 
## and to get and set the value of the inverted matrix
makeCacheMatrix <- function (x=matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  
  get <- function() x
  set_inversion <- function (matrix_f) inverted <<- solve(matrix_f)
  get_inverted <- function () inverted
  list (set = set, get = get, set_inv = set_inversion, get_inv = get_inverted)
}


## cacheSolve function performs matrix inversion only if the inverted matrix is empty

cacheSolve <-function(x, ...) {
  inverted <- x$get_inv()
  if (!is.null(inverted)) { 
    print ("getting cached data")
    return(inverted)
  }
  
  data <- x$get()
  inverted <- solve(data)
  x$set_inv(inverted)
  inverted
}
