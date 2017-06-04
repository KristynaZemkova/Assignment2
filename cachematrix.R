## Because a matrix inversion is usually too demanding when it comes to computation (especially for large matrices)
## so it is a good idea to use cashing instead of calculating it again and again.

## The function below (makeCashMatrix) creates a list of function: set/get the value of the matrix, set/get the value
## of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverze <- function(solve) m <<- solve
  getInverze <- function() m
  list(set = set, get = get,
       setInverze = setInverze,
       getInverze = getInverze)

}


## The below function (cacheSolve) has as an input the matrix inverse. But firstly, it checks whether the value has
## been calculated previously. In case of the value being computated previously, it does not compute it again. If not, 
## it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverze()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverze(m)
  m
}
