setwd('C:/Users/VaidasVB/Documents/Coursera-R')
## I simply set the input x as a matrix
## and then set the solved value "s" as a null
## then I changed every reference to "mean" to "solve"
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {    ## define the argument with default mode of "matrix"
  s <- NULL                                                       ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                                            ## define the set function to assign new 
    x <<- y                                                       ## value of matrix in parent environment
    s <<- NULL                                                    ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                                             ## define the get fucntion - returns value of the matrix argument
  setsolve <- function(solve) s <<- solve                         ## assigns value of inv in parent environment
  getsolve <- function() s                                        ## gets the value of inv where called
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)   ## you need this in order to refer 
}
##
## Same here, changed "mean" to "solve" and "m" to "s"
cacheSolve <- function(x, ...) {                                  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
