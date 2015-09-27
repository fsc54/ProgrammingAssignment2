## Programming Assigment #2:
## UPLOADED FOR PEER ASSESSMENT
##     1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##     2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##          If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  S <- NULL               ## remove contents of S
  set <- function(y) {    ## sets the value of the matrix 
    x <<- y               ## matrix = x 
    S <<- NULL            ## inverse = NULL 
  }
  get <- function() x       ## will return x
  setS <- function(solve) S <<- solve    ## will set the value of the inverse
  getS <- function() S                   ## will return the value of the inverse
  list(set = set, get = get,            ## make a list of functions for setting value, getting value (of matrix)
       setS = setS,                     ## setting value of inverse,
       getS = getS)                     ## returning value of inverse
}


## Write a short comment describing this function
# check if inverse has already been calculated for matrix x
# if inverse is cached, return cached inverse
# if inverse has not been cached, calculate inverse and return calculated inverse

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'y'
        ## this code should return the inverse, whether the submitted matrix is just a matrix or the special matrix object made by makeCacheMatrix
  if (!is.list(y)) {  ## if submitted matrix is not a list, make special matrix object
    x = makeCacheMatrix(y)
  }
  else {  ## if submitted matrix was the special matrix object, proceed
    x = y
  }
  S <- x$getS()  ## S is now the getS component of the matrix object
  if(!is.null(S)) {  ## if S is not = null, then the inverse has been calculated before
    message("getting cached data")  ## say you are retrieving cached data and ...
  return(S)  ## return cached inverse matrix, and exit function
  }
  data <- x$get()  ## otherwise data = the get() component of the x matrix object
  S <- solve(data, ...)  ## solve data (calculate the inverse matrix) set S to it
  x$setS(S)  ## store the inverse matrix in the matrix object
  S  ## return the inverse matrix
}
