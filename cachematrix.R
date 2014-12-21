## This R function is able to cache potentially time-consuming inverse matrix computations

## The first function, 'makeCacheMatrix' creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
# initialize value of inverse matrix
m <- NULL
# set value of matrix from user input y
  set <- function(y){
# sets to global x value of matrix input y
  x <<- y
# clears inversematrix value if it exists
  m <<- NULL
}
# get value of matrix if it has been set
get <- function() x
# set value of the inverse of matrix
setmatrix <- function(solve) m <<- solve
# get value of the inversematrix
getmatrix <- function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}




## The following function ('cacheSolve') computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above.
## However, it first checks if the inverse has already been calculated (and the matrix has not changed).
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it computes the inverse of the special "matrix" returned by 'makeCacheMatrix'.

cacheSolve <- function(x = matrix(), ...) {
	#get value of inverse
    m <- x$getmatrix()

#check to see if inverse is already computed
    if(!is.null(m)){
      message("getting cached data")

#if inverse is cached, get the cached value and return
      return(m)
    }

#otherwise calculates inverse of data
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}


