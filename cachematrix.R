## Begin R Code. Program Assignment 2
## the following function create a standard matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function checks if the inverse is already calculated for the matrix, 
## if so, inverse is returned from cache
## If inverse is not calculated then inverse is calculated, cached and returned 

cacheSolve <- function(x, ...) {
  
  ## Check if inverse available in cache
  matinv <- x$getinverse()
  if(!is.null(matinv)) {
    message("getting cached data.")
    return(matinv)
  }
  ## Compute inverse, cache and return inverse
  data <- x$get()
  matinv <- solve(data)
  x$setinverse(matinv)
  matinv
}
## End of R code ##

## Test1:
## x <-cbind(1, 1:3, c(2,0,1))
## mt <- makeCacheMatrix(x)
## mt$get()
## [,1] [,2] [,3]
## [1,]    1    1    2
## [2,]    1    2    0
## [3,]    1    3    1
## 
## first time, inverse is calculated
## cacheSolve(mt)
## [,1]       [,2]       [,3]
## [1,]  0.6666667  1.6666667 -1.3333333
## [2,] -0.3333333 -0.3333333  0.6666667
## [3,]  0.3333333 -0.6666667  0.3333333## 
## 
## Test2: check that data is retrieve from cache
## cacheSolve(mt)
## getting cached data.
## 
## [,1]       [,2]       [,3]
## [1,]  0.6666667  1.6666667 -1.3333333
## [2,] -0.3333333 -0.3333333  0.6666667
## [3,]  0.3333333 -0.6666667  0.3333333
##
## End of Test
