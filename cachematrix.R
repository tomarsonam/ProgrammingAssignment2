##Assignment: Caching the Inverse of a Matrix


## These two functions 
## 1. create the cached matrix  
## 2. cacluate the inverse or retrieve the cached inverse matrix if it has already been calculated. 

## makeCacheMatrix sets and gets the value of the caching matrix
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
    j <- NULL
    set <- function(y){
      x <<- y
      j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## Write a short comment describing this function

##this function extraxt the inverse of the matrix created by above code
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

##How to run above code

## m<- matrix(rnorm(4),2,2)
## c <- makeCacheMatrix(m)
## cacheSolve(c)

