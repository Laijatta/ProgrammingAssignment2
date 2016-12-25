## Coursera programming assingment 2
## This assignment demonstrates the caching of an inverse of a matrix 
## to demonstrate the concept of lexical scoping

## The makeCacheMatrix  builds a set of functions and returns the functions 
## within a list in the parent environment
setwd("C:/Users/gosa/CourseraLectures/Assignments")

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function () x
  setInv<- function(inverse) inv<<-inverse
  getInv<- function() inv
  
  list(set=set, get=get,
       setInv=setInv, getInv=getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  myMatrix<- x$get()
  inv<- solve(myMatrix, ...)
  x$setInv(inv)
  inv
}

