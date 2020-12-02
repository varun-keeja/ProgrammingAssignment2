## This function creates a special "matrix" object that can cache its inverse.
## Function takes a matrix x as its argument.
## set(x) -> sets the value of the matrix 
## get() -> returns the value of the matrix x
## setinverse(inverse) -> sets the value of inverse of the matrix
## getinverse() -> returns the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y){
    x<<-y
    inv<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
## This function takes the special 'matrix' object as argument and  
## returns the inverse of this special 'matrix' object. 
## It first checks if the inverse has already been calculated.
## If yes, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse of the matrix in the cache 
## using the setinverse function.

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
