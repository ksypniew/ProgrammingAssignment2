## makeCacheMatrix and cacheSolve are designed to create an object to store a matrix and cache its inverse.
 

## makeCacheMatrix creates a list that contains functions to set the matrix, get the matrix, set the inverse matrix and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<- NULL
  }
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  ## list of functions
  list(set=set,get=get,setmean=setmean,getmean=getmean)
}


## cacheSolve first check if inverse matrix has already been calculated and then if yes it gets the inverse from the cache, if no it calculates it and set the inverse in the cache by setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}
