## The two functions together will allow the user to solve for the inverse of a matrix and
## cache (store) its value. If the inverse is required again in the future, it will then
## be retrieved as opposed to being solved again.

## The makeCacheMatrix is a function with the arguement being a square matrix. It will take
## the square matrix and create a special list that can be passed to the cacheSolve
## function.

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inverse) i<<-inverse
  
  getinverse<-function() i
  
  list(set=set,get=get,setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve is a function with the primary arguement being the list generated from the
## makeCacheMatrix. The function will solve for the inverse of the matrix that was input
## into makecCacheMatrix and store it if it had not been already stored. Further runs of
## this function will then call the stored value instead of solving the matrix again.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
