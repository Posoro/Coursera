
makeCacheMatrix <- function(x = matrix()) {
  inversem<-NULL
  set<-function(y){
  x<<-y
  inversem<<-NULL
}
get<-function() x
setinverse <-function(inverse) inversem<<- inverse
getinverse<-function() inversem
list(set=set, get=get,
   setinverse=setinverse,
   getinverse=getinverse)
}

cacheSolve <- function(x=matrix(), ...) {
    inversem<-x$getmatrix()
    if(!is.null(inversem)){
      message("getting cached data")
      return(inversem)
    }
    datos<-x$get()
    inversem<-solve(datos, ...)
    x$setmatrix(inversem)
    inversem
}


