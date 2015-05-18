## The above funtions take the adventage of scoping in R to make calculations more
## faster.

## This function creates a list contanining a function to set and get the value
## of a matrix and set and get its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x<<-y
    inv <<-NULL
  }
  get<-function()x
  setinverse <-function(inverse) inv <<-inverse
  getinverse <-function() inv
  list (get=get,set=set,
        setinverse=setinverse,
        getinverse=getinverse)
    
}


## This function caculates the inverse matrix but first checks if the inverse 
## is already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.nul(inv)){
    message("getting chaced inverse matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinverse(inv)
  inv
}
