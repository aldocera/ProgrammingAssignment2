### Objective 1st function: 
## to create a square matrix so that
## the next function will evaluate if
## it is necesary to compute its inverse.

### Objective 2nd function:
## A check-up is done whether the inverse of 
## the created matrix is already computed.
## If it is, the stored inverse is printed.
## If not, the inverse is calculated and returned.

#----------------------------------------------------------

## This function has the capability to create a matrix. 
#  Likewise, a new matrix can be set within the function,
#  resetting all values.
#  The inverse's values can be set or checked also.

makeCacheMatrix <- function(x = matrix()) {
  
  cache<-NULL
  
  set<- function(nmatrix){ 
    x<<- nmatrix
    cache<<- NULL
  }
  
  getvalue<- function() {x}
  setInverse<- function(inverse) {cache <<- inverse}
  getInverse<- function () {cache}
  
  list(set = set, 
       getvalue = getvalue, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function calculates the inverse of a matrix
#  in case no inverse has been computed.
#  Returns inverse when computation has been done before.

cacheSolve <- function(x, ...) {
  
  cache<-x$getInverse()
  
  if(!is.null(cache)){
    message("getting cache Inverse")
    return(cache)
    }
  
  inverse<-x$getvalue()
  cache<- solve(inverse)
  x$setInverse(cache)
  
  cache
  
}
