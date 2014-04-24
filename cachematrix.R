makeCacheMatrix<- function(m=matrix()){
  i <- NULL
  set <- function(x=matrix()) {
    m <<- x
    i <<- NULL
  }

  get <- function() m
  setInverse<- function(l=matrix()) i<<- l
  getInverse <- function() i  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(m, ...) {
  
  i <- m$getInverse()
  if(!is.null(i)) {           
    message("getting cached data") 
    return(i)                
  }
  data <- m$get()
  i <- solve(data)
  m$setInverse(i)
  i
  
}
