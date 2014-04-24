makeCacheMatrix<- function(m=matrix()){ # makeCacheMatrix creates a list of  4 functions
  i <- NULL 
  set <- function(x=matrix()) { # function which sets the value of a matrix 
    m <<- x
    i <<- NULL
  }

  get <- function() m    #function to get the value of the matrix
  setInverse<- function(l=matrix()) i<<- l   #function which assigns the inverse of the matrix to i
  getInverse <- function() i  #function to retrieve the inverse value of the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)   #creates a list of all the 4 functions which are created
}


cacheSolve <- function(m, ...) { #checks for cached value first and if it does not exist - computes it
  
  i <- m$getInverse()
  if(!is.null(i)) {   #checks if inverse has already been computed   
    message("getting cached data") 
    return(i)                # returns catched value
  }
  data <- m$get()
  i <- solve(data)  #computes the inverse matrix if it does not exist
  m$setInverse(i) #sets the value so that it is cached and will not need computation in the future 
  i 
  
}

