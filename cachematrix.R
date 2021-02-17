## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates and returns a list of functions
## which cacheSolve uses to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  #set the matrix which we will use and cache the inv of
  set <- function(y){
    x<<-y
    cache <<-NULL
  }
  get <-function() x
  
  setInv <- function(inverse) cache <<-inverse
  
  getInv <- function() cache
  
  list(set = set, get=get, setInv = setInv, getInv= getInv)
}


## Write a short comment describing this function
## cacheSolve either retrieves the cache inv matrix stored in makeCacheMatrix
## or it calculates and then caches
cacheSolve <- function(x,...) {
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInv(m)
  m
  
}
