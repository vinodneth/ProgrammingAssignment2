## Matrix Inverse computation is a costly computation, caching could provide 
##   some benefit. The below two functions help implement the caching  
##  
## makeCacheMatrix - is a special matrix function that will cache the inverse
## cacheSolve      - computes the inverse of the special matrix. if cached returns 
##                   else computes and sets in cache

## makeCacheMatrix creates  a list containing functions
##  1. set the value of matrix
##  2. get the value of matrix
##  3. set the inverse of matrix
##  4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of matrix. 
## It returns as per the below steps :
##  - check and return if cached
##  - else prepare inverse using solve (x)
##    - set the inverse computed to cache and return

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
