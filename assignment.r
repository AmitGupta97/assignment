#To get the matrix using makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(solve) m <<- solve
  getinver <- function() m
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

#To get the inverse of a matrix using the cachesolve function
cachesolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
