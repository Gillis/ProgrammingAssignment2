## Put comments here that give an overall description of what your
## functions do

## Make a cachematrix object with set and get functions for matrix and inverse of it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL;
  set <- function(y) {
    x <<- y; # change matrix
    inv <<- NULL; # reset inverse as well
  }
  get <- function() x;
  setinv <- function(inverse) inv <<- inverse;
  getinv <- function() inv;
  list(
    set = set,
    get = get,
    setinverse = setinv,
    getinv = getinv
  );
}



cacheSolve <- function(x, ...) {
  ## return inverse of matrix, from cache or calc to cache
  inv <- x$getinv();
  if (is.null(inv) == FALSE) {
    # there is a cached one, serve up that
    message("retrieving from cache");
    return(inv);
  }else{
    #one needs to be computed and cached for future calls
    data <- x$get();
    inv <- solve(data);
    x$setinverse(inv);
    return(inv);
  }
}
