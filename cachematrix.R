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
  setinv <- function(new_inv) inv <<- new_inv;
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
    message("already cached, retrieving.");
    return(inv);
  }else{
    #one needs to be computed and cached for future calls
    mtrx <- x$get();
    inv <- solve(mtrx);
    x$setinv(inv);
    return(inv);
  }
}
