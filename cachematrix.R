## Matrix inversion is usually a costly computation.
# This script stores in cache the calculated inverse of a matrix.
# Assumption: the matrix supplied IS invertible. If not, the functions will fail.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse.matrix as NULL
  inverse.matrix <- NULL;
  
  # "set": set the value of the matrix
  set <- function(y) {
    x <<- y;
    inverse.matrix <- NULL;
  }
  
  # "get": get the value of the matrix
  get <- function() {
    return(x);
  }
  
  # "setInverse": set the value of the inverse of the matrix
  setInverse <- function(inverseMatrix) {
    inverse.matrix <<- inverseMatrix;
  }
  
  # "getInverse": get the value of the inverse of the matrix
  getInverse <- function() {
    return(inverse.matrix);
  }
  
  # returns the list
  return (list(set = set, get = get, setInverse = setInverse, getInverse = getInverse));
}

## CacheSolve calculates the inverse of the special "matrix" created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  # Try to get the inverse from cache
  inverse.matrix <- x$getInverse();
  
  # If is not null, i.e. the inverse is actually saved
  if(!is.null(inverse.matrix)) {
    # Show message
    message("getting cached data");
    
    # Return stored value
    return(inverse.matrix);
  }
  
  # If IS null, i.e. if the inverse is not saved
  # Get data
  data <- x$get();
  
  # Calculate the inverse
  inverse.matrix<- solve(data, ...);
  
  # Save it
  x$setInverse(inverse.matrix);

  ## Return a matrix that is the inverse of 'x'
  inverse.matrix
}

## Example
m <- matrix(sample(16), ncol = 4, nrow = 4);
v <- makeCacheMatrix(m);
u <- cacheSolve(v);

matrixTimesInv <- m %*% u;
round(matrixTimesInv);

InvTimesMatrix <- u %*% m;
round(InvTimesMatrix);