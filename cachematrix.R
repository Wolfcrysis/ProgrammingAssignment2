#Makecachematrix establishes functions within the environment
makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x                 # obtains the value for the matrix
  setMatrix <- function(inverse) mx <<- inverse  # stores inverted matrix in cache
  getMatrix <- function() mx                       #gets the matrix from the cache
  list(set = set,                              #Lists all created functions in the working environment
       get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}
  
  cacheSolve <- function(x, ...) {   #This function either calls a cached version of inverse matrix or calculates
    mx <- x$getMatrix()
    if (!is.null(mx)) {              #Looks to see if matrix is already in the cache.
      message("getting cached data")
      return(mx)                     #Returns the inverse matrix
    }
    mat <- x$get()                   # Else, calculate inverse matrix
    mx <- solve(mat, ...)
    x$setMatrix(mx)                  # uses the setinv function to set the value within the cache
    mx                               # Returns inverted matrix
  }
    
    ##Testing function to confirm it works
    ##Creating matrix
    my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
    
    ##Establishing it
    my_matrix$get()
    
    ##Solving for mxerse
    my_matrix$getMatrix()
    
    cacheSolve(my_matrix)
    
    
