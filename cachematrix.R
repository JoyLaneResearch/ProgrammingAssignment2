# Adaptaton of example from programming assignment 2 (week 3)
# Contains two functions and code to test their use.

# Two functions makeCacheMatrix (begins line 13) and cacheSolve (begins line 35):

# Optional action for clearing variables from environment
# rm(list=ls())


# makeCacheMatrix accepts matrix input and attaches four listed functions, that
#      allow cached values to be accessed and stored with the matrix.

makeCacheMatrix <- function(x = matrix()) {      # Changed from numeric to matrix.
    m <- NULL               # Initialize an empty cache.
    set <- function(y) {    # If we call 'set()' and pass a matrix to this 
                            # function, its value is assigned to x, and the 
                            # cache, m, is cleared. Both x and m are visible in
                            # the parent environment, which is the 'makeCacheMatrix()'
                            # function.
        x <<- y
        m <<- NULL
    }
    get <- function() x     # Returns whatever value might be stored in x.
    setSol <- function(invrs) m <<- invrs    # Saves the value of 'invrs' in the cache, m.
    getSol <- function() m                 # Returns the value of the cache, 'm'.
    list(set = set, get = get,
         setSol = setSol,
         getSol = getSol)
}


# cacheSolve accepts a matrix created by makeCacheMatrix and, depending on which
#       subfunction is chosen, either computes or retrieves a cached inverse to the matrix.

cacheSolve <- function(x, ...) {    # Function assumes the variable passed is 
                                    # of the type produced by makeCacheMatrix.
    m <- x$getSol()                 # This m is a local variable.
    if(!is.null(m)) {               # If x$getmean provided a value, return it.
        message("getting cached data")
        return(m)
    }                               # Otherwise, ...
    data <- x$get()                  # Get the vector and put it in 'data' (local).
    m <- solve(data, ...)            # Calculate the mean of 'data' as m (local).
    x$setSol(m)                      # Set the mean, m, to the cache.
    m                               # Return the mean (to the display).
}


# The following code, if uncommented, applies the functions above:
# a <- makeCacheMatrix()          # initialize
# a                               # shows that a is now a list of functions
# class(a)                        # shows that a is a list
# class(a$set)                    # shows that the elements of the list are functions
# a$set(matrix(1:4,2,2))          # set the matrix
# a$get()                         # get the matrix 
# cacheSolve(a)                   # calculate the inverse 
# cacheSolve(a)                   # when is called back use the cached inverse 


# Solution verification:
# Should return an identity matrix, provided the input matrix was invertible.
# a$get() %*% cacheSolve(a)
