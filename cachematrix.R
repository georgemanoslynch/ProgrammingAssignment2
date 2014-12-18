## The following code answers the questions for Programming Assignment 2 of the R Programming course on Coursera


## The instructions were to write the following functions:
## Function 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Function 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## Summary
## The following code contain the two functions as well as calls to the functions to ensure they work properly
## An explanation of each variable and action is described in-line in the functions.


## The following function initializes and stores four objects that are used run and store calculations in the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {     # The input data to the function (here "x") is defined to be a matrix in order to ensure only matrix 
                                                # data is entered as a matrix is required to run the solve function and return the inverse
    inv <- NULL   # The object "inv" which will contain the inverse of the matrix once it is calcuated is initialized to NULL
    set <- function(y) {   # A function called "set" is created to allow the data in the matrix to be changed and the objects updated appropriately
                           # without needing to rerun "makeCacheMatrix"g
        x <<- y   # the "<<-" assignment is used so that "x" and "inv" will be available in the parent environment of "set"
        inv <<- NULL
    }
    get <- function() x   # The function "get" is set so that it returns the matrix of data supplied to the function 
    setinv <- function(inv_mtrx) inv <<- inv_mtrx   # The function "setinv" is set so that the calculated inverse of the matrix is stored in "inv" 
                                                    # when it is input as the formal argument to the funciton
    getinv <- function() inv   # The function "getinv" returns "inv" which is the inverse of the matrix once calculated and NULL before
    list(set = set, get = get,   # Store the objects in a list 
         setinv = setinv,        
         getinv = getinv)
}

## The following function calculates the inverse of the matrix input into the formal argument "x" and caches it for future use so that running the 
## inverse calculation is not needed if it has already been done.
cacheSolve <- function(x, ...) {   # the formal argument "x" is the name of the object that that "makeCacheMatrix" was stored in
    inv <- x$getinv()   # Access the function "getinv" which has stored the value of "inv" in the makeCacheMatrix function and put it into
                        # "inv" in this fucntion
    if(!is.null(inv)) {   # If "inv" is not NULL - meaning the function has been run and the cashe value stored - then the stored value is returned 
        message("getting cached data")
        return(inv)
    }
    data <- x$get()   # If "inv" is NULL - meaning the function has yet to be run and the inverse of the matrix does not exsist yet - then the
                      # matrix of data that was stored in "get()" by the "makeCacheMatrix" function is accessed and put into the object "data"
    inv <- solve(data, ...)   # The inverse of the matrix "data" is calculated and stored into "inv"
    x$setinv(inv)   # The calculated inverse of the matrix is stored into "setinv" that way when "getinv" is called it can access the stored value
                    # in future calls and does not need to solve for the inverse of the matrix again
    inv   # the inerse of the matrix "x" is returned and printed to the console
}

made_matrix1 <- makeCacheMatrix(x=matrix(rnorm(9, 5, 2), 3, 3))
cacheSolve(made_matrix1)
made_matrix1$set(matrix(rnorm(9, 10, 2), 3, 3))
cacheSolve(made_matrix1)

