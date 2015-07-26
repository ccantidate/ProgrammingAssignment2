## These two functions, makeCacheMatrix and cacheSolve, whose purpose is to cache 
## the inverse of a matrix, will take advantage of the scoping rules of the R 
## language and show how they can be manipulated to preserve state inside of an 
## R object. 
##
## The first function, makeCacheMatrix, creates a special "matrix" object that   
## can cache its inverse. The function is really a list containing functions to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the solve function   
## 4. get the value of the solve function
##
## The supplied matrix is assumed to be a square invertible matrix.
##
makeCacheMatrix <- function(x = matrix()) {
        ##
        ## Assigns NULL to the inverseResult object defined within this function. 
        inverseResult <- NULL
        ##
        ## Defines set as a function that assigns values to objects in the parent
        ## environment
        set <- function(y) {
                ##
                ## Assigns the supplied matrix, passed as the argument y, 
                ## to x in the parent environment
                x <<- y 
                ##
                ## Assigns NULL to inverseResult in the parent environment. 
                ## Note, when the set function is called via x$set, this resets  
                ## inverseResult to NULL in the x enclosure (i.e., in the parent 
                ## environment). If inverseResult is Null, this will cause  
                ## cacheSolve (when it is called) to calculate the inverse of the 
                ## supplied matrix
                inverseResult <<- NULL  
        }
        ##
        ## Defines get as a function that returns the value of x
        get <- function() x 
        ##
        ## Defines setsolve as a function that sets the value of inverseResult.
        ## The passed argument solve is assigned to the inverseResult object in 
        ## the parent environment. Note, When this function is called within 
        ## the cacheSolve function, the calculated inverse of the supplied matrix 
        ## is passed as the argument solve which is then assigned to the 
        ## inverseResult object in the parent environment.
        setsolve <- function(solve) inverseResult <<- solve
        ##
        ## Defines getsolve as a function that returns the value of inverseResult
        getsolve <- function() inverseResult
        ##
        ## Assigns a name to each of the list objects (i.e., to each function)
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
## The cacheSolve function calculates the inverse of the special "matrix" object 
## created with the above makeCacheMatrix function. The function first checks to 
## see if the inverse of the matrix has already been calculated (and the matrix 
## has not changed). If so, it gets the inverse of the matrix already calculated 
## from the cache (parent environment) and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse of the 
## matrix in the cache via the setsolve function.
##
cacheSolve <- function(x, ...) {
        ##
        ## Assigns the result of the function getsolve in the x enclosure  
        ## (i.e., the value of inverseResult from the parent environment)
        ## to the inverseResult object defined within this function. 
        inverseResult <- x$getsolve()
        ##
        ## if inverseResult is not NULL (i.e., the inverse of the supplied 
        ## matrix was calculated before, has been stored in cache and the  
        ## value of the supplied matrix has not changed from the previous 
        ## value of the supplied matrix), then print a message and return 
        ## the cached value of inverseResult
        if(!is.null(inverseResult)) {
                message("getting cached data")
                return(inverseResult)
        }
        ##
        ## Assigns the result of the function get in the x enclosure to the 
        ## data object
        data <- x$get()
        ##
        ## Assigns the result of the function solve to inverseResult
        inverseResult <- solve(data, ...)       
                                        ## Solve(a, b, ...)
                                        ## this generic function solves the 
                                        ## equation a %*% x = b for x, where:
                                        ## 1) a is a square numeric
                                        ## or complex matrix containing the
                                        ## coefficients of the linear system. 
                                        ## Logical matrices are coerced to 
                                        ## numeric. 
                                        ## 2) b can either be a vector or a 
                                        ## matrix. If missing, b is taken to 
                                        ## be an identity matrix and solve will
                                        ## return the inverse of a.
        ##
        ## Calls the setsolve function in the x enclosure which stores the
        ## calculated inverse of the supplied matrix (the value of inverseResult)
        ## in cache (i.e., in the parent environment).
        x$setsolve(inverseResult)
        ##
        ## Returns the value of inverseResult, i.e., returns a matrix that
        ## is the inverse of 'x'
        inverseResult
}
