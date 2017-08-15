## The purpose of these functions is to create a method for potentially cutting down
## the CPU time for the potentially costly calculation of inverting a matrix, using
## R's lexical scoping to store values in a cache.

## The makeCacheMatrix function does the following:
## 1. Stores a matrix object input by the user
## 2. Initializes several functions that allow the user to access and reset the
##    matrix they input and previously calculated inverses of the matrices
## 3. Initializes a function to store newly calculated inverses of matrices
## 4. Stores all these functions as a list, for easy calling later on

makeCacheMatrix <- function(x = matrix()) { #initialize empty matrix for eventual user input
    inv <- NULL #initialize matrix inverse variable as empty
    set <- function(y) { #initialize set() function for eventual user input
        x <<- y
        inv <<- NULL #reset matrix inverse, just in case
    }
    get <- function() x #initialize get() function to retrieve user input
    setinverse <- function(inverse) inv <<- inverse #initialize setinverse() function for eventual user input
    getinverse <- function() inv #initialize getinverse() function to retrieve cached inverse result
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #store functions in list with names for $ call
}

## -----------------------------------------------------------------------------------------

## The CacheSolve function does the following:
## 1. Checks if an inverse has been previously calculated for a given matrix object.
## 2. If the inverse has been previously calculated, the function returns that inverse
##    value from the global cache, and ends.
## 3. If the inverse has not been previously calculated, the function calculates the
##    new inverse, stores it for later access, and prints the new inverse to the screen.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse() #retrieves stored matrix inverse (if there is one for the given matrix input)
    if(!is.null(inv)) { #if there is a stored inverse then...
        message("getting cached data.") #print that the inverse is being retrieved from the cache
        return(inv) #print the stored inverse and leave function
    }
    data <- x$get() #if the inverse isn't yet stored, then retrieve matrix
    inv <- solve(data) #compute the inverse of the matrix
    x$setinverse(inv) #store the inverse of the matrix in the cache for later
    inv #print the newly calculated inverse
}
