# ******************************** Start of Programming part **********************
# Following two functions enable setting up a interim matrix and using it to calculate 
# inverse by retrieving it from cache as needed

# Function 1  - "makeCacheMatrix " - this function creates a new matrix object and also to 
# cache the inverse 
# This is a test comment
makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Function 2 - This function uses new matrix created using above makeCacheMatrix
# and computes the inverse by retrieving it from cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("No new changes. Getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

# ******************************** End of Programming part **********************


# Following section tests the above code
# UnitTest Results

> z <- diag(2, 2)
> z
     [,1] [,2]
[1,]    2    0
[2,]    0    2
> new_matrix <- makeCacheMatrix(z)
> cacheSolve(new_matrix)
     [,1] [,2]
[1,]  0.5  0.0
[2,]  0.0  0.5
> cacheSolve(new_matrix)
No new changes. Getting cached data
     [,1] [,2]
[1,]  0.5  0.0
[2,]  0.0  0.5
> 
