## Through this assignment we are expected to get hold
## of the lexical scoping rules in R with the use of the
## functions "cacheSolve" and "makeCacheMatrix"that can 
## save us a good amount of time and computation by storing 
## the inverse of a matrix. Understanding lexical scopes is 
## fundamental when programming in R so as to comprehend
## how the values of "free variables" are retreated
## and also, as we can see in this case, cached from 
## their corresponding environments.


## In this function I set the value of the matrix, 
## I get its value, I set the value of the inverse matrix,
## and then I get its value.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y){
                    x <<- y
                    m <<- NULL
            }
            get <- function()x
            setInverse <- function(inverse) m <<- inverse
            getInverse <- function() m
            list(set = set, get = get, 
                 setInverse = setInverse, 
                 getInverse = getInverse)
}

## Here I compute "cacheSolve" through an "if" statement
## to check whether the inverse matrix has already been calculated, 
## if not it sets its value

cacheSolve <- function(x, ...) {
         m <- x$getInverse()
         if(!is.null(m)){
                message("getting cached data")
                return(m)
         }
         mat <- x$get()
         m <- solve(mat,...)
         x$setInverse(m)
         m
}


