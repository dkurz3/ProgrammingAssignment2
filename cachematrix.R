##Dave Kurz, Programming Assignment 2, Coursera - R Programming, 28 April 2014  
##Put comments here that give an overall description of what your
##functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  			#create cache matrix to use in subsequent function (cacheSolve)
        m <- NULL   									#set matrix equal to 0
        set <- function(y) {							#set function(y) equal to x 
                x <<- y
                m <<- NULL  
        }
        get <- function() x								#configure function to call stored matrix 
        setsolve <- function(solve) m <<- solve 		#configure function to solve input matrix
        getsolve <- function() m						#configure function to call solved matrix
        list(set = set, get = get,						#return list of 4 elements (functions)
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {						#function to solve input matrix using functions described above
        m <- x$getsolve()								#place solution (inverse) of matrix in variable 'm'
        if(!is.null(m)) {								#if matrix 'm' is not empty, return solved/inverse matrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()									#if matrix is empty, get solution/inverse of matrix
        m <- solve(data, ...)							#solve/invert matrix
        x$setsolve(m)									
        m												#return solved matrix
}