## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## First create a inversible matrix. Examples below:
## Generate inversible matrix
## M <- matrix(c(2,9,3,6,4,7,7,3,5,3,2,10,7,8,9,6,6,6,10,6,2,9,2,2,4), nrow=5, ncol=5)
## M <- matrix(c(1,3,11,5,15,13,7,9,17), nrow=3)
## M <- matrix(c(1:4), 2, 2)
## M <- matrix(c(1,2,2,2,1,2,2,2,1), nrow=3)

## I used the exact same functions as in cachevector.R
## cachevector.R is well described in the forum, see especially post
## Possible use of makeVector() and cachemean() functions - code explained
## https://class.coursera.org/rprog-006/forum/thread?thread_id=626

## I changed mean funtion to solve function. The variables setmean and getmean were 
## changed to setinverse and getinverse. That's all.
## Usage: my_m <- makeCacheMatix(M)
## makeCacheMatrix gives a reference to the four methods in the function
## These methods are used by cacheSolve function
## To be honest I still don't fully understand the "abstract" functions
## inside makeCacheMatrix nor where things are stored.
## If you call my_m you get get references, probaly memory addresses
## that the methods can use.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Usage: cacheSolve(my_m)
## You can compare this with solve(M)
## This function call the method setinverse in makeCacheMatrix
## if the inverse is solved it uses that and function ends.
## If it's not calculated it solves and stores the inverse
## cacheSolve needs makeCacheMatrix and it's methods to work.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
