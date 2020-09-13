## The makeCacheMatrix function takes a matrix variable and creates 
## matrix object that allows for storing of the cached inverse of
## the matrix passed to the function. 
## set function assigns the passed matrix value to variable x and 
## resets the inv variable used to store the calculated inverse of matrix
## to null so that when cacheSolve function is called, the if statement
## can verify if inverse was previously calculated.
## The get function returns the value of the matrix that was passed by
## the parent function (called 'x'). The setinv function takes the matrix
## x from the parent function and stores its inverse value to variable inv.
## Getinv function returns the stored value of inv.
## list creates a list of all the functions within the object that are
## allowed to be called using method object$functionname from cacheSolve 
##  function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function performs the function of returning the inverse
## of the matrix that was passed through to the object created when using
## makeCacheMatrix function, or by using the object$set method of passing
## matrix variable to makeCacheMatrix. 
## This function first gets the stored value of inverse in the object.
## then it gets the value of the currently set x value from the object
## and assigns it to variable newX for comparison later.
## Then the inverse value returned is not null (means an inverse has previously
## been calculated), it will confirm if the previously inversed matrix is same as
## the newly set matrix in the object. If it is the same, the message is printed
## along with stored value of inv from cache. if it is not the same, the function
## inverst the newX matrix and sets the cache value of inv to the newly calculated
## value and then prints it on screen. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    newX<-x$get()
    if(!is.null(inv)){
        prevX <- solve(inv)
            if(i<-all.equal(prevX,newX)){
                message("getting cached data")
                return(inv)  #Returns cached inverse of 'x'
            }
        }
    inv <- solve(newX)
    x$setinv(inv)
    inv ## Return a matrix that is the inverse of 'x'
}


