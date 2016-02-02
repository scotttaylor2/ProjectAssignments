## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix is unchanged, it retrieves the inverse from the cache directly.


makeCacheMatrix <- function(x = matrix()) {
## x= a square invertible matrix
        ## return = a list containing functions to do the following:
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x= output of makeCacheMatrix()
        ## return= inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
               if (!is.null(inv)){
               message("getting cached data")
               return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}

