
## The makeCacheMatrix function creates a matrix containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        
        ## set() caches the input matrix to x and clears the inverse matrix
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        
        ## get() retreives the cached matrix
        get <- function() x
        
        ## setinverse() saves the inverse to the cache matrix
        setinv <- function(inverse) inv <<- inverse
        
        ## getinv() retrieves the cached inverse matrix
        getinv <- function() inv
        
        ## Return the matrix 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse matrix has already been calculated (and the matrix as not changed), then the 
## cachesolve function will retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) 
{
        ## return the inverse from the cache if it is already been cached 
        inv <- x$getinv()                
        if (!is.null(inv)) 
        {
                print("Getting cached data")
                return(inv)
        }
        
        ## calculate the inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Cache the inverse
        x$setinv(inv)
        
        ## Return the inverse matrix of 'x'
        inv       
        
}
