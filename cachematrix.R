
## creates and cache BOTH the matrix and its inverse, with gets and sets 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                
                ## creation, inverse matrix is NULL
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## computes the inverse matrix, but just if not computed yet 

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {               ##if inverse already computed return cache
                message("getting cached inversed matrix")
                return(i)
        }else{                          ##if is the first time, compute inverse matrix 
                message("calculating inverse matrix...")
                mat <- x$get()
                i <- solve(mat, ...)    ##assuming the matrix supplied is invertible, otherwise an error would be returned
                x$setinverse(i)    
        }
        ## Return a matrix that is the inverse of 'x'
        i        
        
}
