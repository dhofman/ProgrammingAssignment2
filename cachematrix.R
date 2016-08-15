## This function creates a special "matrix" object that can cache its inverse.
## get() returns the original matrix
## set() sets the original matrix
## setinv() sets the value of the inverse matrix
## getinv() gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## test for square matrix
        testbadinput <- function(z) {
                
                if ((class(z) != "matrix") || (dim(z)[1]!=dim(z)[2])) {
                        warning("bad input")
                        return(NULL)
                } else {
                        return(1)
                }
        }
        
        ## function to set matrix and initialize inverse to null.
        set <- function(y) {
                if (!is.null(testbadinput(y))) {
                        x <<- y 
                        
                        ##reset inverted matrix to null when we reset the original one.
                        m_inv <<- NULL
                }
        }
        
        ## function to get original matrix
        get <- function() x
        
        ## function to cache the inverse matrix
        setinv <- function(newinv) {
                m_inv <<- newinv
        }
        
        ## function to get the cached inverse matrix
        getinv <- function() m_inv
        
        ## check input
        if (is.null(testbadinput(x))) return(NULL)
        
        ## set the matrix value based upon input
        set(x)     
        
        ## return newly constructed list of functions
        list(
                set = set, 
                get = get,
                setinv = setinv,
                getinv = getinv
        )
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## The input value x is the output of makeCacheMatrix,
## which has functions set, get, setinv, getinv

cacheSolve <- function(x, ...) {
        
        inv_m <- x$getinv()
        
        ## if inverse matrix is not null, return it.
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        
        ## inverse matrix was null..Get the original matrix and calculate it.
        ## if the original matrix is null we have a bad input and should return.
        data <- x$get()
        if (is.null(data)) {
                warning("error from cacheSolve..missing data")
                return(NULL)
        }
        
        ## Calculate inverse matrix.
        inv_m <- solve(data, ...)
        
        ## store the inverse matrix we just calculated.
        x$setinv(inv_m)
        
        ##return the inverse matrix
        inv_m
}

test_create_cachedmatrix <- function() {
        c = rbind(c(1,-1/4), c(-1/4,1))
        m <- makeCacheMatrix(c)
        print(m$get())
        all.equal.numeric(c, m$get())
}

test_bad_input <- function() {
        c = c(1,2,3,4,5) ## should be matrix!!
        output <- makeCacheMatrix(c)
        
        #expected outcome is that the output will be null
        all.equal(output, NULL)
}

test_cacheSolve <- function() {
        c = rbind(c(1,-1/4), c(-1/4,1))
        m <- makeCacheMatrix(c)
        print(cacheSolve(m))
        all.equal.numeric(cacheSolve(m), solve(c)) #should get cached data message 2nd time.
}
