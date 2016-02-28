## Caching the Inverse of a Matrix
      ## set the value of the vector--set
      ## get the value of the vector--get
      ## set the value of the inverse--setinverse
      ## get the value of the inverse--getinverse

## This function creates a special "matrix" object, which is really a list 
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
      set<-function(y){ ## initiate the vector x stored in the main function
              x<<-y## <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. 
              m<<-NULL
        }
      get<-function() x ##returns the vector x stored in the main function
      
      setinverse<-function(solve) m<<-solve  ##setinverse/getinverse don’t calculate inverse, just store the value of the input in m
      
      getinverse <- function() m 
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)      
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        
      m <- x$getinverse()  ##If the inverse has already been calculated 
      if(!is.null(m)) {    ##(and the matrix has not changed), 
            message("getting cached data") ##then the cachesolve should retrieve the inverse from the cache. 
      }
      
      data <- x$get()  ##If the inverse has not been calculated, the data should be in get() so get from get() to data 
            return(m)  ##data gets the matrix stored with makeCacheMatrix, 
      m <- solve(data, ...) ##m calculates the inverse, 
      x$setinverse(m) ##and x$setinverse(m) stores it in the object m in makeCacheMatrix.
      m
}
}
