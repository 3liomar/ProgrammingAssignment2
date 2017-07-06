## This first function cache the matrix and defines a list of functions to 
## get and set the inverse of this matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {            #function to grab de matrix in the cache
                x <<- y
                inv <<- NULL
        }
        get <- function() x             #function to take de matrix from the cache
        setInv <- function(Inversa) inv <<- Inversa     #function to grab the new inverse
        getInv <- function() inv        #function to get de inverse from the cache
        list(set = set, get = get, setInv = setInv, getInv = getInv) #This is the list... 
                                                   # ... of functions that returns this function
}


## in this function, it takes the matrix of the cache function and the list of function to get and set the inverse

cacheSolve <- function(x, ...) {
        
        inv <- x$getInv()
        if(!is.null(inv)) {             #if we have calculated the inverse jet, and it is in the cache, ... 
                return(inv)             #... the function returns the value in the cache
        }
        
        data <- x$get()                 # Else, we get the new matrix in the cache...
        inv <- solve(data, ...)         # Calculate the inverse, ...
        x$setInv(inv)                   # ... change the inverse in the cache
        inv                             # show the calculated inverse
}

# to make this function works we have to call the function makeCacheMatrix inside cachesolve

# I.E: cacheSolve(makeCacheMatrix("Write here a Square Matrix"))

