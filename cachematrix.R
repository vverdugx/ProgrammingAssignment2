## This two functions allow the calculation of the inverse of a matrix using memoization.

makeCacheMatrix <- function(x = matrix()) {
    ## Creates an objecte containing the following elements.
    ## x = matrix value
    ## InverseMatrix = a cached value, typically the inverse of x.
    ## Functions
    ##  get     returns x
    ##  getInv  returns the cached inverse
    ##  set     sets a new value for x and clears the cache
    ##  setInv  sets the cached inverse 
    InverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) InverseMatrix <<- Inv
    getInv <- function() InverseMatrix
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

cacheSolve <- function(x, ...) {
    ## returns the invers of a matrix (as special object)
    ## if the inverse value is in the cache, returns this value.
    ## if not the inverse is calculated and stored in the cache.
    InverseMatrix <- x$getInv()
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    data <- x$get()
    InverseMatrix <- solve(data, ...)
    x$setInv(InverseMatrix)
    InverseMatrix
}