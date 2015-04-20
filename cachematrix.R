## Two functions below calculate, store and return inverse of the square matrix

## makeCacheMAtrix function stores given matrix in enclosed environment cache

makeCacheMatrix <- function(x = matrix()) {
        ##initialize m variable
        m <- NULL
        ##function storing input matrix in enclosing envirenment
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##function getting input matrix from cache
        getmatrix <- function() x
        ##function calculating inverse matrix
        setinverse <- function(solve) m <<- solve
        ##function getting m vaariable value fro enclosing environment
        getinverse <- function() m
        ##Output of makeCacheMatrix - list
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculate, store, get and return inverse matrix 
## of a given matrix from cache

cacheSolve <- function(x, ...) {
        ##get matrix inverse from cache and storing it in m variable
        m <- x$getinverse()
        ##check if matrix inverse is stored in cache (m variable is not null)
        if(!is.null(m)) {
                ##inform user that matrix is retured from cache
                message("getting cached inverse matrix")
                ##return stored inverse matrix 
                return(m)
        }
        ##Getting matrix from cache
        data <- x$getmatrix()
        ##calculating inverse matrix and storing the inverse matrix in m variable
        m <- solve(data, ...)
        ##putting m vaiable into cache
        x$setinverse(m)
        ##returning m variable - matrix inverse
        m     
}
