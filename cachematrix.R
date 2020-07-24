## makeCacheMatrix takes in a matrix and returns a matrix object which contains
## functions called set, get, setinv and getinv where each of them sets and gets
## the matrix and its inverse respectively. Advantage of lexical scoping in R 
## is taken here and the <<- operator is used to reference variables outside the
## environment
## The getters and setters are similar to OOP in python


makeCacheMatrix <- function(x = matrix()) {
        ## Makes a matrix object with getters and setters for original matrix and its
        ## inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv # inv is an arbitrary variable
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## Returns a cached matrix if exists otherwise returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## Example code derived from Coursera's discussion forum about the same
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
## define your matrix m1
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## myMatrix_object <- makeCacheMatrix(m1)
## cacheSolve(myMatrix_object)
## calling it again will retrieve the data from the previous operation
## cacheSolve(myMatrix_object)
## The message getting cached data under this command will confirm it is getting
## saved data
## setting a new matrix to the same variable(matrix object)
## n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## myMatrix_object$set(n2)
## running cacheSolve will set m to null and run solve on the matrix
## cacheSolve(myMatrix_object)
