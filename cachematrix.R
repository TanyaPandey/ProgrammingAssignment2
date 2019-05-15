## These functions: makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix
## instead of computing it repeatedly.

## Given below is the makeCacheMatrix function. It has creates a special matrix that can 
## cache its inverse. Essentially, it is a list of four functions:
## 1. Setting a matrix
## 2. Getting a matrix
## 3. Setting the value of the inverse of a matrix
## 4. Getting the value of the inverse of a matrix

## makeCacheMatrix returns a list of these four functions.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set_matrix<-function(y){
                x<<-y
                m<<-NULL
        }
        get_matrix<-function() x
        set_inv_matrix<-function (inverse) m <<- inverse
        get_inv_matrix<-function() m
        list(set_matrix=set_matrix,get_matrix=get_matrix,set_inv_matrix=set_inv_matrix,get_inv_matrix=get_inv_matrix)

}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix function. 
## It checks if the inverse of a particular matrix has already been calculated. If so, then 
## it returns the cached value. Else, it returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$get_inv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get_matrix()
        m<-solve(data,...)
        x$set_inv_matrix(m)
        m
}
