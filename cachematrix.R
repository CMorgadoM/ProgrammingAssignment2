## This functions take the value of a matrix and calculate the inverse,
## then, once calculated, save the inverse matrix in the enviroment of the 
## function "makeCacheMatrix", so it dosn't need to be calculated again 


## This function take a matrix, check if it's a singular, squared matrix and then
## creates a list of functions to get the value of the matrix, solve and 
## store the inverse, and call the inverse matrix. The list serves as input for
## the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    
    ##This line of code is for checking if the matrix is squared, if not, set
    ## x value to NULL
    if(nrow(x)!=ncol(x)){
        x<<-NULL
        stop(message("Square matrix is requiered"))
    } 
    
    ## This chunk of code is for checking if the matrix has an inverse. if not,
    ## set x to NULL, write a message explining the error and stop the function
    if(det(x)==0){
        x<<- NULL
        stop(message("Singular matrix, not invertible"))
        
    } 
    
    ## get function call the value of x; setinv function store the value
    ## of the inverse matrix in the environment of makeCacheMatrix function;
    ## getinv function call the Inv value
    
    get <- function() x 
    setinv <- function(solve) Inv <<- solve
    getinv <- function() Inv

    list(set = set, get = get, setinv = setinv, getinv = getinv)

 }

## cacheSolve function use the functions generated in makeCacheMatrix to
## calculate the inverse of the matrix used as argument for the previus 
## function or, if the matrix was already calculated, 
## return it's value from the cache memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## Return the inverse matrix if it's already stored in the cache memory
    Inv <- x$getinv() 
    if(!is.null(Inv)){
        message("Getting cached data")
        return(Inv)
    } 
    ## If it wasn't calculated yet, it solves the matrix and cache the inverse
    Mat <- x$get()
    Inv <- solve(Mat, ...)
    x$setinv(Inv)
    Inv
}
