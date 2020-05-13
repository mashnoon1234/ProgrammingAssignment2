## "makeCacheMatrix" returns a custom square matrix object, which has 
## the ability to cache it's inverse. "cacheSolve" uses this ability 
## to return a calculated inverse of the matrix and also cache the
## inverse to save computation time in future use   

## "makeCacheMatrix" has the ability to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inversed matrix 
## 4. get the inversed matrix 

makeCacheMatrix <- function(x = matrix()) {
    inverse_of_matrix = NULL
    set_matrix = function(new_matrix){
        x <<- new_matrix
        inverse_of_matrix <<- NULL
    }
    get_matrix = function() x
    set_inverse = function(calculated_inverse){
        inverse_of_matrix <<- calculated_inverse
    } 
    get_inverse = function() inverse_of_matrix
    list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## "cacheSolve" does the following:
## 1. attempts to get the matrix inverse
## 2. if successful, returns the matrix
## 3. if not successful, gets the matrix
## 4. calculates the inverse of the matrix
## 5. caches the inverse of the matrix
## 6. returns the calculated inverse

cacheSolve <- function(x, ...) {
    inverse_of_matrix <- x$get_inverse()
    if(!is.null(inverse_of_matrix)) {
        message("getting cached data")
        return(inverse_of_matrix)
    }
    data <- x$get_matrix()
    inverse_of_matrix <- solve(data, ...)
    x$set_inverse(inverse_of_matrix)
    inverse_of_matrix
}
