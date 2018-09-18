## Since Matrix inversion computation can be heavy
## We create bellow two functions, 'makeCacheMatrix'
## and 'cacheSolve' in order to, after computing a
## matrix inverse, cache it.

## The makeCacheMatrix has as input a matrix and outputs 
# a list of getters and setters to cache the given matrix and
# its inverse, which will be computed in the cacheSolve function.

#Since we were told "For this assignment, 
#assume that the matrix supplied is always invertible.",
#no check here is made if the user has inputed a non singular
#square matrix; we'll take it for granted only this
#kind of matrix has been provided


makeCacheMatrix <- function(x = matrix()) {
  
  #initialize the inverse_matrix as a null object
  inverse_matrix <- NULL
  
  #the set_matrix uses the '<<-' operator to
  #modify the existing variable
  #x (the matrix originally inputed) found by walking
  #up the parent environment (WICKHAM, Advanced R)
  #and it also makes the inverse_matrix object NULL again
  #(otherwise, it could associate a wrong inverse to the new
  #matrix computed previously to the old one)
  set_matrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  #In the line bellow, we get the matrix x 
  get_matrix <- function() {x}
  
  #Bellow, we atribute a matrix (the inverse) to the 
  #inverse_matrix
  set_inverse <- function(inverse_y) {
    inverse_matrix <<- inverse_y
    }
  
  #Bellow, we return the matrix inverse, if it has been
  #already computed
  get_inverse <- function() {inverse_matrix}
  
  #we define a list as the output of our
  #'makeCacheMatrix' function
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)  

}


## cacheSolve receives as its input the object object generated 
## by the function 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  #Here we assign to the object inverse_matrix
  #the output of the function that gets its inverse
  inverse_matrix <- x$get_inverse()
  
  #if the inverse has already been computed to this object,
  #it will return to the user, signalling that no new
  #computation has been made (the print message 'getting cached
  #matrix') and stops the execution
  #if the inverse has not been computed, we do it in the
  #else chunk
  if(!is.null(inverse_matrix)) {
    
    message("getting cached matrix")
    
    #the output of this function is the inverse matrix
    return(inverse_matrix)
    
  } else {
    
    #retrieve the original matrix provided to makeCacheMatrix in
    #order to compute its inverse
    original_matrix <- x$get_matrix()
    
    #computing the inverse using the solve function
    inverse_matrix <- solve(original_matrix, 
                            diag(nrow(original_matrix)))
    
    #seting to our "Matrix" object the inverse computed above
    #so the next time we want it we do not need to compute it once
    #again
    x$set_inverse(inverse_matrix)
    
    #the output of this function is the inverse matrix
    return(inverse_matrix )
  }
  
}

