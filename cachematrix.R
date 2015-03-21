# JHU Data Science Specialization - Ian Feller
# R Programming Course Assignment 2
# Created:  3/21/15
# Updated:  3/21/15


# Assignment 2
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. Your 
# assignment is to write a pair of functions that cache the inverse of a matrix.

# The following code provides two functions and an example demonstrating
# the results.


# The first function is makeCacheMatrix, which sets the global variables used
# to calculate the inverse of a user provided matrix and stores the results ("in cache").

makeCacheMatrix <- function(input_matrix = matrix()) {
    
    #Create an empty variable for the matrix inverse
    inverse_matrix <- NULL
    
    #Set global variables "input_matrix" "inverse_matrix" to be stored "in cache."
    set_globals <- function(input_matrix_temp) {
        input_matrix <<- input_matrix_temp
        inverse_matrix <<- NULL
    }
    
    #Obtain the user provided input matrix
    searchfor_matrix <- function() input_matrix
    
    #Calculate the inverse matrix and store globally "in cache."
    solve_inverse <- function(solve) inverse_matrix <<- solve
    
    #Obtain calculated inverse matrix
    searchfor_inverse <- function() inverse_matrix
    
    #The output of this makeCacheMatrix is a list of the functions
    list(set_globals = set_globals, searchfor_matrix = searchfor_matrix, 
         solve_inverse = solve_inverse,
         searchfor_inverse = searchfor_inverse)
}

# The second function is cacheSolve, which employs the functions from
# makeCacheMatrix to test if a user provided  matrix has an inverse that has
# already been calculated. If not, it calculates the inverse and stores the
# results ("in cache").

cacheSolve <- function(input_matrix, ...) {
    
    #Check if the inverse of the user provided matrix was store globally
    # "in cache," and if so, retrieve the data and return the inverse matrix.
    inverse_matrix <- input_matrix$searchfor_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting inverse matrix stored in cache")
        return(inverse_matrix)
    }
    
    #If the inverse has not be calculated, use the functions from makeCacheMatrix 
    #to calcuate and display the inverse of the user provided matrix.
    
    temp_matrix <- input_matrix$searchfor_matrix()
    inverse_matrix <- solve(temp_matrix, ...)
    input_matrix$solve_inverse(inverse_matrix)
    inverse_matrix
}

#Test the functions with a simple matrix to check results.
test_matrix <- matrix(c(2,0,0,2), nrow=2,ncol=2,byrow = TRUE)
test_matrix

using_test_matrix <- makeCacheMatrix(test_matrix)
cacheSolve(using_test_matrix)
