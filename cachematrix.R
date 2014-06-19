################################################
#Inverse of a matrix data computing and caching
################################################

makeCacheMatrix <- function(input_matrix = matrix()) {
    #   used to store & return via set and get sub functions list
    #   both matrix and inverse of matrix  
    
    #   invert_mat used to cache data 
    invert_mat <<- NULL
    set_matrix <- function(loaded_matrix) {
        #   loading new matrix data         
        
        input_matrix <<- loaded_matrix
        invert_mat <<- NULL
    }
    
    get_matrix <- function() input_matrix
    set_matrix_inverse <- function(inverse_val) invert_mat <<- inverse_val
    get_matrix_inverse <- function() invert_mat
    
    list(set_matrix = set_matrix, get_matrix = get_matrix, set_matrix_inverse = set_matrix_inverse, get_matrix_inverse = get_matrix_inverse)
}

cacheSolve <- function(func_obj, ...) {
    #    compute or fetching & storing inverse of matrix data cache
    
    invert_mat <- func_obj$get_matrix_inverse()
    if(!is.null(invert_mat)) {
        message("getting cached data...")
        return(invert_mat)
    }
    mat_data <- func_obj$get_matrix()
    invert_mat <- solve(mat_data)
    func_obj$set_matrix_inverse(invert_mat)
    invert_mat
}