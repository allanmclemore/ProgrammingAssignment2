# Summary - Returns an instance of a matrix that has the ability to cache it's Inverse
#
# Input Parameters:
# x - the matrix instance contained in the function used to compute the inverse matrix
#
# Public Methods:
# Value    - returns the value of the matrix
# Inverse  - returns the Inverse of the matrix from the "cache"
# Solve    - computes the Inverse of the matrix and stores it in the "cache"
makeCacheMatrix <- function(x = matrix()) {
  matrixValue <- x
  inverse <- NULL
  list(
    Value = function(){
      matrixValue 
    },
    Inverse = function(){
      inverse
    },
    Solve = function(){
      cat("Calculating inverse of matrix...")
      inverse <<- solve(matrixValue) %*% matrixValue
    }
  )
}

# Summary - Computes the inverse of the supplied matrix contained in the makeCacheMatrix function instance
# 
# Input Parameters 
# x - instance of makeCacheMatrix
#
# Returns
# inverse of the supplied matrix contained in the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  if(is.null(x$Inverse())){
    x$Solve()
  }
  x$Inverse()
}

# Main Program -   Lines below are the main program that verify the above functions are coded to spec
# 
# First call: should warm cache, perform inverse calc, cache calc, and return Inverse of matrix
# Second call: should return Inverse of matrix from cache
# So debug message 'Calculating inverse of matrix...' should only print one time when this code runs
cMatrix = makeCacheMatrix(matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE))
cat("Inverse is: ")
cat(cacheSolve(cMatrix))
cat("Inverse is: ")
cat(cacheSolve(cMatrix))

