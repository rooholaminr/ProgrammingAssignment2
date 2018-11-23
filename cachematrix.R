##  pass a matrix with non zero determinant and also square shaped and these finctions will return the inverse matrix to you fast and memory efficient using caching methods
## ----------------

## Just like the example I'm going to use four sets of functions for first #makeCacheMatrix function.
## this function returns a list which is an output of those 4 functions including get, set and getInv and setInv
## shortly I can say that the first function acts like a container

## with this order functions inside do these tasks :
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y) {
            y <<- x
            Inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) Inv <<- inverse
      getInv <- function() Inv
      list(set = set ,
           get = get ,
           setInv = setInv ,
           getInv = getInv )
}

## ---------------------
## this function uses the list provided in previous snippet and runs solve function and returns the inverse matrix

cacheSolve <- function(x, ...) {
      
      Inv <- x$getInv()
      if (!is.null(Inv)){
            message('you input list proccessed by makeCacheMatric func')
            return(Inv)
      }
      data <- x$get()
      Inv <- solve(data , ...)
      x$setInv(Inv)
      Inv
}
