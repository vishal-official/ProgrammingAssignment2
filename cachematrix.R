makeCacheMatrix <- function(x=matrix()) {
      ## x is always a square covertible matrix
      
      inv = NULL
      set = function(y){
            x <<- y
            inv <<- NULL
            
## <<- operator in this case assigns the value of y to x from diff. environment
## Good article explaining lexical scoping used above
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
      }
       get = function() x
       ## defines getter for matrix x
       
       setinv = function(inverse) {
             inv <<- inverse
             ## defines setter for the inverse
       }
       getinv = function() inv
       
       list(set=set,get=get,setinv=setinv,getinv=getinv)
      ##Naming the list elements allows us to use the 
      ##$ form of the extract operator to access the functions by name
}

cacheSolve <- function (x,...) {
      inv = x$getinv()
      
      ##if inverse is already cached
      if(!is.null(inv)){
            message ("getting from cache")
            return(inv)
      }
      
      inv.matrix = x$get()
      inv = solve(inv.matrix,...)
      ## calculating the inverse
      
      x$setinv()
      
      return(inv)
}