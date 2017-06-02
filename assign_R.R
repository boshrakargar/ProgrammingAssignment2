
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        mtk <- function(y) {
                x <<- y
                inv <<- NULL
        }
        rtrn <- function() x
        makeInverse <- function(inverse) inv <<- inverse
        rtrnInverse <- function() inv
        list(mtk = mtk ,
             rtrn = rtrn ,
             makeInverse = makeInverse ,
             rtrnInverse = rtrnInverse )
}


cacheSolve <- function(x,) {
        inv <- x$rtrnInverse()
        if (!is.null(inv)) {
                message("No Inverse")
                return(inv)
        }
        mat <- x$rtrn()
        inv <- solve(mat, ...)
        x$makeInverse (inv)
        inv
}