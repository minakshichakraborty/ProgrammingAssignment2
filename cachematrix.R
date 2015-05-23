
## 'makeCacheMatrix' and 'cacheSolve' are a pair of functions that allow 
2 ## caching the inverse of a matrix to avoid calculating it more than once. 
3 ## 
4 ## Sample usage: 
5 # a= makeCacheMatrix(matrix(c(10,20,30,40), nrow=2, ncol=2)) 
6 # > a$get() 
7 #       [,1] [,2] 
8 # [1,]   10   30 
9 # [2,]   20   40 
10 # cacheSolve(a) 
11 #      [,1]  [,2] 
12 # [1,] -0.2  0.15 
13 # [2,]  0.1 -0.05 
14 # a$getinverse()  # Returns matrix inverse 
15 #      [,1]  [,2] 
16 # [1,] -0.2  0.15 
17 # [2,]  0.1 -0.05 
18 # a$set(matrix(c(0,50,90,60), nrow=2, ncol=2)) # Modify existing matrix 
19 # > cacheSolve(a)   # Computes, caches, and returns new matrix inverse 
20 #             [,1] [,2] 
21 # [1,] -0.01333333 0.02 
22 # [2,]  0.01111111 0.00 
23 # a$get()         # Returns matrix 
24 #       [,1] [,2] 
25 # [1,]    0   90 
26 # [2,]   50   60 
27 # a$getinverse()  # Returns matrix inverse 
28 #             [,1] [,2] 
29 # [1,] -0.01333333 0.02 
30 # [2,]  0.01111111 0.00 
31 
 
32 ## makeCacheMatrix: used to create a matrix with cachable inverse. 
33 
 
34 makeCacheMatrix <- function(x = matrix()) { 
35   ## initial the inverse matrix of x with NULL 
36   inverse <- NULL 
37   ## set function: assign y to x and NULL to inverse 
38   set <- function(y) { 
39     x <<- y 
40     inverse <<- NULL 
41   } 
42   ## get function: return x 
43   get <- function() x 
44   ## setinverse/getinverse: used to set and get  
45   ## cached inverse matrix, accordingly. 
46    
47   setinverse <- function(inv) inverse <<- inv 
48   getinverse <- function() inverse 
49   list( 
50     set = set, 
51     get = get, 
52     setinverse = setinverse, 
53     getinverse = getinverse 
54   ) 
55 } 
56 cacheSolve <- function(x, ...) { 
57   ## getinverse from matrix x 
58   inverse <- x$getinverse() 
59    
60   ## if inverse was ever computed, return the existing one. 
61   if(!is.null(inverse)) { 
62     message("getting cached data") 
63     return(inverse) 
64   } 
65   ## in case no cached inversed matrix, do the following: 
66   ## get matrix x 
67   m <- x$get() 
68   inverse <- solve(m, ...) 
69   x$setinverse(inverse) 
70   inverse 
71 } 
 
