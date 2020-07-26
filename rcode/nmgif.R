#The output produced by filled.contour is actually a combination of two plots; one is the filled contour and one is the legend. 
#Two separate coordinate systems are set up for these two plots, but they are only used internally ?C once the function has returned these coordinate systems are lost. 
#If you want to annotate the main contour plot, for example to add points, you can specify graphics commands in the plot.axes argument. See the examples.


library(animation)
############
Nelder.mead <- function(f,start.vertices,tol = 1e-3, plot = F){
  N <- 1  #calculation times
  n <- nrow(start.vertices)
  vertices <- start.vertices
  values <- rep(0, n)
  #evaluate function
  if(plot){
    x <- seq(-5,5,length = 100)
    y <- seq(-5,5,length = 100)
    z <- outer(x,y,function(a,b){apply(cbind(a,b), 1, f)})
    filled.contour(x,y,z, 
                   xlab = expression(x[1]),
                   ylab = expression(x[2]),
                   xlim = c(-5,5), ylim = c(-5,5), color = terrain.colors, 
                   main = parse(text = paste0('"Nelder Mead: "', ' ~ f(x[1],x[2]) == ~ x[1]^2 + x[2]^2 + x[1] * sin(x[2]) + x[2] *sin(x[1])')),
                   plot.axes = { axis(1); axis(2);lines(rbind(vertices,vertices[1,]))})
    
  }
  
  
  for(i in 1:n){
    values[i] <- f(vertices[i,])
  }
  volume <- 1
  while (abs(volume) > tol && N < 100) {
    vertices <- vertices[order(values),]
    values <- sort(values)
    
    
    #centroid
    centroid <- apply(vertices[-n,], 2, mean)
    
    #reflected piont
    reflected <- 2*centroid - vertices[n,]
    refl.value <- f(reflected)
    
    #three cases
    #1. reflected is neither best nor worst point
    if(refl.value >= values[1] && refl.value <= values[n-1]){
      values[n] <- refl.value
      vertices[n,] <- reflected
    }
    #2. reflected is the best point
    else if(refl.value < values[1]){
      expand <- 2*reflected - centroid
      expand.value <- f(expand)
      if(expand.value < refl.value){
        values[n] <- expand.value
        vertices[n,] <- expand
      }
      else{
        values[n] <- refl.value
        vertices[n,] <- reflected
      }
    }
    #3. reflected is the worst point
    else{
      contracted <- 0.5*centroid + 0.5*vertices[n,]
      contr.value <- f(contracted)
      if(contr.value >= values[n]){  #shrink
        for(j in 2:n){
          vertices[i] <- 0.5*(vertices[1] + vertices[i])
          values[i] <- f(vertices[i])
        }
      }
      else{
        values[n] <- contr.value
        vertices[n,] <- contracted
      }
    }
    volume <- 0.5*(det(vertices[-n,] - matrix(rep(vertices[n,],n-1),n-1)))
    N <- N + 1
    if(plot){
      filled.contour(x,y,z, 
                     xlab = expression(x[1]),
                     ylab = expression(x[2]),
                     xlim = c(-5,5), ylim = c(-5,5), 
                     color = terrain.colors, 
                     main = parse(text = paste0('"Nelder Mead: "', ' ~ f(x[1],x[2]) == ~ x[1]^2 + x[2]^2 + x[1] * sin(x[2]) + x[2] *sin(x[1])')),
                     plot.axes = { axis(1); axis(2);lines(rbind(vertices,vertices[1,]))})
    }
    
  }
  vertices <- vertices[order(values),]
  #print(vertices)
  
  values <- sort(values)
  list(times = N, "minimun",vertex = vertices[1,], value = values[1])
  
}




f <- function(x){
  y <- x[1]^2 + x[2]^2 + x[1]*sin(x[2]) + x[2]*sin(x[1])
  return(y)
}
x1 <- c(5,3)
x2 <- c(2,2)
x3 <- c(1.5, 1)
X <- rbind(x1,x2,x3)
Nelder.mead(f,X, plot = F)
saveGIF(Nelder.mead(f,X, plot = T),ani.width = 600,ani.height = 600, movie.name = 'nelder.gif')





