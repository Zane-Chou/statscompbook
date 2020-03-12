f <- function(x) x^3
fx <- as.expression(body(f))
f <- deriv(fx, "x", func = TRUE)
f_value <- f(x0)
f.x0 <- f_value[1]
diff.x0 <- attr(f_value, "gradient")[1]
tol <- 1e-9
maxiter <- 5
count <- 0
x0 <- 4
f_value <- f(x0)
f.x0 <- f_value[1]
diff.x0 <- attr(f_value, "gradient")[1] 
saveGIF(
    {curve(f, -6 , 6, col = 2)
        abline(h=0, col = 'gray')
        segments(x0, 0, x0, f.x0, lty = 2)
        legend("bottomright", c('Function', 'Tangent'), col = c(2,3), lty = c(1, 2))
        curve(f, -6 , 6)
        abline(h=0, col = 'gray')
        segments(x0, 0, x0, f.x0, lty = 2)
        abline(a = f.x0 - x0*diff.x0, b = diff.x0, col = 3, lty = 2)
        legend("bottomright", c('Function', 'Tangent'), col = c(2,3), lty = c(1,2))
        while (abs(f.x0) > tol && count < maxiter){
            xn <- x0-f.x0/diff.x0
            x0 <- xn
            f_value <- f(xn)
            f.x0 <- f_value[1]
            diff.x0 <- attr(f_value, "gradient")[1] 
            count <- count + 1
            curve(f, -6, 6, col = 2)
            abline(h=0, col = 'gray')
            segments(x0, 0, x0, f.x0,  lty = 2)
            legend("bottomright", c('Function', 'Tangent'), col = c(2,3), lty = c(1,2))
            if (count < maxiter){
            Sys.sleep(1)
            curve(f, -6, 6, col = 2)
            abline(h=0, col = 'gray')
            segments(x0, 0, x0, f.x0,  lty = 2)
            abline(a = f.x0 - x0*diff.x0, b = diff.x0, col = 3, lty = 2)
            legend("bottomright", c('Function', 'Tangent'), col = c(2,3), lty = c(1,2))}
        }}, movie.name = 'newton.gif',
    interval = 1, 
    ani.width = 600, 
    ani.height = 400,
    outdir = getwd())

