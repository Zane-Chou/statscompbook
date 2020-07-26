f <- function(x) x^3
fx <- as.expression(body(f))
f <- deriv(fx, "x", func = TRUE)
f_value <- f(x0)
f.x0 <- f_value[1]
diff.x0 <- attr(f_value, "gradient")[1]
tol <- 1e-9
maxiter <- 5
count <- 0
x0 <- 6
f_value <- f(x0)
f.x0 <- f_value[1]
diff.x0 <- attr(f_value, "gradient")[1] 
pdf(file = '~/Documents/Statistical-Computing-Chinese/newtonraphson/newton.pdf')
curve(f, -6 , 6, col = 2)
abline(h=0, col = 'gray')
segments(x0, 0, x0, f.x0, lty = 2)
text(x0, -10, expression(x[0]))
legend("bottomright", c('Function', 'Tangent'), col = c(2,3), lty = c(1, 2))
abline(a = f.x0 - x0*diff.x0, b = diff.x0, col = 3, lty = 2)
while (abs(f.x0) > tol && count < maxiter){
    xn <- x0-f.x0/diff.x0
    x0 <- xn
    f_value <- f(xn)
    f.x0 <- f_value[1]
    diff.x0 <- attr(f_value, "gradient")[1] 
    count <- count + 1
    segments(x0, 0, x0, f.x0,  lty = 2)
    text(x0, -10, bquote('x'[.(count)]))
    if (count < maxiter){
        segments(x0, 0, x0, f.x0,  lty = 2)
        text(x0, -10, bquote('x'[.(count)]))
        abline(a = f.x0 - x0*diff.x0, b = diff.x0, col = 3, lty = 2)}
}
dev.off()
