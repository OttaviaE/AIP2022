library("png")
setwd("C:/Users/huawei/OneDrive/Documenti/GitHub/AIP2022/Rasch")
par(mar = c(5,7,4,2) + 0.1) 
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  y[is.na(y)] = 1
  return(y)
}
difficulty <- c(-3,0, 3)
theta <- theta <- seq(-7, 7, .001)
# item a
plot(theta, IRT(theta, b=0),
     cex.lab= 2, 
     cex.axis =1.5,
     xlab = expression(paste(beta[p], " = ", delta[i])), 
     ylab = expression(paste("P(", X[p][i], "|",beta[p],",",  
                                                       delta[i], " = 1)")),
     xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue")
segments(-7, 0.5, 
         0,  0.5, 
         col = "royalblue", lty = 2)
segments(0, -0.5, 
         0,  0.5, 
         col = "royalblue", lty = 2)
points(0, 0.5, pch=5, col = "red", cex = 3, lwd = 4)

# add on the same graph
lisa = readPNG("lisa.png")
bart = readPNG("bart.png")

segments(-6, exp(2-0)/(1+exp(2-0)), 
         2,  exp(2-0)/(1+exp(2-0)), 
         col = "royalblue", lty = 2)
segments(2, -0.5, 
         2,  exp(2-0)/(1+exp(2-0)), 
         col = "royalblue", lty = 2)
rasterImage(lisa, 1.5, 0.8, 2.5, 1)
mtext(expression(paste(beta[p], " > ", delta[i])), 
      side = 1, at = 2, padj=1.7, cex=2)
segments(-6, exp(-2-0)/(1+exp(-2-0)), 
         -2,  exp(-2-0)/(1+exp(-2-0)), 
         col = "royalblue", lty = 2)
segments(-2, -0.5, 
         -2,  exp(-2-0)/(1+exp(-2-0)), 
         col = "royalblue", lty = 2)
rasterImage(bart, -2.5, 0.03, -1.5, 0.23)
mtext(expression(paste(beta[p], " < ", delta[i])), 
      side = 1, at = -2, padj=1.7, cex=2)
#points(2, exp(2-0)/(1+exp(2-0)), pch=5, col = "red", cex = 3, lwd = 4)


# beta > delta

plot(theta, IRT(theta, b=0),
     cex.lab= 2, 
     cex.axis =1.5,
     xlab = expression(paste(beta[p], " > ", delta[i])), 
     ylab = expression(paste("P(", X[p][i], "|",beta[p],",",  
                                                                                 delta[i], " = 1)")),
     xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue")
segments(-6, exp(2-0)/(1+exp(2-0)), 
         2,  exp(2-0)/(1+exp(2-0)), 
         col = "royalblue", lty = 2)
segments(2, -0.5, 
         2,  exp(2-0)/(1+exp(2-0)), 
         col = "royalblue", lty = 2)
points(2, exp(2-0)/(1+exp(2-0)), pch=5, col = "red", cex = 3, lwd = 4)


# beta < delta

plot(theta, IRT(theta, b=0),
     cex.lab= 2, 
     cex.axis =1.5,
     xlab = expression(paste(beta[p], " < ", delta[i])), 
     ylab = expression(paste("P(", X[p][i], "|",beta[p],",",  
                             delta[i], " = 1)")),
     xlim = c(-5, 5), ylim = c(0, 1), 
     type = "l", lwd = 3, 
     col = "royalblue", 
)

segments(-6, exp(-2-0)/(1+exp(-2-0)), 
         -2,  exp(-2-0)/(1+exp(-2-0)), 
         col = "royalblue", lty = 2)
segments(-2, -0.5, 
         -2,  exp(-2-0)/(1+exp(-2-0)), 
         col = "royalblue", lty = 2)
points(-2, exp(-2-0)/(1+exp(-2-0)), pch=5, col = "red", 
       cex = 3, lwd = 4)
