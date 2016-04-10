# install.packages("sm")
install.packages("rpanel")

library(sm)
?sm.density
# 

s

#  A one-dimensional example
y <- rnorm(50)
sm.density(y, model = "Normal")
# sm.density(y, panel = TRUE)

#  A two-dimensional example
y <- cbind(rnorm(50), rnorm(50))
sm.density(y, display = "image")
# sm.density(y, panel = TRUE)


#  A three-dimensional example
y <- cbind(rnorm(50), rnorm(50), rnorm(50))
sm.density(y, display="slice")



x= tmp[[1]]
sm.density(x[,2:3], weights=x[,5], display="image")
x= tmp[[2]]
sm.density(x[,2:3], weights=x[,5], display="image")
x= tmp[[3]]
sm.density(x[,2:3], weights=x[,5], display="image")
x= tmp[[4]]
sm.density(x[,2:3], weights=x[,5], display="image")

x= tmp[[5]]
sm.density(x[,2:3], display="image")




library(ggplot2)
x = data.frame(tmp[[1]])
ggplot(data=x, aes(X2, X3)) + 
  geom_hex()


x = data.frame(tmp[[2]])
ggplot(data=x, aes(X2, X3)) + 
  geom_hex()

x = data.frame(tmp[[3]])
ggplot(data=x, aes(X2, X3)) + 
  geom_hex()

x = data.frame(tmp[[5]])
ggplot(data=x, aes(X2, X3)) + 
  geom_hex()


