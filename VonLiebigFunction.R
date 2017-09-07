

### von leibig function y=min(1+0.5, 5))

vlb <-function(x){
min(1+0.5*x, 5)
}

x <- matrix(seq(1,10))

plot(apply(x, 1, vlb), type="l")
