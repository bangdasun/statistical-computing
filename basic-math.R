# basic computing
sign(-10:10)
9 %% 4

# integer division
5 %/% 3

# combination
choose(5, 2)
combn(5, 2)
choose(5, 2) * factorial(2)

# logical 
isTRUE(1 == 1)
identical(as.integer(1), 1L)
xor(c(0, 1, 0, 1), c(0, 0, 1, 1))

# vector aggregate
weighted.mean(c(2, 3, 1, 4, 3), c(1, 1, 2, 2, 2))
any(c(2, 1, 4, 3, 1) < 3)
all(c(2, 1, 4, 3, 1) < 3)
duplicated(c(1, 1, 2, 2, 3, 4, 5))

# distribution fit test
set.seed(1)
unif = runif(1000)
ks.test(unif, 'punif')  # -> cannot rject H0

normal = rnorm(1000)
shapiro.test(normal)
ks.test(normal, 'pnorm')

expo = rexp(1000)
ks.test(expo, 'pexp')

gamm = rgamma(1000, 1)
ks.test(gamm, 'pgamma', 1)

t = rt(1000, 25)
ks.test(t, 'pt', 25)

# derivatives
dx = deriv(y ~ x^3, 'x')
dx
x = seq(1, 4, by = .5)
eval(dx)


d2x = deriv3(y ~ x^3, 'x')
d2x
eval(d2x)


fxy = expression(2 * x^2 + y + 3 * x * y^2)
dxy = deriv(fxy, c('x', 'y'))
dxy
x = 1; y = 1
eval(dxy)
