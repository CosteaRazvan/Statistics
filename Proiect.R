################################################################################
# Exc I
################################################################################

#1

# Expected value for continuous random variable
E_c <- function(pdf){
  x1 <- runif(10^6)
  x_inf <- (1-x1)/x1
  f <- function(x) x * pdf(x) * 1/x1^2
  return(mean(f(x_inf)))
}

# Variance for continuous random variable
Var_c <- function(pdf){
  x1 <- runif(10^6)
  x_inf <- (1-x1)/x1
  f <- function(x) x**2 * pdf(x) * 1/x1^2
  return(mean(f(x_inf)) - E_c(pdf)**2)
}

# Expected value for discrete random variable
E_d <- function(pmf) {
  x <- seq(0, 10^2, 1)
  return(sum(x*pmf(x)))
}

# Variance for discrete random variable
Var_d <- function(pmf) {
  x <- seq(0, 10^2, 1)
  prob <- pmf(x)
  return(sum(x**2*prob) - sum(x*prob)**2)
}

#2
mom_3 <- function(X){
  return(mean(abs(X - mean(X))**3))
}

#3
BE_Margin <- function(X_1, n){
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  return(33/4 * (mom_3(X_1)) / (sqrt(n) * sigma**3))
}

names = c('Binomial', 'Geometric', 'Poisson', 'Uniform', 'Exponential', 'Gamma', 'Beta')
volumes = c(30, 100, 1000)

# Create a dataframe with Distribution and Volume columns
BE_Margins <- data.frame(
  'Distribution' = c(c(names, names), names),
  'Volume' = sort(c(c(c(c(c(c(volumes, volumes), volumes), volumes), volumes), volumes), volumes))
)

margins <- c()

for(n in volumes){
  # Discrete rv
  X_1_bin = rbinom(10^6, 3, 0.4)
  X_1_geom = rgeom(10^6, 0.3)
  X_1_pois = rpois(10^6, 10)
  X_1_unif = sample(1:20, 10^6, replace=T)
  
  # Continuous rv
  X_1_exp = rexp(10^6, 3)
  X_1_gamma = rgamma(10^6, 9, 0.5)
  X_1_beta = rbeta(10^6, 2, 2)
  
  # Calculate the B-E margins for every given distribution
  margins <- c(margins, BE_Margin(X_1_bin, n))
  margins <- c(margins, BE_Margin(X_1_geom, n))
  margins <- c(margins, BE_Margin(X_1_pois, n))
  margins <- c(margins, BE_Margin(X_1_unif, n))
  margins <- c(margins, BE_Margin(X_1_exp, n))
  margins <- c(margins, BE_Margin(X_1_gamma, n))
  margins <- c(margins, BE_Margin(X_1_beta, n))
  
}

# Append the Margin column to dataframe
BE_Margins$Margin = margins

# Print
BE_Margins

#4
par(mfrow=c(1, 3))

# Binomial
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- rbinom(10^6, 3, 0.4)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- rbinom(10^6, 3, 0.4)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Binomial with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# Geometric
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- rgeom(10^6, 0.3)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- rgeom(10^6, 0.3)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Geometric with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# Poisson
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- rpois(10^6, 10)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- rpois(10^6, 10)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Poisson with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# Discrete Uniform
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- sample(1:20, 10^6, replace = T)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- sample(1:20, 10^6, replace = T)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Discrete Uniform with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# Exponential
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- rexp(10^6, 3)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- rexp(10^6, 3)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Exponential Uniform with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# Gamma
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- rgamma(10^6, 9, 0.5)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- rgamma(10^6, 9, 0.5)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Gamma Uniform with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# Beta
volumes <- c(30, 100, 1000)

for(n in volumes){
  X_bar <- c()
  
  # Init the sampling mean
  X_1 <- rbeta(10^6, 2, 2)
  X_bar <- c(X_bar, mean(X_1))
  
  for(i in 2:n){
    X_i <- rbeta(10^6, 2, 2)
    X_bar <- c(X_bar, mean(X_i))
  }
  
  # Init Z_n rv
  sigma = sqrt(mean(X_1**2) - mean(X_1)**2)
  Z_n <- (sqrt(n) * (X_bar - mean(X_1))) / sigma
  
  # Use ecdf() function to find the cdf of Z_n
  cdf <- ecdf(Z_n)
  
  # Define [0, 1] interval
  x <- seq(0, 1, 0.001)
  diff <- c()
  for(t in x){
    # Compute the difference
    diff <- c(diff, cdf(t) - pnorm(t))
  }
  
  # Plot
  plot(x, diff, main=sprintf('Beta Uniform with %d volume', n), lwd=0.5)
  lines(x, diff, col="red")
}

# 5

# Compute B-E margin for continous rv
Margin_c <- function(X_1, n, pdf){
  miu <- E_c(pdf)
  sigma <- sqrt(Var_c(pdf))
  return(33/4 * mean(abs(X_1 - miu)**3) / (sqrt(n) * sigma**3))
}

# Compute B-E margin for discrete rv
Margin_d <- function(X_1, n, pmf){
  miu <- E_d(pmf)
  sigma <- sqrt(Var_d(pmf))
  return(33/4 * mean(abs(X_1 - miu)**3) / (sqrt(n) * sigma**3))
}

pdf <- function(x){
  3*exp(-3*x)
}

pmf <- function(x){
  exp(-10)*(10^x)/(factorial(x))
}

generate_c <- function(){
  # we know the pdf/pmf so we can find cdf/cmf and apply inverse method for exp(lambda=3)
  u <- runif(10^6)
  x <- -1/3*log(u)
  return(x)
}

generate_d <- function(){
  # we know the pdf/pmf so we can find cdf/cmf and apply inverse method for pois(lambda=10)
  X <- c()
  for(i in 1:10^6){
    u <- runif(1)
    j <- 0
    p <- exp(-10)
    F_x <- p
    while(u >= F_x){
      p <- (10*p)/(j+1)
      F_x <- F_x + p
      j <- j + 1
    }
    X[i] <- j
  }
  return(X)
}

X_1 <- generate_c()
Margin_c(X_1, 1000, pdf)

X_1 <- generate_d()
Margin_d(X_1, 1000, pmf)



################################################################################
# Exc II
################################################################################

#4)

#partea neconstanta a lui f. in alte cuvinte, f proportional cu fb.
fb <- function(x) {
  (sin(x))^2*exp(-x^2*sqrt(x))
}

#g1,g2,g3 din enunt
g1 <- function(x) {
  (exp(-x))
}

g2 <- function(x) {
  (1/(pi)* (1/(1+x^2/4)))
}

g3 <- function(x) {
  (1/sqrt(pi/2)*exp(-x^2/2))
}



#gasim c1,c2,c3 optimi.
h1 <- function(x){
  fb(x)/g1(x)}
c1 <- optimize(h1, lower=0,upper=10, maximum = T)
h2 <- function(x){
  fb(x)/g2(x)}
c2 <- optimize(h2, lower=0,upper=10, maximum = T)
h3 <- function(x){
  fb(x)/g3(x)}
c3 <- optimize(h3, lower=0,upper=10, maximum = T)

c1 <-as.numeric((c1)[2])
c2 <-as.numeric((c2)[2])
c3 <-as.numeric((c3)[2])


#Cateva grafice ajutatoare

dom = seq(from = 0, to = 3, by = 0.01)
plot(dom,h1(dom),type='l',ylim=c(0,1.5),col='green',main='Functiile h1,h2,h3\nverde,rosu,albastru',xlab='Ox',ylab='Oy')
lines(dom,h2(dom),col='red')
lines(dom,h3(dom),col='blue')


plot(dom,h1(dom)/c1,type='l',ylim=c(0,1.5),col='green',main='Functiile h1,h2,h3 impartite la valorile lor maxime\nverde,rosu,albastru',xlab='Ox',ylab='Oy')
lines(dom,h2(dom)/c2,col='red')
lines(dom,h3(dom)/c3,col='blue')


plot(dom,fb(dom),type='l',ylim=c(0,1.5),col='black',main='Cum acopera functia target f functiile \n g1,g2,g3 inmultite cu c1,c2,c3',xlab='Ox',ylab='Oy')
lines(dom,g1(dom)*c1,col='green')
lines(dom,g2(dom)*c2,col='red')
lines(dom,g3(dom)*c3,col='blue')




#Aplicam algoritmul din metoda respingerii.
t <- 10000


sf_1 <- function(x, n) {
  repeat {
    u <- runif(1, 0, 1)
    y <- rexp(1)
    if (u <= fb(y)/(c1*g1(y))) {
      return(y)
    }}}
X1 <- sapply(1:(t), sf_1)

x1 <- seq(min(X1), max(X1), 0.001)
hist(X1, freq = F,main='Rezultatul metodei respingerii, pentru g1',xlab='Ox',ylab='Oy')

lines(x1, fb(x1), col = "blue")



sf_2 <- function(x) {
  repeat {
    u <- runif(1, 0, 1)
    y <- abs(rcauchy(1,0,1))
    if (u <= fb(y)/(c2*g2(y))) {
      return(y)
    }}}
X2 <- sapply(1:(t), sf_2)

x2 <- seq(min(X2), max(X2), 0.001)
hist(X2, freq = F,main='Rezultatul metodei respingerii, pentru g2',xlab='Ox',ylab='Oy')

lines(x2, fb(x2), col = "blue",)



sf_3 <- function(x, n) {
  repeat {
    u <- runif(1, 0, 1)
    y <- abs(rnorm(1,0,1))
    if (u <= fb(y)/(c3*g3(y))) {
      return(y)
    }}}
X3 <- sapply(1:(t), sf_3)

x3 <- seq(min(X3), max(X3), 0.001)
hist(X3, freq = F,main='Rezultatul metodei respingerii, pentru g3',xlab='Ox',ylab='Oy')

lines(x3, fb(x3), col = "blue")

#Se observa ca functia fb, in lipsa constantei de normalizare, nu se afla deasupra histogramei. 
#Astfel metoda de a deduce constanta de normalizare este sa gasim o constanta care ar face graficul
#functiei sa se potriveasca cu histograma. In cazul nostru, acea constanta este exp(1)*2^0.5

#Vizual, pentru:

f <- function(x) {
  exp(1)*2^0.5*(sin(x))^2*exp(-x^2*sqrt(x))
}

#Gasim ca
hist(X1, freq = F,main='Pentru rezultatul aferent g1',xlab='Ox',ylab='Oy')

lines(x1, f(x1), col = "blue")

hist(X2, freq = F,main='Pentru rezultatul aferent g2',xlab='Ox',ylab='Oy')

lines(x2, f(x2), col = "blue")

hist(X3, freq = F,main='Pentru rezultatul aferent g3',xlab='Ox',ylab='Oy')

lines(x3, f(x3), col = "blue")

#Se potriveste bine, pentru fiecare g!



################################################################################
# Exc III
################################################################################

n <- 10^6 
max_n <- 4
# n = numarul de sample-uri
# max_n = numarul total de exercitii din examen

# Generam cate n valori pentru elevul A pentru fiecare exercitiu
# valoarea 1 -> elevul A rezolva exerciutiul
# valoarea 0 -> elevul A nu rezolva exercitiul
#  ex 1           ex max_n   
# B(a[1]), ... , B(a[max_n])
#
# p este un vector care contine valorile alfa[i]
Bernoulli <- function(p){
  return(ifelse(runif(n * max_n) < 1-p, 0, 1)) 
}

# Generam cate n valori pentru elevul A pentru fiecare exercitiu
# valoarea t -> elevul A ar rezolva exercitiul in t timp
#  ex 1           ex max_n   
# E(l[1]), ... , E(l[max_n])
#
# l este un vector care contine valorile lambda[i]
Exponential <- function(l){
  return((-1/l)*log(runif(n * max_n), base=exp(1)))
}

# generam cate un alfa[i] pentru fiecare exercitiu i
p <- runif(max_n)
# generam cate un lambda[i] pentru fiecare exercitiu i
l <- runif(max_n)
# generam  n      linii   -> numarul de sampleuri
# cu cate  max_n  coloane -> numarul de exercitii
b <- matrix(Bernoulli(p), nrow = n, ncol = max_n)
t <- matrix(Exponential(l), nrow = n, ncol = max_n)

# folosimt functia aply pentru a gasi primul 0 pentru fiecare sample
# i.e. cautam primul exercitiul pentru care elevul A se va opri din rezolvat
#
# MARGIN=1  ->  functia va primia ca input fiecare linie din b
stop <- apply(b, MARGIN = 1, function(x) {
  # pastram indicii pozitiilor unde avem 0 in vectorul linie
  i <- which(x == 0)
  # daca nu exista 0, atunci elevul A rezolva tot examenul
  if (length(i) == 0){
    return(max_n)
  } 
  # daca exista 0, atunci elevul A se opreste la primul 0
  return(i[1]-1)
})

# concatenam coloana indicilor de oprire la matrice t
# ex 1      ex max_n  indice oprire
# t[1]      t[max_n]  stop[i]       -> n astfel de linii
t_stop <- cbind(t, stop)
# MARGIN=1  ->  functia va primia ca input fiecare linie din t_stop
T <- apply(t_stop, MARGIN = 1, function(x){
  # x[length(x)] = indicele de oprire
  # sumam toti timpi pana la indicele de oprire
  return(sum(x[1:x[length(x)]]))
})
################################################################################
#1)
hist(T, xlim = c(0, max_n*10), breaks = 10^5, freq = FALSE)
Expected_value <- mean(T)
print(Expected_value)
################################################################################
#3)
p_finish <-sum(stop == max_n)/n
print(p_finish)
################################################################################
#4)
time <- runif(1, 1, 120)
p_finish_time <- sum(T[which(stop == max_n)] < time)/n
print(p_finish_time)
################################################################################
#5)
time_finish <- T[stop == max_n]
min_time <- min(time_finish)
max_time <- max(time_finish)

hypo_samples <- c()
for(i in 1:max_n){
  if(i==1){
    hypo_samples <- rexp(length(time_finish), l[i])     
  } else{
    hypo_samples <- hypo_samples+rexp(length((time_finish)), l[i])
  }
}
max_time <- max(hypo_samples)

# plotam histogramele una sub celalta
par(mfrow=c(2,1))
hist(time_finish, xlim = c(min_time, max_time), breaks = 10^4, freq = FALSE)
hist(hypo_samples, xlim = c(min_time, max_time), breaks = 10^4, freq = FALSE)
################################################################################
#6)
par(mfrow=c(1,1))
p_k <- c()
for(k in 1:max_n){
  p_k <- c(p_k, sum(stop < k)/n) 
}
plot(1:max_n, p_k, type="l", xlab="k", ylab="p_k", main="Plot of p_k")



################################################################################
# Exc IV
################################################################################
#Proiect Exercitiul 4:
  #1) i) Xn converge in distributie la X
  #X e binomiala de n=1 si p=1/2
  #Sirul Xn e Beta de 1/n si 1/n
# Generam sirul Xn
genXn <- function(n)
{ 
  res <- rbeta(n, 1/n, 1/n)
  return(res)
}
# Folosim functia check.convergence in modul "L" pentru a verifica convergenta in distributie la
# X pe care il introducem prin functia de masa si prin cea de repartitie

check.convergence(500, 250, genXn, mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)}) 

check.convergence(2000, 1000, genXn, mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)}) 

#ii)
a <- 1
b <- 1
# Generam sirul de variabile aleatoare, de aceasta data ca o functie de n, a si b
genXn <- function(n, a, b)
{ 
  res <- rbeta(n, a/n, b/n)
  return(res)
}
# Aici trebuie sa completam cu argumentul argsXn unde adaugam a si b carora le dam niste valori default
# Se disting 3 cazuri:
# a= b
check.convergence(500, 250, genXn, argsXn=list(a = 5, b = 5), mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)})
check.convergence(2000, 1000, genXn, argsXn=list(a = 5, b = 5), mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)})

# a<b
check.convergence(500, 250, genXn, argsXn=list(a = 2, b = 5), mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)})
check.convergence(2000, 1000, genXn, argsXn=list(a = 2, b = 5), mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)})

# a>b
check.convergence(500, 250, genXn, argsXn=list(a = 5, b = 2), mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)})
check.convergence(2000, 1000, genXn, argsXn=list(a = 5, b = 2), mode="L",
                  density = F,
                  densfunc = function(x){dbinom(x, 1, 1/2)},
                  probfunc = function(x){pbinom(x, 1, 1/2)})


 #2)
#i) Xn converge in distributie la o uniforma pe 0, 1?
# Functia care ne genereaza valori cu aceeasi probabilitate pe intervalul discretizat
rUnifDisc <- function(n, discreteUnifValues) sample(discreteUnifValues, n, replace=T)
# Generam Xn pe care il vom folosi in check.convergence
genXn <- function(n)
{
  discreteUnifValues <- seq(from = 1/n, to = 1, by = 1/n)
  res <- rUnifDisc(n, discreteUnifValues)
  return(res)
}

check.convergence(nmax = 500, M = 250, genXn, mode="L",
                  density = F,
                  densfunc = function(x){dunif(x, 0, 1)},
                  probfunc = function(x){punif(x, 0, 1)})


check.convergence(nmax = 2000, M = 1000, genXn, mode="L",
                  density = F,
                  densfunc = function(x){dunif(x, 0, 1)},
                  probfunc = function(x){punif(x, 0, 1)})

#ii) Xn converge in probabilitate la o uniforma pe 0, 1?
# Functia care ne genereaza valori cu aceeasi probabilitate pe intervalul discretizat
rUnifDisc <- function(n, discreteUnifValues) sample(discreteUnifValues, n, replace=T)
# Generam un nou sir Xn, care este obtinut din Xn generat ca in subpunctul 2i) 
# din care  scadem un X cu distributie uniforma pe (0, 1)
genXn <- function(n)
{ X <- runif(1)
  discreteUnifValues <- seq(from = 1/n, to = 1, by = 1/n)
  # Verificam convergenta lui Xn la X, folosind ca generator sirul Xn-X si verificand daca
  # pn descreste spre 0
  res <- rUnifDisc(n, discreteUnifValues) - X
  return(res)
}
check.convergence(nmax = 2000, M = 1000, genXn, mode="p")

#3) Fie X1,X2,….Xn v.a. i.i.d . Notăm cu m şi respectiv M infimumul şi respectiv 
#supremumul mulţimii valorilor pe care le poate lua X1.
#(i.e. P(m≤X≤M)=1, P(X1<a)>0 şi P(X1>b)>0 pentru orice a>m şi respectiv 
#b<M).
#i)Verificaţi că min{X1 , X2 …Xn}𝑎.𝑠.→ m

# Luam Xn exponentiale de 2,
genXn <- function(n)
{# xMin va fi min{X1,X2,...Xn}
  xMin <- cummin(rexp(n, 2))
# infimum luam valoarea minima din xMin
  infimum <- min(xMin)
# verificam convergenta aproape sigura a lui xMin la infimum
  return(xMin - infimum)
}

check.convergence(nmax = 2000, M = 1000, genXn, mode = "as")

genXn <- function(n)
{
  xMin <- cummin(rpois(n, 5))
  infimum <- min(xMin)
  return(xMin - infimum)
}

check.convergence(nmax = 2000, M = 1000, genXn, mode = "as")

#(ii)Verificaţi că max{X1 , X2 …Xn}𝑎.𝑠.→ M

# Luam Xn exponentiale de 2, 
genXn <- function(n)
{ # xMax va fi max{X1,X2,...Xn}
  xMax <- cummax(rexp(n, 2))
  # supremum luam valoarea maxima din xMax
  supremum <- max(xMax)
  # verificam convergenta aproape sigura a lui xMax la supremum
  return(xMax - supremum)
}

check.convergence(nmax = 2000, M = 1000, genXn, mode = "as")

genXn <- function(n)
{ 
  xMax <- cummax(rpois(n, 5))
  supremum <- max(xMax)
  return(xMax - supremum)
}

check.convergence(nmax = 2000, M = 1000, genXn, mode = "as")




