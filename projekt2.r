#projekt2
library(lubridate)
library(dplyr)
library(plotly)
setwd(".")
wig20 <- read.csv("wig20.csv")
wig20_2020 <- wig20 %>% filter(year(Data) == "2020")




delta = function(v, dS) (v[1] - v[3]) / (2 * dS)
gamma = function(v, dS) (v[1] - 2 * v[2] + v[3])  / dS^2


##---- I - naszagórna granica siatki
check_stability <- function(dt, sigma, I, ds, a, b){
  v1 <- 1/((sigma^2)*(I^2))
  v2 <- 2*a/abs(b)
  if(dt <= v1 && ds <= v2){
    return(c(TRUE,TRUE))
  }
  else{
    if(dt > v1){
      if(ds <= v2){
        return(c(FALSE, TRUE))
      }
      else{
        return(c(FALSE, FALSE))
      }
    }
      else{
        return(c(TRUE, FALSE))
      }
    }
  }

#S - max S
maximal_stable <- function(S, a , b, sigma )
{
  # a <- a * S^2
  # b <- b * S
  # ds <- 2 * a / abs(b) 
  ds <- 10
  I <- ceiling(S/ds)
  dt <- 1 / (sigma^2 * I^2)
  wynik <- c(ds, dt)
}

create_grid <- function(indeks, dS, dt, T, B, typ, K = 0)
{
  if(typ == "put")
  {
    maxS <- 3*K
    minS <- B 
  } else {
    maxS <- B 
    minS <- 0
  }

  maxT <- T
  minT <- 0
  vS <- seq(minS, maxS, by = dS)
  vt <- seq(maxT, minT, by = -dt)
  m <- matrix(0, ncol = length(vt), nrow = length(vS))
  list(grid = m, time = vt, indeks = vS)
}

payoff <- function(S, K, B, call = TRUE)
{
  #S cena kursu
  #K cena wykonania
  #B bariera
  if(call == TRUE)
  {
  cond <- S>=B
  S <- pmax(S - K,0)
  S[cond] <- 0
  } else {
    cond <- S<=B
    S <- pmax(K - S, 0)
    S[cond] <- 0
  }
  S
}



value_option_central <- function(a, b, c, v, dS, dt, S)
{
  #funkcja liczy cenê opcji dla ceny i w chwili k+1, kiedy wartoœci v1, v2, v3 s¹ niepuste
  #v1 - cena opcji dla ceny i - 1 w chwili k
  #v2 - cena opcji dla ceny i w chwili k
  #v3 - cena opcji dla ceny i + 1 w chwili k
  #tutaj k + 1 jest wczeœniej ni¿ k XD
  d = delta(v, dS)
  g = gamma(v, dS)
  a <- a * S^2
  b <- b * S
  v <- (v[2] + (a * g + b*d +c * v[2])*dt)
  return(v)
}

value_option_boundary <- function(a, b, c, v1, v2, v3, dS, dt, S, is_max)
{
  d = delta(v, dS)
  g = gamma(v, dS)
  a <- a * S^2
  b <- b * S
  if(is_max == T)
  {
    #v3 - cena opcji dla ceny i - 2 w chwili k
    v <- v2 + a * dt / dS^2 * (-2*v1 + v2 + v3) + b * dt / dS * (v2 - v1) + c * v2 * dt
  } else {
    #v1 - cena opcji dla ceny i + 2 w chwili k
    v <- v2 + a * dt / dS^2 * (-2*v3 + v2 + v1) + b * dt / dS * (v3 - v2) + c * v2 * dt
  }
  v
}


stabilne <- maximal_stable(S = 3*K, a = a, b= b , sigma = sigma)
dS <- stabilne[1]
dt <- stabilne[2]

#---- Warunki pocz
T <- 0.83
dS <- 10
sigma <- 0.18
r <- 0.01
K <- 2150
bariera <- 2400
dni <- 30


#--- brzegi
lower <- function(k){
  return(0)
}

upper <- function(k){
  return(0)
}
#CALL
typ_opcji <- "call"
#grid <- create_grid(indeks = wig20_2020, dS = dS, dt = dt, T = T, B = bariera, typ = typ_opcji)


simulate_FD <- function(dS, T, K, bariera, call = TRUE, sigma, r, lower, upper)
{
  a <- 1/2*sigma^2
  b <- r
  c <- -1*r
  vS <- seq(bariera, 0, by = -dS)
  I <- length(vS)
  dt <- 1/((sigma^2)*(I^2))
  print(dt)
  vt <- seq(0, T, by = dt)
  p <- sapply(vS, payoff, K = K, B = bariera, call = call)
  grid <- list(grid = p, time = vt, indeks = vS)
  
  
  
  k <- floor(T/dt)

  new_row <- p
  while (k >= 1)
  {
    
    current_row <- new_row
    for(i in (I-1):2) #dodalem bez 2400, bo tam cena opcji jest 0 (w callu)
      {
        new_row[i] <- value_option_central(a = a, b = b, c = c, v = current_row[i+1:i-1], dS = dS, dt = dt, S = grid$indeks[i])
      }
    new_row[1] <- lower(k)
    new_row[I] <- upper(k)
    grid$grid <- cbind(grid$grid, new_row, deparse.level = 0)
    print(k)
    k <- k-1
    
  }
  return(grid)
}

boczki <- simulate_FD(dS = dS, dni = dni, K = K , bariera = bariera, sigma = sigma, r = r, lower = lower, upper = upper)
View(boczki$grid)

fig <- plot_ly(z = ~boczki$grid)
fig <- fig %>% add_surface()

fig
