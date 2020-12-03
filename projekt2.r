#projekt2
library(lubridate)
library(dplyr)
setwd(".")
wig20 <- read.csv("wig20.csv")
wig20_2020 <- wig20 %>% filter(year(Data) == "2020")

T <- 1
dt <- 1/nrow(wig20_2020 %>% filter(as.Date(Data) <= "2020-09-30"))


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

payoff <- function(S, K, B, typ)
{
  #S cena kursu
  #K cena wykonania
  #B bariera
  if(typ == "call")
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



value_option_central <- function(a, b, c, v1, v2, v3, dS, dt, S)
{
  #funkcja liczy cenê opcji dla ceny i w chwili k+1, kiedy wartoœci v1, v2, v3 s¹ niepuste
  #v1 - cena opcji dla ceny i - 1 w chwili k
  #v2 - cena opcji dla ceny i w chwili k
  #v3 - cena opcji dla ceny i + 1 w chwili k
  #tutaj k + 1 jest wczeœniej ni¿ k XD
  a <- a * S^2
  b <- b * S
  v <- v2 + a * dt / dS^2 * (v3 - 2 * v2 + v1) + b * dt / (2 * dS) * (v3 - v1) + c * v2 * dt
  v
}

value_option_boundary <- function(a, b, c, v1, v2, v3, dS, dt, S, is_max)
{
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

dS <- 10
sigma <- 0.18
r <- 0.01
a <- 1/2*sigma^2
b <- r
c <- -r
K <- 2150
bariera <- 2400
dni <- 251


stabilne <- maximal_stable(S = 3*K, a = a, b= b , sigma = sigma)
dS <- stabilne[1]
dt <- stabilne[2]

#CALL
typ_opcji <- "call"
#grid <- create_grid(indeks = wig20_2020, dS = dS, dt = dt, T = T, B = bariera, typ = typ_opcji)

vS <- seq(bariera, 0, by = -dS)
vt <- seq(0, 1, by = (1/dni))
p <- payoff(S = vS, K = K, B = bariera, typ = typ_opcji)
grid <- list(grid = p, time = vt, indeks = vS)

k <- 1/dt + 1

new_row <- p
while (k >= 1)
{
  current_row <- new_row
  for(i in length(grid$indeks):1) #dodalem bez 2400, bo tam cena opcji jest 0 (w callu)
  {
    if(i == length(grid$indeks) | i == 1)
      {
      if(i == length(grid$indeks))
      {
       new_row[i] <- value_option_boundary(a = a, b = b, c = c, v1 = current_row[i - 1], v2 = current_row[i], v3 = current_row[i - 2], dS = dS, dt = dt, S = grid$indeks[i], is_max = T) 
      } else {
        new_row[i] <- value_option_boundary(a = a, b = b, c = c, v1 = current_row[i + 2], v2 =current_row[i ], v3 = current_row[i + 1], dS = dS, dt = dt, S = grid$indeks[i], is_max = F) }
      } else {
        new_row[i] <- value_option_central(a = a, b = b, c = c, v1 = current_row[i - 1], v2 = current_row[i], v3 = current_row[i + 1], dS = dS, dt = dt, S = grid$indeks[i])
      }
  }
  grid$grid <- cbind(new_row, grid$grid, deparse.level = 0)
  print(k)
  k <- k-1
  print(k)
}
View(grid$grid)


