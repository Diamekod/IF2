#projekt2
library(lubridate)
library(dplyr)
wig20 <- read.csv("wig20.csv")
wig20_2020 <- wig20 %>% filter(year(Data) == "2020")

create_grid <- function(indeks, S_length, t_length, T)
{
  maxS <- 4 * indeks$Zamkniecie[1]
  minS <- 0
  maxT <- T
  minT <- 0
  vS <- seq(maxS, minS, length.out = S_length)
  vt <- seq(minT, maxT, length.out = t_length)
  m <- matrix(0, ncol = length(vt), nrow = length(vS))
  list(grid = m, time = vt, indeks = vS)
}

payoff_call <- function(S, K, B)
{
  #S cena kursu
  #K cena wykonania
  #B bariera
  cond <- S>=B
  S <- pmax(S - K,0)
  S[cond] <- 0
  S
}

payoff_put <- function(S, K, B)
{
  cond <- S<=B
  S <- pmax(K - S, 0)
  S[cond] <- 0
  S
}


grid <- create_grid(wig20_2020, 1000, 100, 3/4)

grid$grid[,ncol(grid$grid)] <- payoff_call(S = grid$indeks, K = 2150, B = 2400)

