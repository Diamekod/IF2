pay_off = function(S, K, czy_call = T) if(czy_call) pmax(S - K, 0) else pmax(K - S, 0)

delta = function(v, dS) (v[1] - v[3]) / (2 * dS)
gamma = function(v, dS) (v[1] - 2 * v[2] + v[3])  / dS^2

wycena = function(v, dt, dS, S, r, zmiennosc_roczna){
  d = delta(v, dS)
  g = gamma(v, dS)
  if (length(zmiennosc_roczna) != 1){
    if (g < 0) zmiennosc_roczna = zmiennosc_roczna[1]
    else zmiennosc_roczna = zmiennosc_roczna[2]
  }
  a = zmiennosc_roczna^2 * S^2 / 2
  b = r * S
  c = -r
  return(v[2] + (a * g + b * d + c * v[2]) * dt)
}

wycena_douglas_explicit = function(v, dt, dS){
  return((v[3] - 2 * v[2] + v[3])*dt/dS^2 + v[2])
}




finite_diference_call = function(dS, dt, t = 0.837, K, r, zmiennosc_roczna, bariera, amerykanska = F)
{
  V = function(v, S) wycena(v = v, dt = dt, dS = dS, S = S, r = r, zmiennosc_roczna = zmiennosc_roczna)
  S_v = seq(0, bariera, dS)
  t_v = seq(0, t, dt)
  n_S = length(S_v)
  n_t = length(t_v)
  siatka = matrix(0, n_S, n_t)
  siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = T)
  siatka[n_S, n_t] = 0
  for (j in (n_t - 1):1) {
    for (i in (n_S - 1):2) {
      siatka[i, j] <- ifelse(amerykanska == F, V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), max(V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), pay_off(S_v[i], K)))
      if (sum(siatka[i:(i + 1), j + 1]) == 0) break
    }
  }
  row.names(siatka) = S_v
  return(siatka)
}


finite_diference_put = function(dS, dt, t = 0.837, K, r, zmiennosc_roczna, bariera, amerykanska = F){
  V = function(v, S) wycena(v = v, dt = dt, dS = dS, S = S, r = r, zmiennosc_roczna = zmiennosc_roczna)
  S_v = seq(bariera, K * 3, dS) #chyba cena wykonania * 3 miaa byæ
  t_v = seq(0, t, dt)
  n_S = length(S_v)
  n_t = length(t_v)
  siatka = matrix(0, n_S, n_t)
  siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = F)
  siatka[1, n_t] = 0
  for (j in (n_t - 1):1) {
    for (i in (2:(n_S - 1))) {
     siatka[i, j] <- ifelse(amerykanska == F, V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), max(V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), pay_off(S_v[i], K, czy_call = F)))
      if (sum(siatka[i:(i+1), j + 1]) == 0) break
    }
  }
  row.names(siatka) = S_v
  return(siatka)
}

to_df <- function(result, dt)
{
  data.frame(S = rep(as.numeric(row.names(result)), times = ncol(result)), t = rep(seq(0, 0.837, dt), rep(nrow(result), ncol(result))), option_value = as.vector(result))
}

to_df_dywidendy <- function(result, dt, kwotowa = T, dywidenda, kiedy, czy_put = F)
{
  t <- seq(0, 0.837, dt)
  ile <- max(which(t < kiedy))
  if(kwotowa)
  {
    if(czy_put) dywidenda <- -dywidenda
    S <- c(rep(as.numeric(row.names(result)) + dywidenda, times = ile), rep(as.numeric(row.names(result)), times = ncol(result) - ile))
    
  } else {
    if(czy_put) dywidenda <- -dywidenda
    S <- c(rep(as.numeric(row.names(result))/(1-dywidenda), times = ile), rep(as.numeric(row.names(result)), times = ncol(result) - ile))
    
  }
  data.frame(S = S, t = rep(seq(0, 0.837, dt), rep(nrow(result), ncol(result))), option_value = as.vector(result))
  
}



cena_opcji<- function(df_wynik, K, r, sigma, czy_put = F, bariera)
{
  
  type <- ifelse(czy_put, "pdo", "cuo")
  df <- data.frame(S = 0, t = 0, option_value = 0)
  for(j in sort(unique(df_wynik$t)))
  {
    for(i in sort(unique(df_wynik$S)))
    {
      
      df <- rbind(df, c(i, j, StandardBarrierOption(TypeFlag = type, S = i, X = K, H = bariera, r = r, sigma = sigma, K = 0, Time = 0.837 - j, b = 0)@price))
    }
  }
  df$option_value[is.na(df$option_value)] <- 0 #dla ceny akcji == 0
  df[-1,]
}


douglas_scheme_call = function(dS, dt, t = 0.837, K, r, zmiennosc_roczna, bariera, amerykanska = F)
{
  S_v = seq(0, bariera, dS)
  t_v = seq(0, t, dt)
  n_S = length(S_v)
  n_t = length(t_v)
  siatka = matrix(0, n_S, n_t)
  siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = T)
  siatka[n_S, n_t] = 0
  for (j in (n_t - 1):1) {
    for (i in (n_S - 1):2) {
      siatka[i, j] <- ifelse(amerykanska == F, wycena_douglas_explicit(v = siatka[(i + 1):(i - 1), j + 1], dt = dt, dS = dS), max(wycena_douglas_explicit(v = siatka[(i + 1):(i - 1), j + 1], dt = dt, dS = dS), pay_off(S_v[i], K)))
      if (sum(siatka[i:(i + 1), j + 1]) == 0) break
    }
  }
  row.names(siatka) = S_v
  return(siatka)
}

###################################
#CALL##############################
###################################
library(fExoticOptions)
dS <- 50
zmiennosc_roczna <- 0.2
bariera <- 2400
K <- 2150
r <- 0.01


dt <- 1/(zmiennosc_roczna^2*ceiling(bariera/dS)^2)

#europejska
wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera)
wynik_douglas <- douglas_scheme_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera)
#zrobilem funkcje, ktora zamienia wynik na df, nie wiem czy potrzeba
df_wynik <- to_df(wynik, dt)
df_wynik_douglas <- to_df(wynik_douglas, dt)
df_wynik_BSM <- cena_opcji(df_wynik = df_wynik, K = K, r = r, sigma = zmiennosc_roczna, bariera = bariera)
#MSE
mean((df_wynik$option_value-df_wynik_BSM$option_value)^2)/nrow(df_wynik)
mean((df_wynik$option_value-df_wynik_douglas$option_value)^2)/nrow(df_wynik)


library(rgl)

plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)
plot3d(y = df_wynik_BSM$S, x = df_wynik_BSM$t, z = df_wynik_BSM$option_value)



#amerykanska
wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = T)
df_wynik <- to_df(wynik, dt)

#DLA AMERYKANSKIEJ NIE MAM POROWNANIA, TRZEBA POSZUKAC
plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)




###################################
#PUT##############################
###################################
bariera <- 1900
#dla EP 3*k
dt <- 1/(zmiennosc_roczna^2*ceiling(K * 3/dS)^2)

wynik = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera)
df_wynik <- to_df(wynik, dt)
df_wynik_BSM <- cena_opcji(df_wynik = df_wynik, K = K, r = r, sigma = zmiennosc_roczna, bariera = bariera, czy_put = T)
mean((df_wynik$option_value-df_wynik_BSM$option_value)^2)/nrow(df_wynik)
plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)


#amerykanska
wynik = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = T)
df_wynik <- to_df(wynik, dt)

plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)
plot3d(y = df_wynik_BSM$S, x = df_wynik_BSM$t, z = df_wynik_BSM$option_value)




#BY BYLA NIEPEWNOSC TO TRZEBA DAC NIEPEWNOSC = TRUE i zmiennosc_roczna jako wektor

#DYWIDENDY 

df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = T, dywidenda = 50, kiedy = 0.5)
df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = F, dywidenda = 0.3, kiedy = 0.5)


