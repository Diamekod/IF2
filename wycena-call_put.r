library(rgl)
library(plotly)
library(ggplot2)
library(fExoticOptions)
library(gridExtra)
library(manipulate)
library(dplyr)

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




finite_diference_call = function(dS, dt, t = 0.837, K, r, zmiennosc_roczna, bariera, amerykanska = F, dywidenda = NA, moment_dywidendy = NA)
{
V = function(v, S) wycena(v = v, dt = dt, dS = dS, S = S, r = r, zmiennosc_roczna = zmiennosc_roczna)
S_v = seq(0, bariera, dS)
t_v = seq(0, t, dt)
n_S = length(S_v)
n_t = length(t_v)
siatka = matrix(0, n_S, n_t)
siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = T)
siatka[n_S, n_t] = 0
S_v_old <- S_v
for (j in (n_t - 1):1) {
  if(!is.na(moment_dywidendy))
  {
    if(t_v[j]<= moment_dywidendy & t_v[j+1] > moment_dywidendy)
    {
      
      if(dywidenda>1)
      {
        S_v <- S_v+dywidenda
      } else {
        S_v <- S_v/(1-dywidenda)
      }
    }
  }
  for (i in (n_S - 1):2) {
    siatka[i, j] <- ifelse(amerykanska == F, V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), max(V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), pay_off(S_v[i], K)))
    if(S_v[i]>=bariera) siatka[i,j] <- 0
    #if (sum(siatka[i:(i + 1), j + 1]) == 0) break
  }
}
row.names(siatka) <- as.character(S_v_old)
return(siatka)
}


finite_diference_put = function(dS, dt, t = 0.837, K, r, zmiennosc_roczna, bariera, amerykanska = F, dywidenda = NA, moment_dywidendy = NA){
  V = function(v, S) wycena(v = v, dt = dt, dS = dS, S = S, r = r, zmiennosc_roczna = zmiennosc_roczna)
  S_v = seq(bariera, K * 3, dS) #chyba cena wykonania * 3 miaa bya
  t_v = seq(0, t, dt)
  n_S = length(S_v)
  n_t = length(t_v)
  siatka = matrix(0, n_S, n_t)
  siatka[, n_t] = pay_off(S = S_v, K = K, czy_call = F)
  siatka[1, n_t] = 0
  S_v_old <- S_v
  for (j in (n_t - 1):1) {
    if(!is.na(moment_dywidendy))
    {
      if(t_v[j]<= moment_dywidendy & t_v[j+1] > moment_dywidendy)
      {
        
        if(dywidenda>1)
        {
          S_v <- S_v+dywidenda
        } else {
          S_v <- S_v/(1-dywidenda)
        }
      }
    }
    for (i in (2:(n_S - 1))) {
      siatka[i, j] <- ifelse(amerykanska == F, V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), max(V(siatka[(i + 1):(i - 1), j + 1], S_v[i]), pay_off(S_v[i], K, czy_call = F)))
      if(S_v[i]<=bariera) siatka[i,j] <- 0
      if (sum(siatka[i:(i+1), j + 1]) == 0) break
    }
  }
  row.names(siatka) <- as.character(S_v_old)
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


cena_opcji_amerykanska <- function(df_wynik, K, czy_put = F, bariera, I)
{
  for(i in 1:nrow(df_wynik))
  {
    print(i)
    if((i + I + 1) > nrow(df_wynik)) break
    df_wynik[i,]$option_value <- max(df_wynik[i + I + 1,]$option_value, pay_off(df_wynik[i,]$S, K, czy_call = !czy_put))
    if(df_wynik[i,]$S == bariera) df_wynik[i,]$option_value <- 0
  }
  return(df_wynik)
}

###################################
#CALL##############################
###################################

dS <- 50
zmiennosc_roczna <- 0.2
bariera <- 2400
K <- 2150
r <- 0.01


dt <- 1/(zmiennosc_roczna^2*ceiling(bariera/dS)^2)

#europejska
wynik_niepewnosc = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = c(0.15, 0.25), bariera = bariera)
wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera)


#zrobilem funkcje, ktora zamienia wynik na df, nie wiem czy potrzeba
df_wynik <- to_df(wynik, dt)
df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)
df_wynik_douglas <- to_df(wynik_douglas, dt)
df_wynik_BSM_EU <- cena_opcji(df_wynik = df_wynik, K = K, r = r, sigma = zmiennosc_roczna, bariera = bariera)
#MSE
mean((df_wynik$option_value-df_wynik_BSM_EU$option_value)^2)/nrow(df_wynik)
mean((df_wynik$option_value-df_wynik_douglas$option_value)^2)/nrow(df_wynik)




breaki <- c( 0, 0.01, 0.5, 1, 3, 5, 7, 
             seq(10, max(df_wynik$option_value), by = 10))
labelki = paste("=<", breaki) 

df_wynik$option_value.f <- cut(df_wynik$option_value,
                               breaks = breaki,
                               include.lowest = T)
g2 <- ggplot(df_wynik, aes(x = t, S)) +
  geom_tile(aes(fill = option_value.f)) + labs(title = 'EP@2150, stala zmiennosc 0.2')+ theme(legend.position = "none")

g2

breaki <- c( 0, 0.01, 0.5, 1, 3, 5, 7, 
             seq(10, max(df_wynik_niepewnosc$option_value), by = 10))

df_wynik_niepewnosc$option_value.f <- cut(df_wynik_niepewnosc$option_value,
                                          breaks = breaki,
                                          include.lowest = T)
df_wynik_niepewnosc$stala <- df_wynik$option_value
g1 <- ggplot(df_wynik_niepewnosc, aes(x = t, S, z = option_value)) +
  geom_tile(aes(fill = option_value.f)) + 
  stat_contour(aes(z = stala), breaks = breaki[seq(1, length(breaki), by = 2)], bins = 12) + 
  labs(title = 'EP@2150, niepewna zmiennosc (0,15,0.25)')  + 
  theme(legend.position = "none")
g1

grid.arrange(g1, g2, ncol = 2, nrow = 1)

plotcall <- function(bariera, a=FALSE){
  
  dS <- 50
  zmiennosc_roczna <- 0.2
  bariera <- bariera
  K <- 2150
  r <- 0.01
  
  
  dt <- 1/(zmiennosc_roczna^2*ceiling(bariera/dS)^2)
  
  #europejska
  wynik_niepewnosc = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = c(0.15, 0.25), bariera = bariera, amerykanska = a)
  wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = a)
  
  
  #zrobilem funkcje, ktora zamienia wynik na df, nie wiem czy potrzeba
  df_wynik <- to_df(wynik, dt)
  df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)
  
  
x = range(df_wynik_niepewnosc$S)
y = df_wynik_niepewnosc$option_value[df_wynik$t == 0]
g1 <- plot_ly(x = seq(x[1], x[2], length = length(y)), 
              y =  y
              , type = 'scatter', mode = 'lines+markers', name = paste('zmienna sigma, bariera ', bariera, sep =''))
g1 <- g1 %>% add_trace(y = df_wynik$option_value[df_wynik$t == 0] , name = 'stala sigma',
                       mode = 'lines+markers',
                       showlegend= FALSE) 
g1 <- g1 %>% layout(title = paste("Wartosci opcji AC@2150, bariera 2400-", bariera, ', bez dywidendy, w t=0 ', sep =''),
                    xaxis = list(title = "S"),
                    yaxis = list (range = c(0,600), title = "V"))  
return(g1)
}

g1 <- plotcall(2400)
g2 <- plotcall(2500)
g3 <- plotcall(2600)
g4 <- plotcall(2700)

sub <- subplot(g1, g2, g3, g4, nrows = 2)
sub

g1 <- plotcall(2400, a = T)
g2 <- plotcall(2500, a = T)
g3 <- plotcall(2600, a = T)
g4 <- plotcall(2700, a = T)

sub <- subplot(g1, g2, g3, g4, nrows = 2)
sub

plotput <- function(bariera, a=FALSE){
  
  dS <- 50
  zmiennosc_roczna <- 0.2
  bariera <- bariera
  K <- 2150
  r <- 0.01
  
  #dla EP 3*k
  dt <- 1/(zmiennosc_roczna^2*ceiling(K * 3/dS)^2)
  
  #europejska
  wynik_niepewnosc = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = c(0.15, 0.25), bariera = bariera, amerykanska = a)
  wynik = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = a)
  
  
  #zrobilem funkcje, ktora zamienia wynik na df, nie wiem czy potrzeba
  df_wynik <- to_df(wynik, dt)
  df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)
  
  
  x = range(df_wynik_niepewnosc$S)
  y = df_wynik_niepewnosc$option_value[df_wynik$t == 0]
  g1 <- plot_ly(x = seq(x[1], x[2], length = length(y)), 
                y =  y
                , type = 'scatter', mode = 'lines+markers', name = paste('zmienna sigma, bariera ', bariera, sep =''))
  g1 <- g1 %>% add_trace(y = df_wynik$option_value[df_wynik$t == 0] , name = 'stala sigma',
                         mode = 'lines+markers',
                         showlegend= FALSE) 
  g1 <- g1 %>% layout(title = paste("Wartosci opcji AP@2150, bariera 1900-", bariera, ', bez dywidendy, w t=0 ', sep =''),
                      xaxis = list(title = "S"),
                      yaxis = list (range = c(0,700), title = "V"))  
  return(g1)
}

g1 <- plotput(1900)
g2 <- plotput(1800)
g3 <- plotput(1700)
g4 <- plotput(1600)

sub <- subplot(g1, g2, g3, g4, nrows = 2)
sub

g1 <- plotput(1900,a = T)
g2 <- plotput(1800,a = T)
g3 <- plotput(1700,a = T)
g4 <- plotput(1600,a = T)

sub <- subplot(g1, g2, g3, g4, nrows = 2)
sub

#amerykanska

bariera <- 2700
wynik_niepewnosc = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = c(0.15, 0.25), bariera = bariera, amerykanska = T)
wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = T)
df_wynik <- to_df(wynik, dt)
df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)


#DLA AMERYKANSKIEJ NIE MAM POROWNANIA, TRZEBA POSZUKAC
df_wynik_BSM_AM <- cena_opcji_amerykanska(df_wynik = df_wynik_BSM_EU, K = K, bariera = bariera, I = bariera/dS)
mean((df_wynik$option_value-df_wynik_BSM_AM$option_value)^2)/nrow(df_wynik)


plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)

breaki <- c( 0, 0.01, 0.5, 1, 3, 5, 7, 
             seq(10, max(df_wynik$option_value), by = 10))
labelki = paste("=<", breaki) 

df_wynik$option_value.f <- cut(df_wynik$option_value,
                               breaks = breaki,
                               include.lowest = T)
g2 <- ggplot(df_wynik, aes(x = t, S)) +
  geom_tile(aes(fill = option_value.f)) + labs(title = 'Wartosci opcji AC@2150, dla bariery 2400')
# Given that factors have discrete levels, we need to use scale_fill_manual instead of scale_fill_continuous
#scale_fill_manual(values = RColorBrewer::brewer.pal(20, "PiYG")) # Use RColorBrewer for discrete categories

g2


###################################
#PUT##############################
###################################
bariera <- 1600
#dla EP 3*k
dt <- 1/(zmiennosc_roczna^2*ceiling(K * 3/dS)^2)
wynik_niepewnosc = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = c(0.15,0.25), bariera = bariera)
wynik = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera)
df_wynik <- to_df(wynik, dt)
df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)

df_wynik_BSM <- cena_opcji(df_wynik = df_wynik, K = K, r = r, sigma = zmiennosc_roczna, bariera = bariera, czy_put = T)
mean((df_wynik$option_value-df_wynik_BSM$option_value)^2)/nrow(df_wynik)
plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)



#amerykanska
wynik_niepewnosc = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = seq(0.15, 0.25, by = 0.05), bariera = bariera, niepewnosc = TRUE, amerykanska = T)
wynik = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = T)
df_wynik <- to_df(wynik, dt)
df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)

plot3d(y = df_wynik$S, x = df_wynik$t, z = df_wynik$option_value)
plot3d(y = df_wynik_BSM$S, x = df_wynik_BSM$t, z = df_wynik_BSM$option_value)


breaki <- c( 0, 0.01, 0.5, 1, 3, 5, 7, 
             seq(10, max(df_wynik$option_value), by = 10))
labelki = paste("=<", breaki) 

df_wynik$option_value.f <- cut(df_wynik$option_value,
                               breaks = breaki,
                               include.lowest = T)
g2 <- ggplot(df_wynik, aes(x = t, S)) +
  geom_tile(aes(fill = option_value.f)) + labs(title = 'Wartosci opcji AP@2150, dla bariery 1900')
# Given that factors have discrete levels, we need to use scale_fill_manual instead of scale_fill_continuous
#scale_fill_manual(values = RColorBrewer::brewer.pal(20, "PiYG")) # Use RColorBrewer for discrete categories

g2




#BY BYLA NIEPEWNOSC TO TRZEBA DAC NIEPEWNOSC = TRUE i zmiennosc_roczna jako wektor

#DYWIDENDY 

df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = T, dywidenda = 50, kiedy = 0.5)
df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = F, dywidenda = 0.1, kiedy = 0.5)




###funkcje plotujace

dplot <- function(wynik, bariera, K, call = TRUE, dS, dt){
  if(call){
    y = seq(0,bariera, dS) 
  }
  else{
    y = seq(bariera, 3*K, dS)
  }
  wplot <- plot_ly(x = seq(0, 0.837, dt),y = y ,z = wynik) %>% 
    add_surface(
      contours = list(
        z = list(
          show=TRUE,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
      )
    ) %>% layout(scene = list(xaxis = list(title = "t"), yaxis = list(title = "S")))
  return(wplot)
}

heatplot <- function(df_wynik){
  
  breaki <- c( 0, 0.01, 0.5, 1, 3, 5, 7, 
               seq(10, max(df_wynik$option_value), length = 5))
  
  
  df_wynik$option_value.f <- cut(df_wynik$option_value,
                                 breaks = breaki,
                                 include.lowest = T)
  g2 <- plot_ly(x = df_wynik$t, 
                y = df_wynik$S, 
                z = as.integer(df_wynik$option_value.f), 
                type = 'heatmap',
                colors = rev(RColorBrewer::brewer.pal(9, "PuRd")), 
                showscale = FALSE)
  
  return(g2)
}

interactive <- function(dS = 50, r = 0.01, bariera, K, call = TRUE){
  if(call){
    dt <- 1/((0.2^2)*ceiling(bariera/dS)^2)
    wynik_niepewnosc = finite_diference_call(dS = dS, 
                                             dt = dt, 
                                             K = K, r = r, zmiennosc_roczna = seq(0.15, 0.25, by = 0.05), 
                                             bariera = bariera, niepewnosc = TRUE)
  }
  else{
    dt <- 1/((0.2^2)*ceiling(K * 3/dS)^2)
    wynik_niepewnosc = finite_diference_put(dS = dS, 
                                            dt = dt, 
                                            K = K, r = r, zmiennosc_roczna = seq(0.15, 0.25, by = 0.05), 
                                            bariera = bariera, niepewnosc = TRUE)
  }
  df_wynik_niepewnosc <- to_df(wynik_niepewnosc, dt)
  trojwym <- dplot(wynik_niepewnosc, bariera, K, call)
  #heat <- heatplot(df_wynik_niepewnosc)
  trojwym
}

suwaczki <- function(){
  manipulate(
    interactive(bariera = bariera, K = K, call = call),
    bariera = slider(1000, 4000, step = 100, initial = 2400),
    K = slider(1000, 4000, step = 100, initial = 2150),
    call = checkbox(TRUE, 'call?')
  )
}

T <- 0.837
dywidenda_plot <- function(t = 0.4185, bar = 2400, dyw = 0.1, strike = 2150, kwotowa = F, call = TRUE, amerykanska = FALSE){
  if(call == TRUE){
    zmiennosc_roczna <- c(0.15, 0.25)
    bariera <- bar
    dS <- 50
    K <- strike
    r <- 0.01
    dt <- 1/(zmiennosc_roczna[1]^2*ceiling(bariera/dS)^2)
    wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = amerykanska)
    df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = kwotowa, dywidenda = dyw, kiedy = t)
    
  }
  else{
    dS <- 50
    zmiennosc_roczna <-  c(0.15, 0.25)
    bariera <- bariera
    K <- 2150
    r <- 0.01
    
    #dla EP 3*k
    dt <- 1/(zmiennosc_roczna[1]^2*ceiling(K * 3/dS)^2)
    
    
    wynik = finite_diference_put(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = amerykanska)
    df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = kwotowa, dywidenda = dyw, kiedy = t)
  }
  dyw_ds <- df_wynik$S[2]-df_wynik$S[1]
  g1 <- dplot(wynik = df_wynik$option_value, bariera = bar, K = strike, call = call, dS = dyw_ds, dt =dt) 
  g1 <- g1 %>% layout(title = paste('Wartosc opcji przy dywidendzie ', dyw, 'strike ', strike, 
                                    'bariera ', bar, sep =''))
  g2 <- heatplot(df_wynik)
  g2 <- g2  %>% layout(title = paste('Wartosci opcji przy dywidendzie ', dyw, 'strike ', strike, 
                                  'bariera ', bar, sep =''))
  f1 <- subplot(g1,g2, nrows = 1)
  
  return(f1)
}

wynik = finite_diference_call(dS = dS, dt = dt, K = K, r = r, zmiennosc_roczna = zmiennosc_roczna, bariera = bariera, amerykanska = T)
df_wynik1 <- to_df(wynik, dt)
df_wynik <- to_df_dywidendy(wynik, dt, kwotowa = F, dywidenda = 0.3, kiedy = 0.415)

dywid <- df_to_matrix(df_wynik)
g1 <- plot_ly(
              z = dywid) %>% add_surface()
g1

g2 <- plot_ly(z = wynik) %>% add_surface()

plt <- subplot(g1,g2, nrows = 1)
plt

df_to_matrix <- function(df){
  x <- sort(unique(df_wynik$t))
  y <- sort(unique(df_wynik$S))
  matrix <- matrix(nrow = length(y), ncol = length(x))
  for(i in seq(1, length(x))){
    for(j in seq(1, length(y))){
      matrix[j,i] <- df$option_value[df$t == x[i] & df$S == y[j]]
    }
  }
  return(matrix)
}
