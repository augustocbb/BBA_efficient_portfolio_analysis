library(installr)

library (vars)

library(openxlsx)

library(gtools)
library(readxl)

# For obtaining GDP data
library(WDI)

library(plm)
library(sqldf)

# For manipulating data
library(magrittr)

library(rprojroot)
library(forcats)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
# For descriptive statistics and graphing
library(skimr)
library(ggplot2)
library(scales)
library(gridExtra)
library(forecast)
library(fpp2)
library(readxl)
library(stringr)
library(FitAR)
library(gtrendsR)
library(quantmod)
library(gtrends)

library(BatchGetSymbols)
library(tidyverse)
library(ggplot2)
library(Amelia)
library(reshape2)
library(ggthemes) 
library(plyr)
library(tseries)
library(plm)
library(dplyr)
library(tidyr)

library(urca)
library(forecast)

library(fPortfolio)

library(timeSeries)
write.csv(tickers, "tickers.csv")

## 1. Declarando o intervalo ####
bg<-'2009-12-01' #o per???odo inicial, geralmente ap???s os anos 2000
lst<-'2020-12-31'  #aqui coloquei a data de hoje, mas poderia ser qualquer outra

## 2. Colhendo dados de cotacoes ####
acoes<-GetIbovStocks()
acoes <- acoes$tickers

tickers <- paste(acoes$tickers,".SA", sep = "")


bench<-'^BVSP' 
cotacoes_bruto <- BatchGetSymbols(tickers = tickers,bench.ticker = bench,
                                  first.date = bg,last.date = lst, freq.data = "monthly") 
x <- monthlyReturn(as.time.series(cotacoes_bruto))

cotacoes <- cotacoes_bruto$df.tickers
cotacoes <- cotacoes[complete.cases(cotacoes),]
plot (dados)

dados <- as.timeSeries(cotacoes)
str(dados)
Spec = portfolioSpec()
setRiskFreeRate(Spec) <- 0.005261694


dados <- as.timeSeries(dados, start = c(2009, 12), freq = 12 )
Frontier = portfolioFrontier(dados, Spec  )
## Plotagem da fronteira eficiente no gráfico
frontierPlot(Frontier, pch = 19, xlim = c(0, 0.25), ylim = c(0, 0.025))
grid()
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
minvariancePoints(Frontier, pch = 19, col = "red")
equalWeightsPoints(Frontier, pch = 15, col = "Blue")
t6 <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")
tangencyLines(Frontier)
singleAssetPoints(bovespa_portifolio, pch = 19, cex = 1.5, col = ("green") )

singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = c("yellow") )



bovespa_dados <- as.timeSeries(bovespa)

bovespa_portifolio = portfolioFrontier(bovespa_dados, Spec  )
sharpeRatioLines(Frontier, col = "orange", lwd = 4)
p1 <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")

Frontier 
p1




Frontier = portfolioFrontier(dados, Spec)
## Plotagem da fronteira eficiente no gráfico
frontierPlot(Frontier, col = c("orange", "orange"), pch = 19)
## adicinando informações ao gráfico
## pontos associados a possíveis carteiras
monteCarloPoints(Frontier, mcSteps = 20000, cex = 0.25, pch = 19)
## mostrando o local da carteira que com proporções iguais em cada ativo
equalWeightsPoints(Frontier, pch = 15, col = "red")
## mostrando os pontos relativos a cada ativo individualmente
singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = "yellow")
## fronteiras calculadas com ativos 2 a 2
twoAssetsLines(Frontier, lwd = 2, col = "orange")













### enxugando e cruzando bases trends

ticker2 <- subset.data.frame(ticker, select = c(date, keyword, hits))
ticker2$hits <- str_replace_all(ticker2$hits, pattern = "<1", "0.5")
names(ticker2) <- c("date" , "keyword", "ticker")

noticias2 <- subset.data.frame(noticias, select = c(date, keyword, hits))
noticias2$hits <- str_replace_all(noticias2$hits, pattern = "<1", "0.5")
names(noticias2) <- c("date" , "keyword", "noticias")

ticker_br2 <- subset.data.frame(ticker_br, select = c(date, keyword, hits))
ticker_br2$hits <- str_replace_all(ticker_br2$hits, pattern = "<1", "0.5")
names(ticker_br2) <- c("date" , "keyword", "ticker_br")


noticias_br2 <- subset.data.frame(noticias_br, select = c(date, keyword, hits))
noticias_br2$hits <- str_replace_all(noticias_br2$hits, pattern = "<1", "0.5")
names(noticias_br2) <- c("date" , "keyword", "noticias_br")

## 3.2 an???lise de correla??????o entre as s???ries ####

btrends <- merge(ticker2, ticker_br2, by = c("keyword", "date") )

btrends2 <- merge(noticias2, noticias_br2, by = c("keyword", "date") )

base_trends <- merge(btrends, btrends2, by = c("keyword", "date"))

base_trends$ticker <- as.numeric( base_trends$ticker)
base_trends$ticker_br <- as.numeric( base_trends$ticker_br)
base_trends$noticias <- as.numeric( base_trends$noticias)
base_trends$noticias_br <- as.numeric( base_trends$noticias_br)



lista_trends <- split.data.frame(base_trends, base_trends$keyword)

# teste de estacionariedade
length(lista_trends)
adf.test(lista_trends[1])

adf.test(ts(as.data.frame(lista_trends[1])[3], start = c(2014, 1), frequency = 12))

lapply(lista_trends, function(x) x%>% select())

for (i in 1:length(lista_trends)) {
  adf.test(lista_trends[1]) 
}

## 4. cruzando bases ####

### criando base mensal de volume e preco

btrends <- merge(ticker, ticker_br, by = c("keyword", "date") )

btrends2 <- merge(noticias, noticias_br, by = c("keyword", "date") )

base_trends <- merge(btrends, btrends2, by = c("keyword", "date"))

cotacoes_volume <- subset(cotacoes, select = c(ref.date, ticker, volume, price.close))
names(cotacoes_volume) <- c("data", "ticker", "volume", "preco" )

cotacoes_volume$mes_ano <- floor_date(cotacoes_volume$data, "month")

a <- monthlyReturn(cotacoes)

cotacoes2 <- cotacoes
cotacoes_volume2 <- cotacoes_volume
cotacoes_mensal <- cotacoes_volume %>% 
  group_by(mes_ano, ticker) %>%
  dplyr::summarise(std_preco = sd(preco),
                   std_volume = sd(volume), preco = mean(preco), volume = mean(volume) )

cotacoes_mensal$ticker <- str_remove_all(cotacoes_mensal$ticker, ".SA")
cotacoes_mensal <- as.data.frame(cotacoes_mensal)
base_trends2 <- base_trends 
base_trends2 <- as.Date(base_trends2$date) 

str(base_trends)
str(cotacoes_mensal)

base_analise <- merge( x = cotacoes_mensal, y = base_trends, by.x = c("mes_ano", "ticker"), 
                       by.y = c("date", "keyword"), copy = FALSE)



lista_analise <- split.data.frame(base_analise, base_analise$ticker)




a <- as.data.frame(lista_analise[15])
names(a) <- names(base_analise)

a <- a %>%
  select(volume, ticker.y,    ticker_br,   noticias,    noticias_br,)

a <- as.data.frame(a)


# Obtendo valores criticos para teste de cointegracao

johansen <- ca.jo(a, type = "trace", ecdet = "none" ,spec = "longrun", K = 2)
summary(johansen)
a <- johansen@cval

vcritico <- a[,2]

as.numeric(johansen@teststat)


# An???lise de estacionaridade

var_adf <-  ts(a$volume)

plot(a$volume)
adf.test(var_adf)

a <- as.data.frame(lista_analise[i])
names(a) <- names(base_analise)

a <- a %>%
  select(volume, ticker.y,    ticker_br,   noticias,    noticias_br, )

a <- as.data.frame(a)

var_adf <-  ts(a$volume)
var_adf_res <- adf.test(var_adf)

df_lista <- list()
df_lista <- matrix(nrow = 64, ncol = 10)

capture.output( for (i in 1:length(lista_analise)) {
  print(i)
  
  a <- as.data.frame(lista_analise[i])
  names(a) <- names(base_analise)
  
  
  
  a <- as.data.frame(a)
  df_lista[i,1] <- names(lista_analise[i])
  for (j in 3:10) {
    print(names(lista_analise[i]))
    print(names(a[j]))
    var_adf <-  ts(a[j])
    var_adf_res <- adf.test(var_adf)
    
    print(var_adf_res)
    
    df_lista[i, (j)] = var_adf_res$p.value
  }
}, file = "adf1.txt")
df_lista <- as.data.frame(df_lista)


names(base_analise)

print(df_lista[1]) 


# Teste de Johansen 


for (i in 1:length(lista_analise)) {
  print(i)
  
  a <- as.data.frame(lista_analise[i])
  names(a) <- names(base_analise)
  
  a <- a %>%
    select(volume, ticker.y,    ticker_br,   noticias,    noticias_br, )
  
  a <- as.data.frame(a)
  
  auto.arima(a)  
  # Obtendo valores criticos para teste de cointegracao
  
  johansen <-
    tryCatch(
      ca.jo(
        a,
        type = "trace",
        ecdet = "none" ,
        spec = "longrun",
        K = 2
      ),
      warning = function(w)
        w
    )
  if (inherits(johansen, "warning")) {
    result <- c(names(lista_analise[i]), 0 , 0 , 0, 0, 0)
    
    if (i > 1) {
      teste_johansen <- rbind (teste_johansen, result)
    }
    else{
      teste_johansen <- result
    }
  }
  
  
  else{
    result <- c(names(lista_analise[i]), as.numeric(johansen@teststat))
    
    if (i > 1) {
      teste_johansen <- rbind (teste_johansen, result)
    }
    else{
      teste_johansen <- result
    }
    
  }
}

# write.csv2(teste_johansen, file = "teste_johansen")
# 
# vcritico <- johansen@cval[,2]
# vcritico <- johansen@teststat
# summary(johansen)
wb <- createWorkbook()
# 
addWorksheet(wb, "Forecasts")
writeData(wb, "Forecasts", cotacoes, rowNames = TRUE)
saveWorkbook(wb, "base_cotacoes.xlsx", overwrite = TRUE)



a <- as.data.frame(lista_analise[45])
names(a) <- names(base_analise)

a <- as.data.frame(a)

ts_volume <- ts(a$volume, start = min(a$mes_ano), frequency = 12)

modelo_un <- auto.arima(ts_volume)
res_modelo_un <- as.data.frame(modelo_un$residuals)

df_vec <- cbind(a, res_modelo_un)

df_vec <- df_vec %>%
  select( volume ,  ticker , noticias)

df_vec <- as.data.frame(df_vec)
m_vec <- VAR(df_vec)

summary(vec)


m_vec_prev <- predict(vec, n.ahead  = 6)

plot(m_vec_prev)

# Obtain IRF
ir <- irf(m_vec, n.ahead = 12, impulse = "R", response = "Dp",
          ortho = FALSE, runs = 500)

# Plot
plot(ir)



summary(m_vec)
plot(a$volume)
ndiffs(a$volume)

modelo <- VAR(a)
est <- summary(modelo)
est
result <- list()

modelos <- list()


if (stats_var[1, 4] < 0.05) u <- "ticker.y"




for (i in 1:length(lista_analise)) {
  print(i)
  
  a <- as.data.frame(lista_analise[i])
  names(a) <- names(base_analise)
  
  a <- a %>%
    select(volume, ticker.y,    ticker_br,   noticias,    noticias_br)
  
  
  a <- as.data.frame(a)
  
  
  # Obtendo valores criticos para teste de cointegracao
  
  tvar <-
    tryCatch(
      VAR(a),
      warning = function(w)
        w
    )
  if (inherits(tvar, "warning")) {
    modelos[i] <- 0
    
  }
  
  
  
  
  else{
    #modelo incial
    est <- summary(tvar)
    modelos[i] <- tvar
    if (i > 1) {
      stats_var <-
        rbind(
          stats_var,
          c(
            names(lista_analise[i]),
            est$varresult$volume$r.squared,
            est$varresult$volume$adj.r.squared,
            est$varresult$volume$coefficients[, 4]
          )
        )
    }
    else{
      stats_var <-
        c(
          names(lista_analise[i]),
          est$varresult$volume$r.squared,
          est$varresult$volume$adj.r.squared,
          est$varresult$volume$coefficients[, 4]
        )
      
    }
    
  }
}

exp_stats_var <- cbind(stats_var[,1], as.data.frame(cbind(as.numeric(stats_var[,2]),
                                                          as.numeric(stats_var[,3]),
                                                          as.numeric(stats_var[,4]),
                                                          as.numeric(stats_var[,5]),
                                                          as.numeric(stats_var[,6]),
                                                          as.numeric(stats_var[,7]),
                                                          as.numeric(stats_var[,8]),
                                                          as.numeric(stats_var[,9]))))

wb <- createWorkbook()
# 
addWorksheet(wb, "Forecasts")
writeData(wb, "Forecasts", stats_var, rowNames = TRUE, sep = ",")
saveWorkbook(wb, "stats_var.xlsx", overwrite = TRUE)


names(result) = names(lista_analise)

stats_var2 <- list()
j = 0
for (i in 1:length(lista_analise)) {
  print(i)
  
  a <- as.data.frame(lista_analise[i])
  names(a) <- names(base_analise)
  a <- a %>%
    select(volume, ticker.y,    ticker_br,   noticias,    noticias_br)
  a <- as.data.frame(a)
  
  
  b <- a %>%
    select(volume)
  if (stats_var[1, 4] < 0.10) {
    ticker.y = a$ticker.y
    b <- cbind(b, ticker.y)
  }
  if (stats_var[i, 5] < 0.10) {
    ticker_br = a$ticker_br
    b <- cbind(b, ticker_br)
  }
  if (stats_var[i, 6] < 0.10) {
    noticias = a$noticias
    b <- cbind(b, noticias)
  }
  
  if (stats_var[i, 7] < 0.10) {
    noticias_br = a$noticias_br
    b <- cbind(b, noticias_br)
  }
  if (ncol(b) > 1 ){
    tvar <-
      tryCatch(
        VAR(b),
        warning = function(w)
          w
      )
    if (inherits(tvar, "warning")) {
      
    }
    
    else{
      est <- summary(tvar)
      
      
      stats_var2[i-j] <-
        as.data.frame(c(
          names(lista_analise[i]),
          est$varresult$volume$r.squared,
          est$varresult$volume$adj.r.squared,
          est$varresult$volume$coefficients[, 4]
        ))
      
      
      
    }
  }
  else{j = j+1}
}


x <- as.data.frame(matrix(nrow = 1, ncol = 8))  
for (i in 1:length(stats_var2)) {
  u <- stats_var2[[i]]
  for (j in 1:length(u)) {
    x[i,j] <- u[j]  
  }
  
}



wb <- createWorkbook()
# 
addWorksheet(wb, "Forecasts")
writeData(wb, "Forecasts", x, rowNames = TRUE)
saveWorkbook(wb, "stats_var2.xlsx", overwrite = TRUE)

