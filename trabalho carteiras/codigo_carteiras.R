library(quantmod)
library(ggplot2)
library(fPortfolio)


setwd("C:/Users/augusto.bastos/Desktop/UFMG/fin 1/trabalho carteiras/")



bovespa <- monthlyReturn(as.timeSeries(  getSymbols(c("BOVA11.SA"),
                                                    src = "yahoo", 
                                                    from = "2009-01-01",
                                                    to = "2020-12-31",
                                                    auto.assign = TRUE), optional = true))

bovespa <-cbind(bovespa, monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP", "CMIG4.SA",
                                                      "ABEV3.SA"),
                                                    src = "yahoo", 
                                                    from = "2009-01-01",
                                                    to = "2020-12-31",
                                                    auto.assign = TRUE), optional = true)))
bovespa <- setNames(bovespa, c("BVSP", "CMIG4.SA"))



acoes = c("CMIG4.SA",
          "ABEV3.SA",
          "PETR4.SA",
          "USIM5.SA",
          "BBAS3.SA",
          "VALE3.SA",
          "ITUB4.SA",
          "CSMG3.SA",
          "SBSP3.SA",
          "LREN3.SA",
        "CSNA3",
          "BRFS3.SA",
          "OIBR3.SA",
          "B3SA3.SA",
          "BRKM5.SA",
          "RADL3.SA",
          "VLID3.SA",
          "JBSS3.SA",
          "GGBR4.SA",
          "EMBR3.SA",
          "WEGE3.SA",
          "ELET3.SA"
          )


acoes_6ativos = c("CMIG4.SA",
                  "ABEV3.SA",
                  "PETR4.SA",
                  "USIM5.SA",
                  "BBAS3.SA",
                  "VALE3.SA")

base_6ativos <- monthlyReturn(as.timeSeries(  getSymbols(acoes[1],
                                                              src = "yahoo", 
                                                              from = "2009-01-01",
                                                              to = "2020-12-31",
                                    auto.assign = TRUE), optional = true))

for (i in 2:6) {
  
base_6ativos <-  cbind(base_6ativos,  monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                src = "yahoo", 
                from = "2009-01-01",
                to = "2020-12-31",
                auto.assign = TRUE), optional = TRUE)))
}


base_6ativos <- setNames(base_6ativos, acoes_6ativos)


acoes_14ativos = c(
                   "ABEV3.SA",
                   "PETR4.SA",
                   "USIM5.SA",
                   "BBAS3.SA",
                   "VALE3.SA",
                   "ITUB4.SA",
                   "CSMG3.SA",
                   "SBSP3.SA",
                   "LREN3.SA",
                  
                   "BRFS3.SA",
                   "OIBR3.SA",
                   "B3SA3.SA")
rm(base_14ativos)
base_14ativos <- na.fill( monthlyReturn(as.timeSeries(
  getSymbols(
    acoes[2],
    src = "yahoo",
    from = "2009-01-01",
    to = "2020-12-31",
    auto.assign = TRUE
  ),
  optional = true
) 
), fill = 0
)
  

for (i in 2:12) {
  
  base_14ativos <-  cbind(base_14ativos,   monthlyReturn(as.timeSeries( na.fill( getSymbols(acoes[i],
                                                                         src = "yahoo", 
                                                                         from = "2009-01-01",
                                                                         to = "2020-12-31",
                                                                         auto.assign = TRUE),  fill = 0)
                                                                         ), optional = TRUE))
}


base_14ativos <- setNames(base_14ativos, acoes_14ativos)



base_nativos <- monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                                                         src = "yahoo", 
                                                         from = "2009-01-01",
                                                         to = "2020-12-31",
                                                         auto.assign = TRUE), optional = TRUE))

for (i in 2:23) {
  
  base_nativos <-  cbind(base_nativos,  monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                                                                                 src = "yahoo", 
                                                                                 from = "2009-01-01",
                                                                                 to = "2020-12-31",
                                                                                 auto.assign = TRUE), optional = true)))
}


base_nativos <- setNames(base_nativos, acoes)
write.csv(base_nativos, file = "nativos.csv")

require(fPortfolio)
library(fPortfolio)

require(timeSeries)
library(timeSeries)
dados <- as.timeSeries(base_6ativos)
str(dados)
Spec = portfolioSpec()
setRiskFreeRate(Spec) <- 0.001651581

#### 6 ativos #####

dados <- as.timeSeries(base_6ativos)



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

Frontier@portfolio@portfolio
Frontier@spec
Frontier@constraints
Frontier@portfolio@portfolio$targetReturn

###### 14 ativos ########

dados <- as.timeSeries(nativos[2:14], start = '2009-01-01', end = '2020-12-31')
str(dados)
Spec = portfolioSpec()
setRiskFreeRate(Spec) <- 0.001651581

write.csv(dados, file = "14a.csv")
dados <- as.timeSeries(dados  )
dados <- na.fill(dados, fill = 0)
base_14ativos <- setNames(base_14ativos, acoes_14ativos)
base_14ativos <- na.omit(base_14ativos)
dados <- as.timeSeries(base_14ativos)
Frontier = portfolioFrontier(base_14ativos , Spec, constraints = "LongOnly" )
Frontier = frontierPortfolio(base_14ativos , Spec)

## Plotagem da fronteira eficiente no gráfico
frontierPlot(Frontier, pch = 19, xlim = c(0, 0.25), ylim = c(0, 0.025))
grid()
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
minvariancePoints(Frontier, pch = 19, col = "red")
equalWeightsPoints(Frontier, pch = 15, col = "Blue")
singleAssetPoints(bovespa_portifolio, pch = 19, cex = 1.5, col = ("green") )

singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = "yellow" )
t14 <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")
tangencyLines(Frontier)
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

###### nativos #########

dados <- as.timeSeries(base_nativos)
Frontier = portfolioFrontier(dados, Spec  )
## Plotagem da fronteira eficiente no gráfico
frontierPlot(Frontier, pch = 19, xlim = c(0, 0.25), ylim = c(0, 0.055))
grid()
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
minvariancePoints(Frontier, pch = 19, col = "red")
equalWeightsPoints(Frontier, pch = 19, col = "Blue")
singleAssetPoints(bovespa_portifolio, pch = 19, cex = 1.5, col = ("green") )

singleAssetPoints(Frontier, pch = 19, cex = 1.5, col = "yellow" )
tn <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")
tangencyLines(Frontier)
sharpeRatioLines(Frontier, col = "orange", lwd = 4)
p1 <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")

Frontier 
p1

Frontier = portfolioFrontier(dados, Spec)
##   Plotagem da fronteira eficiente no gráfico
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





dados <- read_excel("Retornos.xlsx")

dados$Data <- as.Date(dados$Data)

dados[is.na(dados)] = 0

## Media dos Retornos
er <- colMeans(dados[,2:ncol(dados)])

er <- as.matrix(er)

## Matriz de cov
cov.mat <- cov(dados[,2:ncol(dados)])
inv.cov.mat <- inv(cov.mat)
colnames(inv.cov.mat) <- colnames(cov.mat)
rownames(inv.cov.mat) <- rownames(cov.mat)
vetor_1s <- matrix(rep(1, ncol(inv.cov.mat)))


r.free = 0.0039


#### Min Variancia
weights_mv <- inv.cov.mat %*% vetor_1s
weights_mv <- weights_mv/sum(weights_mv)
er_mv <- sum(weights_mv * er)
sd_mv <- sqrt(t(weights_mv) %% (cov.mat %% (weights_mv)))
weights_mv <- t(weights_mv)

#### Portfolio T 
weights_t <- inv.cov.mat %*% (er - r.free)
weights_t <- weights_t/sum(weights_t)
er_t <- sum(weights_t * er)
sd_t <- sq.
