#parteb


library(knitr)
setwd("C:/Users/Augusto/Desktop/finanças 1/2ª vez/Trabalhos/trabalho carteiras b")
spin('codigotb.R', precious = FALSE, doc = '#')

#dados passados

library(quantmod)
library(ggplot2)
library(fPortfolio)



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
          "CSNA3.SA",
          "BRFS3.SA",
          "VIVT4.SA",
          "HGTX3.SA",
          "BRKM5.SA",
          "RADL3.SA",
          "VLID3.SA",
          "JBSS3.SA",
          "GGBR4.SA",
          "EMBR3.SA",
          "WEGE3.SA",
          "ELET3.SA"
)



bovespa <- monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP"),
                                                    src = "yahoo", 
                                                    from = "2009-01-01",
                                                    to = "2018-06-01",
                                                    auto.assign = TRUE), optional = true))

bovespa <-cbind(bovespa, monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP", "CMIG4.SA",
                                                                    "ABEV3.SA"),
                                                                  src = "yahoo", 
                                                                  from = "2009-01-01",
                                                                  to = "2018-06-01",
                                                                  auto.assign = TRUE), optional = true)))
bovespa <- setNames(bovespa, c("BVSP", "CMIG4.SA"))




base_nativos <- monthlyReturn(as.timeSeries(  getSymbols(acoes[1],
                                                         src = "yahoo", 
                                                         from = "2009-01-01",
                                                         to = "2018-06-01",
                                                         auto.assign = TRUE), optional = true))

for (i in 2:23) {
  
  base_nativos <-  cbind(base_nativos,  monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                                                                                 src = "yahoo", 
                                                                                 from = "2009-01-01",
                                                                                 to = "2018-06-01",
                                                                                 auto.assign = TRUE), optional = true)))
}


base_nativos <- setNames(base_nativos, acoes)




#dados futuros




bovespa_nova <- monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP"),
                                                    src = "yahoo", 
                                                    from = "2018-06-01",
                                                    to = "2019-09-01",
                                                    auto.assign = TRUE), optional = true))

bovespa_nova <-cbind(bovespa_nova, monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP", "CMIG4.SA",
                                                                    "ABEV3.SA"),
                                                                  src = "yahoo", 
                                                                  from = "2018-06-01",
                                                                  to = "2019-09-01",
                                                                  auto.assign = TRUE), optional = true)))
bovespa_nova <- setNames(bovespa_nova, c("BVSP", "CMIG4.SA"))





base_nova_nativos <- monthlyReturn(as.timeSeries(  getSymbols(acoes[1],
                                                              src = "yahoo", 
                                                              from = "2018-06-01",
                                                              to = "2019-09-01",
                                                              auto.assign = TRUE), optional = true))

for (i in 2:23) {
  
  base_nova_nativos <-  cbind(base_nova_nativos,  monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                                                                                           src = "yahoo", 
                                                                                           from = "2018-06-01",
                                                                                           to = "2019-09-01",
                                                                                           auto.assign = TRUE), optional = true)))
}


base_nova_nativos <- setNames(base_nova_nativos, acoes)

#portifolio tangente dados passados

require(fPortfolio)
library(fPortfolio)

require(timeSeries)
library(timeSeries)

bovespa_toda <- monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP"),
                                                         src = "yahoo", 
                                                         from = "2018-06-01",
                                                         to = "2019-09-01",
                                                         auto.assign = TRUE), optional = true))

bovespa_toda <-monthlyReturn(as.timeSeries(  getSymbols(c("^BVSP"
),
src = "yahoo", 
from = "2018-06-01",
to = "2019-09-01",
auto.assign = TRUE), optional = true))
bovespa_toda <- bovespa_toda


bovespa_ts <- as.timeSeries(bovespa_toda)



dados <- as.timeSeries(base_nativos)

Spec = portfolioSpec()
setRiskFreeRate(Spec) <- 0.005261694


tn <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")
peso_tn <- tn@portfolio@portfolio$weights

min_risk_p <- minriskPortfolio(dados, Spec)
peso_min_risk <- min_risk_p@portfolio@portfolio$weights

maxr_p <- maxratioPortfolio(dados, Spec)
peso_maxr <- maxr_p@portfolio@portfolio$weights


##Pesos portifolio Tangente

##specficacoes do modelo

dados <- as.timeSeries(base_nova_nativos)

setRiskFreeRate(Spec2) <- 0.005261694
rf <- (1+0.005261694)^(15/12)-1

setWeights(Spec2) <- peso_tn


retorno_tn <- Return.portfolio(dados, weights = peso_tn, value = 1000, verbose = TRUE)

retorno_tn[["returns"]]

summary(retorno_tn$returns )

##VaR

varRisk(dados, peso_tn, alpha = 0.05)

##Traynor

correlacao_tn <-cor(retorno_tn$returns, bovespa_ts)

(sum(retorno_tn$returns) - rf)/correlacao_tn

##Sharpe

(sum(retorno_tn$returns) - rf)/sd(retorno_tn$returns)

##Valor em acoes
retorno_tn[["BOP.Value"]]



##contribuicao 
retorno_tn[["contribution"]]


##Pesos risco minimo


##specficacoes do modelo

dados <- as.timeSeries(base_nova_nativos)

setRiskFreeRate(Spec2) <- 0.005261694
rf <- (1+0.005261694)^(15/12)-1

setWeights(Spec2) <- peso_min_risk


retorno_min_risk <- Return.portfolio(dados, weights = peso_min_risk, value = 1000, verbose = TRUE)

retorno_min_risk[["returns"]]

summary(retorno_min_risk$returns )

##VaR

varRisk(dados, peso_min_risk, alpha = 0.05)

##Traynor

correlacao_min_risk <-cor(retorno_min_risk$returns, bovespa_ts)

(sum(retorno_min_risk$returns) - rf)/correlacao_min_risk

##Sharpe

(sum(retorno_min_risk$returns) - rf)/sd(retorno_min_risk$returns)

##Valor em acoes
retorno_min_risk[["BOP.Value"]]



##contribuicao 
retorno_min_risk[["contribution"]]

##Pesos maximo indice risco retorno


##specficacoes do modelo

dados <- as.timeSeries(base_nova_nativos)

setRiskFreeRate(Spec2) <- 0.005261694
rf <- (1+0.005261694)^(15/12)-1

setWeights(Spec2) <- peso_maxr 


retorno_maxr  <- Return.portfolio(dados, weights = peso_maxr , value = 1000, verbose = TRUE)

retorno_maxr [["returns"]]

summary(retorno_maxr $returns )

##VaR

varRisk(dados, peso_maxr , alpha = 0.05)

##Traynor

correlacao_maxr  <-cor(retorno_maxr $returns, bovespa_ts)

(sum(retorno_maxr $returns) - rf)/correlacao_maxr 

##Sharpe

(sum(retorno_maxr $returns) - rf)/sd(retorno_maxr $returns)

##Valor em acoes
retorno_maxr [["BOP.Value"]]



##contribuicao 
retorno_maxr [["contribution"]]


acoes <- c(          "USIM5.SA",
                      "CSNA3.SA",
                      "BRKM5.SA",
                      "VLID3.SA",
                      "GGBR4.SA",
                      "EMBR3.SA",
                      "WEGE3.SA"
)




base_nativos <- monthlyReturn(as.timeSeries(  getSymbols(acoes[1],
                                                         src = "yahoo", 
                                                         from = "2009-01-01",
                                                         to = "2018-06-01",
                                                         auto.assign = TRUE), optional = true))

for (i in 2:7) {
  
  base_nativos <-  cbind(base_nativos,  monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                                                                                 src = "yahoo", 
                                                                                 from = "2009-01-01",
                                                                                 to = "2018-06-01",
                                                                                 auto.assign = TRUE), optional = true)))
}


base_nativos <- setNames(base_nativos, acoes)




#dados futuros





base_nova_nativos <- monthlyReturn(as.timeSeries(  getSymbols(acoes[1],
                                                              src = "yahoo", 
                                                              from = "2018-06-01",
                                                              to = "2019-09-01",
                                                              auto.assign = TRUE), optional = true))

for (i in 2:7) {
  
  base_nova_nativos <-  cbind(base_nova_nativos,  monthlyReturn(as.timeSeries(  getSymbols(acoes[i],
                                                                                           src = "yahoo", 
                                                                                           from = "2018-06-01",
                                                                                           to = "2019-09-01",
                                                                                           auto.assign = TRUE), optional = true)))
}


base_nova_nativos <- setNames(base_nova_nativos, acoes)


dados <- as.timeSeries(base_nativos)



dados <- as.timeSeries(base_nativos)

Spec = portfolioSpec()
setRiskFreeRate(Spec) <- 0.005261694


tn <- tangencyPortfolio(dados, Spec, constraints = "LongOnly")
peso_tn <- tn@portfolio@portfolio$weights

min_risk_p <- minriskPortfolio(dados, Spec)
peso_min_risk <- min_risk_p@portfolio@portfolio$weights

maxr_p <- maxratioPortfolio(dados, Spec)
peso_maxr <- maxr_p@portfolio@portfolio$weights

##Pesos portifolio Tangente

##specficacoes do modelo

dados <- as.timeSeries(base_nova_nativos)

setRiskFreeRate(Spec2) <- 0.005261694
rf <- (1+0.005261694)^(15/12)-1

setWeights(Spec2) <- peso_tn


retorno_tn <- Return.portfolio(dados, weights = peso_tn, value = 1000, verbose = TRUE)

retorno_tn[["returns"]]

summary(retorno_tn$returns )

##VaR

varRisk(dados, peso_tn, alpha = 0.05)

##Traynor

correlacao_tn <-cor(retorno_tn$returns, bovespa_ts)

(sum(retorno_tn$returns) - rf)/correlacao_tn

##Sharpe

(sum(retorno_tn$returns) - rf)/sd(retorno_tn$returns)

##Valor em acoes
retorno_tn[["BOP.Value"]]



##contribuicao 
retorno_tn[["contribution"]]


##Pesos risco minimo


##specficacoes do modelo

dados <- as.timeSeries(base_nova_nativos)

setRiskFreeRate(Spec2) <- 0.005261694
rf <- (1+0.005261694)^(15/12)-1

setWeights(Spec2) <- peso_min_risk


retorno_min_risk <- Return.portfolio(dados, weights = peso_min_risk, value = 1000, verbose = TRUE)

retorno_min_risk[["returns"]]

summary(retorno_min_risk$returns )

##VaR

varRisk(dados, peso_min_risk, alpha = 0.05)

##Traynor

correlacao_min_risk <-cor(retorno_min_risk$returns, bovespa_ts)

(sum(retorno_min_risk$returns) - rf)/correlacao_min_risk

##Sharpe

(sum(retorno_min_risk$returns) - rf)/sd(retorno_min_risk$returns)

##Valor em acoes
retorno_min_risk[["BOP.Value"]]



##contribuicao 
retorno_min_risk[["contribution"]]

##Pesos maximo indice risco retorno


##specficacoes do modelo

dados <- as.timeSeries(base_nova_nativos)

setRiskFreeRate(Spec2) <- 0.005261694
rf <- (1+0.005261694)^(15/12)-1

setWeights(Spec2) <- peso_maxr 


retorno_maxr  <- Return.portfolio(dados, weights = peso_maxr , value = 1000, verbose = TRUE)

retorno_maxr [["returns"]]

summary(retorno_maxr $returns )

##VaR

varRisk(dados, peso_maxr , alpha = 0.05)

##Traynor

correlacao_maxr  <-cor(retorno_maxr $returns, bovespa_ts)

(sum(retorno_maxr$returns) - rf)/correlacao_maxr 

##Sharpe

(sum(retorno_maxr$returns) - rf)/sd(retorno_maxr $returns)

##Valor em acoes
retorno_maxr [["BOP.Value"]]



##contribuicao 
retorno_maxr [["contribution"]]
