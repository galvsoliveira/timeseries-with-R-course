# install.packages("renv")
# renv::init()

renv::restore(clean = TRUE)

# Lista de pacotes necessários
packages <- c("BatchGetSymbols", "rugarch", "tidyverse", "ggthemes", "ggplot2", "ragg", "cli", "curl", "cowplot")

# Instalar pacotes que ainda não estão instalados
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(cowplot)
library(BatchGetSymbols) ### pegar dados yahoo finance (pre�os di�rias ativos negociados em bolsa)
library(rugarch)
library(tidyverse)
library(ggthemes) 

####limpar o diret�rio
rm(list=ls())

##### pegar os tickers das empresas que voc� deseja - identificador das a��es
#### ticker - identificador local
#### 4 primeiras letras: empresa, n�meros finais: tipo de a��o (ordin�ria, preferencial)
#### site da B3 tem as informa��es: empresas listadas. Basta digiar o nome da empresa e aparecer� o ticker. 
##### http://www.b3.com.br/pt_br/produtos-e-servicos/negociacao/renda-variavel/empresas-listadas.htm
##### ou no site do yahoo Finance

# para pegar v�rios, podemos usar o argumento abaixo:
tickers = c('EQTL3.SA', 'PETR4.SA', 'VALE3.SA', 'WEGE3.SA','EMBR3.SA',
            'CSNA3.SA', 'USIM5.SA','TOTS3.SA','ABEV3.SA','LREN3.SA','CIEL3.SA',
            'RADL3.SA', 'RENT3.SA', 'MDIA3.SA','EZTC3.SA', 'FLRY3.SA','OIBR3.SA','CVCB3.SA')


#### Sys.time() � a �ltima data dispon�vel

assets <- BatchGetSymbols(tickers,
                          first.date = '2014-01-01',
                          last.date = Sys.time(),
                          type.return = "log",
                          freq.data = "daily")

#selecionando df.tickers
assets <- assets[[2]]

#### "Olhando" os dados
glimpse(assets)

## Vamos selecionar apenas a PETR4.SA
petro = assets%>% filter(ticker=='PETR4.SA')

### E fazer alguns gr�ficos

#Gr�fico 1: pre�os

p<-ggplot(petro,aes(ref.date,price.close))+geom_line(color='#006600')+
  labs(x = "",y='Preço Fechamento',title="Cotação Diária da PETR4",
         subtitle = "Período: de 02/01/2014 a 10/09/2021",
         caption = "Fonte: B3")+
  theme_economist()

plot(p)

#Gr�fico 2: retornos di�rios
daily_returns <- petro %>% 
  select(ref.date,ticker,ret.closing.prices) 

plot.returns <- ggplot(daily_returns) +
  geom_line(aes(x = ref.date, y = ret.closing.prices), color='#006600') +
  labs( x = "" , y = 'Retornos', title="Retornos da PETR4",
        subtitle = "Per�odo: de 02/01/2014 a 10/09/2021", 
        caption = "Fonte: B3")+
    theme_economist()

plot.returns

#Gr�fico 3: Retornos absolutos 
plot.volatility <- ggplot(daily_returns) +
  geom_line(aes(x = ref.date, y = abs(ret.closing.prices)), color='#006600') +
  labs( x = "" , y = 'Retornos absolutos', title="Retornos abs da PETR4",
        subtitle = "Per�odo: de 02/01/2014 a 10/09/2021", 
        caption = "Fonte: B3")+
  theme_economist()

plot.volatility

#Gr�fico 4: qqplot
qqplot <- ggplot(daily_returns, aes(sample = ret.closing.prices)) + 
  stat_qq() + 
  stat_qq_line() +
    labs( x = "Te�rico" , y = 'Amostra', title="QQplot",
        subtitle = "Retornos di�rios da PETR4.SA", 
        caption = "Fonte: Elaborado a partir de dados da B3")+
  theme_economist()

# plotar qqplot
qqplot

#gr�fico 5: Histograma

histogram <- ggplot(daily_returns) +
  geom_histogram(aes(x=ret.closing.prices,y = ..density..),
                 color="white", fill="Dark grey",linetype="solid",alpha = 0.8) +
  geom_density(aes(x = ret.closing.prices,y = ..density..),color="black") + 
  labs( x = "" , y = 'Densidade', title="Histograma",
        subtitle = "Retornos di�rios da PETR4.SA", 
        caption = "Fonte: Elaborado a partir de dados da B3")+
  theme_economist()

histogram

##colando os gr�ficos lado a lado 
cowplot::plot_grid(plot.returns,qqplot,plot.volatility,histogram, nrow = 2)

# Get a list of all installed packages
installed <- installed.packages()

# Extract the package names and dependencies
pkgs <- installed[, c("Package", "Depends", "Imports", "LinkingTo")]

# Write the package information to a file
write.csv(pkgs, file = "R_dependencies.csv", row.names = FALSE)