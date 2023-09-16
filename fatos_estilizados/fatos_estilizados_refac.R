# install.packages("renv")
# renv::init()

renv::restore(clean = TRUE)

# Lista de pacotes necessários
packages <- c("BatchGetSymbols", "rugarch", "tidyverse", "ggthemes", "ggplot2", "ragg", "cli", "curl", "cowplot")

# Instalar pacotes que ainda não estão instalados
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Carregar pacotes
library(cowplot)
library(BatchGetSymbols)
library(rugarch)
library(tidyverse)
library(ggthemes)

# Limpar o diretório
rm(list = ls())

# Função para obter dados do Yahoo Finance
get_data <- function(tickers) {
  assets <- BatchGetSymbols(tickers,
                            first.date = '2014-01-01',
                            last.date = Sys.time(),
                            type.return = "log",
                            freq.data = "daily")
  assets[[2]]
}

# Função para plotar gráfico de preços
plot_prices <- function(data, iticker) {
  data %>%
    filter(ticker == iticker) %>%
    ggplot(aes(ref.date, price.close)) +
    geom_line(color = '#006600') +
    labs(x = "", y = 'Preço Fechamento', title = paste0("Cotação Diária da ", iticker),
         subtitle = "Período: de 02/01/2014 a 10/09/2021",
         caption = "Fonte: B3") +
    theme_economist()
}

# Função para plotar gráfico de retornos diários
plot_returns <- function(data, iticker) {
  data %>%
    filter(ticker == iticker) %>%
    ggplot(aes(ref.date, ret.closing.prices)) +
    geom_line(color = '#006600') +
    labs(x = "", y = 'Retornos', title = paste0("Retornos da ", iticker),
         subtitle = "Período: de 02/01/2014 a 10/09/2021",
         caption = "Fonte: B3") +
    theme_economist()
}

# Função para plotar gráfico de retornos absolutos
plot_volatility <- function(data, iticker) {
  data %>%
    filter(ticker == iticker) %>%
    ggplot(aes(ref.date, abs(ret.closing.prices))) +
    geom_line(color = '#006600') +
    labs(x = "", y = 'Retornos absolutos', title = paste0("Retornos abs da ", iticker),
         subtitle = "Período: de 02/01/2014 a 10/09/2021",
         caption = "Fonte: B3") +
    theme_economist()
}

# Função para plotar qqplot
plot_qqplot <- function(data, iticker) {
  data %>%
    filter(ticker == iticker) %>%
    ggplot(aes(sample = ret.closing.prices)) +
    stat_qq() +
    stat_qq_line() +
      labs(x = "Teórico" , y = 'Amostra', title = "QQplot",
          subtitle = paste0("Retornos diários da ", iticker),
          caption = "Fonte: Elaborado a partir de dados da B3") +
    theme_economist()
}

# Função para plotar histograma
plot_histogram <- function(data, iticker) {
  data %>%
    filter(ticker == iticker) %>%
    ggplot(aes(x=ret.closing.prices,y=..density..)) +
      geom_histogram(color="white", fill="Dark grey",linetype="solid",alpha=0.8) +
      geom_density(aes(x=ret.closing.prices,y=..density..),color="black") +
      labs(x="", y='Densidade', title="Histograma",
           subtitle=paste0("Retornos diários da ",iticker),
           caption="Fonte: Elaborado a partir de dados da B3")+
      theme_economist()
}

# Definir tickers das empresas desejadas
tickers <- c('EQTL3.SA', 'PETR4.SA', 'VALE3.SA', 'WEGE3.SA','EMBR3.SA',
             'CSNA3.SA', 'USIM5.SA','TOTS3.SA','ABEV3.SA','LREN3.SA','CIEL3.SA',
             'RADL3.SA', 'RENT3.SA', 'MDIA3.SA','EZTC3.SA', 'FLRY3.SA','OIBR3.SA','CVCB3.SA')

# Obter dados
assets <- get_data(tickers)
colnames(assets)

# Plotar gráficos para a PETR4.SA
plot_prices(assets, 'PETR4.SA')
plot_returns(assets, 'PETR4.SA')
plot_volatility(assets, 'PETR4.SA')
plot_qqplot(assets, 'PETR4.SA')
plot_histogram(assets, 'PETR4.SA')

# plotando os graficos lado a lado
plot_grid(#plot_prices(assets, 'PETR4.SA'),
          plot_returns(assets, 'PETR4.SA'),
          plot_volatility(assets, 'PETR4.SA'),
          plot_qqplot(assets, 'PETR4.SA'),
          plot_histogram(assets, 'PETR4.SA'),
          ncol = 2, nrow = 3)
