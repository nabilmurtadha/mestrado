# pacotes
library(tidymodels)
library(glmnet)
library(tidyverse)
library(rbcb)
library(quantmod)
library(bestNormalize)
library(gghighlight)
library(corrplot)
library(tseries)
library(dynamac)
library(dynamac)
library(tseries)
library(urca)
library(lmtest)
library(readxl)
library(dplyr)
library(ARDL)

# Selecionando as variáveis -----------------------------------------------

# inad cartão de crédito
inad_cartao <- get_series(list("inad_cartao" = 21129))

# inad Mercado de crédit
inad_mercado <- get_series(list("inad_mercado" = 21084))

# PIB
pib_mensal <- get_series(list("pib_mensal" = 4380))

# PIB acumulado mensal


# Taxa de Juros
selic_mensal <- get_series(list("selic_mensal" = 4390))

# Inflação
ipca <- openxlsx::read.xlsx("C:/Users/nabil/Documents/Mestrado/Catolica/0. Dissertação/1. Base de dados/ipca_mensal.xlsx", sheet = 1) %>%
  mutate(data = as.Date(data, origin = "1899-12-30"))

# Concessão de crédito
concessoes_credito <- get_series(list("concessoes_credito" = 20662)) 

# Volume de crédito
saldo_credito <- get_series(list("saldo_credito" = 20570))

# Taxa de cambio 
cambio_dolar <- get_series(list("cambio_dolar" = 3697))

# Taxa de desemprego
PME <- read.csv2("C:/Users/nabil/Documents/Mestrado/Catolica/0. Dissertação/1. Base de dados/taxa_desemprego_pme.csv")
PME <- PME %>% mutate(data = as.Date(data, format = "%d/%m/%Y"))

PNADC <- read.csv2("C:/Users/nabil/Documents/Mestrado/Catolica/0. Dissertação/1. Base de dados/taxa_desemprego_pnad.csv")
PNADC <- PNADC %>% mutate(data = as.Date(data, format = "%d/%m/%Y"))
taxa_desemprego <- rbind(PME %>% filter(data < min(PNADC$data)),PNADC)
rm(PME)
rm(PNADC)

# Crise

#2007
#2008
#2015
#2016
#2020

crise_fin <- data.frame(date = pib_mensal$date,
                        crise_fin = ifelse(floor_date(pib_mensal$date,"year") %in%
                                             c("2007-01-01","2008-01-01","2015-01-01","2016-01-01","2020-01-01"),1,0))

# Saldo cartão de crédito
saldo_ccredito <- get_series(list("saldo_ccredito" = 20590)) 


# Concessão de crédtio - Cartão de crédito
concessoes_ccredito <- get_series(list("concessoes_ccredito" = 20682))  

# Taxa média de juros
22024

# SELIC
11

# Juntando as bases -------------------------------------------------------


base_ardl <- inad_cartao |> 
  left_join(inad_mercado) |> 
  left_join(pib_mensal) |> 
  left_join(selic_mensal) |> 
  left_join(ipca, by = c("date" = "data")) |> 
  left_join(concessoes_credito) |> 
  left_join(saldo_credito) |> 
  left_join(cambio_dolar) |> 
  left_join(taxa_desemprego, by = c("date" = "data")) |> 
  left_join(crise_fin) |> 
  left_join(saldo_ccredito) |> 
  left_join(concessoes_ccredito) |> 
  mutate(prop_sld_credito_pib = saldo_credito/pib_mensal,
         prop_sld_ccredito_pib = saldo_ccredito/pib_mensal) |> 
  drop_na()
  
  

# Teste de raiz unitária --------------------------------------------------


# Função para p-valor ADF
adf_pval <- function(x) {
  tryCatch({
    teste <- adf.test(x)
    teste$p.value
  }, error = function(e) NA)
}

# Função para p-valor KPSS
kpss_pval <- function(x) {
  tryCatch({
    test <- kpss.test(x, null = "Level")
    test$p.value
  }, error = function(e) NA)
}

# Função que aplica os testes em nível e na primeira diferença
resultado <- map_dfr(names(base_ardl)[-1], function(nome) {
  x <- base_ardl[[nome]]
  dx <- diff(x)
  
  tibble(
    variavel = nome,
    p_valor_adf_nivel = adf_pval(x),
    p_valor_kpss_nivel = kpss_pval(x),
    p_valor_adf_diff = adf_pval(dx),
    p_valor_kpss_diff = kpss_pval(dx)
  )
})

resultado

# evidencia gráfica
base_ardl |> mutate(primeira_dif = selic_mensal - lag(selic_mensal)) |> 
  drop_na() |> 
  ggplot(aes(x = date, y = primeira_dif))+
  geom_line()


# evidencia gráfica
base_ardl |> mutate(primeira_dif = inad_mercado - lag(inad_mercado)) |> 
  drop_na() |> 
  ggplot(aes(x = date, y = primeira_dif))+
  geom_line()



# Teste de correlação -----------------------------------------------------

# Suponha que df_series contenha todas as séries, inclusive inad_mercado e inad_cartao
# Função para p-valor KPSS
cria_dif <- function(x) {
  tryCatch({
    x - lag(x)
  }, error = function(e) NA)
}


# Seleciona apenas colunas numéricas
df_num <- base_ardl %>%
  select(where(is.numeric)) %>%
  mutate(across(everything(), ~cria_dif(.x))) |> 
  drop_na()


# Calcula correlação com inad_mercado
cor_inad_mercado <- cor(df_num, df_num$inad_mercado, use = "pairwise.complete.obs")

# Calcula correlação com inad_cartao
cor_inad_cartao <- cor(df_num, df_num$inad_cartao, use = "pairwise.complete.obs")

# Junta em data.frame
correlacoes <- data.frame(
  Variavel = rownames(cor_inad_mercado),
  Correlacao_com_inad_mercado = round(cor_inad_mercado[,1], 3),
  Correlacao_com_inad_cartao = round(cor_inad_cartao[,1], 3)
)

# Remove inad_mercado e inad_cartao da própria comparação (cor = 1)
correlacoes <- correlacoes |> 
  filter(!Variavel %in% c("inad_mercado", "inad_cartao"))

# Visualizar
correlacoes

correlacoes |> clipr::write_clip(dec = ",")


# Modelo ARDL -------------------------------------------------------------

#saveRDS(base_ardl,file = "base_ardl.RDS")


## Modelo sem correção dos erros

modelo_ardl <- auto_ardl(inad_cartao ~ pib_mensal+selic_mensal+ipca_mensal+concessoes_ccredito+tx_desemprego+cambio_dolar+crise_fin+prop_sld_ccredito_pib,
                         data = df_num |> select(-inad_mercado, -saldo_credito, -concessoes_credito),
                         max_order = 5,
                         selection = "BIC")


ardl1 <- modelo_ardl$best_model

summary(ardl1)

modelo_ardl$top_orders |> head(1) # (2,1,1,0,4,1,0,0,0) BIC = -59.37

## Modelo UECM (Unrestricted Error Correction Model)

uecm_ardl1 <- uecm(ardl1)

summary(uecm_ardl1)

##  RECM (Restricted Error Correction Model) 

ardl1_case2 <- recm(uecm_ardl1, case = 2)

summary(ardl1_case2)


ardl1_case3 <- recm(uecm_ardl1, case = 3)

summary(ardl1_case3)


## bound test

bounds_f_test(ardl1, case = 2, alpha = 0.01)

## resumo

multipliers(ardl1, type = "sr")
multipliers(ardl1)


## Previsão

