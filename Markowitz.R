## Código para trabalho de finanças PUC-Rio ##

## Favor mencionar a fonte ao uso deste código ##

### NADA SIMULADO AQUI É RECOMENDAÇÃO DE INVESTIMENTO ###


# Feito por Gabriel Boechat (Twitter: @gab_boechat)


# Para contabilizar em quanto tempo o código roda

começo = Sys.time()

# Bibliotecas utilizadas ------------------------

library(tidyverse) # Para fazer limpeza em geral dos dados
library(gridExtra) # Para ver tabelas nos gráficos
library(ggplot2) # Pacote para gerar gráficos
library(PerformanceAnalytics) # Funções de finanças
library(quantmod) # Puxar dados financeiros
library(fPortfolio) # Realiza todo esse código, em resumo
library(tseries) # Para fazer a fronteira eficente sem short

# Puxando os dados ------------------------------

# Ações utilizadas no trabalho para cada setor

# OBS: Totalmente aleatórias

# Não há recomendação de investimentos

Varejo = c("MGLU3.SA", "VVAR3.SA", "LAME4.SA")
Químico = c("UNIP6.SA", "BRKM5.SA", "DTEX3.SA")
Energia = c("ENBR3.SA", "EGIE3.SA", "CPLE6.SA", "ENGI11.SA")
Financeiro = c("SANB11.SA", "ITSA4.SA", "BBDC4.SA", "BBAS3.SA")
Extração = c("PETR4.SA", "PRIO3.SA", "VALE3.SA")
Siderurgia = c("GGBR4.SA", "CSNA3.SA", "USIM5.SA")

tickers_acoes = c(Varejo, 
                  Químico, 
                  Energia, 
                  Financeiro, 
                  Extração, 
                  Siderurgia) # Todos tickers juntos

# Puxando as cotações do Yahoo Finance
# Pacote {quantmod}

cotacao = getSymbols(tickers_acoes, # Vetor com todos tickers 
                     src = 'yahoo', # Fonte
                     from = "2015-09-30", # Data inicial
                     to = "2020-09-30", # Data final
                     periodicity = "daily", # Dados diários
                     auto.assign = TRUE,
                     warnings = FALSE) %>% # Puxando do Yahoo
  map(~Ad(get(.))) %>% # Isolando as séries em objetos individuais
  reduce(merge) %>% # Juntando as séries em um xts apenas
  `colnames<-`(tickers_acoes) %>% # Renomeando as colunas
  na.omit() %>% # Retirando dados faltantes
  abs() # Retorna valores absolutos para evitar preços negativos

cotacao$MGLU3.SA[1:387] = cotacao$MGLU3.SA[1:387] + 1.01726 # Erro do yahoo finance

# Observando as cotações ------------------------

cor = "black"

tema_trabalho_G2 = {theme(panel.background = element_rect(fill = cor),
                         plot.background = element_rect(fill = cor),
                         # Remove panel border
                         panel.border = element_blank(),  
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                         colour = "white"),
                         panel.grid.major.x = element_blank(),
                         # Add axis line
                         axis.line = element_line(colour = "white"),
                         # Ajusting labs 
                         plot.title = element_text(color = "white",
                                                   size = 18),
                         axis.title.x = element_text(color = "white",
                                                     size = 14),
                         axis.title.y = element_text(color = "white",
                                                     size = 14),
                         axis.ticks = element_line(colour = "white"),
                         axis.text = element_text(color = "white",
                                                  size = 11),
                         axis.text.x = element_text(color = "white",
                                                    size = 11,
                                                    angle = 45,
                                                    hjust = 1),
                         legend.background = element_rect(fill = cor, 
                                                          colour = "white"),
                         legend.text = element_text(color = "white",
                                                    size = 9),
                         legend.title = element_text(color = "white",
                                                     size = 11),
                         legend.key = element_rect(fill = cor, 
                                                   colour = "white"),
                         plot.caption = element_text(color = "white",
                                                     size = 9,
                                                     face = "italic"))
}

# Varejo ----------------------------------------

cotacao_Varejo = cotacao[,Varejo] %>%
  as.data.frame() %>%
  gather(key = Ticker,
         value = Cotação) %>%
  cbind(Data = rep(index(cotacao), times = length(Varejo))) # Para usarmos a legenda facilmente no ggplot2

ggplot(data = cotacao_Varejo, mapping = aes(x = as.Date(Data), y = Cotação, colour = Ticker)) + # Dados usados
  geom_line(lwd = 0.7) + # Gráfico de linha
  labs(x = NULL, y = NULL, title = "Empresas do setor varejeiro cotadas na B3",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") + # Títulos, nomes dos eixos etc
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ "),
                     breaks = scales::pretty_breaks(n = 4)) + # Escala dos dados em R$
  tema_trabalho_G2 # Tema constrúido anteriormente (puramente estético, não é preciso)



# Energia ---------------------------------------

cotacao_Energia = cotacao[,Energia] %>%
  as.data.frame() %>%
  gather(key = Ticker,
         value = Cotação) %>%
  cbind(Data = rep(index(cotacao), times = length(Energia))) # Para plotarmos no ggplot2

ggplot(data = cotacao_Energia, mapping = aes(x = as.Date(Data), y = Cotação, colour = Ticker)) +
  geom_line(lwd = 0.7) +
  labs(x = NULL, y = NULL, title = "Empresas do setor energético cotadas na B3",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ "),
                     breaks = scales::pretty_breaks(n = 4)) +
  tema_trabalho_G2



# Químico ---------------------------------------

cotacao_Químico = cotacao[,Químico] %>%
  as.data.frame() %>%
  gather(key = Ticker,
         value = Cotação) %>%
  cbind(Data = rep(index(cotacao), times = length(Químico))) # Para plotarmos no ggplot2

ggplot(data = cotacao_Químico, mapping = aes(x = as.Date(Data), y = Cotação, colour = Ticker)) +
  geom_line(lwd = 0.7) +
  labs(x = NULL, y = NULL, title = "Empresas do setor químico cotadas na B3",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ "),
                     breaks = scales::pretty_breaks(n = 4)) +
  tema_trabalho_G2



# Financeiro ---------------------------------------

cotacao_Financeiro = cotacao[,Financeiro] %>%
  as.data.frame() %>%
  gather(key = Ticker,
         value = Cotação) %>%
  cbind(Data = rep(index(cotacao), times = length(Financeiro))) # Para plotarmos no ggplot2

ggplot(data = cotacao_Financeiro, mapping = aes(x = as.Date(Data), y = Cotação, colour = Ticker)) +
  geom_line(lwd = 0.7) +
  labs(x = NULL, y = NULL, title = "Empresas do setor financeiro cotadas na B3",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ "),
                     breaks = scales::pretty_breaks(n = 4)) +
  tema_trabalho_G2



# Extração ---------------------------------------

cotacao_Extração = cotacao[,Extração] %>%
  as.data.frame() %>%
  gather(key = Ticker,
         value = Cotação) %>%
  cbind(Data = rep(index(cotacao), times = length(Extração))) # Para plotarmos no ggplot2

ggplot(data = cotacao_Extração, mapping = aes(x = as.Date(Data), y = Cotação, colour = Ticker)) +
  geom_line(lwd = 0.7) +
  labs(x = NULL, y = NULL, title = "Empresas do setor de extração cotadas na B3",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ "),
                     breaks = scales::pretty_breaks(n = 4)) +
  tema_trabalho_G2



# Siderurgia ---------------------------------------

cotacao_Siderurgia = cotacao[,Siderurgia] %>%
  as.data.frame() %>%
  gather(key = Ticker,
         value = Cotação) %>%
  cbind(Data = rep(index(cotacao), times = length(Siderurgia))) # Para plotarmos no ggplot2

ggplot(data = cotacao_Siderurgia, mapping = aes(x = as.Date(Data), y = Cotação, colour = Ticker)) +
  geom_line(lwd = 0.7) +
  labs(x = NULL, y = NULL, title = "Empresas do setor siderúrgico cotadas na B3",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "R$ "),
                     breaks = scales::pretty_breaks(n = 4)) +
  tema_trabalho_G2

# Retornos --------------------------------------

# Pacote {PerformanceAnalytics}

retornos = Return.calculate(cotacao,
                            method = "discrete")

# Retornos começam na primeira data; nenhuma posição 
# anterior, retorno 0

retornos[1,] = 0 

# Taxa DI diária no período

benchmark = 0.00031888 

# Matriz de covariância de todas as ações:

Cov = cov(retornos)

media_variancia = matrix(data = NA,
                         nrow = 2, # Guardando retorno e vol
                         ncol = length(tickers_acoes)) # Uma coluna para cada ação

for(i in c(1:length(tickers_acoes))) {
  
  # Média e variância para cada ação
  
  media_variancia[1,i] = mean(retornos[,i]) 
  media_variancia[2,i] = sd(retornos[,i])
  
}

media_variancia_df = data.frame(t(media_variancia))

media_variancia_df = cbind(media_variancia_df,tickers_acoes)

names(media_variancia_df) = c("Retorno Medio", "Volatilidade", "Acao")

media_variancia_df = media_variancia_df %>%
  mutate(Classificacao = case_when(Acao %in% Energia ~ "Energia",
                                   Acao %in% Químico ~ "Químico",
                                   Acao %in% Siderurgia ~ "Siderurgia",
                                   Acao %in% Varejo ~ "Varejo",
                                   Acao %in% Energia ~ "Energia",
                                   Acao %in% Financeiro ~ "Financeiro",
                                   Acao %in% Extração ~ "Extração"))

media_variancia_df = media_variancia_df %>%
  mutate(Sharpe = (`Retorno Medio` - benchmark)/Volatilidade) # Tabela completa reunindo todos os dados de cada ação escolhida

tema_trabalho_G2.2 = {theme(panel.background = element_rect(fill = cor),
                          plot.background = element_rect(fill = cor),
                          # Remove panel border
                          panel.border = element_blank(),  
                          panel.grid.minor.x = element_blank(),
                          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                                          colour = "white"),
                          panel.grid.major.x = element_blank(),
                          # Add axis line
                          axis.line = element_line(colour = "white"),
                          # Ajusting labs 
                          plot.title = element_text(color = "white",
                                                    size = 18),
                          axis.title.x = element_text(color = "white",
                                                      size = 14),
                          axis.title.y = element_text(color = "white",
                                                      size = 14),
                          axis.ticks = element_line(colour = "white"),
                          axis.text = element_text(color = "white",
                                                   size = 11),
                          axis.text.x = element_text(color = "white",
                                                     size = 11),
                          legend.background = element_rect(fill = cor, 
                                                           colour = "white"),
                          legend.text = element_text(color = "white",
                                                     size = 9),
                          legend.title = element_text(color = "white",
                                                      size = 11),
                          legend.key = element_rect(fill = cor, 
                                                    colour = "white"),
                          plot.caption = element_text(color = "white",
                                                      size = 9,
                                                      face = "italic"))
}

ggplot(data = media_variancia_df, mapping = aes(x = Volatilidade, 
                                                y = `Retorno Medio`, 
                                                colour = Classificacao,
                                                label = Acao)) +
  geom_point() +
  ggrepel::geom_text_repel(vjust = -1) +
  labs(x = "Volatilidade",
       y = "Retorno Médio",
       title = "Risco-Retorno",
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance",
       legend = "Classificação") +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  tema_trabalho_G2.2


matplot(retornos*100, type = "l", lty = 1, 
        x = as.Date(index(retornos)),
        main = "Retorno Diário de cada ativo",
        xlab = "Data",
        ylab = "Retornos (%)") # Observando subidas e descidas de cada uma das ações


# Markowitz ---------------------------

# Quantas n observações para realizar Markowitz?

n = 5000

# Quantos ativos para realizar a simulação?

x = 3


# Para cronometrar em quanto tempo a simulção roda

começo_markov = Sys.time()

{
  
# Sorteando as ações
  
acoes_sorteadas = sample(tickers_acoes, # Tickers de todas as ações que temos dados
                         size = x, # Valor estabelecido acima; quantos devemos sortear
                         replace = FALSE) # Não queremos escolher a mesma ação mais de uma vez
print(acoes_sorteadas) # Mostra quais são essas ações que foram sorteadas


# Retorno/Vol das ações escolhidas

dados_sorteados = media_variancia_df %>%
  dplyr::filter(Acao %in% acoes_sorteadas) %>%
  arrange(factor(Acao, levels = acoes_sorteadas))


retornos_sorteados = retornos[,acoes_sorteadas]

retornos_sorteados = xts::xts(retornos_sorteados, index(retornos)) # Séries temporais dos retornos das ações


Cov_sorteados = cov(retornos_sorteados) # Matriz de covariância das ações
View(cor(Cov_sorteados)) # Matriz de correlação sendo visualizada


# Matriz de pesos

w = matrix(data = actuar::rpareto(n*x, # Pesos com distribuição de Pareto
                                  shape = 1,
                                  scale = 1),
           nrow = n, # Cada linha é uma carteira; temos, então, n carteiras
           ncol = x,  # Quantidade de ações por cada cadeira; temos, então, x ações
           byrow = TRUE) # Temos aqui uma matriz de pesos com a maior combinação de pesos possíveis

for(i in c(1:n)) {
  
  w[i,] = w[i,] / sum(w[i,]) # Normalizando para a soma de pesos ser igual a 1 (100%) em cada carteira (linha)
  
}


# Montando a volatilidade para cada peso e cada carteira
# Usaremos a forma algébrica

vol_w = rep(NA, n)

for(i in c(1:n)) {
  
  w_momento = matrix(data = w[i,],
                     ncol = 1)
  
  # Transposta do vetor de peso da carteira i
  # multiplicação matricial matriz de covariância
  # multiplicação matricial do vetor de peso da carteira i
  
  
  vol_momento = sqrt( t(w_momento) %*% Cov_sorteados %*% w_momento )
  
  # Armazenando a volatilidade de cada carteira
  
  vol_w[i] = vol_momento
  
}

# Montando o retorno para cada peso

ret_w = rep(NA, n)

# Peso de cada ação no retorno; 
# será apagado a cada iteração

soma = rep(NA, x) 

for(i in c(1:n)) {
  
  for(j in c(1:x)) {
    
    # Peso aleatório vezes retorno médio de cada ação
    
    soma[j] = w[i,j] * dados_sorteados$`Retorno Medio`[j]
    
  }
  
  # Retorno da carteira é a soma dos retornos de cada ação
  
  ret_w[i] = sum(soma)
  
}

# Unindo ambas informações em um dataframe apenas:

markowitz = tibble(ret_w, vol_w)

markowitz = markowitz %>%
  mutate(Sharpe = (ret_w-benchmark)/vol_w)

names(markowitz) = c("Retorno Medio", "Volatilidade", "Sharpe")




# Montando a fronteira eficiente:

# Pontos para cada valor de retorno que apresentam 
# a menor variância possível.

# Fazendo a fronteira à mão:

# Pegando pontos entre o menor e o maior valor 
# de retorno das carteiras simuladas

ret_vec_front = seq(min(ret_w), max(ret_w), length.out = 1000) 

vol_vec_front = sapply(ret_vec_front, function(er) {
  
  # Pacote {tseries}
  
  op = portfolio.optim(as.matrix(retornos_sorteados), er, 
                       shorts = FALSE, # Posições compradas apenas -> não existem pesos negativos nesse exemplo
                       riskless = FALSE) # Queremos apenas o valor da carteira de mínima variância
  
  # Menor variância possível para cada ponto 
  
  return(op$ps)
  
}) # Armazenando os valores de variância mínima para cada valor de retorno

fronteira_by_hand = tibble(ret_vec_front, vol_vec_front)

names(fronteira_by_hand) = c("Retorno Medio", "Volatilidade")



# Pacote {fPortfolio}
# Monta a simulação apenas com esse código

fronteira = portfolioFrontier(as.timeSeries(retornos_sorteados)) # Objeto que contém todas as informações que já fizemos nesse código

frontierPlot(fronteira,
             pch = 20,
             cex = 1.5,
             type = "o",
             lwd = 2) # Gráfico da fronteira

singleAssetPoints(fronteira,
                  col = c(1:x),
                  pch = 20,
                  cex = 1.5) # Retorno/Vol de cada ação

monteCarloPoints(fronteira,
                 mcSteps = n, # Quantidade de simulações; mesmo valor que usamos no código para simular "à mão"
                 pch = 20,
                 cex = 0.1,
                 col = "steelblue") # Simulações com pesos aleatórios

# Dessa forma, todos os gráficos ficam na mesma imagem



# Distribuição/comportamento de retorno e vol das carteiras simuladas

par(mfrow = c(1,2)) # Para vermos um gráfico do lado do outro

hist(ret_w*100, breaks = 200, col = "#569E9B",
     xlab = NA,
     main = "Retornos das carteiras simuladas (%)") # Histograma dos retornos 

hist(vol_w*100, breaks = 200, col = "#FCF081",
     xlab = NA,
     main = "Volatilidade das carteiras simuladas (%)") # Histograma das volatilidades



# Carteira de mínima variância

mínima_variância = fronteira_by_hand %>%
  arrange(Volatilidade) %>%
  slice(1)



# Descobrindo a carteira ótima

ótimo = IntroCompFinR::tangency.portfolio(er = dados_sorteados$`Retorno Medio`, # Retorno de cada ação
                                          cov.mat = Cov_sorteados, # Matriz de covariância
                                          risk.free = benchmark, # Taxa livre de risco (DI)
                                          shorts = FALSE) # Apenas posições compradas

ret_esp_ótimo = ótimo[[2]] # Retorno esperado da carteira ótima
vol_esp_ótimo = ótimo[[3]] # Volatilidade da carteira ótima

carteira_ótima = tibble(ret_esp_ótimo, vol_esp_ótimo)

names(carteira_ótima) = c("Retorno Medio", "Volatilidade")



# Código abaixo Não utilizado no gráfico

# Serve para traçar tangente entre a taxa de juros livre de risco
# com a carteira ótima

livre_de_risco = c(benchmark, 0)

livre_de_risco = rbind(livre_de_risco,carteira_ótima) 

# OBS: a tangente à curva na carteira ótima distancia muito da linha de pontos, dificultando sua visualização;
# como já achamos o ponto de tangência, não é necessário observarmos a reta de tangência.



# Visualizando os resultados

gráfico_markovitz =
  ggplot(data = markowitz, mapping = aes(x = Volatilidade, 
                                         y = `Retorno Medio`,
                                         colour = Sharpe)) +
  geom_point(pch = 20,
             lwd = 0.4) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_colour_gradient2(low = "red", high = "forestgreen", mid = "darkorange") +
  labs(title = paste0(n," simulações e ",x," ações"),
       y = "Retorno Médio") +
  geom_point(data = fronteira_by_hand, mapping = aes(x = Volatilidade,
                                                     y = `Retorno Medio`),
             colour = "steelblue",
             pch = 20,
             lwd = 0.5) +
  geom_point(data = mínima_variância, 
             colour = "red",
             lwd = 3) +
  geom_point(data = carteira_ótima,
             colour = "yellow",
             lwd = 3) +
  tema_trabalho_G2.2

gráfico_dispersão =
  ggplot(data = dados_sorteados, mapping = aes(x = Volatilidade, 
                                             y = `Retorno Medio`,
                                             colour = Classificacao,
                                             label = Acao)) +
  geom_point(data = dados_sorteados) +
  ggrepel::geom_text_repel(vjust = -1) +
  labs(x = "Volatilidade",
       y = "Retorno Médio",
       title = gsub(" , "," ",toString(acoes_sorteadas)),
       caption = "Dados de 30/09/2015 até 30/09/2020
       Fonte: Yahoo Finance") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  tema_trabalho_G2.2

# No mesmo gráfico:

gridExtra::grid.arrange(gráfico_dispersão, gráfico_markovitz, nrow = 1)


# Para vermos os pesos ótimos para cada ação numa tabela:

pesos_ótimos = ótimo[[4]] # Pesos para cada ativo

matriz_ótima = matrix(data = NA,
                      nrow = 3,
                      ncol = length(acoes_sorteadas))

matriz_ótima[1,] = paste0(round(pesos_ótimos*100, digits = 2),"%") # Pesos ótimos
matriz_ótima[2,] = paste0(round(dados_sorteados$`Retorno Medio`*100, digits = 2),"%") # Retornos individuais
matriz_ótima[3,] = paste0(round(dados_sorteados$Volatilidade*100, digits = 2),"%") # Volatilidades individuais

matriz_ótima = as.data.frame(matriz_ótima, 
                             row.names = c("Pesos", "Retorno", "Volatilidade"))

names(matriz_ótima) = acoes_sorteadas


# Matriz de correlação e pesos ótimos em tabelas

grid.arrange(tableGrob(round(cor(Cov_sorteados), digits = 3)),
             tableGrob(matriz_ótima))

}

fim_markov = Sys.time()

tempo_markov = fim_markov - começo_markov

print(tempo_markov) # Eficiência da simulação



fim = Sys.time()

fim - começo # Eficiência do código como inteiro