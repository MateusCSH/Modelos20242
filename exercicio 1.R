install.packages("wooldridge")
library(wooldridge)

dados_c1 <- wage1

#C1-1
sd(dados_c1$educ,na.rm = TRUE)
mean(dados_c1$educ, na.rm = TRUE)

#C1-2
mean(dados_c1$wage,na.rm = TRUE)

#C1-5
install.packages("dplyr")
library(dplyr)

dados_f <- dados_c1 %>% filter(female == 1)
table(dados_f$female)

dados_f <- dados_c1 %>% filter(married == 1)
table(dados_f$married)

#C2-1
dados_c2 <- bwght

table (dados_c2$cigs>0) / nrow(dados_c2)

#C2-2
mean(dados_c2$cigs,na.rm = TRUE)

#C2-3
dados_fumam <- dados_c2%>% filter(cigs>0)
mean(dados_fumam$cigs,na.rm = TRUE)

#C2-5
mean(dados_c2$faminc,na.rm = TRUE)
sd(dados_c2$faminc*5.65,na.rm = TRUE)

#C5-1
dados_c5 <- fertil2
min(dados_c5$children,na.rm = TRUE)
max(dados_c5$children,na.rm = TRUE)
mean(dados_c5$children,na.rm = TRUE)

#C5-2
table (dados_c5$electric==1) / nrow(dados_c5)

#C5-3
dados_s <- dados_c5%>% filter(electric==1)
mean(dados_s$children,na.rm = TRUE)
dados_n <- dados_c5%>% filter(electric==0)
mean(dados_n$children,na.rm = TRUE)



# C6 -> 1 qts 

dados_c6  <- countymurders
?countymurders

# NÚMERO DE CONDADOS
nrow(dados_c6)

#Zero ass (1996)
'table(dados_c6$murders != 0 & dados_c6$year==1996)'
dados_hom = dados_c6 %>% filter(murders != 0)   # Filtrando
table(dados_hom$year == 1996)                   # Mostrando o resultado
dd2 = dados_hom %>% filter(year == 1996)        # Ampliando o filtro
dd3 = table(dd2$year)                           # Mostrando o resultado
paste("# Dados resultado = ", dd3, sep = ' ')   # Personalizando


# Forma encontrada que deu certo
dados_hom_not <- dados_c6 %>% filter(year == 1996, murders == 0)  # Filtrar por não assasinatos.
contagem <- dados_hom_not %>%                       # Contar a quantidade de não assasinatos.
  summarise(contagem = sum(murders == 0))
contagem                                            # Mostrar o resultado.

porcent = contagem / nrow(dados_c6)                 # Calcular a porcentagem.
porcent                                             # Mostrar a porcentagem.


#c6 -> 2
max(dados_c6$murders)                               # Mostrar o valor máximo de uma coluna.

max(dados_c6$execs)

porcent_exec = max(dados_c6$execs)  / max(dados_c6$murders)
porcent_exec


# C6 -> 3Coeficinete de correlação
cor.test(countymurders$murders, countymurders$execs)




#C7-1
dados_c7 <- alcohol
table(dados_c7$abuse>0)/nrow(dados_c7)

#C7-2
dados_Abuse <- dados_c7%>% filter(abuse==1)
table(dados_Abuse$status==3)

#C7-3
dados_Abuse <- dados_c7%>% filter(abuse==0)
table(dados_Abuse$status==3)

