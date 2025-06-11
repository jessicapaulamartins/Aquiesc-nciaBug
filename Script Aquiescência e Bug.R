# ESCALA GERAL DE AUTOEFICÁCIA X ITENS BUG
# OBJETIVO: Avaliar a estrutura de dois fatores latentes

# INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# install.packages("readxl") #importa os dados do Excel
# install.packages("lavaan") #testa modelo com 2 fatores latentes
# install.packages("ggplot2") #criação de gráficos: visualizar cargas fatoriais

# Carregar os pacotes instalados
library(readxl)
library(lavaan)
library(ggplot2)

# IMPORTAR O BANCO DE DADOS DO EXCEL
arquivo <- choose.files()
dados <- read_excel(arquivo)

############## Modelo 0: Autoeficacia ##########################
modelo_cfa0 <-'
Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
'
ajuste_modelo_cfa0 <- cfa(modelo_cfa0, data = dados, std.lv = TRUE)
summary(ajuste_modelo_cfa0, fit.measures=T, standardized=T)



############# Modelo 1: Autoeficacia X Aquiescencia ############
modelo_cfa1_comAcq <-'
Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10

AcqEF =~        1*AFg1 + 1*AFg4 + 1*AFg8 + 1*AFg10 +
                1*AFgNeg1 + 1*AFgNeg4 + 1*AFgNeg8 + 1*AFgNeg10

AcqEF ~~ NA*AcqEF  #estima a variancia de acq
Autoeficacia ~~ 0*AcqEF  #fixa a correlaco em 0
'
ajuste_modelo_cfa1_comAcq <- cfa(modelo_cfa1_comAcq, data = dados, std.lv = TRUE)
summary(ajuste_modelo_cfa1_comAcq, fit.measures=T, standardized=T)



############# Modelo 2: Autoeficacia X Bug #####################
# modelo_cfa2_comBug <-'
# Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
#                 AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
# 
# Bug =~          1*Acq1 + 1*Acq2 + 1*Acq3 + 1*Acq5 +
#                 1*AcqNeg1 + 1*AcqNeg2 + 1*AcqNeg3 + 1*AcqNeg5
# 
# Bug ~~ NA*Bug  #estima a variancia de acq
# Autoeficacia ~~ 0*Bug  #fixa a correlaco em 0
# '
# ajuste_modelo_cfa2_comBug <- cfa(modelo_cfa2_comBug, data = dados, std.lv = TRUE)
# summary(ajuste_modelo_cfa2_comBug, fit.measures=T, standardized=T)



############# Modelo 3: Modelo Fatorial Bug ####################
dados$acq_bug <- rowMeans(dados[, c("Acq1", "Acq2", "Acq3", "Acq5",
                                    "AcqNeg1", "AcqNeg2", "AcqNeg3", "AcqNeg5")], 
                          na.rm = TRUE)
summary(dados$acq_bug)
sd(dados$acq_bug)
table(dados$Acq1)


modelo_cfa3_comAcq <-'
Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10

AcqEF =~        1*AFg1 + 1*AFg4 + 1*AFg8 + 1*AFg10 +
                1*AFgNeg1 + 1*AFgNeg4 + 1*AFgNeg8 + 1*AFgNeg10

AcqEF ~~ NA*AcqEF  #estima a variancia de acq
Autoeficacia ~~ 0*AcqEF  #fixa a correlaco em 0

AcqEF ~~ acq_bug
Autoeficacia ~~ acq_bug
'
ajuste_modelo_cfa3_comAcq <- cfa(modelo_cfa3_comAcq, data = dados, std.lv = TRUE)
summary(ajuste_modelo_cfa3_comAcq, fit.measures=T, standardized=T)





############# Modelo 4: Autoeficacia X Acq X Bug ###############
modelo_cfa3b_comAcqBug <-'
Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10

AcqEF =~        1*AFg1 + 1*AFg4 + 1*AFg8 + 1*AFg10 +
                1*AFgNeg1 + 1*AFgNeg4 + 1*AFgNeg8 + 1*AFgNeg10

Bug =~          1*Acq1 + 1*Acq2 + 1*Acq3 + 1*Acq5 +
               -1*AcqNeg1 + -1*AcqNeg2 + -1*AcqNeg3 + -1*AcqNeg5

AcqEF ~~ NA*AcqEF
Autoeficacia ~~ 0*AcqEF
'
ajuste_modelo_cfa3b_comAcqBug <- cfa(modelo_cfa3b_comAcqBug, data = dados, std.lv = TRUE)
summary(ajuste_modelo_cfa3b_comAcqBug, fit.measures=T, standardized=T)





# modelo_cfa3c_comAcq_teste <-'
# Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
#                 AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
#           
#            
# AFg1 ~ acq_bug
# AFg4 ~ acq_bug
# AFg8 ~ acq_bug
# AFg10 ~ acq_bug
# AFgNeg1 ~ acq_bug
# AFgNeg4 ~ acq_bug
# AFgNeg8 ~ acq_bug
# AFgNeg10 ~ acq_bug
# 
# AcqEF ~~ NA*AcqEF  #estima a variancia de acq
# Autoeficacia ~~ 0*AcqEF  #fixa a correlaco em 0
# 
# Autoeficacia ~~ 0*acq_bug
# AcqEF ~~ 1*acq_bug
# 
# 
# '
# ajuste_modelo_cfa3c_comAcq_teste <- cfa(modelo_cfa3c_comAcq_teste, data = dados, std.lv = TRUE)
# summary(ajuste_modelo_cfa3c_comAcq_teste, fit.measures=T, standardized=T)
# 
# summary(ajuste_modelo_cfa1_comAcq, fit.measures=T, standardized=T)




## tentar incluir os itens de interesse
#mimic de acqEF + bug sobre os itens de interesse..


############################################
############ lixo ##########################
############################################



############# Modelo 3 ##############################
#modelo_cfa2_comBug <-'
#Autoeficacia =~ AFg1 + AFg4  + AFg8 + AFg10 +
#                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
#
#AcqEF =~        1*AFg1 + 1*AFg4  + 1*AFg8 + 1*AFg10 +
#                1*AFgNeg1 + 1*AFgNeg4  + 1*AFgNeg8 + 1*AFgNeg10
#'
#dados$acq_bug <- rowMeans(dados[, c("Acq1", "Acq2", "Acq3", "Acq5",
#                                    "AcqNeg1", "AcqNeg2", "AcqNeg3", "AcqNeg5")], 
#                          na.rm = TRUE)
#
#summary(dados$acq_bug)
#sd(dados$acq_bug)
#table(dados$Acq1)
############## Fim do Modelo 3 ######################


# modelo_cfa2_comBug <-'
# Autoeficacia =~ AFg1 + AFg4  + AFg8 + AFg10 +
#                 AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
# 
# AcqEF =~        1*AFg1 + 1*AFg4  + 1*AFg8 + 1*AFg10 +
#                 1*AFgNeg1 + 1*AFgNeg4  + 1*AFgNeg8 + 1*AFgNeg10
# 
# Bug =~          1*Acq1 + 1*Acq2 + 1*Acq3 + 1*Acq4 + 1*Acq5 +
#                 -1*AcqNeg1 + -1*AcqNeg2 + -1*AcqNeg3 + -1*AcqNeg4 + -1*AcqNeg5
# 
# Bug ~~ NA*Bug
# AcqEF ~~ NA*AcqEF
# Autoeficacia ~~ NA*Bug
# Autoeficacia ~~ 0*AcqEF
# AcqEF ~~ NA*Bug
# '
# 
# ajuste_modelo_cfa2_comBug <- cfa(modelo_cfa2_comBug, data = dados, std.lv = TRUE)
# summary(ajuste_modelo_cfa2_comBug, fit.measures=T, standardized=T)
#
# # # DEFINIÇÃO DO MODELO FATORIAL
# # #modelo_cfa2b_comBug_alternativo <-'
# # Autoeficacia =~ AFg1 + AFg4  + AFg8 + AFg10 +
# #                 AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
# # 
# # AcqEF =~        1*AFg1 + 1*AFg4  + 1*AFg8 + 1*AFg10 +
# #                 1*AFgNeg1 + 1*AFgNeg4  + 1*AFgNeg8 + 1*AFgNeg10
# # 
# # Bug =~          1*Acq1 + 1*Acq2 + 1*Acq3 + 1*Acq4 + 1*Acq5 +
# #                 -1*AcqNeg1 + -1*AcqNeg2 + -1*AcqNeg3 + -1*AcqNeg4 + -1*AcqNeg5
# # 
# # Bug ~~ NA*Bug
# # AcqEF ~~ NA*AcqEF
# # #Autoeficacia ~~ NA*Bug
# # Autoeficacia ~~ 0*AcqEF
# # #AcqEF ~~ NA*Bug
# # Bug ~ AcqEF + Autoeficacia
# # '
# # ajuste_modelo_cfa2b_comBug_alternativo <- cfa(modelo_cfa2b_comBug_alternativo, data = dados, std.lv = TRUE)
# # summary(ajuste_modelo_cfa2b_comBug_alternativo, fit.measures=T, standardized=T)
# 
# modelo_cfa2 <-'
# Autoeficacia =~ AFg1 + AFg4 + AFg7 + AFg8 + AFg10 +
#                 AFgNeg1 + AFgNeg4 + AFgNeg7 + AFgNeg8 + AFgNeg10
# 
# AcqEF =~        1*AFg1 + 1*AFg4 + 1*AFg7 + 1*AFg8 + 1*AFg10 +
#                 1*AFgNeg1 + 1*AFgNeg4 + 1*AFgNeg7 + 1*AFgNeg8 + 1*AFgNeg10
# 
# Bug =~          1*Acq1 + 1*Acq2 + 1*Acq3 + 1*Acq4 + 1*Acq5 +
#                 -1*AcqNeg1 + -1*AcqNeg2 + -1*AcqNeg3 + -1*AcqNeg4 + -1*AcqNeg5
# 
# Bug ~~ NA*Bug
# AcqEF ~~ NA*AcqEF
# Autoeficacia ~~ NA*Bug
# Autoeficacia ~~ 0*AcqEF
# AcqEF ~~ NA*Bug
# '
# 
# ajuste2 <- cfa(modelo_cfa2, data = dados, std.lv = TRUE)
# summary(ajuste2, fit.measures=T, standardized=T)
# 
# 
# modelo_bug <-"
# Bug =~          1*Acq1 + 1*Acq2 + 1*Acq3 + 1*Acq4 + 1*Acq5 +
#                 -1*AcqNeg1 + -1*AcqNeg2 + -1*AcqNeg3 + -1*AcqNeg4 + -1*AcqNeg5
# 
# Bug ~~ NA*Bug
# "
# 
# ajustebug <- cfa(modelo_bug, data = dados, std.lv = TRUE)
# summary(ajustebug)
# 
# summary(ajuste, fit.measures = TRUE, standardized = TRUE)
# # Mostra resumo completo do modelo
# # Mostra os índices de ajuste, que indicam se o modelo está bom
# # Mostra cargas padronizadas dos itens, ou seja,
# # Mostra o quanto cada item contribui para o fator
# 
# # ggplot = cria gráficos
# # Organiza os itens no eixo vertical
# # Organiza as cargas padronizadas no eixo horizontal
# ggplot(cargas_fatoriais, aes(x = reorder(rhs, std.all), y = std.all, fill = lhs)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   labs(title = "Cargas Fatoriais Padronizadas – Todos os Itens",
#        x = "Itens",
#        y = "Carga Fatorial",
#        fill = "Fator") +
#   theme_minimal()
# 
# # SOMENTE ITENS DA ESCALA BUG
# cargas_bug <- filter(cargas_fatoriais, lhs == "Bug")
# 
# ggplot(cargas_bug, aes(x = reorder(rhs, std.all), y = std.all)) +
#   geom_bar(stat = "identity", fill = "purple") +
#   coord_flip() +
#   labs(title = "Cargas Fatoriais – Fator Bug",
#        x = "Itens",
#        y = "Carga Fatorial Padronizada") +
#   theme_minimal()
# 
# # CORRELAÇÃO ENTRE OS FATORES
# parameterEstimates(ajuste, standardized = TRUE) %>%
#   filter(op == "", lhs == "Autoeficacia", rhs == "Bug")