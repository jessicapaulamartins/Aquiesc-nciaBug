# ESCALA DE AUTOEFICÁCIA GERAL X AQUIESCÊNCIA X ITENS BUG
# OBJETIVO: Comparar itens balanceados versus itens Bug

# INSTALAÇÃO
install.packages("readxl")
install.packages("lavaan")

# CARREGAR PACOTES INSTALADOS
library(readxl)
library(lavaan)

# IMPORTAR BANCO DE DADOS DO EXCEL
arquivo <- choose.files()
dados <- read_excel(arquivo)

############## MODELO 0: Autoeficacia ##############
modelo_cfa0 <-'
Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10
'
ajuste_modelo_cfa0 <- cfa(modelo_cfa0, data = dados, std.lv = TRUE)
summary(ajuste_modelo_cfa0, fit.measures=T, standardized=T)


############## MODELO 1: Autoeficacia X Aquiescencia ##############
modelo_cfa1_comAcq <-'
Autoeficacia =~ AFg1 + AFg4 + AFg8 + AFg10 +
                AFgNeg1 + AFgNeg4 + AFgNeg8 + AFgNeg10

AcqEF =~        1*AFg1 + 1*AFg4 + 1*AFg8 + 1*AFg10 +
                1*AFgNeg1 + 1*AFgNeg4 + 1*AFgNeg8 + 1*AFgNeg10

AcqEF ~~ NA*AcqEF  #estima a variancia de acq
Autoeficacia ~~ 0*AcqEF  #fixa a correlacao em 0
'
ajuste_modelo_cfa1_comAcq <- cfa(modelo_cfa1_comAcq, data = dados, std.lv = TRUE)
summary(ajuste_modelo_cfa1_comAcq, fit.measures=T, standardized=T)


############## MODELO 2: Modelo Fatorial Bug ##############
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


############## MODELO 3: Autoeficacia X Acq X Bug ##############
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
