# ########################################
# AVAILABLE 25/02/2021
# App-based symptom tracking to optimize SARS-CoV-2 testing strategy using machine learning
# ########################################



# Libraries ---------------------------------------------------------------
library(tidyverse)




# Data input and preparation ----------------------------------------------
dados = read_delim("Data/ddb_modelo_temporal_16-07_testes.csv",";", escape_double = FALSE, trim_ws = TRUE)

dados = dados[-which(dados$test_result == "Inconclusivo"),]

dados = dados %>%
    map_if(is.logical, as.character) %>%
    bind_cols() %>%
    select(-c(priority, date_of_birth)) %>%
    # replace(. == "TRUE", "Sim") %>%
    # replace(. == "FALSE", "Nao") %>%
    map_if(is.character, as.factor) %>%
    bind_cols() %>%
    filter(Indicado == "FALSE") %>%
    bind_cols() %>%
    filter(SemanaEpidemiologica >= 0) %>%
    bind_cols() %>%
    filter(DiasAteDiagnostico >= 0) %>%
    bind_cols() %>%
    filter(idade_anos <= 100) %>%
    bind_cols() %>%
    select(-c(Indicado,
              Bairro, DataDosSintomas, SemanaEpidemiologica,
              DiasAteDiagnostico))




# Descriptive Table (Table 1) ---------------------------------------------
tabela = matrix(NA, ncol = 4, nrow = 27)

colnames(tabela) = c("Testou Positivo",NA,"Testou Negativo",NA)

positivos = dados %>% 
    filter(test_result == "Positivo")

negativos = dados %>% 
    filter(test_result == "Negativo")

tabela[1,c(1,3)] = c(nrow(positivos),nrow(negativos))

tabela[2,c(1,3)] = c(length(which(positivos$Gender == "Feminino"))/nrow(positivos)*100,length(which(negativos$Gender == "Feminino"))/nrow(negativos)*100)

tabela[3,] = c(mean(positivos$idade_anos),sd(positivos$idade_anos),mean(negativos$idade_anos),sd(negativos$idade_anos))

# Calculating proportion of positive an negative for each variable (Age, Sex and self-reported symptoms)
for (j in 4:25) {
    tabela[j,c(1,3)] = c(length(which(positivos[,j + 2] == TRUE))/nrow(positivos) * 100,length(which(negativos[, j + 2] == TRUE))/nrow(negativos) * 100)
}

tabela[26,c(1,3)] = c(length(which(positivos$ProfissionalSaude == "TRUE"))/nrow(positivos)*100,length(which(negativos$ProfissionalSaude == "TRUE"))/nrow(negativos)*100)

tabela[27,c(1,3)] = c(length(which(positivos$CovidConfirmadoCasa == "TRUE"))/nrow(positivos)*100,length(which(negativos$CovidConfirmadoCasa == "TRUE"))/nrow(negativos)*100)

rownames(tabela) = c("Quantidade","Mulheres (%)","Idade (anos)",
                      paste0(colnames(dados)[6:27]," (%)"),
                     "Profissional de Saúde", 
                     "Covid confirmado em casa")


# Exporting Table
write.csv2(tabela,"Output/Main/table1_descriptive_symptoms_2020-07-27.csv")


# Finished
