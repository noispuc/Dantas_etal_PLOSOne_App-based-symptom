# ##############################################################################
# DATA PREPARATION AND DESCRIPTIVE ANALYSIS 
#
# Code for the paper: 
# App-based symptom tracking to optimize SARS-CoV-2 testing strategy 
# using machine learning
#
# Guilherme F.G. de Souza, Leonardo S.L. Bastos (@lslbastos)
#
# Available in February 25 2021
# ##############################################################################



# Libraries ---------------------------------------------------------------
library(tidyverse)



# Data input and preparation ----------------------------------------------
dados = read_delim("Data/ddb_modelo_temporal_16-07_testes.csv",";",
                    escape_double = FALSE, trim_ws = TRUE)

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
df_descriptive <- 
    dados %>% 
    mutate(
        participant = 1,
        isFemale = if_else(Gender == "Feminino", 1, 0),
        test_result = if_else(test_result == "Positivo", "Positive", "Negative")
    ) %>% 
    select(
        test_result, participant, isFemale, idade_anos, CovidConfirmadoCasa, 
        ProfissionalSaude, Coriza, Tosse, Mialgia, DorGarganta, Febre,
        Diarreia, PerdaOlfato, Enjoo, FaltaAr, `NA-Sintoma`
        ) %>% 
    mutate_if(
        is.factor, function(x) { if_else(as.character(x) == "TRUE", "Yes", "No")}
    )


ls_desc_labels <- 
    list(
        participant = "Participants, n (%)",
        isFemale = "Gender, n (%)",
        idade_anos = "Age (years), median (IQR)", 
        CovidConfirmadoCasa = "Cohabitation - lives with a SARS-CoV-2 infected person, n(%)", 
        ProfissionalSaude = "Health professional, n (%)", 
        Coriza = "Coryza", 
        Tosse = "Cough", 
        Mialgia = "Myalgia", 
        DorGarganta = "Sore throat", 
        Febre = "Fever",
        Diarreia = "Diarrhea", 
        PerdaOlfato = "Loss of smell", 
        Enjoo = "Nausea", 
        FaltaAr = "Shortness of breath", 
        `NA-Sintoma` = "No symptoms above"
    )



tb_descriptive_symptoms <- 
    df_descriptive %>% 
    gtsummary::tbl_summary(
        by = test_result,
        label = ls_desc_labels
    ) %>% 
    gtsummary::add_overall(col_label =  "Total")
    



# Exporting Table
write_csv2(tb_descriptive_symptoms$table_body %>%
               select(Characteristics = var_label,
                      Total    = stat_0,
                      Negative = stat_1,
                      Positive = stat_2),
           "Output/Main/table1_descriptive_symptoms_2020-07-27.csv")


# Finished
