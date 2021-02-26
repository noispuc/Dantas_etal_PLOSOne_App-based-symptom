# ########################################
# AVAILABLE 25/02/2021
# App-based symptom tracking to optimize SARS-CoV-2 testing strategy using machine learning
# ########################################



# Libraries ---------------------------------------------------------------
library(tidyverse)




# Adjusted OR for symptoms ------------------------------------------------

# Odds Ratio (OR) with 95% confidence intervals using logistic regression models
# for each feature adjusted by age and gender

dados = read_delim("Data/ddb_modelo_temporal_16-07_testes.csv",";", escape_double = FALSE, trim_ws = TRUE)
dados = dados[-which(dados$test_result == "Inconclusivo"),]
dados = dados %>%
    map_if(is.logical, as.character) %>%
    bind_cols() %>%
    select(-c(priority, date_of_birth)) %>%
    replace(. == "TRUE", "Sim") %>%
    replace(. == "FALSE", "Nao") %>%
    map_if(is.character, as.factor) %>%
    bind_cols() %>%
    filter(Indicado  == "Nao") %>%
    bind_cols() %>%
    filter(SemanaEpidemiologica >= 0) %>%
    bind_cols() %>%
    filter(DiasAteDiagnostico >= 0) %>%
    bind_cols() %>%
    filter(idade_anos <= 100) %>%
    bind_cols() %>%
    select(-c(Cirrose, Cancer, Transplantado, Diabetes, Obesidade,
              GestacaoRisco, DoenteFigado, TratamentoImunossupressor,
              Renal, Cardiaco, ProblemaPulmonar, Indicado, `NA-Risco`,
              ProfissionalSaude, Bairro, DataDosSintomas, SemanaEpidemiologica,
              DiasAteDiagnostico))

aux_matrix = matrix(NA, nrow = ncol(dados) - 3, ncol = 4)
colnames(aux_matrix) = c("VAR","INF","COEF","SUP")
geral_com_ajuste = aux_matrix

for (i in 1:nrow(aux_matrix)) {
    aux = data.frame(y = dados[,1], x = dados[,i + 3])
    names(aux) = c("y","x")
    
    fit2 = glm(y ~ x + dados$Gender + dados$idade_anos, data = aux, family = 'binomial')
    geral_com_ajuste[i,3] = exp(coef(fit2)[2])
    geral_com_ajuste[i,2] = exp(confint(fit2)[2,1])
    geral_com_ajuste[i,4] = exp(confint(fit2)[2,2])
}

geral_com_ajuste = as.data.frame(geral_com_ajuste)
geral_com_ajuste$VAR = names(dados)[-c(1:3)]

geral_sintomas = geral_com_ajuste

geral_sintomas %>% 
    mutate(VAR = fct_reorder(VAR, COEF)) %>%
    ggplot(aes(y = VAR, x = COEF, group = 1)) + 
    geom_errorbar(aes(xmin = INF, xmax = SUP)) +
    geom_point(aes(x = COEF,y = VAR)) +
    geom_vline(xintercept = 1, color = "tomato") +
    theme_minimal() +
    ylab("") + xlab("OR") +
    scale_y_discrete(labels = c("Loss of smell",
                                "Fever", "Myalgia","Cough",
                                "Nausea","Shortness of breath",
                                "Diarrhea","Coryza","Sore throat",
                                "No symptoms above", "Cohabitation SARS-CoV-2"),
                     breaks = c("PerdaOlfato",
                                "Febre","Mialgia","Tosse",
                                "Enjoo","FaltaAr",
                                "Diarreia","Coriza","DorGarganta",
                                "NA-Sintoma", "CovidConfirmadoCasa")) +
    theme(axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 12)) +
    ggsave("Output/Main/figure1_OR_symptoms_27-07-2020.tiff", width = 12)


write.csv2(geral_sintomas,"Output/Supplementary/table_OR_symptoms_27-07-2020.csv")



# Finished
