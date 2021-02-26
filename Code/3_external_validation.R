# ########################################
# AVAILABLE 25/02/2021
# App-based symptom tracking to optimize SARS-CoV-2 testing strategy using machine learning
# ########################################



# Libraries ---------------------------------------------------------------
library(tidyverse)




# -------------------------

# EXTERNAL VALIDATION AND HYPOTHESIS TEST

library(MLmetrics)
library(klaR)

positividade <- read_delim("Output/Main/positividade.csv",";",
                           escape_double = FALSE, trim_ws = TRUE)


xTestes = positividade %>%
    filter(result_date >= "2020-06-15") %>%
    filter(result_date <= "2020-07-16")

yTestes = positividade %>%
    filter(result_date >= "2020-08-01") %>%
    filter(result_date <= "2020-09-01")

print(paste0("Positivos antes: ",sum(xTestes$positivos)))
print(paste0("Testados antes: ",sum(xTestes$testes)))
print(paste0("Taxa antes: ",sum(xTestes$positivos)/sum(xTestes$testes)))
print(paste0("Positivos depois: ",sum(yTestes$positivos)))
print(paste0("Testados depois: ",sum(yTestes$testes)))
print(paste0("Taxa depois: ",sum(yTestes$positivos)/sum(yTestes$testes)))

wilcox.test(xTestes$pos_acumulada, yTestes$pos_acumulada, alternative = "less")

