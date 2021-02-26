# ########################################
# AVAILABLE 25/02/2021
# App-based symptom tracking to optimize SARS-CoV-2 testing strategy using machine learning
# ########################################



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(caret)
library(pROC)




# Custom functions --------------------------------------------------------

# Function for training predictive models --> Classification
# Summary Function MCC Calculation -----
mcc = function(data, lev = NULL, model = NULL) {
    pred_num = ifelse(data$pred == "Positivo", 1, 0)
    act_num = ifelse(data$obs == "Positivo", 1, 0)
    mcc_val = mccr::mccr(pred = pred_num, act = act_num)
    c(MCC = mcc_val)
}


# Summary Function aggStats Settings ----
aggStats = function(...) c(mcc(...), multiClassSummary(...))


# Test Parameter Settings ----
runParams = function(model, dados_pred) {
    if(model == "RF") {
        test_params = list(
            # Random Forest
            RF_null = list(sampling_control = NULL,
                           model_method = "ranger",
                           model_params = expand.grid(mtry = 1:ncol(dados_pred),
                                                      splitrule = c("gini", "hellinger"),
                                                      min.node.size = 1)),
            RF_up = list(sampling_control = "up",
                         model_method = "ranger",
                         model_params = expand.grid(mtry = 1:ncol(dados_pred),
                                                    splitrule = c("gini", "hellinger"),
                                                    min.node.size = 1)),
            RF_down = list(sampling_control = "down",
                           model_method = "ranger",
                           model_params = expand.grid(mtry = 1:ncol(dados_pred),
                                                      splitrule = c("gini", "hellinger"),
                                                      min.node.size = 1)),
            RF_smote = list(sampling_control = "smote",
                            model_method = "ranger",
                            model_params = expand.grid(mtry = 1:ncol(dados_pred),
                                                       splitrule = c("gini", "hellinger"),
                                                       min.node.size = 1)),
            RF_rose = list(sampling_control = "rose",
                           model_method = "ranger",
                           model_params = expand.grid(mtry = 1:ncol(dados_pred),
                                                      splitrule = c("gini", "hellinger"),
                                                      min.node.size = 1))
        )
    } else if (model == "DT") { 
        test_params = list(
            ## Decision Tree - C5.0
            c50_null = list(sampling_control = NULL,
                            model_method = "C5.0",
                            model_params = expand.grid(trials = c(1, 10, 20, 50),
                                                       winnow = c(FALSE, TRUE),
                                                       model = c("tree", "rules"))),
            c50_up = list(sampling_control = "up",
                          model_method = "C5.0",
                          model_params = expand.grid(trials = c(1, 10, 20, 50),
                                                     winnow = c(FALSE, TRUE),
                                                     model = c("tree", "rules"))),
            c50_down = list(sampling_control = "down",
                            model_method = "C5.0",
                            model_params = expand.grid(trials = c(1, 10, 20, 50),
                                                       winnow = c(FALSE, TRUE),
                                                       model = c("tree", "rules"))),
            c50_smote = list(sampling_control = "smote",
                             model_method = "C5.0",
                             model_params = expand.grid(trials = c(1, 10, 20, 50),
                                                        winnow = c(FALSE, TRUE),
                                                        model = c("tree", "rules"))),
            c50_rose = list(sampling_control = "rose",
                            model_method = "C5.0",
                            model_params = expand.grid(trials = c(1, 10, 20, 50),
                                                       winnow = c(FALSE, TRUE),
                                                       model = c("tree", "rules")))
        )
    } else if (model == "NB") {
        test_params = list(
            # Naive Bayes
            NB_null = list(sampling_control = NULL,
                           model_method = "nb",
                           model_params = NULL),
            NB_up = list(sampling_control = "up",
                         model_method = "nb",
                         model_params = NULL),
            NB_down = list(sampling_control = "down",
                           model_method = "nb",
                           model_params = NULL),
            NB_smote = list(sampling_control = "smote",
                            model_method = "nb",
                            model_params = NULL),
            NB_rose = list(sampling_control = "rose",
                           model_method = "nb",
                           model_params = NULL)
        )
    } else if (model == "XGB") {
        test_params = list(
            ## Gradient Boosting Machine
            XGBTree_null = list(sampling_control = NULL,
                                model_method = "xgbTree",
                                model_params = NULL),
            XGBTree_up = list(sampling_control = "up",
                              model_method = "xgbTree",
                              model_params = NULL),
            XGBTree_down = list(sampling_control = "down",
                                model_method = "xgbTree",
                                model_params = NULL),
            XGBTree_smote = list(sampling_control = "smote",
                                 model_method = "xgbTree",
                                 model_params = NULL),
            XGBTree_rose = list(sampling_control = "rose",
                                model_method = "xgbTree",
                                model_params = NULL)
        )
    } else if (model == "LR") {
        test_params = list(
            ## Gradient Boosting Machine
            LR_null = list(sampling_control = NULL,
                           model_method = "glmStepAIC",
                           model_params = NULL),
            LR_up = list(sampling_control = "up",
                         model_method = "glmStepAIC",
                         model_params = NULL),
            LR_down = list(sampling_control = "down",
                           model_method = "glmStepAIC",
                           model_params = NULL),
            LR_smote = list(sampling_control = "smote",
                            model_method = "glmStepAIC",
                            model_params = NULL),
            LR_rose = list(sampling_control = "rose",
                           model_method = "glmStepAIC",
                           model_params = NULL)
        )
    }  else { 
        test_params = NULL
    }
    
    return(test_params)
}

# Model Run ----
runModel = function(model, dados_treino, metrica_avaliacao) {
    
    # Data pre-processing -----
    
    dados_pred = dados_treino %>% select(-test_result)
    dados_resposta = dados_treino$test_result
    
    test_params = runParams(model, dados_pred)
    
    train_control = trainControl(method = "cv",
                                 number = 5,
                                 classProbs = TRUE, 
                                 verboseIter = FALSE,
                                 summaryFunction = aggStats,
                                 allowParallel = TRUE)
    
    # Run model ---------------------------------------------------------------
    for (i in seq_along(test_params)) {
        train_control$sampling = test_params[[i]]$sampling_control
        if(!(test_params[[i]]$model_method %in% c("xgbTree", "C5.0", "LR"))) {
            set.seed(2^31 - 1)
            model_train = train(y = dados_resposta,
                                x = dados_pred,
                                method = test_params[[i]]$model_method,
                                metric = metrica_avaliacao,
                                trControl = train_control, 
                                tuneGrid = test_params[[i]]$model_params)
        } else {
            set.seed(2^31 - 1)
            model_train = train(dados ~ .,
                                data = bind_cols(dados = dados_resposta, 
                                                 dados_pred),
                                method = test_params[[i]]$model_method,
                                metric = metrica_avaliacao,
                                trControl = train_control,
                                tuneGrid = test_params[[i]]$model_params)
        }
        save(model_train, file = paste0("Output/Main/Models/", metrica_avaliacao,"/model_train_GERAL_", names(test_params[i]), ".RData"))
        print(names(test_params[i]))
        if(i == length(test_params)) {
            print("Execution finished!")
        }
    }
}


# -----------------------------------------------------



# Data Preparation --------------------------------------------------------

# Input data -----
dados <- read_delim("Data/ddb_modelo_temporal_16-07_testes.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)


# Data preparation
dados = dados[-which(dados$test_result == "Inconclusivo"),]
names(dados)
summary(dados$test_result)
dados = dados %>%
    map_if(is.logical, as.character) %>%
    bind_cols() %>%
    select(-c(priority, date_of_birth)) %>% 
    replace(. == "TRUE", "Sim") %>%
    replace(. == "FALSE", "Nao") %>% 
    map_if(is.character, as.factor) %>% 
    bind_cols() %>%
    filter(Indicado == "Nao") %>%
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
              DiasAteDiagnostico, `NA-Sintoma`))
summary(dados)




# Partition data ----------------------------------------------------------
set.seed(2 ^ 31 - 1)
trainIndex = createDataPartition(dados$test_result,
                                 p = 0.8,
                                 list = FALSE,
                                 times = 1)[,1]
# Obtainting training set
dados_treino = dados[trainIndex, ]
# Obtainting test set
dados_teste = dados[-trainIndex, ]

summary(dados$test_result)

## "RF" -> Random Forest
## "DT" -> Decision Tree (C 5.0)
## "NB" -> Naive Bayes
## "XGB" -> Gradient Boosting
## "LR" -> Regressão Logística
model_set = c("LR", "RF", "DT", "NB", "XGB")
metrica_avaliacao = c("AUC")
for (j in 1:length(metrica_avaliacao)) {
    print(metrica_avaliacao[j])
    for (i in 1:length(model_set)){
        runModel(model_set[i], dados_treino, metrica_avaliacao[j])
    }
    
    # Logistic Regression
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_LR_null.RData"))
    treino_LR_null = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_LR_down.RData"))
    treino_LR_down = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_LR_up.RData"))
    treino_LR_up = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_LR_smote.RData"))
    treino_LR_smote = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_LR_rose.RData"))
    treino_LR_rose = model_train
    
    lista_treinos_LR = list(treino_LR_null, treino_LR_down, treino_LR_up, treino_LR_smote, treino_LR_rose)
    
    resultados_LR = lista_treinos_LR[[1]]$results
    resultados_LR$metodo = lista_treinos_LR[[1]]$method
    resultados_LR$sampling = ifelse(is.null(lista_treinos_LR[[1]]$control$sampling$name), "null", lista_treinos_LR[[1]]$control$sampling$name)
    valores = seq(1,18)
    
    resultados_LR = resultados_LR %>% select(c(32,33, valores))
    for (i in 2:length(lista_treinos_LR)){
        resultados_aux = lista_treinos_LR[[i]]$results
        resultados_aux$metodo = lista_treinos_LR[[i]]$method
        resultados_aux$sampling = ifelse(is.null(lista_treinos_LR[[i]]$control$sampling$name), "null", lista_treinos_LR[[i]]$control$sampling$name)
        resultados_aux = resultados_aux %>% select(c(32,33, valores))
        resultados_LR = bind_rows(resultados_LR, resultados_aux)
    }
    
    # Random Forest
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_RF_null.RData"))
    treino_RF_null = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_RF_down.RData"))
    treino_RF_down = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_RF_up.RData"))
    treino_RF_up = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_RF_smote.RData"))
    treino_RF_smote = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_RF_rose.RData"))
    treino_RF_rose = model_train
    
    lista_treinos_RF = list(treino_RF_null, treino_RF_down, treino_RF_up, treino_RF_smote, treino_RF_rose)
    
    resultados_RF = lista_treinos_RF[[1]]$results
    resultados_RF$metodo = lista_treinos_RF[[1]]$method
    resultados_RF$sampling = ifelse(is.null(lista_treinos_RF[[1]]$control$sampling$name), "null", lista_treinos_RF[[1]]$control$sampling$name)
    valores = seq(1,18)
    
    resultados_RF = resultados_RF %>% select(c(34,35, valores))
    for (i in 2:length(lista_treinos_RF)){
        resultados_aux = lista_treinos_RF[[i]]$results
        resultados_aux$metodo = lista_treinos_RF[[i]]$method
        resultados_aux$sampling = ifelse(is.null(lista_treinos_RF[[i]]$control$sampling$name), "null", lista_treinos_RF[[i]]$control$sampling$name)
        resultados_aux = resultados_aux %>% select(c(34,35, valores))
        resultados_RF = bind_rows(resultados_RF, resultados_aux)
    }
    
    # Decision Tree
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_c50_null.RData"))
    treino_C50_null = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_c50_down.RData"))
    treino_C50_down = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_c50_up.RData"))
    treino_C50_up = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_c50_smote.RData"))
    treino_C50_smote = model_train
    load(paste0("Output/Main/Models/", metrica_avaliacao[j],"/model_train_GERAL_c50_rose.RData"))
    treino_C50_rose = model_train
    
    lista_treinos_C50 = list(treino_C50_null, treino_C50_down, treino_C50_up, treino_C50_smote, treino_C50_rose)
    
    resultados_C50 = lista_treinos_C50[[1]]$results
    resultados_C50$metodo = lista_treinos_C50[[1]]$method
    resultados_C50$sampling = ifelse(is.null(lista_treinos_C50[[1]]$control$sampling$name), "null", lista_treinos_C50[[1]]$control$sampling$name)
    valores = seq(1,18)
    
    resultados_C50 = resultados_C50 %>% select(c(34,35, valores))
    for (i in 2:length(lista_treinos_C50)){
        resultados_aux = lista_treinos_C50[[i]]$results
        resultados_aux$metodo = lista_treinos_C50[[i]]$method
        resultados_aux$sampling = ifelse(is.null(lista_treinos_C50[[i]]$control$sampling$name), "null", lista_treinos_C50[[i]]$control$sampling$name)
        resultados_aux = resultados_aux %>% select(c(34,35, valores))
        resultados_C50 = bind_rows(resultados_C50, resultados_aux)
    }
    
    # Naive Bayes
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_NB_null.RData"))
    treino_NB_null = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_NB_down.RData"))
    treino_NB_down = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_NB_up.RData"))
    treino_NB_up = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_NB_smote.RData"))
    treino_NB_smote = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_NB_rose.RData"))
    treino_NB_rose = model_train
    
    lista_treinos_NB = list(treino_NB_null, treino_NB_down, treino_NB_up, treino_NB_smote, treino_NB_rose)
    
    resultados_NB = lista_treinos_NB[[1]]$results
    resultados_NB$metodo = lista_treinos_NB[[1]]$method
    resultados_NB$sampling = ifelse(is.null(lista_treinos_NB[[1]]$control$sampling$name), "null", lista_treinos_NB[[1]]$control$sampling$name)
    valores = seq(1,18)
    
    resultados_NB = resultados_NB %>% select(c(34,35, valores))
    for (i in 2:length(lista_treinos_NB)){
        resultados_aux = lista_treinos_NB[[i]]$results
        resultados_aux$metodo = lista_treinos_NB[[i]]$method
        resultados_aux$sampling = ifelse(is.null(lista_treinos_NB[[i]]$control$sampling$name), "null", lista_treinos_NB[[i]]$control$sampling$name)
        resultados_aux = resultados_aux %>% select(c(34,35, valores))
        resultados_NB = bind_rows(resultados_NB, resultados_aux)
    }
    
    # Gradient Boosting
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_XGBTree_null.RData"))
    treino_XGB_null = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_XGBTree_down.RData"))
    treino_XGB_down = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_XGBTree_up.RData"))
    treino_XGB_up = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_XGBTree_smote.RData"))
    treino_XGB_smote = model_train
    load(paste0("Output/Main/Models/",metrica_avaliacao[j],"/model_train_GERAL_XGBTree_rose.RData"))
    treino_XGB_rose = model_train
    
    lista_treinos_XGB = list(treino_XGB_null, treino_XGB_down, treino_XGB_up, treino_XGB_smote, treino_XGB_rose)
    
    resultados_XGB = lista_treinos_XGB[[1]]$results
    resultados_XGB$metodo = lista_treinos_XGB[[1]]$method
    resultados_XGB$sampling = ifelse(is.null(lista_treinos_XGB[[1]]$control$sampling$name), "null", lista_treinos_XGB[[1]]$control$sampling$name)
    valores = seq(1,22)
    
    resultados_XGB = resultados_XGB %>% select(c(38,39, valores))
    for (i in 2:length(lista_treinos_XGB)){
        resultados_aux = lista_treinos_XGB[[i]]$results
        resultados_aux$metodo = lista_treinos_XGB[[i]]$method
        resultados_aux$sampling = ifelse(is.null(lista_treinos_XGB[[i]]$control$sampling$name), "null", lista_treinos_XGB[[i]]$control$sampling$name)
        resultados_aux = resultados_aux %>% select(c(38,39, valores))
        resultados_XGB = bind_rows(resultados_XGB, resultados_aux)
    }
    
    
    ## Compile the results
    lista_treinos = c(lista_treinos_RF, lista_treinos_C50, lista_treinos_NB, lista_treinos_XGB, lista_treinos_LR)
    previsao = predict(lista_treinos[[1]], dados_teste)
    pred_num = ifelse(previsao == "Positivo", 1, 0)
    act_num = ifelse(dados_teste$test_result == "Positivo", 1, 0)
    mcc_val = mccr::mccr(pred = pred_num, act = act_num)
    auc = as.numeric(auc(roc(act_num, pred_num)))
    matriz_confusao = confusionMatrix(previsao, dados_teste$test_result, positive = "Positivo")
    metricas = data.frame(metodo = lista_treinos[[1]]$method,
                          sampling = ifelse(is.null(lista_treinos[[1]]$control$sampling$name), "null", lista_treinos[[1]]$control$sampling$name),
                          TN = matriz_confusao$table[1],
                          FP = matriz_confusao$table[2],
                          FN = matriz_confusao$table[3],
                          TP = matriz_confusao$table[4],
                          Accuracy = matriz_confusao$overall[[1]],
                          Kappa = matriz_confusao$overall[[2]],
                          Sensitivity = matriz_confusao$byClass[[1]],
                          Specificity = matriz_confusao$byClass[[2]],
                          PPV = matriz_confusao$byClass[[3]],
                          NPV = matriz_confusao$byClass[[4]],
                          Precision = matriz_confusao$byClass[[5]],
                          Recall = matriz_confusao$byClass[[6]],
                          F1 = matriz_confusao$byClass[[7]],
                          AUC = auc,
                          MCC = mcc_val)
    
    for (i in 2:length(lista_treinos)){
        previsao = predict(lista_treinos[[i]], dados_teste)
        pred_num = ifelse(previsao == "Positivo", 1, 0)
        act_num = ifelse(dados_teste$test_result == "Positivo", 1, 0)
        mcc_val = mccr::mccr(pred = pred_num, act = act_num)
        auc = as.numeric(auc(roc(act_num, pred_num)))
        matriz_confusao = confusionMatrix(previsao, dados_teste$test_result, positive = "Positivo")
        metricas_aux = data.frame(metodo = lista_treinos[[i]]$method,
                                  sampling = ifelse(is.null(lista_treinos[[i]]$control$sampling$name), "null", lista_treinos[[i]]$control$sampling$name),
                                  TN = matriz_confusao$table[1],
                                  FP = matriz_confusao$table[2],
                                  FN = matriz_confusao$table[3],
                                  TP = matriz_confusao$table[4],
                                  Accuracy = matriz_confusao$overall[[1]],
                                  Kappa = matriz_confusao$overall[[2]],
                                  Sensitivity = matriz_confusao$byClass[[1]],
                                  Specificity = matriz_confusao$byClass[[2]],
                                  PPV = matriz_confusao$byClass[[3]],
                                  NPV = matriz_confusao$byClass[[4]],
                                  Precision = matriz_confusao$byClass[[5]],
                                  Recall = matriz_confusao$byClass[[6]],
                                  F1 = matriz_confusao$byClass[[7]],
                                  AUC = auc,
                                  MCC = mcc_val)
        metricas = bind_rows(metricas, metricas_aux)
    }
    
    rm(list=setdiff(ls(),
                    c("resultados_RF", "resultados_LR", "resultados_C50", "resultados_NB",
                      "resultados_XGB", "metricas", "dados", "model_set", "metrica_avaliacao",
                      "dados_teste", "dados_treino", "i", "j", "path", "aggStats", "runModel",
                      "runParams", "mcc")))
    
    write_csv2(metricas, paste0("Output/Main/Models/", metrica_avaliacao[j],"/metricas_melhores_modelos_", metrica_avaliacao[j], ".csv"))
    write_csv2(resultados_RF, paste0("Output/Main/Models/", metrica_avaliacao[j],"/resultados_treinamento_RF_", metrica_avaliacao[j], ".csv"))
    write_csv2(resultados_C50, paste0("Output/Main/Models/", metrica_avaliacao[j],"/resultados_treinamento_DT_", metrica_avaliacao[j], ".csv"))
    write_csv2(resultados_NB, paste0("Output/Main/Models/", metrica_avaliacao[j],"/resultados_treinamento_NB_", metrica_avaliacao[j], ".csv"))
    write_csv2(resultados_XGB, paste0("Output/Main/Models/", metrica_avaliacao[j],"/resultados_treinamento_XGB_", metrica_avaliacao[j], ".csv"))
    write_csv2(resultados_LR, paste0("Output/Main/Models/", metrica_avaliacao[j],"/resultados_treinamento_LR_", metrica_avaliacao[j], ".csv"))
    rm(list=setdiff(ls(), c("dados", "model_set", "metrica_avaliacao",
                            "dados_teste", "dados_treino", "i", "j",
                            "path", "aggStats", "runModel", "runParams", "mcc")))
    
}








# ------------------------------------------------

# COMPARISON MCC

Comparison_MCC <- read_delim("Output/Main/Models/MCC_comparison.csv", ";",
                             escape_double = FALSE, trim_ws = TRUE) %>% print()

Comparison_MCC <- Comparison_MCC %>%
    gather(key = "Strategies", value = "MCC", Downsampling, Upsampling, SMOTE, ROSE) %>%
    rstatix::convert_as_factor(Method, Strategies)
head(Comparison_MCC, 3)

Comparison_MCC %>%
    group_by(Strategies) %>%
    rstatix::get_summary_stats(MCC, type = "common")

Methods <- factor(Comparison_MCC$Method, level = c("Logistic Regression","Random Forest","Gradient Boosting","Decision Tree","Naive Bayes"))
Comparison_MCC %>% ggplot(aes(x=Methods, y=MCC))+
    geom_boxplot(show.legend = F) +
    geom_point(position=position_jitterdodge(jitter.width=0, dodge.width = 0), 
               pch=21, size=3, aes(fill=Strategies), show.legend = T) +
    xlab("") +
    theme_classic() +
    theme(text = element_text(size=10)) + 
    ggsave("Output/Main/figure2_results_methods_MCC.tiff")

summary(dados)





# -----------------------------------------------------------------

# PREDICTIVE VALUES (TESTING SET)
load("Output/Main/Models/AUC/model_train_GERAL_LR_up.RData")
summary(model_train)
previsao <- predict(model_train, dados_teste, type = "prob")[,2]
act_num = ifelse(dados_teste$test_result == "Positivo", 1, 0)
pred_num<-factor(ifelse(previsao > 0.5, 1, 0))
mcc_val = mccr::mccr(pred = pred_num, act = act_num)
auc = as.numeric(auc(roc(act_num, previsao)))
testRESULTS<-data.frame(RESULTS = factor(act_num))
matriz_confusao = confusionMatrix(data=pred_num, reference=testRESULTS$RESULTS, positive = levels(testRESULTS$RESULTS)[2])
matriz_confusao






# ----------------------------------------
# OUT OF SAMPLE DATA

dados_outofsample <- read_delim("Data/ddb_modelo_temporal_16-07_semtestes.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)

dados_outofsample = dados_outofsample %>%
    map_if(is.logical, as.character) %>%
    bind_cols() %>%
    select(-c(priority, date_of_birth)) %>% 
    replace(. == "TRUE", "Sim")%>%
    replace(. == "FALSE", "Nao") %>% 
    map_if(is.character, as.factor) %>% 
    bind_cols()%>%
    filter(Indicado=="Nao") %>%
    bind_cols()%>%
    filter(SemanaEpidemiologica>=0) %>%
    bind_cols()%>%
    filter(DiasAteDiagnostico>=0) %>%
    bind_cols()%>% 
    filter(idade_anos<=100) %>%
    bind_cols()%>% 
    select(-c(Cirrose, Cancer, Transplantado, Diabetes, Obesidade,
              GestacaoRisco, DoenteFigado, TratamentoImunossupressor,
              Renal, Cardiaco, ProblemaPulmonar, Indicado, `NA-Risco`, `NA-Sintoma`,
              ProfissionalSaude, DataDosSintomas, SemanaEpidemiologica, DiasAteDiagnostico,
              Coluna1, test_result))

summary(model_train)
previsao_outofsample <- predict(model_train, dados_outofsample, type = "prob")[,2]
value_class<-factor(ifelse(previsao_outofsample > 0.5, 1, 0))
previsao_outofsample2<-data.frame(previsao_outofsample, value_class)
summary(previsao_outofsample2$value_class)
99431/287714
write_csv(previsao_outofsample2, "Output/Main/probabilidades_outofsample2.csv")







# ----------------------------------------------------------------
# Density plot by groups (testing set)

submit <- data.frame(Observed=testRESULTS$RESULTS, RESULT = previsao)
submit <- as_tibble(submit)

previsao_outofsample <- data.frame(previsao_outofsample)

previsao_outofsample <- 
    previsao_outofsample %>% rename(probability = previsao_outofsample) %>% print()
submit <- 
    submit %>% rename(Predicted_Value = RESULT)

Observed <- NULL
Observed[submit$Observed == 0] <- "Negative"
Observed[submit$Observed == 1] <- "Positive"
Observed <- as.factor(Observed)
Observed <- as.data.frame(Observed)
submit <- cbind(Observed,submit[,c(2)])

mu <- plyr::ddply(submit, "Observed", summarise, grp.mean=mean(Predicted_Value))


ggplot(submit, aes(x=Predicted_Value, fill=Observed)) +
    scale_x_continuous(limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
    scale_y_continuous(breaks=seq(0,1200,by=200)) +
    geom_density(alpha=.5, aes(colour = Observed, y = ..density..*(9950*0.033)),
                 size=1, fill="white")+
    geom_histogram(aes(y = ..count..), alpha=.3,
                   position="identity") + 
    theme_classic() +
    ylab("Frequency")+
    xlab("Predicted value")+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=Observed),
               linetype="dashed") +
    geom_vline(xintercept = 0.5) +
    theme(text = element_text(size=12)) + 
    ggsave("Output/Supplementary/figure_density_count1.tiff")






# --------------------------------------
# ERRORS ANALYSIS

previsao2 <- predict(model_train, newdata = dados_teste, type = "raw")

matriz_confusao

dados_after_pred <- data.frame(Predicted_result = previsao2,
                               dados_teste)
dados_after_pred <- as_tibble(dados_after_pred) %>% print()


false_positive <- filter(dados_after_pred, Predicted_result == "Positivo" &
                             test_result == "Negativo")
false_negative <- filter(dados_after_pred, Predicted_result == "Negativo" &
                             test_result == "Positivo")
summary(false_positive)
summary(false_negative)


previsao_outofsample_raw <- predict(model_train, dados_outofsample, type = "raw")
summary(previsao_outofsample_raw)
99431/287714
