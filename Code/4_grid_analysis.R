# ########################################
# AVAILABLE 25/02/2021
# App-based symptom tracking to optimize SARS-CoV-2 testing strategy using machine learning
# ########################################



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(gridExtra)

# --------------------------------------------------
# GRID ANALYSIS

analise_grids2 <- read_delim("Output/Main/analise_grid.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)
analise_grids2 <- filter(analise_grids2, total_users>=10)

# Summary:
# Very low <-1.5
# Low between -1.5 e -0.5
# Medium between -0.5 e 0.5
# Hight between 0.5 e 1.5
# Very High > 1.5

summary(analise_grids2)

very_low <- mean(analise_grids2$total_risco) - 1.5*sd(analise_grids2$total_risco)
low <- mean(analise_grids2$total_risco) - 0.5*sd(analise_grids2$total_risco)
medium <- mean(analise_grids2$total_risco)
high <- mean(analise_grids2$total_risco) + 0.5*sd(analise_grids2$total_risco)
very_high <- mean(analise_grids2$total_risco) + 1.5*sd(analise_grids2$total_risco)

cut_off <- cbind(very_low, low, medium, high, very_high)
rownames(cut_off) <- c("Group")
cut_off <- t(cut_off)
cut_off

very_low_risk <- filter(analise_grids2, total_risco<very_low)
low_risk <- filter(analise_grids2, total_risco>=very_low & total_risco<low)
medium_risk <- filter(analise_grids2, total_risco>=low & total_risco<high)
high_risk <- filter(analise_grids2, total_risco>=high & total_risco<very_high)
very_high_risk <- filter(analise_grids2, total_risco>=very_high)

very_low_risk <- nrow(very_low_risk)
low_risk <- nrow(low_risk)
medium_risk <- nrow(medium_risk)
high_risk <- nrow(high_risk)
very_high_risk <- nrow(very_high_risk)
count_groups <- cbind(very_low_risk, low_risk, medium_risk,
                      high_risk, very_high_risk)
count_groups <- t(count_groups)
colnames(count_groups) <- c("Number of grids")

Groups <- c("Very low [0.00-0.17[", "Low [0.17-0.33[","Medium [0.33-0.48[","High[0.48-0.63[", "Very high [0.63-1.00]")
table <- cbind(Groups, count_groups)

group <- NULL
analise_grids2$group[analise_grids2$total_risco<very_low] <- "Very_low"
analise_grids2$group[analise_grids2$total_risco>=very_low & analise_grids2$total_risco<low] <- "Low"
analise_grids2$group[analise_grids2$total_risco>=low & analise_grids2$total_risco<high] <- "Medium"
analise_grids2$group[analise_grids2$total_risco>=high & analise_grids2$total_risco<very_high] <- "High"
analise_grids2$group[analise_grids2$total_risco>=very_high] <- "Very_high"
analise_grids2$group <- as.factor(analise_grids2$group)

cut_off <- data.table(cut_off)
Risk_Group <- c("Very low", "Low", "Medium", "High", "Very high")
Risk <- cbind(Risk_Group, cut_off)
Risk <- Risk[c(1,2,4,5)]
analise_grids2$group <- factor(analise_grids2$group, levels = c("Very_low", "Low", "Medium","High","Very_high"))

ggplot(analise_grids2, aes(x=total_risco, fill=group)) +
    scale_x_continuous(limits=c(0,1), breaks=c(0,0.2,0.4,0.6,0.8,1)) +
    geom_density(alpha=.5, aes(y = ..density..*(2400*0.023)),
                 size=1, fill="white")+
    geom_histogram(aes(y = ..count..),bins=50,
                   position="identity", alpha=0.7) +
    # scale_fill_manual(values=c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15"))+
    scale_fill_manual(values=c("#74C476", "#A1D99B", "#FEB24C", "#FB6A4A", "#CB181D"))+
    # scale_fill_grey(start = 0.8, end = 0.2)+
    theme_classic() +
    ylab("Frequency")+
    xlab("Grid risk")+
    geom_vline(data=Risk, aes(xintercept=Group),
               linetype="dashed", show.legend = F) +
    geom_text(aes(x = cut_off[[1,1]], y = 0, label = 0.17), size=4)+
    geom_text(aes(x = cut_off[[2,1]], y = 0, label = 0.33), size=4)+
    geom_text(aes(x = cut_off[[4,1]], y = 0, label = 0.48), size=4)+
    geom_text(aes(x = cut_off[[5,1]], y = 0, label = 0.63), size=4)+
    annotation_custom(tableGrob(table, rows=NULL), xmin=0.8, xmax=1, ymin=100, ymax=200)+
    theme(text = element_text(size=12)) + 
    ggsave("Output/Supplementary/figure_grid_risk.tiff", width = 12, height = 6)

summarize(group_by(analise_grids2, group), mean(total_users))

