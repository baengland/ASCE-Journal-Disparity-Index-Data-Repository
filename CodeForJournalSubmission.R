#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("nortest")
#install.packages("dplyr")
#install.packages("mosaic")
#install.packages("devtools")
#install.packages("COINr")
#install.packages("gridExtra")
#install.packages("DiagrammeR")
#install.packages("DiagrammeRsvg")
#install.packages("rsvg")
#install.packages("magrittr")

library(tidyverse)
library(ggplot2)
library(readxl)
library(nortest)
library(dplyr)
library(mosaic)
library(devtools)
library(COINr)
library(gridExtra)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(magrittr)

#####Drinking Water Disparity Index#####

###Mean Imputation###

##Data Importation##
MeanImputedDrinkingWaterData <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\MeanImputationJournalWaterTreatmentPlantDataNoDistribution.xlsx", sheet = "Summary")

##Normality Testing##
#1-Sample Kolmogorov-Smirnov Normality Test#
ks.test(MeanImputedDrinkingWaterData$Population, "pnorm")
ks.test(MeanImputedDrinkingWaterData$Age, "pnorm")
ks.test(MeanImputedDrinkingWaterData$Violations, "pnorm")
ks.test(MeanImputedDrinkingWaterData$WaterLoss, "pnorm")
ks.test(MeanImputedDrinkingWaterData$API, "pnorm")

#Shapiro-Wilk Normality Test#
shapiro.test(MeanImputedDrinkingWaterData$Population)
shapiro.test(MeanImputedDrinkingWaterData$Age)
shapiro.test(MeanImputedDrinkingWaterData$Violations)
shapiro.test(MeanImputedDrinkingWaterData$WaterLoss)
shapiro.test(MeanImputedDrinkingWaterData$API)

#Anderson-Darling Normality Test#
ad.test(MeanImputedDrinkingWaterData$Population)
ad.test(MeanImputedDrinkingWaterData$Age)
ad.test(MeanImputedDrinkingWaterData$Violations)
ad.test(MeanImputedDrinkingWaterData$WaterLoss)
ad.test(MeanImputedDrinkingWaterData$API)

##Correlation Testing##
#Spearman Rank Correlation Test#
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$Age, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$Violations, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$WaterLoss, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$API, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$Violations, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$WaterLoss, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$API, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Violations, MeanImputedDrinkingWaterData$WaterLoss, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$Violations, MeanImputedDrinkingWaterData$API, method = "spearman")
cor.test(MeanImputedDrinkingWaterData$WaterLoss, MeanImputedDrinkingWaterData$API, method = "spearman")

#2-Sample Kolmogorov-Smirnov Correlation Test#
ks.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$Age)
ks.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$Violations)
ks.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$WaterLoss)
ks.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$API)
ks.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$Violations)
ks.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$WaterLoss)
ks.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$API)
ks.test(MeanImputedDrinkingWaterData$Violations, MeanImputedDrinkingWaterData$WaterLoss)
ks.test(MeanImputedDrinkingWaterData$Violations, MeanImputedDrinkingWaterData$API)
ks.test(MeanImputedDrinkingWaterData$WaterLoss, MeanImputedDrinkingWaterData$API)

#Kendall Tau Rank Correlation Test#
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$Age, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$Violations, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$WaterLoss, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Population, MeanImputedDrinkingWaterData$API, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$Violations, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$WaterLoss, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Age, MeanImputedDrinkingWaterData$API, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Violations, MeanImputedDrinkingWaterData$WaterLoss, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$Violations, MeanImputedDrinkingWaterData$API, method = "kendall")
cor.test(MeanImputedDrinkingWaterData$WaterLoss, MeanImputedDrinkingWaterData$API, method = "kendall")

##Meta File Importation##
MeanImputedWaterMeta <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\JournalWaterTreatmentMeta.xlsx")

##Drinking Water Disparity Index Set-Up Code##
MeanImputedWaterDisparityIndex <- new_coin(iData = MeanImputedDrinkingWaterData, iMeta = MeanImputedWaterMeta, level_names = c("Indicators", "Groups", "Index"))

##Normalization##
MeanImputedWaterDisparityIndex <- Normalise(MeanImputedWaterDisparityIndex, dset = "Raw", global_specs = list(f_n = "n_minmax", f_n_para = list(c(1,100))))

##Aggregation##
MeanImputedWaterDisparityIndex <- Aggregate(MeanImputedWaterDisparityIndex, dset = "Normalised")

##Uncertainty and Sensitivity Analysis##
norm_alts <- list(list(f_n = "n_minmax", f_n_para = list(c(1,100))), list(f_n = "n_zscore", f_n_para = list(c(10,2))), list(f_n = "n_rank"))
l_norm <- list(Address = "$Log$Normalise$global_specs", Distribution = norm_alts, Type = "discrete")

w_nom <- MeanImputedWaterDisparityIndex$Meta$Weights$Original
noise_specs <-  data.frame(Level = c(1,2), NoiseFactor = c(0.25, 0.25))
noisy_wts <- get_noisy_weights(w = w_nom, noise_specs = noise_specs, Nrep = 200)
l_weights <- list(Address = "$Log$Aggregate$w", Distribution = noisy_wts, Type = "discrete")

l_agg <- list(Address = "$Log$Aggregate$f_ag", Distribution = c("a_amean", "a_gmean"), Type = "discrete")

SA_specs <- list(Normalisation = l_norm, Weights = l_weights, Aggregation = l_agg)

MeanImputedWaterUA <- get_sensitivity(MeanImputedWaterDisparityIndex, SA_specs = SA_specs, N = 5000, SA_type = "UA", dset = "Aggregated", iCode = "Index")

MeanImputedWaterSA <- get_sensitivity(MeanImputedWaterDisparityIndex, SA_specs = SA_specs, N = 5000, SA_type = "SA", dset = "Aggregated", iCode = "Index", Nboot = 5000)

MeanImputedWaterIndicatorRemoval <- remove_elements(MeanImputedWaterDisparityIndex, Level = 1, dset = "Aggregated", iCode = "Index")

##Making Plots##
MeanImputationWaterResultsWithRurality <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                                     Index = c(22, 38, 43, 47, 49, 51, 51, 52, 55, 68, 68, 73, 83),
                                                     Rurality = c("Urban", "Rural", "Rural", "Rural", "Urban", "Rural", "Urban", "Rural", "Rural", "Rural", "Urban", "Rural", "Rural"))

MeanImputedWaterResultsWithRuralityPlot <- ggplot(data = MeanImputationWaterResultsWithRurality, aes(x = County, y = Index, fill = Rurality)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MeanImputationWaterResultsWithRurality$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  scale_fill_manual(values = c("#008000", "#0000FF")) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MeanImputedWaterResultsWithRuralityPlot.png", MeanImputedWaterResultsWithRuralityPlot, width = 28, height = 15, units = "cm")

MeanImputationWaterResultsWithEconomics <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                                     Index = c(22, 38, 43, 47, 49, 51, 51, 52, 55, 68, 68, 73, 83),
                                                     Economics = c("At-Risk", "Distressed", "At-Risk", "Distressed", "Transitional", "Transitional", "Attainment", "Distressed", "Transitional", "At-Risk", "Competitive", "Competitive", "Competitive"))

MeanImputedWaterResultsWithEconomicsPlot <- ggplot(data = MeanImputationWaterResultsWithEconomics, aes(x = County, y = Index, fill = Economics)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MeanImputationWaterResultsWithEconomics$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_fill_manual(values = c("Distressed" = "#FF0000", "At-Risk" = "#FFCC66", "Transitional" = "#76EEC6", "Competitive" = "#9999FF", "Attainment" = "#3A5FCD"), breaks = c("Distressed", "At-Risk", "Transitional", "Competitive", "Attainment")) + 
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MeanImputedWaterResultsWithEconomicsPlot.png", MeanImputedWaterResultsWithEconomicsPlot, width = 28, height = 15, units = "cm")

MeanImputedWaterUncertainty <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                          Nominal = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                                          Median = c(13, 12, 10, 10, 8, 8, 10, 6, 5, 3, 4, 2, 1),
                                          Lower = c(13, 12, 11, 12, 10, 9, 12, 9, 8, 4, 4, 3, 1),
                                          Upper = c(13, 10, 7, 6, 6, 6, 6, 5, 5, 2, 3, 2, 1))

MeanImputedWaterUncertaintyPlot <- ggplot(MeanImputedWaterUncertainty, aes(x = County, y = Median)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) + 
  xlab("Counties") + ylab("Rank") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 20), axis.title.x = element_text(vjust = 0.5), axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_y_continuous(trans = "reverse", breaks = c(13,11,9,7,5,3,1)) + 
  scale_x_discrete(limits = MeanImputedWaterUncertainty$County) + 
  geom_point(aes(x = County, y = Nominal), shape = 4, size = 4)

ggsave("MeanImputedWaterUncertaintyPlot.png", MeanImputedWaterUncertaintyPlot, width = 28, height = 12, units = "cm")

MeanImputedWaterSABarPlotData <- data.frame(Step = c("Normalization", "Normalization", "Weights", "Weights", "Aggregation", "Aggregation"),
                                            Value = c(0.057, 0.259, 0.160, 0.222, 0.4, 0.324),
                                            Type = c("Main Effect", "Interactions", "Main Effect", "Interactions", "Main Effect", "Interactions"))

MeanImputedWaterSABarPlot <- ggplot(MeanImputedWaterSABarPlotData, aes(x = Step, y = Value, fill = Type)) + 
  geom_bar(stat = "identity", position = "stack", width = 1.5) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme(text = element_text(family = "Times", face = "plain", size = 16)) + 
  scale_x_discrete(limits = MeanImputedWaterSABarPlotData$Step)

ggsave("MeanImputedWaterSABarPlot.png", MeanImputedWaterSABarPlot, width = 18, height = 12, units = "cm")

MeanImputedWaterSAPieChartData <- data.frame(Step = c("Normalization", "Weights", "Aggregation", "Interactions"),
                                             Value = c(0.057, 0.160, 0.400, 0.383))

MeanImputedWaterSAPieChart <- ggplot(data = MeanImputedWaterSAPieChartData, aes(x = "", y = Value, fill = Step)) + 
  geom_bar(stat = "identity", width = 2) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  scale_fill_manual(values = c("#669900", "#FF66CC", "#0000CC", "#990000")) + 
  theme(text = element_text(family = "Times New Roman", face = "plain", size = 18))

ggsave("MeanImputedWaterSAPieChart.png", MeanImputedWaterSAPieChart, width = 18, height = 12, units = "cm")

MeanImputedWaterSAFirstOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"),
                                                      Si = c(0.057, 0.160, 0.400),
                                                      Siq5 = c(0.011, 0.108, 0.332),
                                                      Siq95 = c(0.101, 0.211, 0.467))

MeanImputedWaterSAFirstOrderSensitivityPlot <- ggplot(MeanImputedWaterSAFirstOrderSensitivity, aes(x = Step, y = Si)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Siq95, ymin = Siq5)) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MeanImputedWaterSAFirstOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Main Effect Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

MeanImputedWaterSATotalOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"), 
                                                      STi = c(0.316, 0.381, 0.724),
                                                      STiq5 = c(0.303, 0.367, 0.690),
                                                      STiq95 = c(0.329, 0.396, 0.759))

MeanImputedWaterSATotalOrderSensitivityPlot <- ggplot(MeanImputedWaterSATotalOrderSensitivity, aes(x = Step, y = STi)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = STiq95, ymin = STiq5)) + 
  xlab("Index Step") + ylab("") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MeanImputedWaterSATotalOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Interactions Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

MeanImputedWaterSAConfidencePlot <- grid.arrange(MeanImputedWaterSAFirstOrderSensitivityPlot, MeanImputedWaterSATotalOrderSensitivityPlot, nrow = 1)

ggsave("MeanImputedWaterSAConfidencePlot.png", MeanImputedWaterSAConfidencePlot, width = 25, height = 12, units = "cm")

MeanOfWaterRanks <- mean(MeanImputedWaterANOVAData$Rank)

SDOfWaterRanks <- sd(MeanImputedWaterANOVAData$Rank)

MeanImputedWaterANOVAData <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                        Rank = c(22, 38, 43, 47, 49, 51, 51, 52, 55, 68, 68, 73, 83),
                                        Rurality = c("Urban", "Rural", "Rural", "Rural", "Urban", "Rural", "Urban", "Rural", "Rural", "Rural", "Urban", "Rural", "Rural"),
                                        Economics = c("At-Risk", "Distressed", "At-Risk", "Distressed", "Transitional", "Transitional", "Attainment", "Distressed", "Transitional", "At-Risk", "Competitive", "Competitive", "Competitive"),
                                        Normalized = ((MeanImputedWaterANOVAData$Rank - MeanOfWaterRanks) / SDOfWaterRanks))

MeanImputedWaterTwoWayANOVAModel <- aov(Normalized ~ Rurality * Economics, data = MeanImputedWaterANOVAData)

NonNormalizedMeanImputedWaterTwoWayANOVAModel <- aov(Rank ~ Rurality * Economics, data = MeanImputedWaterANOVAData)

###Median Imputation###

##Data Importation##
MedianImputedDrinkingWaterData <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\MedianImputationJournalWaterTreatmentPlantDataNoDistribution.xlsx", sheet = "Summary")

##Normality Testing##
#1-Sample Kolmogorov-Smirnov Normality Test#
ks.test(MedianImputedDrinkingWaterData$Population, "pnorm")
ks.test(MedianImputedDrinkingWaterData$Age, "pnorm")
ks.test(MedianImputedDrinkingWaterData$Violations, "pnorm")
ks.test(MedianImputedDrinkingWaterData$WaterLoss, "pnorm")
ks.test(MedianImputedDrinkingWaterData$API, "pnorm")

#Shapiro-Wilk Normality Test#
shapiro.test(MedianImputedDrinkingWaterData$Population)
shapiro.test(MedianImputedDrinkingWaterData$Age)
shapiro.test(MedianImputedDrinkingWaterData$Violations)
shapiro.test(MedianImputedDrinkingWaterData$WaterLoss)
shapiro.test(MedianImputedDrinkingWaterData$API)

#Anderson-Darling Normality Test#
ad.test(MedianImputedDrinkingWaterData$Population)
ad.test(MedianImputedDrinkingWaterData$Age)
ad.test(MedianImputedDrinkingWaterData$Violations)
ad.test(MedianImputedDrinkingWaterData$WaterLoss)
ad.test(MedianImputedDrinkingWaterData$API)

##Correlation Testing##
#Spearman Rank Correlation Test#
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$Age, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$Violations, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$WaterLoss, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$API, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$Violations, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$WaterLoss, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$API, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Violations, MedianImputedDrinkingWaterData$WaterLoss, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$Violations, MedianImputedDrinkingWaterData$API, method = "spearman")
cor.test(MedianImputedDrinkingWaterData$WaterLoss, MedianImputedDrinkingWaterData$API, method = "spearman")

#2-Sample Kolmogorov-Smirnov Correlation Test#
ks.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$Age)
ks.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$Violations)
ks.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$WaterLoss)
ks.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$API)
ks.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$Violations)
ks.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$WaterLoss)
ks.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$API)
ks.test(MedianImputedDrinkingWaterData$Violations, MedianImputedDrinkingWaterData$WaterLoss)
ks.test(MedianImputedDrinkingWaterData$Violations, MedianImputedDrinkingWaterData$API)
ks.test(MedianImputedDrinkingWaterData$WaterLoss, MedianImputedDrinkingWaterData$API)

#Kendall Tau Rank Correlation Test#
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$Age, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$Violations, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$WaterLoss, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Population, MedianImputedDrinkingWaterData$API, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$Violations, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$WaterLoss, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Age, MedianImputedDrinkingWaterData$API, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Violations, MedianImputedDrinkingWaterData$WaterLoss, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$Violations, MedianImputedDrinkingWaterData$API, method = "kendall")
cor.test(MedianImputedDrinkingWaterData$WaterLoss, MedianImputedDrinkingWaterData$API, method = "kendall")

##Meta File Importation##
MedianImputedWaterMeta <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\JournalWaterTreatmentMeta.xlsx")

##Drinking Water Disparity Index Set-Up Code##
MedianImputedWaterDisparityIndex <- new_coin(iData = MedianImputedDrinkingWaterData, iMeta = MedianImputedWaterMeta, level_names = c("Indicators", "Groups", "Index"))

##Normalization##
MedianImputedWaterDisparityIndex <- Normalise(MedianImputedWaterDisparityIndex, dset = "Raw", global_specs = list(f_n = "n_minmax", f_n_para = list(c(1,100))))

##Aggregation##
MedianImputedWaterDisparityIndex <- Aggregate(MedianImputedWaterDisparityIndex, dset = "Normalised")

##Uncertainty and Sensitivity Analysis##
MedianImputedWaterUA <- get_sensitivity(MedianImputedWaterDisparityIndex, SA_specs = SA_specs, N = 5000, SA_type = "UA", dset = "Aggregated", iCode = "Index")

MedianImputedWaterSA <- get_sensitivity(MedianImputedWaterDisparityIndex, SA_specs = SA_specs, N = 5000, SA_type = "SA", dset = "Aggregated", iCode = "Index", Nboot = 5000)

MedianImputedWaterIndicatorRemoval <- remove_elements(MedianImputedWaterDisparityIndex, Level = 1, dset = "Aggregated", iCode = "Index")

##Making Plots##
MedianImputationWaterResultsWithRurality <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                                       Index = c(22, 38, 45, 47, 49, 51, 51, 53, 55, 68, 68, 73, 83),
                                                       Rurality = c("Urban", "Rural", "Rural", "Rural", "Urban", "Rural", "Urban", "Rural", "Rural", "Rural", "Urban", "Rural", "Rural"))

MedianImputedWaterResultsWithRuralityPlot <- ggplot(data = MedianImputationWaterResultsWithRurality, aes(x = County, y = Index, fill = Rurality)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MedianImputationWaterResultsWithRurality$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  scale_fill_manual(values = c("#008000", "#0000FF")) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MedianImputedWaterResultsWithRuralityPlot.png", MedianImputedWaterResultsWithRuralityPlot, width = 28, height = 15, units = "cm")

MedianImputationWaterResultsWithEconomics <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                                       Index = c(22, 38, 45, 47, 49, 51, 51, 53, 55, 68, 68, 73, 83),
                                                       Economics = c("At-Risk", "Distressed", "At-Risk", "Distressed", "Transitional", "Transitional", "Attainment", "Distressed", "Transitional", "At-Risk", "Competitive", "Competitive", "Competitive"))

MedianImputedWaterResultsWithEconomicsPlot <- ggplot(data = MedianImputationWaterResultsWithEconomics, aes(x = County, y = Index, fill = Economics)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MedianImputationWaterResultsWithEconomics$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_fill_manual(values = c("Distressed" = "#FF0000", "At-Risk" = "#FFCC66", "Transitional" = "#76EEC6", "Competitive" = "#9999FF", "Attainment" = "#3A5FCD"), breaks = c("Distressed", "At-Risk", "Transitional", "Competitive", "Attainment")) + 
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MedianImputedWaterResultsWithEconomicsPlot.png", MedianImputedWaterResultsWithEconomicsPlot, width = 28, height = 15, units = "cm")

MedianImputedWaterUncertainty <- data.frame(County = c("Carter", "Bledsoe", "Carroll", "Scott", "Sullivan", "Lawrence", "Williamson", "Cocke", "Smith", "Houston", "Davidson", "Cheatham", "Wilson"),
                                            Nominal = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                                            Median = c(13, 12, 10, 10, 9, 8, 10, 6, 5, 3, 4, 2, 1),
                                            Lower = c(13, 12, 11, 12, 11, 9, 12, 9, 8, 4, 4, 3, 1),
                                            Upper = c(13, 10, 7, 6, 6, 6, 6, 5, 5, 2, 3, 2, 1))

MedianImputedWaterUncertaintyPlot <- ggplot(MedianImputedWaterUncertainty, aes(x = County, y = Median)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) + 
  xlab("Counties") + ylab("Rank") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 20), axis.title.x = element_text(vjust = 0.5), axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_y_continuous(trans = "reverse", breaks = c(13,11,9,7,5,3,1)) + 
  scale_x_discrete(limits = MedianImputedWaterUncertainty$County) + 
  geom_point(aes(x = County, y = Nominal), shape = 4, size = 4)

ggsave("MedianImputedWaterUncertaintyPlot.png", MedianImputedWaterUncertaintyPlot, width = 28, height = 12, units = "cm")

MedianImputedWaterSABarPlotData <- data.frame(Step = c("Normalization", "Normalization", "Weights", "Weights", "Aggregation", "Aggregation"),
                                              Value = c(0, 0.364, 0.236, 0.151, 0.421, 0.265),
                                              Type = c("Main Effect", "Interactions", "Main Effect", "Interactions", "Main Effect", "Interactions"))

MedianImputedWaterSABarPlot <- ggplot(MedianImputedWaterSABarPlotData, aes(x = Step, y = Value, fill = Type)) + 
  geom_bar(stat = "identity", position = "stack", width = 1.5) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme(text = element_text(family = "Times", face = "plain", size = 16)) + 
  scale_x_discrete(limits = MedianImputedWaterSABarPlotData$Step)

ggsave("MedianImputedWaterSABarPlot.png", MedianImputedWaterSABarPlot, width = 18, height = 12, units = "cm")

MedianImputedWaterSAPieChartData <- data.frame(Step = c("Normalization", "Weights", "Aggregation", "Interactions"),
                                             Value = c(0, 0.236, 0.421, 0.343))

MedianImputedWaterSAPieChart <- ggplot(data = MedianImputedWaterSAPieChartData, aes(x = "", y = Value, fill = Step)) + 
  geom_bar(stat = "identity", width = 2) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  scale_fill_manual(values = c("#669900", "#FF66CC", "#0000CC", "#990000")) + 
  theme(text = element_text(family = "Times New Roman", face = "plain", size = 18))

ggsave("MedianImputedWaterSAPieChart.png", MedianImputedWaterSAPieChart, width = 18, height = 12, units = "cm")

MedianImputedWaterSAFirstOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"),
                                                        Si = c(0, 0.236, 0.421), 
                                                        Siq5 = c(0, 0.182, 0.354), 
                                                        Siq95 = c(0.041, 0.290, 0.489))

MedianImputedWaterSAFirstOrderSensitivityPlot <- ggplot(MedianImputedWaterSAFirstOrderSensitivity, aes(x = Step, y = Si)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Siq95, ymin = Siq5)) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MedianImputedWaterSAFirstOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Main Effect Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

MedianImputedWaterSATotalOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"), 
                                                        STi = c(0.364, 0.387, 0.686), 
                                                        STiq5 = c(0.350, 0.372, 0.652), 
                                                        STiq95 = c(0.379, 0.402, 0.721))

MedianImputedWaterSATotalOrderSensitivityPlot <- ggplot(MedianImputedWaterSATotalOrderSensitivity, aes(x = Step, y = STi)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = STiq95, ymin = STiq5)) + 
  xlab("Index Step") + ylab("") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MedianImputedWaterSATotalOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Interactions Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

MedianImputedWaterSAConfidencePlot <- grid.arrange(MedianImputedWaterSAFirstOrderSensitivityPlot, MedianImputedWaterSATotalOrderSensitivityPlot, nrow = 1)

ggsave("MedianImputedWaterSAConfidencePlot.png", MedianImputedWaterSAConfidencePlot, width = 25, height = 12, units = "cm")

#####Wastewater Disparity Index#####

###Mean Imputation###

##Data Importation##
MeanImputedWastewaterData <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\Wastewater\\MeanImputation\\MeanImputationJournalWastewaterTreatmentPlantData.xlsx", sheet = "Summary")

##Normality Testing##
#1-Sample Kolmogorov-Smirnov Normality Test#
ks.test(MeanImputedWastewaterData$Population, "pnorm")
ks.test(MeanImputedWastewaterData$Age, "pnorm")
ks.test(MeanImputedWastewaterData$Violations, "pnorm")
ks.test(MeanImputedWastewaterData$II, "pnorm")
ks.test(MeanImputedWastewaterData$API, "pnorm")

#Shapiro-Wilk Normality Test#
shapiro.test(MeanImputedWastewaterData$Population)
shapiro.test(MeanImputedWastewaterData$Age)
shapiro.test(MeanImputedWastewaterData$Violations)
shapiro.test(MeanImputedWastewaterData$II)
shapiro.test(MeanImputedWastewaterData$API)

#Anderson-Darling Normality Test#
ad.test(MeanImputedWastewaterData$Population)
ad.test(MeanImputedWastewaterData$Age)
ad.test(MeanImputedWastewaterData$Violations)
ad.test(MeanImputedWastewaterData$II)
ad.test(MeanImputedWastewaterData$API)

##Correlation Testing##
#Spearman Rank Correlation Test#
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$Age, method = "spearman")
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$Violations, method = "spearman")
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$II, method = "spearman")
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$API, method = "spearman")
cor.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$Violations, method = "spearman")
cor.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$II, method = "spearman")
cor.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$API, method = "spearman")
cor.test(MeanImputedWastewaterData$Violations, MeanImputedWastewaterData$II, method = "spearman")
cor.test(MeanImputedWastewaterData$Violations, MeanImputedWastewaterData$API, method = "spearman")
cor.test(MeanImputedWastewaterData$II, MeanImputedWastewaterData$API, method = "spearman")

#2-Sample Kolmogorov-Smirnov Correlation Test#
ks.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$Age)
ks.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$Violations)
ks.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$II)
ks.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$API)
ks.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$Violations)
ks.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$II)
ks.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$API)
ks.test(MeanImputedWastewaterData$Violations, MeanImputedWastewaterData$II)
ks.test(MeanImputedWastewaterData$Violations, MeanImputedWastewaterData$API)
ks.test(MeanImputedWastewaterData$II, MeanImputedWastewaterData$API)

#Kendall Tau Rank Correlation Test#
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$Age, method = "kendall")
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$Violations, method = "kendall")
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$II, method = "kendall")
cor.test(MeanImputedWastewaterData$Population, MeanImputedWastewaterData$API, method = "kendall")
cor.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$Violations, method = "kendall")
cor.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$II, method = "kendall")
cor.test(MeanImputedWastewaterData$Age, MeanImputedWastewaterData$API, method = "kendall")
cor.test(MeanImputedWastewaterData$Violations, MeanImputedWastewaterData$II, method = "kendall")
cor.test(MeanImputedWastewaterData$Violations, MeanImputedWastewaterData$API, method = "kendall")
cor.test(MeanImputedWastewaterData$II, MeanImputedWastewaterData$API, method = "kendall")

##Meta File Importation##
MeanImputedWastewaterMeta <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\Wastewater\\JournalWastewaterTreatmentMeta.xlsx")

##Wastewater Disparity Index Set-Up##
MeanImputedWastewaterDisparityIndex <- new_coin(iData = MeanImputedWastewaterData, iMeta = MeanImputedWastewaterMeta, level_names = c("Indicators", "Groups", "Index"))

##Normalization##
MeanImputedWastewaterDisparityIndex <- Normalise(MeanImputedWastewaterDisparityIndex, dset = "Raw", global_specs = list(f_n = "n_minmax", f_n_para = list(c(1,100))))

##Aggregation##
MeanImputedWastewaterDisparityIndex <- Aggregate(MeanImputedWastewaterDisparityIndex, dset = "Normalised")

##Uncertainty and Sensitivity Analysis##
WW_norm <- list(Address = "$Log$Normalise$global_specs", Distribution = norm_alts, Type = "discrete")

WW_w_nom <- MeanImputedWastewaterDisparityIndex$Meta$Weights$Original
noise_specs <-  data.frame(Level = c(1,2), NoiseFactor = c(0.25, 0.25))
WW_noisy_wts <- get_noisy_weights(w = WW_w_nom, noise_specs = noise_specs, Nrep = 200)
WW_weights <- list(Address = "$Log$Aggregate$w", Distribution = WW_noisy_wts, Type = "discrete")

WW_agg <- list(Address = "$Log$Aggregate$f_ag", Distribution = c("a_amean", "a_gmean"), Type = "discrete")

WW_SA_specs <- list(Normalisation = WW_norm, Weights = WW_weights, Aggregation = WW_agg)

MeanImputedWastewaterUA <- get_sensitivity(MeanImputedWastewaterDisparityIndex, SA_specs = WW_SA_specs, N = 5000, SA_type = "UA", dset = "Aggregated", iCode = "Index")

MeanImputedWastewaterSA <- get_sensitivity(MeanImputedWastewaterDisparityIndex, SA_specs = WW_SA_specs, N = 5000, SA_type = "SA", dset = "Aggregated", iCode = "Index", Nboot = 5000)

MeanImputedWastewaterIndicatorRemoval <- remove_elements(MeanImputedWastewaterDisparityIndex, Level = 1, dset = "Aggregated", iCode = "Index")

##Making Plots##
MeanImputedWastewaterResultsWithRurality <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"),
                                                       Index = c(26, 30, 39, 46, 47, 48, 65, 66, 69, 74, 77, 82, 83),
                                                       Rurality = c("Rural", "Rural", "Urban", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Urban", "Urban", "Urban", "Rural"))

MeanImputedWastewaterResultsWithRuralityPlot <- ggplot(data = MeanImputedWastewaterResultsWithRurality, aes(x = County, y = Index, fill = Rurality)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MeanImputedWastewaterResultsWithRurality$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  scale_fill_manual(values = c("#008000", "#0000FF")) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MeanImputedWastewaterResultsWithRuralityPlot.png", MeanImputedWastewaterResultsWithRuralityPlot, width = 28, height = 15, units = "cm")

MeanImputedWastewaterResultsWithEconomics <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"), 
                                                        Index = c(26, 30, 39, 46, 47, 48, 65, 66, 69, 74, 77, 82, 83), 
                                                        Economics = c("Distressed", "Distressed", "At-Risk", "At-Risk", "Transitional", "Distressed", "Competitive", "Transitional", "At-Risk", "Transitional", "Competitive", "Attainment", "Competitive"))

MeanImputedWastewaterResultsWithEconomicsPlot <- ggplot(data = MeanImputedWastewaterResultsWithEconomics, aes(x = County, y = Index, fill = Economics)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MeanImputedWastewaterResultsWithEconomics$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_fill_manual(values = c("Distressed" = "#FF0000", "At-Risk" = "#FFCC66", "Transitional" = "#76EEC6", "Competitive" = "#9999FF", "Attainment" = "#3A5FCD"), breaks = c("Distressed", "At-Risk", "Transitional", "Competitive", "Attainment")) + 
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MeanImputedWastewaterResultsWithEconomicsPlot.png", MeanImputedWastewaterResultsWithEconomicsPlot, width = 28, height = 15, units = "cm")

MeanImputedWastewaterUncertainty <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"),
                                               Nominal = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                                               Median = c(13, 12, 11, 9, 9, 8, 7, 5, 5, 4, 3, 2, 1),
                                               Lower = c(13, 13, 11, 11, 10, 10, 7, 7, 6, 5, 7, 2, 1),
                                               Upper = c(12, 12, 10, 8, 8, 8, 5, 3, 4, 3, 3, 2, 1))

MeanImputedWastewaterUncertaintyPlot <- ggplot(MeanImputedWastewaterUncertainty, aes(x = County, y = Median)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) + 
  xlab("Counties") + ylab("Rank") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 20), axis.title.x = element_text(vjust = 0.5), axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_y_continuous(trans = "reverse", breaks = c(13,11,9,7,5,3,1)) + 
  scale_x_discrete(limits = MeanImputedWastewaterUncertainty$County) + 
  geom_point(aes(x = County, y = Nominal), shape = 4, size = 4)

ggsave("MeanImputedWastewaterUncertaintyPlot.png", MeanImputedWastewaterUncertaintyPlot, width = 28, height = 12, units = "cm")

MeanImputedWastewaterSABarPlotData <- data.frame(Step = c("Normalization", "Normalization", "Weights", "Weights", "Aggregation", "Aggregation"), 
                                                 Value = c(0.201, 0.319, 0.209, 0.241, 0.249, 0.244), 
                                                 Type = c("Main Effect", "Interactions", "Main Effect", "Interactions", "Main Effect", "Interactions"))

MeanImputedWastewaterSABarPlot <- ggplot(MeanImputedWastewaterSABarPlotData, aes(x = Step, y = Value, fill = Type)) + 
  geom_bar(stat = "identity", position = "stack", width = 1.5) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme(text = element_text(family = "Times", face = "plain", size = 16)) + 
  scale_x_discrete(limits = MeanImputedWastewaterSABarPlotData$Step)

ggsave("MeanImputedWastewaterSABarPlot.png", MeanImputedWastewaterSABarPlot, width = 18, height = 12, units = "cm")

MeanImputedWastewaterSAPieChartData <- data.frame(Step = c("Normalization", "Weights", "Aggregation", "Interactions"), 
                                                  Value = c(0.201, 0.209, 0.249, 0.341))

MeanImputedWastewaterSAPieChart <- ggplot(data = MeanImputedWastewaterSAPieChartData, aes(x = "", y = Value, fill = Step)) + 
  geom_bar(stat = "identity", width = 2) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  scale_fill_manual(values = c("#669900", "#FF66CC", "#0000CC", "#990000")) + 
  theme(text = element_text(family = "Times New Roman", face = "plain", size = 18))

ggsave("MeanImputedWastewaterSAPieChart.png", MeanImputedWastewaterSAPieChart, width = 18, height = 12, units = "cm")

MeanImputedWastewaterSAFirstOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"),
                                                      Si = c(0.201, 0.209, 0.249),
                                                      Siq5 = c(0.150, 0.159, 0.200),
                                                      Siq95 = c(0.254, 0.258, 0.300))

MeanImputedWastewaterSAFirstOrderSensitivityPlot <- ggplot(MeanImputedWastewaterSAFirstOrderSensitivity, aes(x = Step, y = Si)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Siq95, ymin = Siq5)) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MeanImputedWastewaterSAFirstOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Main Effect Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

MeanImputedWastewaterSATotalOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"), 
                                                      STi = c(0.520, 0.450, 0.493),
                                                      STiq5 = c(0.501, 0.434, 0.468),
                                                      STiq95 = c(0.540, 0.466, 0.519))

MeanImputedWastewaterSATotalOrderSensitivityPlot <- ggplot(MeanImputedWastewaterSATotalOrderSensitivity, aes(x = Step, y = STi)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = STiq95, ymin = STiq5)) + 
  xlab("Index Step") + ylab("") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MeanImputedWastewaterSATotalOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Interactions Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

MeanImputedWastewaterSAConfidencePlot <- grid.arrange(MeanImputedWastewaterSAFirstOrderSensitivityPlot, MeanImputedWastewaterSATotalOrderSensitivityPlot, nrow = 1)

ggsave("MeanImputedWastewaterSAConfidencePlot.png", MeanImputedWastewaterSAConfidencePlot, width = 25, height = 12, units = "cm")

MeanOfWWRanks <- mean(MeanImputedWastewaterANOVAData$Rank)

SDOfWWRanks <- sd(MeanImputedWastewaterANOVAData$Rank)

MeanImputedWastewaterANOVAData <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"),
                                        Rank = c(26, 30, 39, 46, 47, 48, 65, 66, 69, 74, 77, 82, 83),
                                        Rurality = c("Rural", "Rural", "Urban", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Urban", "Urban", "Urban", "Rural"),
                                        Economics = c("Distressed", "Distressed", "At-Risk", "At-Risk", "Transitional", "Distressed", "Competitive", "Transitional", "At-Risk", "Transitional", "Competitive", "Attainment", "Competitive"),
                                        Normalized = ((MeanImputedWastewaterANOVAData$Rank - MeanOfWWRanks) / SDOfWWRanks))

MeanImputedWastewaterANOVATwoWayModel <- aov(Normalized ~ Rurality * Economics, data = MeanImputedWastewaterANOVAData)

NonNormalizedMeanImputedWastewaterTwoWayModel <- aov(Rank ~ Rurality * Economics, data = MeanImputedWastewaterANOVAData)

###Median Imputation###

##Data Importation##
MedianImputedWastewaterData <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\Wastewater\\MedianImputation\\MedianImputationJournalWastewaterTreatmentPlantData.xlsx", sheet = "Summary")

##Normality Testing##
#1-Sample Kolmogorov-Smirnov Normality Test#
ks.test(MedianImputedWastewaterData$Population, "pnorm")
ks.test(MedianImputedWastewaterData$Age, "pnorm")
ks.test(MedianImputedWastewaterData$Violations, "pnorm")
ks.test(MedianImputedWastewaterData$II, "pnorm")
ks.test(MedianImputedWastewaterData$API, "pnorm")

#Shapiro-Wilk Normality Test#
shapiro.test(MedianImputedWastewaterData$Population)
shapiro.test(MedianImputedWastewaterData$Age)
shapiro.test(MedianImputedWastewaterData$Violations)
shapiro.test(MedianImputedWastewaterData$II)
shapiro.test(MedianImputedWastewaterData$API)

#Anderson-Darling Normality Test#
ad.test(MedianImputedWastewaterData$Population)
ad.test(MedianImputedWastewaterData$Age)
ad.test(MedianImputedWastewaterData$Violations)
ad.test(MedianImputedWastewaterData$II)
ad.test(MedianImputedWastewaterData$API)

##Correlation Testing##
#Spearman Rank Correlation Test#
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$Age, method = "spearman")
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$Violations, method = "spearman")
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$II, method = "spearman")
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$API, method = "spearman")
cor.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$Violations, method = "spearman")
cor.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$II, method = "spearman")
cor.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$API, method = "spearman")
cor.test(MedianImputedWastewaterData$Violations, MedianImputedWastewaterData$II, method = "spearman")
cor.test(MedianImputedWastewaterData$Violations, MedianImputedWastewaterData$API, method = "spearman")
cor.test(MedianImputedWastewaterData$II, MedianImputedWastewaterData$API, method = "spearman")

#2-Sample Kolmogorov-Smirnov Correlation Test#
ks.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$Age)
ks.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$Violations)
ks.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$II)
ks.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$API)
ks.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$Violations)
ks.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$II)
ks.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$API)
ks.test(MedianImputedWastewaterData$Violations, MedianImputedWastewaterData$II)
ks.test(MedianImputedWastewaterData$Violations, MedianImputedWastewaterData$API)
ks.test(MedianImputedWastewaterData$II, MedianImputedWastewaterData$API)

#Kendall Tau Rank Correlation Test#
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$Age, method = "kendall")
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$Violations, method = "kendall")
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$II, method = "kendall")
cor.test(MedianImputedWastewaterData$Population, MedianImputedWastewaterData$API, method = "kendall")
cor.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$Violations, method = "kendall")
cor.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$II, method = "kendall")
cor.test(MedianImputedWastewaterData$Age, MedianImputedWastewaterData$API, method = "kendall")
cor.test(MedianImputedWastewaterData$Violations, MedianImputedWastewaterData$II, method = "kendall")
cor.test(MedianImputedWastewaterData$Violations, MedianImputedWastewaterData$API, method = "kendall")
cor.test(MedianImputedWastewaterData$II, MedianImputedWastewaterData$API, method = "kendall")

##Meta File Importation##
MedianImputedWastewaterMeta <- read_excel("C:\\Users\\engla\\OneDrive\\Documents\\JournalSubmission\\JournalSubmissionCode\\Data\\Wastewater\\JournalWastewaterTreatmentMeta.xlsx")

##Wastewater Disparity Index Set-Up##
MedianImputedWastewaterDisparityIndex <- new_coin(iData = MedianImputedWastewaterData, iMeta = MedianImputedWastewaterMeta, level_names = c("Indicators", "Groups", "Index"))

##Normalization##
MedianImputedWastewaterDisparityIndex <- Normalise(MedianImputedWastewaterDisparityIndex, dset = "Raw", global_specs = list(f_n = "n_minmax", f_n_para = list(c(1,100))))

##Aggregation##
MedianImputedWastewaterDisparityIndex <- Aggregate(MedianImputedWastewaterDisparityIndex, dset = "Normalised")

##Uncertainty and Sensitivity Analysis##
MedianImputedWastewaterUA <- get_sensitivity(MedianImputedWastewaterDisparityIndex, SA_specs = WW_SA_specs, N = 5000, SA_type = "UA", dset = "Aggregated", iCode = "Index")

MedianImputedWastewaterSA <- get_sensitivity(MedianImputedWastewaterDisparityIndex, SA_specs = WW_SA_specs, N = 5000, SA_type = "SA", dset = "Aggregated", iCode = "Index", Nboot = 5000)

MedianImputedWastewaterIndicatorRemoval <- remove_elements(MedianImputedWastewaterDisparityIndex, Level = 1, dset = "Aggregated", iCode = "Index")

##Making Plots##
MedianImputedWastewaterResultsWithRurality <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"),
                                                         Index = c(26, 30, 40, 46, 47, 48, 65, 66, 69, 73, 77, 82, 83),
                                                         Rurality = c("Rural", "Rural", "Urban", "Rural", "Rural", "Rural", "Rural", "Rural", "Rural", "Urban", "Urban", "Urban", "Rural"))

MedianImputedWastewaterResultsWithRuralityPlot <- ggplot(data = MedianImputedWastewaterResultsWithRurality, aes(x = County, y = Index, fill = Rurality)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MedianImputedWastewaterResultsWithRurality$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  scale_fill_manual(values = c("#008000", "#0000FF")) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MedianImputedWastewaterResultsWithRuralityPlot.png", MedianImputedWastewaterResultsWithRuralityPlot, width = 28, height = 15, units = "cm")

MedianImputedWastewaterResultsWithEconomics <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"),
                                                          Index = c(26, 30, 40, 46, 47, 48, 65, 66, 69, 73, 77, 82, 83),
                                                          Economics = c("Distressed", "Distressed", "At-Risk", "At-Risk", "Transitional", "Distressed", "Competitive", "Transitional", "At-Risk", "Transitional", "Competitive", "Attainment", "Competitive"))

MedianImputedWastewaterResultsWithEconomicsPlot <- ggplot(data = MedianImputedWastewaterResultsWithEconomics, aes(x = County, y = Index, fill = Economics)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = MedianImputedWastewaterResultsWithEconomics$County) + 
  xlab("County") + ylab("Index Value") + 
  theme(text = element_text(family = "Times", face = "plain", size = 22)) + 
  guides(fill = guide_legend(title = NULL)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_fill_manual(values = c("Distressed" = "#FF0000", "At-Risk" = "#FFCC66", "Transitional" = "#76EEC6", "Competitive" = "#9999FF", "Attainment" = "#3A5FCD"), breaks = c("Distressed", "At-Risk", "Transitional", "Competitive", "Attainment")) + 
  geom_text(aes(label = Index, vjust = -0.5))

ggsave("MedianImputedWastewaterResultsWithEconomicsPlot.png", MedianImputedWastewaterResultsWithEconomicsPlot, width = 28, height = 15, units = "cm")

MedianImputedWastewaterUncertainty <- data.frame(County = c("Bledsoe", "Scott", "Carter", "Houston", "Smith", "Cocke", "Wilson", "Lawrence", "Carroll", "Sullivan", "Davidson", "Williamson", "Cheatham"),
                                                 Nominal = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                                                 Median = c(13, 12, 11, 9, 9, 8, 7, 5, 6, 4, 3, 2, 1),
                                                 Lower = c(13, 13, 11, 11, 10, 10, 7, 7, 6, 5, 7, 2, 1),
                                                 Upper = c(12, 12, 10, 8, 8, 8, 5, 3, 4, 3, 3, 2, 1))

MedianImputedWastewaterUncertaintyPlot <- ggplot(MedianImputedWastewaterUncertainty, aes(x = County, y = Median)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) + 
  xlab("Counties") + ylab("Rank") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 20), axis.title.x = element_text(vjust = 0.5), axis.text.x = element_text(angle = 45, vjust = 0.6)) + 
  scale_y_continuous(trans = "reverse", breaks = c(13,11,9,7,5,3,1)) + 
  scale_x_discrete(limits = MedianImputedWastewaterUncertainty$County) + 
  geom_point(aes(x = County, y = Nominal), shape = 4, size = 4)

ggsave("MedianImputedWastewaterUncertaintyPlot.png", MedianImputedWastewaterUncertaintyPlot, width = 28, height = 12, units = "cm")

MedianImputedWastewaterSABarPlotData <- data.frame(Step = c("Normalization", "Normalization", "Weights", "Weights", "Aggregation", "Aggregation"), 
                                                 Value = c(0.235, 0.305, 0.188, 0.223, 0.217, 0.262), 
                                                 Type = c("Main Effect", "Interactions", "Main Effect", "Interactions", "Main Effect", "Interactions"))

MedianImputedWastewaterSABarPlot <- ggplot(MedianImputedWastewaterSABarPlotData, aes(x = Step, y = Value, fill = Type)) + 
  geom_bar(stat = "identity", position = "stack", width = 1.5) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme(text = element_text(family = "Times", face = "plain", size = 16)) + 
  scale_x_discrete(limits = MedianImputedWastewaterSABarPlotData$Step)

ggsave("MedianImputedWastewaterSABarPlot.png", MedianImputedWastewaterSABarPlot, width = 18, height = 12, units = "cm")

MedianImputedWastewaterSAPieChartData <- data.frame(Step = c("Normalization", "Weights", "Aggregation", "Interactions"), 
                                                  Value = c(0.235, 0.188, 0.217, 0.360))

MedianImputedWastewaterSAPieChart <- ggplot(data = MedianImputedWastewaterSAPieChartData, aes(x = "", y = Value, fill = Step)) + 
  geom_bar(stat = "identity", width = 2) + 
  coord_polar("y", start = 0) + 
  theme_void() + 
  scale_fill_manual(values = c("#669900", "#FF66CC", "#0000CC", "#990000")) + 
  theme(text = element_text(family = "Times New Roman", face = "plain", size = 18))

ggsave("MedianImputedWastewaterSAPieChart.png", MedianImputedWastewaterSAPieChart, width = 18, height = 12, units = "cm")

MedianImputedWastewaterSAFirstOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"),
                                                           Si = c(0.235, 0.188, 0.217),
                                                           Siq5 = c(0.186, 0.141, 0.167),
                                                           Siq95 = c(0.288, 0.236, 0.266))

MedianImputedWastewaterSAFirstOrderSensitivityPlot <- ggplot(MedianImputedWastewaterSAFirstOrderSensitivity, aes(x = Step, y = Si)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = Siq95, ymin = Siq5)) + 
  xlab("Index Step") + ylab("Average Rank Change") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MedianImputedWastewaterSAFirstOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Main Effect Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))

MedianImputedWastewaterSATotalOrderSensitivity <- data.frame(Step = c("Normalization", "Weights", "Aggregation"), 
                                                           STi = c(0.540, 0.411, 0.479),
                                                           STiq5 = c(0.520, 0.397, 0.454),
                                                           STiq95 = c(0.560, 0.426, 0.504))

MedianImputedWastewaterSATotalOrderSensitivityPlot <- ggplot(MedianImputedWastewaterSATotalOrderSensitivity, aes(x = Step, y = STi)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymax = STiq95, ymin = STiq5)) + 
  xlab("Index Step") + ylab("") + 
  theme_bw() + 
  theme(text = element_text(family = "Times", face = "plain", size = 18)) + 
  scale_x_discrete(limits = MedianImputedWastewaterSATotalOrderSensitivity$Step) + 
  scale_y_continuous(limits = c(0, 0.8)) + 
  ggtitle("Interactions Sensitivity") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank())

MedianImputedWastewaterSAConfidencePlot <- grid.arrange(MedianImputedWastewaterSAFirstOrderSensitivityPlot, MedianImputedWastewaterSATotalOrderSensitivityPlot, nrow = 1)

ggsave("MedianImputedWastewaterSAConfidencePlot.png", MedianImputedWastewaterSAConfidencePlot, width = 25, height = 12, units = "cm")