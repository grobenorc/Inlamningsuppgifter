# -------------------------------------
# Script: 

# Author: --- ----

# Purpose: Inlämnningsuppgift 1 - STAG42

# Notes:

# License: MIT

# Encoding: UTF-8-SV
Sys.setlocale("LC_ALL", "sv_SE.UTF-8")

# Packages:
library(tidyverse)
library(readr)
library(readxl)
library(ggthemes)
library(kableExtra)

# -------------------------------------

setwd("---")
 
library(tidyverse)
library(readxl)



### Inladdning data delfrågor -----
dat1 <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat1")

dat2 <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat2")

dat3 <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat3")

dat4 <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat4")

dat5 <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat5")


### Processering

dat1_processed <- dat1 %>%
  mutate(forandring = kurs_9_juli - kurs_2_juli,
         forandring_sign = ifelse(kurs_9_juli - kurs_2_juli > 0, "+",
                                  ifelse(kurs_9_juli - kurs_2_juli < 0, "-", "ties")),
         forandring_sign_true_false = ifelse(kurs_9_juli - kurs_2_juli > 0, TRUE,
                                             ifelse(kurs_9_juli - kurs_2_juli < 0, FALSE, NA)))



dat2_processed <- dat2 %>%
  mutate(forandring = ekonomer - veterinarer,
         forandring_sign = ifelse(ekonomer - veterinarer > 0, "+",
                                  ifelse(ekonomer - veterinarer < 0, "-", "ties")),
         forandring_sign_true_false = ifelse(ekonomer - veterinarer > 0, TRUE,
                                             ifelse(ekonomer - veterinarer < 0, FALSE, NA)))



dat3_processed <- dat3


dat4_processed <- dat4 %>%
  mutate(forandring = chef_b - chef_a,
         forandring_sign = ifelse(chef_b - chef_a > 0, "+",
                                  ifelse(chef_b - chef_a < 0, "-", "ties")),
         forandring_sign_true_false = ifelse(chef_b - chef_a > 0, TRUE,
                                             ifelse(chef_b - chef_a < 0, FALSE, NA)))


dat5_processed <- dat5 %>%
  mutate(utbildningsniva_factor = factor(utbildningsniva,
                                         levels = c("Mycket låg", "Låg", "Medel", "Hög", "Mycket hög")),
         arbetsuppgifter_factor = factor(arbetsuppgifter,
                                         levels = c("Mycket enkla", "Enkla", "Mindre avancerade", "Avancerade", "Mycket avancerade"))) %>% 
  mutate(utbildningsniva_num = as.integer(utbildningsniva_factor),
         arbetsuppgifter_num = as.integer(arbetsuppgifter_factor))



rm(dat1, dat2, dat3, dat4, dat5)


### Fråga 1 ----
# Data
dat1_processed <-read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat1") %>%
  mutate(forandring = kurs_9_juli - kurs_2_juli,
         forandring_sign = ifelse(kurs_9_juli - kurs_2_juli > 0, "+",
                                  ifelse(kurs_9_juli - kurs_2_juli < 0, "-", "ties")),
         forandring_sign_true_false = ifelse(kurs_9_juli - kurs_2_juli > 0, TRUE,
                                             ifelse(kurs_9_juli - kurs_2_juli < 0, FALSE, NA)))


# Teckentest
summary(dat1_processed$forandring_sign_true_false)
knitr::kable(as.matrix(summary(dat1_processed$forandring_sign_true_false)), format = "latex", booktabs = TRUE)

success <- summary(dat1_processed$forandring_sign_true_false)[[3]] %>% as.numeric()
trials <- dat1_processed %>% select(forandring_sign_true_false) %>% drop_na() %>% nrow()

teckentest_binom <- binom.test(success, trials, p = 0.5, alternative = "greater", conf.level = 0.99)
results_tecken_df <- data.frame(
  "Successes" =teckentest_binom$statistic[[1]],
  "Trials" = teckentest_binom$parameter[[1]],
  "P-Value" = round(teckentest_binom$p.value[[1]], digits = 4),
  "Confidence Interval" = paste0(round(teckentest_binom$conf.int[1], digits  = 2), " - ", teckentest_binom$conf.int[2])
)
results_tecken_df

knitr::kable(results_tecken_df, format = "latex", booktabs = TRUE)


# Teckenrangtest
teckenrangtest_wilcox <- dat1_processed %>%
  select(kurs_2_juli, kurs_9_juli) %>%
  with(wilcox.test(kurs_9_juli, kurs_2_juli, paired = TRUE, alternative = "greater", conf.level = 0.99, exact = TRUE))
teckenrangtest_wilcox

results_teckenrang_df <- data.frame(
  "V" = teckenrangtest_wilcox$statistic[[1]],
  "P-Value" = round(teckenrangtest_wilcox$p.value[[1]], digits = 4),
  "Mothypotes:" = stringr::str_to_title(teckenrangtest_wilcox$alternative)
)

knitr::kable(results_teckenrang_df, format = "latex", booktabs = TRUE)


rm(list = ls())







### Fråga 2 -------

# Data

dat2_processed <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat2") %>%
  mutate(forandring = ekonomer - veterinarer,
         forandring_sign = ifelse(ekonomer - veterinarer > 0, "+",
                                  ifelse(ekonomer - veterinarer < 0, "-", "ties")),
         forandring_sign_true_false = ifelse(ekonomer - veterinarer > 0, TRUE,
                                             ifelse(ekonomer - veterinarer < 0, FALSE, NA)))


mann_whitney <- dat2_processed %>% select(1:2) %>% tidyr::gather(value = "value") %>% 
  with(wilcox.test(value ~ key, conf.level = 0.95, exact = TRUE))
  


results_mann_whitney_df <- data.frame(
  "V" = mann_whitney$statistic[[1]],
  "P-Value" = round(mann_whitney$p.value[[1]], digits = 4),
  "Mothypotes:" = stringr::str_to_title(mann_whitney$alternative)
)

knitr::kable(results_mann_whitney_df, format = "latex", booktabs = TRUE)


rm(list = ls())





### Fråga 3 ---------------

# Data 


dat3_processed <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat3")

kruskal_wallis_test <- kruskal.test(dat3_processed)

results_kruskal_wallis_df <- data.frame(
  "V" = kruskal_wallis_test$statistic[[1]],
  "P-Value" = round(kruskal_wallis_test$p.value[[1]], digits = 4)
)

knitr::kable(results_kruskal_wallis_df, format = "latex", booktabs = TRUE)
  

rm(list= ls())


### Fråga 4 ---------------

# Data
dat4_processed <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat4") %>%
  mutate(forandring = chef_b - chef_a,
         forandring_sign = ifelse(chef_b - chef_a > 0, "+",
                                  ifelse(chef_b - chef_a < 0, "-", "ties")),
         forandring_sign_true_false = ifelse(chef_b - chef_a > 0, TRUE,
                                             ifelse(chef_b - chef_a < 0, FALSE, NA)))



result = cor(dat4_processed$chef_a, dat4_processed$chef_b, method = "spearman")
cat("Spearman correlation coefficient is:", round(result, digits = 2))

spearman_cor_test <- cor.test(dat4_processed$chef_a, dat4_processed$chef_b, method = "spearman", alternative = "greater", conf.level = 0.95)


results_spearman_cor_test_df <- data.frame(
  "S" = spearman_cor_test$statistic[[1]],
  "P-Value" = round(spearman_cor_test$p.value[[1]], digits = 4),
  "rho" = round(spearman_cor_test$estimate[[1]], digits = 2),
  "Hypotes" = spearman_cor_test$alternative[[1]] 
)

knitr::kable(results_spearman_cor_test_df, format = "latex", booktabs = TRUE)


rm(list = ls())


dat4_processed %>% mutate(rang_chef_a=rank(chef_a),
                          rang_chef_b=rank(chef_b),
                          d_i = abs(rang_chef_a -rang_chef_b ),
                          d_i_2  = d_i**2) %>% 
  summarise(6*sum(d_i_2) / (length(d_i_2) *(length(d_i_2**2-1))))




### Fråga 5 ---------------
dat5_processed <- read_excel("Datorlaborationer/datorlab_1_dat.xlsx", sheet = "dat5") %>%
  mutate(utbildningsniva_factor = factor(utbildningsniva,
                                         levels = c("Mycket låg", "Låg", "Medel", "Hög", "Mycket hög")),
         arbetsuppgifter_factor = factor(arbetsuppgifter,
                                         levels = c("Mycket enkla", "Enkla", "Mindre avancerade", "Avancerade", "Mycket avancerade"))) %>% 
  mutate(utbildningsniva_num = as.integer(utbildningsniva_factor),
         arbetsuppgifter_num = as.integer(arbetsuppgifter_factor))


dat5_processed %>% ggplot(aes(x = utbildningsniva_factor, y = factor(arbetsuppgifter_factor))) +
  geom_point(aes(group = "1")) +
  geom_smooth(aes(group = "1"), method = "lm", se = FALSE) + 
  labs(title = "Arbetsuppgifter mot Utbildningsnivå", y = "Arbetsuppgifter", x = "Utbildningsnivå")



corr_test_spearman <- cor.test(dat5_processed$arbetsuppgifter_num,  dat5_processed$utbildningsniva_num, method = "spearman", alternative = "greater")


results_spearman_cor_test_df <- data.frame(
  "S" = corr_test_spearman$statistic[[1]],
  "P-Value" = round(corr_test_spearman$p.value[[1]], digits = 4),
  "rho" = round(corr_test_spearman$estimate[[1]], digits = 2),
  "Hypotes" = corr_test_spearman$alternative[[1]] 
)

knitr::kable(results_spearman_cor_test_df, format = "latex", booktabs = TRUE)
