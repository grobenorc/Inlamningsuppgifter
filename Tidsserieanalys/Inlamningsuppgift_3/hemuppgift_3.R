
setwd("---")
library(tidyverse)
library(cowplot)
library(tseries)
library(broom)
library(purrr)
library(texreg)
library(stargazer)


# Uppgift 1 inlämning
# Skapar här random_walk processer, kör seed 100
random_walk_processer <- function(antal_processer, langd_processer, seed){
  set.seed(seed)
  antal <- antal_processer
  langd <- langd_processer
  df    <- data.frame(matrix(nrow = langd, ncol = antal))
  for (i in 1:langd) {
    sample <- rnorm(langd, 0, 1)
    df[i,] <- sample
  }
  
  df_rw <<- cbind(Index = 1:langd, df)
  df_cumsum <<- cbind(Index = 1:langd, cumsum(df))
  min_cum <- round(min(df_cumsum[, -1]), digits = -1)-5 
  max_cum <- round(max(df_cumsum[, -1]), digits = -1)+5 
  
  # Skapar här plottarna
  plots <- list()
  for (col in grep("^X", colnames(df_cumsum), value = TRUE)) {
    plot_data <- df_cumsum[, c("Index", col)]
    plot <- ggplot(plot_data, aes(x = Index, y = .data[[col]])) + 
      geom_point() +
      geom_smooth(method = "lm") +
      geom_abline(intercept = max(df_cumsum[, -1]), slope = 0, color = "red") + geom_abline(intercept = min(df_cumsum[, -1]), slope = 0, color = "blue") + 
      ylim(min_cum, max_cum) + 
      ggtitle(paste("Plot för", col))
    plots[[col]] <- plot
  }
  
  # Kombinera alla ggplots till en fil
  grid <- cowplot::plot_grid(plotlist = plots, ncol = 2)
  # Visa filen med ggdraw
  cowplot::ggdraw(grid)
}
# win.graph()
random_walk_processer(4, 200, 100)


random_walk_processer_lm_summary <- function(){
# Loopar här igenom och skapar lm modeller för varje simulering
  cols <- grep("^X", colnames(df_cumsum), value = TRUE)
  random_walk_lm_modeller <<- lapply(cols, function(col) {
    lm(formula = paste(col, "~ Index"), data = df_cumsum)
  })
  return(stargazer(random_walk_lm_modeller, digits = 2))
}
random_walk_processer_lm_summary()

random_walk_processer_residualer <- function(){
  residuals_list <- lapply(random_walk_lm_modeller, resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  # Plottar här residualerna
  p <- ggplot(residuals_df, aes(x = index, y = residuals)) +
    geom_point() +
    facet_wrap(. ~ model, nrow = 2, scales = "free_x") +
    xlab("Index av Residual") +
    ylab("Residualer") +
    ggtitle("Residualer för respektive model") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16))
  ggsave("img/rw_fyra_processer_residualplot.png", p)
  p
}
random_walk_processer_residualer()

random_walk_processer_residualer_qqplot <- function(){
  residuals_list <- lapply(random_walk_lm_modeller, resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                                model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                                residuals = unlist(residuals_list))
  
  p <- ggplot(residuals_df, aes(sample=residuals)) +
    geom_qq() +
    stat_qq_line(color = "red") + 
    facet_wrap(~model) +
    labs(title = "QQ-plot av residualer", x = "Teoretiska kvantilerna", y = "Observerade kvantilerna") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16))
  ggsave("img/rw_fyra_processer_qqplot.png", p)  
  p
}
random_walk_processer_residualer_qqplot()

##### Uppgift 2 inlämning #####
# Differensbildning
diff_random_walk_processer <- function(){
  df_cumsum <- df_cumsum
  
  diff_list <- lapply(df_cumsum[-1], diff, lag = 1)
  diff_df <<- data.frame(Index = 1:lengths(diff_list)[[1]], do.call(cbind, diff_list))
  
  df_long <- tidyr::gather(diff_df, key = "variable", value = "value", -Index)
  p <- ggplot(data = df_long, aes(x = Index, y = value, color = variable)) + 
    geom_line() +
    facet_wrap(~variable)  +
    labs(x = "Index", y = "Diff") +
    guides(colour=FALSE)  + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA),
                            strip.text.x = element_text(size = 16))
  ggsave("img/rw_diff_fyra_processer.png", p)  
  p
}
diff_random_walk_processer()


# Lm modeller av differentisierad serie
random_walk_processer_diff_lm_summary <- function(){
  # Loopar här igenom och skapar lm modeller för varje simulering
  cols <- grep("^X", colnames(diff_df), value = TRUE)
  random_walk_diff_lm_modeller <<- lapply(cols, function(col) {
    lm(formula = paste(col, "~ Index"), data = diff_df)
  })
  return(stargazer(random_walk_diff_lm_modeller, digits = 2))
}
random_walk_processer_diff_lm_summary()

# Residualplot av differentierad serie
random_walk_diff_processer_residualer <- function(){
  residuals_list <- lapply(random_walk_diff_lm_modeller, resid)
  residuals_df   <- data.frame(Index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  # Plottar här residualerna
  p <- ggplot(residuals_df, aes(x = Index, y = residuals, color = model)) +
    geom_point() +
    facet_wrap(. ~ model, nrow = 2, scales = "free_x") +
    xlab("Index av Residual") +
    ylab("Residualer") +
    ggtitle("Residualer för respektive model") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16)) +
    guides(color = FALSE)
  ggsave("img/rw_fyra_processer_diff_residualplot.png", p)
  p
}
random_walk_diff_processer_residualer()

# QQplot av differentierad serie
random_walk_processer_residualer_qqplot <- function(){
  residuals_list <- lapply(random_walk_diff_lm_modeller, resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  
  p <- ggplot(residuals_df, aes(sample=residuals, color = model)) +
    geom_qq() +
    stat_qq_line(color = "black") + 
    facet_wrap(~model) +
    labs(title = "QQ-plot av residualer", x = "Teoretiska kvantilerna", y = "Observerade kvantilerna") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16)) +
    guides(color = FALSE)
  ggsave("img/rw_fyra_processer_diff_qqplot.png", p)  
  p
}
random_walk_processer_residualer_qqplot()


### ADF-testning
# Test på ursprungliga serier
adf_testning <- function(){
  df_cumsum <- df_cumsum
  for (i in 2:length(df_cumsum)) {
    print(adf.test(df_cumsum[,i]))

  }
}
adf_testning()


# Test på differentierade
adf_testning_diff <- function(){
  diff_df <- diff_df
  for (i in 2:length(diff_df)) {
    print(adf.test(diff_df[,i]))
  }
}
adf_testning_diff()




##### Uppgift 3 AR(p) #####
acf_olika_phi <- function(antal, seed){
  set.seed(seed)
  n <- antal
  phis <- seq(0.1, 0.9, by=0.1) 
  acf_df <- data.frame(phi=phis)
  for (i in 1:length(phis)) {
    phi <- phis[i]
    if (abs(phi) < 1) {
      ar1 <- arima.sim(model=list(ar=phi), n=n)
      acf_values <- acf(ar1, lag.max = 47, plot=FALSE)$acf
      acf_df[,paste0("lag_", 1:length(acf_values))] <- acf_values
    }
  }
  acf_long <- acf_df %>%
    pivot_longer(cols=starts_with("lag_"), names_to="lag", values_to="acf_value") %>%
    mutate(lag = as.numeric(gsub("lag_", "", lag)))
  
  p <- ggplot(data = acf_long, aes(x = lag, y = acf_value, color = as.factor(phi))) + 
    geom_line() + 
    facet_wrap(~phi) +
    theme_minimal_grid() + theme(legend.position = "none",
                                 strip.text.x = element_text(size = 16))
    labs(title = "ACF för olika Phi", y = "ACF-värde")
  p
  ggsave("img/acf_olika_phi.png", plot = p)
}
acf_olika_phi(100,100)

# ggplot(data = acf_long, aes(x = lag, y = acf_value, color = as.factor(phi))) + geom_line() + facet_wrap(~phi)
# ggplot(data = acf_long, aes(x = lag, y = acf_value, color = as.factor(phi))) + geom_line()

adf_test_olika_phi <- function(antal, seed){
  set.seed(seed)
  n <- antal
  phi_values <- seq(-0.9, 0.9, by=0.1)
  adf_results <- data.frame(phi = phi_values, adf_stat = rep(NA, length(phi_values)))
  
  for (i in 1:length(phi_values)) {
    phi <- phi_values[i]
    if (abs(phi) < 1) {
      ar1 <- arima.sim(model=list(ar=phi), n=n)
      adf_test <- adf.test(ar1)
      adf_results[i, "adf_stat"] <- adf_test$statistic
    }
  }
  
#  adf_results <- adf_results 
  p <- ggplot(data = adf_results, aes(x = phi, y = adf_stat)) + 
    geom_line() + 
    geom_point(size=8, colour="white") + 
    geom_point(size = 4, color = "blue") + 
    xlim(-1, 1) +
    labs(title = "ADF-test för olika Phi", y = "ADF teststatistika", x = "Phi") +
    theme_minimal()
  ggsave("img/adf_test_olika_phi.png", plot = p)
  p
}
adf_test_olika_phi(200,100)



### Simuleringar av AR modeller med olika phi-värden
arima_olika_phi_graf <- function(antal, seed){
  set.seed(seed)
  n <- antal
  simulate_ar1 <- function(phi) { # Antal tidspunkter att simulera
    x <- numeric(n)  # Skapa en vektor för simuleringen
    x[1] <- rnorm(1)  # Börja simuleringen med ett slumpmässigt startvärde
    for (i in 2:n) {
      x[i] <- phi * x[i-1] + rnorm(1)  # Simulera nästa tidspunkt
    }
    return(x)
  }
  
  # Skapa en data.frame med simuleringarna
  phi_values <- seq(-0.9, 0.9, by = 0.2)
  simulations <- lapply(phi_values, simulate_ar1)
  simulations_df <<- data.frame(simulation = unlist(simulations),
                                phi = rep(phi_values, each = n),
                                time = rep(1:n, times = length(phi_values)))
  
  p <- ggplot(data = simulations_df, aes(x = time, y = simulation, color = as.factor(phi))) + 
    geom_line() + 
    facet_wrap(~phi) +
    theme_minimal_grid() +
    theme(panel.border = element_rect(fill = NA),
          strip.text.x = element_text(size = 16),
          legend.position = "none") +
    xlab("Index") +
    ylab("Simulerade värden") +
    ggtitle("Simulering av AR(1) modeller med olika Phi")
  ggsave("img/ar1_simulering_olika_phi.png", plot = p)
  p

}
arima_olika_phi_graf(200,100)


### LM av varje simulering
lm_ar1_simulering_olika_phi <- function(antal, seed){
  n <- antal
  set.seed(seed)
  
  
  # Skapar listobjekt som sen fylls
  lm_list <- list()
  
  phi_values <- seq(-0.9, 0.9, by = 0.3)
  for (phi in phi_values) {
    # Skapa en simulering av en AR(1)-process med det specifika phi-värdet
    x <- numeric(n)  # Skapa en vektor för simuleringen
    x[1] <- rnorm(1)  # Börja simuleringen med ett slumpmässigt startvärde
    for (i in 2:n) {
      x[i] <- phi * x[i-1] + rnorm(1)  # Simulera nästa tidspunkt
    }
    
    # Skapa en data.frame med simuleringen
    simulation_df <- data.frame(x = x, y = lag(x))
    
    # Använd lm() för att beräkna en linjär regression och lägg till fit:en i lm_list
    lm_fit <- lm(y ~ x, data = simulation_df)
    lm_list <- append(lm_list, list(lm_fit))
  }
  
  # Skriv ut summary() för varje lm-fit i lm_list
  for (i in 1:length(lm_list)) {
    cat("Phi = ", phi_values[i], "\n")
    print(summary(lm_list[[i]]))
    cat("\n")
  }
  
  lista_lm <<- lm_list
  return(stargazer(lm_list, fontsize = "tiny"))
  
}
lm_ar1_simulering_olika_phi(200, 100)

### Residualplot av lm AR(1)
ar1_processer_residualer <- function(){
  residuals_list <- lapply(lista_lm[-4], resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  # Plottar här residualerna
  p <- ggplot(residuals_df, aes(x = index, y = residuals, color = model)) +
    geom_point() +
    facet_wrap(. ~ model, nrow = 2, scales = "free_x") +
    xlab("Index av Residual") +
    ylab("Residualer") +
    ggtitle("Residualer för respektive model") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16),
          legend.position = "none")
  ggsave("img/ar1_olika_phi_residualplot.png", p)
  p
}
ar1_processer_residualer()


### QQ-plot av lm AR(1)
ar1_olika_phi_qqplot <- function(){
  residuals_list <- lapply(lista_lm[-4], resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  
  p <- ggplot(residuals_df, aes(sample=residuals, color = model)) +
    geom_qq() +
    stat_qq_line(color = "black") + 
    facet_wrap(~model) +
    labs(title = "QQ-plot av residualer", x = "Teoretiska kvantilerna", y = "Observerade kvantilerna") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16),
          legend.position = "none")
  ggsave("img/ar1_olika_phi_qqplot.png", p)  
  p
}

ar1_olika_phi_qqplot()






##### Läng på processen spelar roll? #####

# Kör raderna 387-571 på där du skapat en process med önskat n med funktionen 1000. Annat n behöver då bytas. och köra om raderna.

rm(list = ls())
random_walk_processer <- function(antal_processer, langd_processer, seed){
  set.seed(seed)
  antal <- antal_processer
  langd <- langd_processer
  df    <- data.frame(matrix(nrow = langd, ncol = antal))
  for (i in 1:langd) {
    sample <- rnorm(langd, 0, 1)
    df[i,] <- sample
  }
  
  df_rw <<- cbind(Index = 1:langd, df)
  df_cumsum <<- cbind(Index = 1:langd, cumsum(df))
  min_cum <- round(min(df_cumsum[, -1]), digits = -1)-5 
  max_cum <- round(max(df_cumsum[, -1]), digits = -1)+5 
  
  # Skapar här plottarna
  plots <- list()
  for (col in grep("^X", colnames(df_cumsum), value = TRUE)) {
    plot_data <- df_cumsum[, c("Index", col)]
    plot <- ggplot(plot_data, aes(x = Index, y = .data[[col]])) + 
      geom_point() +
      geom_smooth(method = "lm") +
      geom_abline(intercept = max(df_cumsum[, -1]), slope = 0, color = "red") + geom_abline(intercept = min(df_cumsum[, -1]), slope = 0, color = "blue") + 
      ylim(min_cum, max_cum) + 
      ggtitle(paste("Plot för", col))
    plots[[col]] <- plot
  }
  
  # Kombinera alla ggplots till en fil
  grid <- cowplot::plot_grid(plotlist = plots, ncol = 2)
  # Visa filen med ggdraw
  cowplot::ggdraw(grid)
}
random_walk_processer(4, 10000, 100)

random_walk_processer_lm_summary <- function(){
  # Loopar här igenom och skapar lm modeller för varje simulering
  cols <- grep("^X", colnames(df_cumsum), value = TRUE)
  random_walk_lm_modeller <<- lapply(cols, function(col) {
    lm(formula = paste(col, "~ Index"), data = df_cumsum)
  })
  return(stargazer(random_walk_lm_modeller, digits = 2))
}
random_walk_processer_lm_summary()


random_walk_processer_residualer <- function(){
  residuals_list <- lapply(random_walk_lm_modeller, resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  # Plottar här residualerna
  p <- ggplot(residuals_df, aes(x = index, y = residuals)) +
    geom_point() +
    facet_wrap(. ~ model, nrow = 2, scales = "free_x") +
    xlab("Index av Residual") +
    ylab("Residualer") +
    ggtitle("Residualer för respektive model") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16))
  p
}
random_walk_processer_residualer()

random_walk_processer_residualer_qqplot <- function(){
  residuals_list <- lapply(random_walk_lm_modeller, resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  
  p <- ggplot(residuals_df, aes(sample=residuals)) +
    geom_qq() +
    stat_qq_line(color = "red") + 
    facet_wrap(~model) +
    labs(title = "QQ-plot av residualer", x = "Teoretiska kvantilerna", y = "Observerade kvantilerna") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16))
  p
}
random_walk_processer_residualer_qqplot()


### Differensbildning
diff_random_walk_processer <- function(){
  df_cumsum <- df_cumsum
  
  diff_list <- lapply(df_cumsum[-1], diff, lag = 1)
  diff_df <<- data.frame(Index = 1:lengths(diff_list)[[1]], do.call(cbind, diff_list))
  
  df_long <- tidyr::gather(diff_df, key = "variable", value = "value", -Index)
  p <- ggplot(data = df_long, aes(x = Index, y = value, color = variable)) + 
    geom_line() +
    facet_wrap(~variable)  +
    labs(x = "Index", y = "Diff") +
    guides(colour=FALSE)  + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA),
                            strip.text.x = element_text(size = 16))
  p
}
diff_random_walk_processer()


# Lm modeller av differentisierad serie
random_walk_processer_diff_lm_summary <- function(){
  # Loopar här igenom och skapar lm modeller för varje simulering
  cols <- grep("^X", colnames(diff_df), value = TRUE)
  random_walk_diff_lm_modeller <<- lapply(cols, function(col) {
    lm(formula = paste(col, "~ Index"), data = diff_df)
  })
  return(stargazer(random_walk_diff_lm_modeller, digits = 2))
}
random_walk_processer_diff_lm_summary()

# Residualplot av differentierad serie
random_walk_diff_processer_residualer <- function(){
  residuals_list <- lapply(random_walk_diff_lm_modeller, resid)
  residuals_df   <- data.frame(Index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  # Plottar här residualerna
  p <- ggplot(residuals_df, aes(x = Index, y = residuals, color = model)) +
    geom_point() +
    facet_wrap(. ~ model, nrow = 2, scales = "free_x") +
    xlab("Index av Residual") +
    ylab("Residualer") +
    ggtitle("Residualer för respektive model") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16)) +
    guides(color = FALSE)
  p
}
random_walk_diff_processer_residualer()

# QQplot av differentierad serie
random_walk_processer_residualer_qqplot <- function(){
  residuals_list <- lapply(random_walk_diff_lm_modeller, resid)
  residuals_df   <- data.frame(index = rep(1:length(residuals_list[[1]]), length(lengths(residuals_list))), 
                               model = paste("Modell", rep(1:length(residuals_list), each = length(residuals_list[[1]]))),
                               residuals = unlist(residuals_list))
  
  p <- ggplot(residuals_df, aes(sample=residuals, color = model)) +
    geom_qq() +
    stat_qq_line(color = "black") + 
    facet_wrap(~model) +
    labs(title = "QQ-plot av residualer", x = "Teoretiska kvantilerna", y = "Observerade kvantilerna") + 
    theme_minimal() + theme(panel.border = element_rect(fill = NA)) + 
    theme(strip.text.x = element_text(size = 16)) +
    guides(color = FALSE)
  p
}
random_walk_processer_residualer_qqplot()


# Skapa en data.frame med en kolumn för varje Arima.sim-objekt
plot_simulering_ar <- function(phi1, phi2, antal, antal2){
  sim1 <- arima.sim(list(order = c(1,0,0), ar = phi1), n = antal)
  sim2 <- arima.sim(list(order = c(1,0,0), ar = phi1), n = antal2)
  sim3 <- arima.sim(list(order = c(1,0,0), ar = phi2), n = antal)
  sim4 <- arima.sim(list(order = c(1,0,0), ar = phi2), n = antal2)
  
  df1000  <- data.frame(Index = 1:antal, sim_phi1_antal = sim1, sim_phi2_antal = sim3)
  df10000 <- data.frame(Index = 1:antal2, sim_phi1_antal2 = sim2, sim_phi2_antal2 = sim4)
  
  df_long <- rbind(
    pivot_longer(df1000, cols = c("sim_phi1_antal", "sim_phi2_antal"), names_to = "Kategori", values_to = "Värde"),
    pivot_longer(df10000, cols = c("sim_phi1_antal2", "sim_phi2_antal2"), names_to = "Kategori", values_to = "Värde")
  )
  p <- ggplot(data = df_long, aes(x = Index, y = Värde, color = Kategori)) + 
    geom_line() + 
    facet_wrap(~ Kategori, scales = "free_x") +
    theme_minimal_grid() +
    theme(legend.position = "none",
          strip.text.x = element_text(size = 16),
          panel.border = element_rect(fill = NA))
  ggsave("img/ar_simuleringar_olika_phi_olika_n.png", plot = p)
  p
}
plot_simulering_ar(0.3, 0.9, 1000,10000)


