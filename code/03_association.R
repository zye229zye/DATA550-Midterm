library(ggplot2)
library(dplyr)
library(broom)
library(here)
library(RColorBrewer)
library(tidyr)
library(scales) 
library(config) 


Sys.setenv(WHICH_CONFIG = "default")  

config_list <- config::get(config = Sys.getenv("WHICH_CONFIG"))

covid_enabled <- config_list$covid

data_clean_covid <- read.csv(here::here("covid_sub.csv"))


theme_set(theme_bw(base_size = 15))


if (!dir.exists(here::here("output"))) {
  dir.create(here::here("output"), recursive = TRUE)
}

data_clean_covid <- data_clean_covid %>%
  mutate(
  
    ICU = ifelse(ICU == "Yes", 1, ifelse(ICU == "No", 0, NA)),
    
    
    MORTALITY = ifelse(is.na(DATE_DIED), 0, 1)
  )

if (covid_enabled) {

  temp_data_covid <- data_clean_covid %>%
    filter(CLASIFICATION_FINAL >= 1 & CLASIFICATION_FINAL <= 3)
  
  
  severity_model <- glm(ICU ~ DIABETES + RENAL_CHRONIC + SEX + AGE + HIPERTENSION,
                        data = temp_data_covid %>% filter(!is.na(ICU)),
                        family = binomial)
  
  severity_results <- tidy(severity_model, conf.int = TRUE)
  
 
  write.csv(severity_results, file = here::here("output/covid_severity_results.csv"))
  
  
  num_terms <- nrow(severity_results %>% filter(term != "(Intercept)"))
  custom_colors <- hue_pal()(num_terms)
  
  ggplot(severity_results %>% filter(term != "(Intercept)"),
         aes(x = term, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high), color = term)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
      title = "Odds Ratios for ICU Admission (COVID Cases)",
      x = "Variable",
      y = "Odds Ratio (Exp(Estimate))"
    ) +
    scale_color_manual(values = custom_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  

  ggsave(here::here("output/covid_severity_odds_ratios.png"), width = 10, height = 6, dpi = 300)
  

  mortality_model <- glm(MORTALITY ~ AGE + HIPERTENSION + SEX + DIABETES + COPD + OBESITY,
                         data = temp_data_covid %>% filter(!is.na(MORTALITY)),
                         family = binomial)
  
  mortality_results <- tidy(mortality_model, conf.int = TRUE)
  

  write.csv(mortality_results, file = here::here("output/covid_mortality_results.csv"))
  
  
  num_terms <- nrow(mortality_results %>% filter(term != "(Intercept)"))
  custom_colors <- hue_pal()(num_terms)
  
  ggplot(mortality_results %>% filter(term != "(Intercept)"),
         aes(x = term, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high), color = term)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
      title = "Odds Ratios for Mortality (COVID Cases)",
      x = "Variable",
      y = "Odds Ratio (Exp(Estimate))"
    ) +
    scale_color_manual(values = custom_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  

  ggsave(here::here("output/covid_mortality_odds_ratios.png"), width = 10, height = 6, dpi = 300)
  

  tobacco_obesity_model <- glm(ICU ~ TOBACCO + OBESITY + SEX + AGE,
                               data = temp_data_covid %>% filter(!is.na(ICU), !is.na(TOBACCO), !is.na(OBESITY)),
                               family = binomial)
  
  tobacco_obesity_results <- tidy(tobacco_obesity_model, conf.int = TRUE)
  

  write.csv(tobacco_obesity_results, file = here::here("output/covid_tobacco_obesity_results.csv"))
  

  num_terms <- nrow(tobacco_obesity_results %>% filter(term != "(Intercept)"))
  custom_colors <- hue_pal()(num_terms)
  
  ggplot(tobacco_obesity_results %>% filter(term != "(Intercept)"),
         aes(x = term, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high), color = term)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
      title = "Odds Ratios for ICU Admission (Tobacco and Obesity - COVID Cases)",
      x = "Variable",
      y = "Odds Ratio (Exp(Estimate))"
    ) +
    scale_color_manual(values = custom_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  

  ggsave(here::here("output/covid_tobacco_obesity_odds_ratios.png"), width = 10, height = 6, dpi = 300)
  

  temp_bar_data <- temp_data_covid %>%
    filter(!is.na(ICU), !is.na(TOBACCO), !is.na(OBESITY)) %>%
    group_by(ICU, TOBACCO, OBESITY) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(ICU = factor(ICU, levels = c(0, 1), labels = c("No", "Yes")))
  
  ggplot(temp_bar_data, aes(x = TOBACCO, y = count, fill = OBESITY)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ICU) +
    labs(
      title = "Stacked Bar Plot: Tobacco and Obesity vs ICU Admission (COVID Cases)",
      x = "Tobacco Use",
      y = "Proportion",
      fill = "Obesity"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  

  ggsave(here::here("output/covid_tobacco_obesity_stacked_bar.png"), width = 10, height = 6, dpi = 300)
  
} else {
  temp_data_non_covid <- data_clean_covid %>%
    filter(CLASIFICATION_FINAL >= 4)
  


  severity_model <- glm(ICU ~ DIABETES + RENAL_CHRONIC + SEX + AGE + HIPERTENSION,
                        data = temp_data_non_covid %>% filter(!is.na(ICU)),
                        family = binomial)
  
  severity_results <- tidy(severity_model, conf.int = TRUE)
  

  write.csv(severity_results, file = here::here("output/non_covid_severity_results.csv"))
  
 
  num_terms <- nrow(severity_results %>% filter(term != "(Intercept)"))
  custom_colors <- hue_pal()(num_terms)
  
  ggplot(severity_results %>% filter(term != "(Intercept)"),
         aes(x = term, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high), color = term)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
      title = "Odds Ratios for ICU Admission (Non-COVID Cases)",
      x = "Variable",
      y = "Odds Ratio (Exp(Estimate))"
    ) +
    scale_color_manual(values = custom_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  

  ggsave(here::here("output/non_covid_severity_odds_ratios.png"), width = 10, height = 6, dpi = 300)
  
 
  mortality_model <- glm(MORTALITY ~ AGE + HIPERTENSION + SEX + DIABETES + COPD + OBESITY,
                         data = temp_data_non_covid %>% filter(!is.na(MORTALITY)),
                         family = binomial)
  
  mortality_results <- tidy(mortality_model, conf.int = TRUE)
  

  write.csv(mortality_results, file = here::here("output/non_covid_mortality_results.csv"))
  

  num_terms <- nrow(mortality_results %>% filter(term != "(Intercept)"))
  custom_colors <- hue_pal()(num_terms)
  
  ggplot(mortality_results %>% filter(term != "(Intercept)"),
         aes(x = term, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high), color = term)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
      title = "Odds Ratios for Mortality (Non-COVID Cases)",
      x = "Variable",
      y = "Odds Ratio (Exp(Estimate))"
    ) +
    scale_color_manual(values = custom_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

  ggsave(here::here("output/non_covid_mortality_odds_ratios.png"), width = 10, height = 6, dpi = 300)
  

  tobacco_obesity_model <- glm(ICU ~ TOBACCO + OBESITY + SEX + AGE,
                               data = temp_data_non_covid %>% filter(!is.na(ICU), !is.na(TOBACCO), !is.na(OBESITY)),
                               family = binomial)
  
  tobacco_obesity_results <- tidy(tobacco_obesity_model, conf.int = TRUE)
  

  write.csv(tobacco_obesity_results, file = here::here("output/non_covid_tobacco_obesity_results.csv"))
  

  num_terms <- nrow(tobacco_obesity_results %>% filter(term != "(Intercept)"))
  custom_colors <- hue_pal()(num_terms)
  
  ggplot(tobacco_obesity_results %>% filter(term != "(Intercept)"),
         aes(x = term, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high), color = term)) +
    geom_pointrange() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(
      title = "Odds Ratios for ICU Admission (Tobacco and Obesity - Non-COVID Cases)",
      x = "Variable",
      y = "Odds Ratio (Exp(Estimate))"
    ) +
    scale_color_manual(values = custom_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  

  ggsave(here::here("output/non_covid_tobacco_obesity_odds_ratios.png"), width = 10, height = 6, dpi = 300)
  

  temp_bar_data <- temp_data_non_covid %>%
    filter(!is.na(ICU), !is.na(TOBACCO), !is.na(OBESITY)) %>%
    group_by(ICU, TOBACCO, OBESITY) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(ICU = factor(ICU, levels = c(0, 1), labels = c("No", "Yes")))
  
  ggplot(temp_bar_data, aes(x = TOBACCO, y = count, fill = OBESITY)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ICU) +
    labs(
      title = "Stacked Bar Plot: Tobacco and Obesity vs ICU Admission (Non-COVID Cases)",
      x = "Tobacco Use",
      y = "Proportion",
      fill = "Obesity"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  

  ggsave(here::here("output/non_covid_tobacco_obesity_stacked_bar.png"), width = 10, height = 6, dpi = 300)
}