## Set up -------------------------------

# Source the required scripts
source(here("functions", "sensitivity_fn.R"))
set.seed(123)

# Propensity to forward tests for subtyping
forward_min <- forward_max <- forward_main

## Parameter setup: different severity scenarios 
source(here("parameters.R"))


## Simualate for different testing assumptions ----------------------

out <- list()

increase_testing <- c(FALSE, TRUE)

k <- 1

for (i in increase_testing) {
  
  # set testing means for each healthcare setting
  if (i) {
    h.m   <- 0.9
    uc.m  <- 0.9
    ic.m  <- 0.9
  } else {
    h.m   <- 0.53
    uc.m  <- 0.5
    ic.m  <- 0.46
  }
  
  # Baseline testing parameters
  
  # baseline testing in different settings (VISION)
  h_testing  <- truncnorm::rtruncnorm(n.samples, a = .2,  b = .95, mean = h.m,  sd = 0.1)
  uc_testing <- truncnorm::rtruncnorm(n.samples, a = .1,  b = .9,  mean = uc.m, sd = 0.1)
  ic_testing <- truncnorm::rtruncnorm(n.samples, a = .01, b = .95, mean = ic.m, sd = 0.1)
  
  # Community testing (from UK scenarios): 
  cm_testing <- runif(n.samples, 30/1000, 30/500)
  
  
  # Analysis: simulate severity scenarios    
  
  res <- list()
  
  for(p in 1:length(scenarios)) {
    
    res[[p]] <- simulate(p_symp[p,], 
                         h_testing,      uc_testing,      ic_testing,    cm_testing,
                         h_careseek[p,], uc_careseek[p,], ic_careseek[p,], 
                         base_uc, hosp_icu_correction,
                         forward_min, forward_max) %>%
      mutate(scenario = scenarios[p])
  }
  
  res <- bind_rows(res)
  
  # Calculating probability of detection for each incidence, population, and type
  out[[k]] <- res %>%
    gather(var, val, prob.detect, p.presentation, p.testing, perc.detected,
           num.tests, num.detected, detected.per.test) %>%
    group_by(incidence, population, num.cases, type, var, scenario) %>%
    summarise(val.m   = median(val),
              val.l95 = quantile(val, .025),
              val.u95 = quantile(val, .975),
              val.l50 = quantile(val, .25),
              val.u50 = quantile(val, .75) 
    ) %>%
    ungroup() %>%
    mutate(increase_tests = i)
  
  k <- k + 1
}

out <- bind_rows(out)

## Plot output --------------------------------------------

summary_sev <- rename_vars(out, rename_cases = TRUE) %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("Base-line", "COVID-like", 
                                      "Int 1", "Int 2", "Recent H5"))) %>%
  mutate(type = factor(type, levels = c("Community", "UC/ED", "H", "ICU")),
         increase_tests = ifelse(increase_tests, "Increased testing", "Baseline testing"))

# palette when no community setting (to present ordering)
colvals3   <- RColorBrewer::brewer.pal(4, "Dark2")[c(2, 3, 4)]


## graphs to show results
p_detect <- 
  summary_sev %>% filter(var %in% c("prob.detect"), num.cases == "100 cases") %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ increase_tests) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Probability of\ndetecting at least one") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2")


p_tests <- 
  summary_sev %>% filter(var %in% c("num.tests"), num.cases == "100 cases") %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ increase_tests) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + scale_y_log10(label = comma) +
  labs(x = NULL, y = "Number of tests\nper month") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2")


p_effort <- summary_sev %>% filter(var %in% c("detected.per.test"), num.cases == "100 cases") %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ increase_tests) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Number detected\nper 100k tests") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") + 
  scale_y_continuous(trans = "log10")

p_perc <- summary_sev %>% filter(var %in% c("perc.detected"), num.cases == "100 cases") %>%
  ggplot(aes(x = scenario, color = type, y = val.m *100, group = type)) +
  geom_errorbar(aes(ymin = val.l50 *100, ymax = val.u50 *100), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95 *100, ymax = val.u95 *100), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  facet_wrap(~ increase_tests) +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Percent\ndetected (%)") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") 


fig_S2 <- p_detect / p_tests / p_effort / p_perc + 
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
