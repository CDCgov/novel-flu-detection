# Set up -------------------------------

source(here("functions", "severity_seasonal_fn.R"))
set.seed(123)

# Assumptions about average testing
increase_testing <- FALSE

# set testing means for each healthcare setting
if (increase_testing) {
  h.m   <- 0.95
  uc.m  <- 0.95
  ic.m  <- 0.95
} else {
  h.m   <- 0.53
  uc.m  <- 0.5
  ic.m  <- 0.46
}

# Test forwarding
forward_min <- forward_max <- forward_main

## Assumptions about % reduction in testing in off-peak vs peak months
if (!exists("test_freq")) {
  test_freq <- 1
}
print(paste0("Off-peak testing = ", test_freq * 100, "%"))

## Parameter setup: different severity scenarios 
source(here("parameters.R"))


# Baseline testing parameters ---------------------

# baseline testing in different settings (VISION)
h_testing  <- truncnorm::rtruncnorm(n.samples, a = .2,  b = .95, mean = h.m,  sd = 0.1)
uc_testing <- truncnorm::rtruncnorm(n.samples, a = .1,  b = .9,  mean = uc.m, sd = 0.1)
ic_testing <- truncnorm::rtruncnorm(n.samples, a = .01, b = .95, mean = ic.m, sd = 0.1)

# Community testing (from UK scenarios): 
# 1/1000 -- 1/200 daily --> approx 30/1000 -- 30/200 per month
# lower upper bound since # tests drowns out all other settings 
cm_testing <- runif(n.samples, 30/1000, 30/500)


## Analysis: simulate severity scenarios ---------------------------------------     

res <- list()


for(p in 1:length(scenarios)) {
  
  res[[p]] <- simulate_seasonal(p_symp[p,],      test_freq,
                                h_testing,      uc_testing,      ic_testing,    cm_testing,
                                h_careseek[p,], uc_careseek[p,], ic_careseek[p,],
                                base_uc,
                                forward_min, forward_max
                                ) %>%
    mutate(scenario = scenarios[p])
}

res <- bind_rows(res)


# Calculating probability of detection for each incidence, population, and type
summary_sev0 <- res %>%
  gather(var, val, p.presentation:PPV.out, perc.detected.in, perc.detected.out) %>%
  group_by(incidence, population, num.cases, type, var, scenario) %>%
  summarise(val.m   = median(val),
            val.l95 = quantile(val, .025),
            val.u95 = quantile(val, .975),
            val.l50 = quantile(val, .25),
            val.u50 = quantile(val, .75) 
  ) %>%
  ungroup()  %>% 
  mutate(activity = str_extract(var,  "in|out"),
         var      = str_replace(var, ".in|.out", replacement = ""))  


## Plot output --------------------------------------------

summary_sev <- rename_vars(summary_sev0, rename_cases = TRUE) %>% 
  filter(type != "Community") %>%
  mutate(scenario = factor(scenario, 
                           levels = c("Base-line", "COVID-like", 
                                      "Int 1", "Int 2", "Recent H5"))) %>%
  mutate(activity = ifelse(is.na(activity), "in", activity),
         type0 = type,
         type = ifelse(activity == "in", paste(type, "(peak)"), paste(type, "(off-peak)") ) ) %>%
  mutate(type = factor(type, levels = c("UC/ED (peak)", "UC/ED (off-peak)",
                                        "H (peak)", "H (off-peak)", 
                                        "ICU (peak)", "ICU (off-peak)")),
         type0 = factor(type0, levels = c("UC/ED", "H", "ICU"))) %>%
  filter(num.cases == "100 cases")


# manual color-scale settings to ensure settings keep same colors as baseline plots
colvals3   <- RColorBrewer::brewer.pal(4, "Dark2")[c(2, 3, 4)]

basecols <- RColorBrewer::brewer.pal(4, "Dark2")
darkcols <- colorspace::darken(basecols, amount = 0.4)
colvals  <- c(basecols, darkcols)[c(2, 6, 3, 7, 4, 8)]

barsze <- 2.7

# graphs to show assumptions
p_present <-  
  summary_sev %>% filter(var %in% c("p.presentation"), num.cases == "100 cases",
                         type != "Community") %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Probability of\npresentation") +
  #scale_y_continuous(limits = c(0, 0.35)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual(guide = "none", values = colvals3)

p_testing <- 
  summary_sev %>% filter(var %in% c("p.test"), num.cases == "100 cases", 
                         type != "Community", scenario == "Base-line") %>%
  ggplot(aes(x = type0, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + ylim(c(0, 1)) +
  labs(x = NULL, y = "Proportion\ntested") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual(guide = "none", values = colvals) 

## graphs to show results
p_detect <- 
  summary_sev %>% filter(var %in% c("prob.detect")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  #facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Probability of\ndetecting at least one") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual(guide = "none", values = colvals)


p_tests <- 
  summary_sev %>% filter(var %in% c("num.tests")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  #facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + scale_y_log10(label = comma) +
  labs(x = NULL, y = "Number of tests\nper month") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual("Setting", values = colvals) 


p_effort <- summary_sev %>% filter(var %in% c("detected.per.test")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = barsze, width = 0, position = position_dodge(width = 0.5)) +
  #facet_wrap(~ num.cases, scales= "free_y") +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Number detected\nper 100k tests") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual("Setting", values = colvals) + scale_y_continuous(trans = "log10", labels = comma)

p_perc <- summary_sev %>% filter(var %in% c("perc.detected")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m *100, group = type)) +
  geom_errorbar(aes(ymin = val.l50 *100, ymax = val.u50 *100), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95 *100, ymax = val.u95 *100), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Percent\ndetected (%)") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual(guide = "none", values = colvals) 

fig_tmp <- (p_present + p_testing) /
  p_detect / p_tests / p_effort / p_perc  +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")


if(test_freq == 1) { 
  figS3 <- fig_tmp
} else { 
  fig2 <- fig_tmp
}
