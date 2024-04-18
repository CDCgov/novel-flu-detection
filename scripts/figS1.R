## Set up -------------------------------

source(here("functions", "sensitivity_fn.R"))
set.seed(123)

## Input assumptions ----------------------

# Propensity to forward tests for subtyping
forward <- c(0.25, 0.5, 0.75, 1)

# Assumptions about testing
h.m   <- 0.53
uc.m  <- 0.5
ic.m  <- 0.46

# Parameter setup: different severity scenarios
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

k <- 1

for(p in 1:length(scenarios)) {
  for (f in 1:length(forward)) {
    
    res[[k]] <- simulate(p_symp[p,], 
                         h_testing,      uc_testing,      ic_testing,    cm_testing,
                         h_careseek[p,], uc_careseek[p,], ic_careseek[p,], 
                         base_uc, hosp_icu_correction,
                         forward[f], forward[f]) %>%
      mutate(scenario = scenarios[p], forward = forward[f])
    
    k <- k + 1
  }
}

res <- bind_rows(res)


# Calculating probability of detection for each incidence, population, and type
summary_sev <- res %>%
  gather(var, val, prob.detect, p.presentation, p.testing, perc.detected,
         num.tests, num.detected, detected.per.test) %>%
  group_by(incidence, population, num.cases, type, var, scenario, forward) %>%
  summarise(val.m   = median(val),
            val.l95 = quantile(val, .025),
            val.u95 = quantile(val, .975),
            val.l50 = quantile(val, .25),
            val.u50 = quantile(val, .75) 
  ) %>%
  ungroup()


## Plot output --------------------------------------------

summary_sev <- rename_vars(summary_sev, rename_cases = TRUE) %>% 
  mutate(type = factor(type, levels = c("Community", "UC/ED", "H", "ICU"))) %>%
  mutate(forward = paste0(forward*100, "% forwarded"),
         forward = factor(forward, levels = paste0(!!forward * 100, "% forwarded"))) %>%
  # focus on baseline severity in paper
  filter(scenario == "Base-line")

# palette when no community setting (to present ordering)
colvals3   <- RColorBrewer::brewer.pal(4, "Dark2")[c(2, 3, 4)]


## graphs to show results
p_detect <- 
  summary_sev %>% filter(var %in% c("prob.detect")) %>%
  ggplot(aes(x = forward, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Probability of\ndetecting at least one") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer(guide = "none", palette = "Dark2")


p_tests <- 
  summary_sev %>% filter(var %in% c("num.tests")) %>%
  ggplot(aes(x = forward, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + scale_y_log10(label = comma) +
  labs(x = NULL, y = "Number of tests\nper month") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer(guide = "none", palette = "Dark2")


p_effort <- summary_sev %>% filter(var %in% c("detected.per.test")) %>%
  ggplot(aes(x = forward, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Number detected\nper 100k tests") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer(guide = "none", palette = "Dark2") + 
  scale_y_continuous(trans = "log10", labels = comma)

## percent of cases detected
p_perc <- summary_sev %>% filter(var %in% c("perc.detected")) %>%
  ggplot(aes(x = forward, color = type, y = val.m *100, group = type)) +
  geom_errorbar(aes(ymin = val.l50 *100, ymax = val.u50 *100), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95 *100, ymax = val.u95 *100), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Percent\ndetected (%)") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") + guides(color=guide_legend(nrow=2, byrow=TRUE))

design <- "
11 
22 
33
45 
"    

fig_S1 <- p_detect + p_tests + p_effort + p_perc + guide_area() +
  plot_layout(design = design, guides = "collect") + plot_annotation(tag_levels = "A")

