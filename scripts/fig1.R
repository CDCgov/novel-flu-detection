## Set up -------------------------------

source(here("functions", "sensitivity_fn.R"))
set.seed(123)

## Input assumptions ----------------------

# Assumptions about testing
h.m   <- 0.53
uc.m  <- 0.5
ic.m  <- 0.46

# Propensity to forward tests for subtyping

# PRE-PANDEMIC:
# - Sumner et al 2023 Lancet Microbe: 47% Aof flu As had a known subtype in FluSurvNET (40-61% from 2010/11 -- 2019/20)
#   [see Table S2] https://doi.org/10.1016/S2666-5247(23)00187-8
# - Chow et al JAMA Net Open 2020: 44% of flu A infections had subtyping performed in FluSurvNET (2010/11 -- 2018/19)
#   https://jamanetwork.com/journals/jamanetworkopen/article-abstract/2762991

forward_min <- forward_max <- forward_main

print(paste0("Test forwarding = ", forward_min*100, "%"))

# Load severity scenario parameters
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
  
  res[[p]] <- simulate(p_symp[p,], 
                       h_testing,      uc_testing,      ic_testing,    cm_testing,
                       h_careseek[p,], uc_careseek[p,], ic_careseek[p,], 
                       base_uc, hosp_icu_correction,
                       forward_min, forward_max) %>%
    mutate(scenario = scenarios[p])
}

res <- bind_rows(res)


# Calculating probability of detection for each incidence, population, and type
summary_sev <- res %>%
  gather(var, val, prob.detect, p.presentation, p.testing, num.tests, 
         num.detected, detected.per.test, perc.detected) %>%
  group_by(incidence, population, num.cases, type, var, scenario) %>%
  summarise(val.m   = median(val),
            val.l95 = quantile(val, .025),
            val.u95 = quantile(val, .975),
            val.l50 = quantile(val, .25),
            val.u50 = quantile(val, .75) 
  ) %>%
  ungroup()


## Plot output --------------------------------------------

summary_sev <- rename_vars(summary_sev, rename_cases = TRUE) %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("Base-line", "COVID-like", 
                                      "Int 1", "Int 2", "Recent H5"))) %>%
  mutate(type = factor(type, levels = c("Community", "UC/ED", "H", "ICU")))

# palette when no community setting (to present ordering)
colvals3   <- RColorBrewer::brewer.pal(4, "Dark2")[c(2, 3, 4)]

# graphs to show assumptions
p_present <-  
  summary_sev %>% filter(var %in% c("p.presentation"), num.cases == "100 cases",
                         type != "Community") %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Probability of\npresentation") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_manual(guide = "none", values = colvals3)

p_testing <- 
  summary_sev %>% filter(var %in% c("p.testing"), num.cases == "100 cases") %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Proportion\ntested") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") + guides(color=guide_legend(nrow=2, byrow=TRUE))


## graphs to show results
p_detect <- 
  summary_sev %>% filter(var %in% c("prob.detect")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Probability of\ndetecting at least one") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") + guides(color=guide_legend(nrow=2, byrow=TRUE))


p_tests <- 
  summary_sev %>% filter(var %in% c("num.tests")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ num.cases) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + scale_y_log10(label = comma) +
  labs(x = NULL, y = "Number of tests\nper month") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") + guides(color=guide_legend(nrow=2, byrow=TRUE))


p_effort <- summary_sev %>% filter(var %in% c("detected.per.test")) %>%
  ggplot(aes(x = scenario, color = type, y = val.m, group = type)) +
  geom_errorbar(aes(ymin = val.l50, ymax = val.u50), 
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = val.l95, ymax = val.u95), alpha = 0.4,
                size = 3, width = 0, position = position_dodge(width = 0.5)) +
  facet_wrap(~ num.cases, scales = "free_y") +
  geom_point(size = 2.5, position = position_dodge(width = 0.5), color = "black") +
  get_theme(txt = txtsize) + 
  labs(x = NULL, y = "Number detected\nper 100k tests") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
  scale_color_brewer("Setting", palette = "Dark2") + 
  scale_y_continuous(trans = "log10", labels = comma) + guides(color=guide_legend(nrow=2, byrow=TRUE))

## percent of cases detected
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
  scale_color_brewer("Setting", palette = "Dark2") + guides(color=guide_legend(nrow=2, byrow=TRUE))

design <- "
12 
33 
44 
55 
67 
"    

fig1 <- p_present + p_testing +
  p_detect + p_tests + p_effort + p_perc + guide_area() +
  plot_layout(design = design, guides = "collect") + plot_annotation(tag_levels = "A")

