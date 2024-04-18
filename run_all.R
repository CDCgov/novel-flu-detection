## Run all analyses for paper

pacman::p_load(tidyverse, here, scales, data.table, stringr, patchwork)

source(here("functions", "helper_fns.R"))
n.samples <- 10000


## Common fixed inputs -----------------------------

# Incidences and population totals (chosen for 100, 1000 total cases)
pops <- 330e6
incidences <- 1/c(3.3e6, 3.3e5)

# Variable for the different surveillance systems: 
#  H = inpatient (hospitalization), UC/ED = Urgent care or emergency department
types <- factor(c("H", "UC/ED", "ICU", "Community"))

# correct for people in ICU also in hospital?
hosp_icu_correction <- FALSE

# % +ve tests forwarded in main analysis
forward_main <- 0.5
 

# Figure 1 results-----------

txtsize <- 9

# Generate figure
source(here::here("scripts", "fig1.R"))

# Get in-text quoted numbers
summary_sev1 <- summary_sev

# BASELINE probability of detection
summary_sev1 %>% filter(var %in% c("prob.detect"), 
                        num.cases %in% c("100 cases", "1000 cases"),
                        scenario %in% c("Base-line")) 

# BASELINE percent of all cases detected 
summary_sev1 %>% filter(var %in% c("perc.detected"), num.cases == "100 cases") %>% 
  select(type, scenario, val.m, val.u95)%>% filter(scenario == "Base-line")

# BASELINE detected per 100k
summary_sev1 %>% filter(var %in% c("detected.per.test"), num.cases == "100 cases") %>% 
  select(type, scenario, val.m, val.u95)%>% filter(scenario == "Base-line")

# ALL probability of detection with 100 cases, H5 severity
summary_sev1 %>% filter(var %in% c("prob.detect"), 
                        num.cases %in% c("100 cases"), scenario == "Recent H5") 

# ALL probability of detection in ICU
summary_sev1 %>% filter(var %in% c("prob.detect"), 
                        num.cases %in% c("100 cases", "1000 cases"), 
                        type == "ICU") 

# percent of all cases detected (Largest first)
summary_sev1 %>% filter(var %in% c("perc.detected"), num.cases == "100 cases") %>% arrange(desc(val.u95))


# Figure 2 results-----------

# Generate figure
test_freq <- 0.5  # reduction in testing %s in off-peak vs peak periods

txtsize <- 9

source(here::here("scripts", "fig2_figS3.R"))

# Get in-text quoted numbers
summary_sev2 <- summary_sev

summary_sev2 %>% filter(var %in% c("prob.detect"), 
                         num.cases == "100 cases",
                         type %in% c("UC/ED (off-peak)","H (off-peak)", "ICU (off-peak)")) %>% 
  select(type, scenario, val.m, val.l95, val.u95) %>% arrange(scenario)


# Figure S1 results-----------

# Generate figure
source(here::here("scripts", "figS1.R"))

# Get associated numbers
summary_sevS1 <- summary_sev

summary_sevS1 %>% filter(var %in% c("prob.detect"), num.cases == "100 cases") %>% 
  select(type, forward, val.m) %>% spread(forward, val.m)

summary_sevS1 %>% filter(var %in% c("prob.detect"), num.cases == "100 cases") %>% 
  select(type, forward, val.m, val.l95, val.u95) %>% filter(type == "UC/ED")


# Figure S2 results-----------

# Generate figure
source(here::here("scripts", "figS2.R"))

# Get in-text quoted numbers
summary_sevS2 <- summary_sev

summary_sevS2 %>% 
    filter(var %in% c("num.tests"), type == "ICU", num.cases == "1000 cases") %>% 
    select(type, scenario, val.l95, val.m, val.u95) 


# Figure S3 results-----------

# Generate figure
test_freq <- 1   # reduction in testing %s in off-peak vs peak periods
source(here::here("scripts", "fig2_figS3.R"))

# Get in-text quoted numbers
summary_sevS3 <- summary_sev

summary_sevS3 %>% filter(var %in% c("prob.detect"), 
                       num.cases == "100 cases",
                       type %in% c("UC/ED (off-peak)","H (off-peak)", "ICU (off-peak)")) %>% 
  select(type, scenario, val.m, val.u95)


