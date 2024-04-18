
scenarios <- c("Base-line", "COVID-like", "Int 1", "Int 2", "Recent H5")

# 4 Scenarios:
# 1. Seasonal (baseline): CHR = 1-2%, ICU = 10-20% hospitalizations
# 2. COVID-like: CHR = ; ICU = 25% hospitalizations
# 3. 1918-like: CHR = 5% average; ICU = 35% hospitalizations
# 4. H5-like: CHR = ; ICU = 50% hospitalizations

# Pr(symptoms) -- set to 1 if using IHRs instead of CHRs for hospitalizations
p_symp <- rbind(
  runif(n.samples, 0.4, 0.8),  # seasonal flu
  runif(n.samples, 1, 1),      # covid-like (CDC planning scenarios)
  runif(n.samples, 1, 1),      # intermediate
  runif(n.samples, 1, 1),      # severe
  runif(n.samples, 1, 1)       # current h5-like
)


# multiply by P(symptomatic) for scenarios where IHR is being used (and so P(symp) = 1 above)
base_uc <- runif(n.samples, 0.1, 0.2)

uc_careseek <- rbind(
  base_uc,                                              # seasonal flu
  base_uc * runif(n.samples, 0.4, 0.8),                 # covid * p(symp)
  base_uc * 1.25 * runif(n.samples, 0.4 * 1.25, 1),     # p(symp) = 25% x flu AND p(uc) = 25% x flu
  base_uc * 1.5  * runif(n.samples, 0.4 * 1.5,  1),
  base_uc * 1.5  * runif(n.samples, 0.4 * 1.5,  1)
)


# each row represents a different severity scenario 
h_careseek  <- rbind(
  runif(n.samples, 0.01, 0.02),    # seasonal flu     -- CHR x p_symp = 0.4-0.8)
  runif(n.samples, 0.01, 0.02),    # covid-like       -- IHR x p_symp = 1)
  runif(n.samples, 0.045, 0.055),  # intermediate     -- IHR x p_symp = 1)
  runif(n.samples, 0.095, 0.105),  # severe           -- IHR x p_symp = 1)
  runif(n.samples, 0.60,  0.70)    # current h5-like  -- IHR x p_symp = 1)
)

# icu as a fraction of hospitalizations (with 10% range around assumed mean)
ic_careseek  <- rbind(
  runif(n.samples, 0.15,  0.2),  # seasonal flu
  runif(n.samples, 0.2,  0.3),   # covid-like
  runif(n.samples, 0.3,  0.4),   # intermediate
  runif(n.samples, 0.45, 0.55),  # severe
  runif(n.samples, 0.75, 0.85)   # current H5-like
)


# Correct for people in H also presenting in ICU -- don't count twice
if (hosp_icu_correction) {
  ic_careseek <- h_careseek * ic_careseek
  h_careseek  <- h_careseek - ic_careseek
}
