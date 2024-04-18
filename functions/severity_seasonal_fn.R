simulate_seasonal <- function(p_symp,     test_freq = 1,
                              h_testing,  uc_testing,  ic_testing, cm_testing,
                              h_careseek, uc_careseek, ic_careseek,
                              base_uc, 
                              forward_min = 1, forward_max = 1) {
  
  full <- data.table(expand.grid(iter=1:n.samples, type=types, incidence = incidences, population=pops))
  
  testing.vars <- data.table(
    iter = 1:n.samples,
    
    # Testing variables
    sen.PCR           = runif(n.samples, 0.80, 1),      # sensitivity of PCR
    spec.PCR          = runif(n.samples, 0.95, 1),      # specificity of PCR
    
    detection.time.health = runif(n.samples, 0.5, 0.85),    # % people seek care within 7d
    detection.time.commun = runif(n.samples, 0.25, 0.5),    # months virus detectable
    
    # Test forwarding
    p.forward         = runif(n.samples, forward_min, forward_max),
    
    # Symptom variables for cases
    p.symp            = p_symp,      # What % of people who are infected have symptoms? 
    
    # Peak activity months: What % of cases seek care and are tested in each setting
    p.test.case.h     = 
      h_careseek *                      # What % symptomatic cases seek care at hospital
      h_testing,                        # What % of people in H are tested? (VISION)
    
    p.test.case.uc      = 
      uc_careseek *                     # What % symptomatic cases seek care at urgent care
      uc_testing,                       # What % of people in urgent care are tested?
    
    p.test.case.ic      = 
      h_careseek *                      # What % symptomatic cases seek care at ICU
      ic_careseek *                     # = expressed as a fraction of hospitalizations
      ic_testing,                       # What % of people in urgent care are tested?
    
    # Months outside peak activity
    p.test.case.h.out   = 
      h_careseek *                      # What % symptomatic cases seek care at hospital
      h_testing * test_freq,            # What % of people in H are tested? (VISION)
    
    p.test.case.uc.out  = 
      uc_careseek *                     # What % symptomatic cases seek care at urgent care
      uc_testing * test_freq,           # What % of people in urgent care are tested?
    
    p.test.case.ic.out  = 
      h_careseek *                      # What % symptomatic cases seek care at ICU
      ic_careseek *                     # = expressed as a fraction of hospitalizations
      ic_testing * test_freq,           # What % of people in urgent care are tested?
    
    p.test.case.cm      = cm_testing,   # What % of people in community are tested?
    
    # Non-cases: peak activity - what % of general population seek care and are tested in each setting
    # Months of peak activity
    p.test.noncase.h  = 
      runif(n.samples, .0004, .001) *   # What % general population present with ARI at hospital
      h_testing,                        # What % of people in H are tested? (FluSurv-Net)
    
    p.test.noncase.uc = 
      runif(n.samples, .01, .06) *    # Population prevalence of ILI
      base_uc *                       # What % symptomatic cases seek care at urgent care
      uc_testing,                     # What % of people in urgent care are tested?
    
    p.test.noncase.ic = 
      runif(n.samples, .0004, .001) * # What % general population present with ARI at ICU?
      0.35 *                          # MarketScan => ~ 35% of hospitalizations
      ic_testing,
    
    # Months outside peak activity
    p.test.noncase.h.out  = 
      runif(n.samples, .0003, .0009) *        # What % general population present with ARI at hospital
      h_testing * test_freq,                  # What % of people in H are tested? (VISION)
    
    p.test.noncase.uc.out = 
      runif(n.samples, .006, .025) *          # Population prevalence of ILI
      base_uc *                           # What % symptomatic cases seek care at urgent care
      uc_testing * test_freq,                 # What % of people in urgent care are tested?
    
    p.test.noncase.ic.out = 
      runif(n.samples, .0003, .0009) *        # What % general population present with ARI at ICU?
      0.35 *                                  # MarketScan => ~ 35% of hospitalizations
      ic_testing * test_freq,
  
    p.test.noncase.cm = cm_testing,       # What % general population present with ARI at ICU?
    
    # Placeholders: (for graphing assumptions)
    
    # chance of presenting in setting
    p.presentation.h  = h_careseek,
    p.presentation.uc = uc_careseek,
    p.presentation.ic = h_careseek * ic_careseek,
    p.presentation.cm = 1,
    
    # chance of testing in setting
    p.testing.h  = h_testing,
    p.testing.uc = uc_testing,
    p.testing.ic = ic_testing,
    p.testing.cm = cm_testing,
    
    p.testing.h.out  = h_testing  * test_freq,
    p.testing.uc.out = uc_testing * test_freq,
    p.testing.ic.out = ic_testing * test_freq,
    p.testing.cm.out = cm_testing
  )
  
  
  # Adding data on surveillance system specific assumptions
  
  full <- merge(full, testing.vars, by="iter")
  
  
  ## Analysis --------------------------------------------------------------------        
  
  # Creating variables for Hospital (H)
  
  full[type == "H", `:=` ( 
    # keep track of for plots
    p.presentation = p.symp * p.presentation.h,
    p.test.in   = p.testing.h,
    p.test.out  = p.testing.h.out,
    
    detection.time = detection.time.health,
    
    # Number of infected people tested (in and out of peak activity)
    num.case.tested.in  = population * incidence * p.symp * p.test.case.h, 
    num.case.tested.out = population * incidence * p.symp * p.test.case.h.out, 
    
    # Number of negative people tested  (in and out of peak activity)
    num.noncase.tested.in  = population * (1-incidence) * p.test.noncase.h,
    num.noncase.tested.out = population * (1-incidence) * p.test.noncase.h.out,
    
    # include propensity to forward in healthcare settings
    sen.PCR = sen.PCR * p.forward
  )]
  
  
  ## -------------------------------------------------------------------------------------------#   
  
  # Creating variables for Urgent care (UC)
  
  full[type == "UC/ED", `:=` ( 
    # keep track of for plots
    p.presentation = p.symp * p.presentation.uc,
    p.test.in   = p.testing.uc,
    p.test.out  = p.testing.uc.out,
    
    detection.time = detection.time.health,
    
    # Number of infected people tested (in and out of peak activity)
    num.case.tested.in  = population * incidence * p.symp * p.test.case.uc, 
    num.case.tested.out = population * incidence * p.symp * p.test.case.uc.out, 
    
    # Number of negative people tested (in and out of peak activity)
    num.noncase.tested.in  = population * (1-incidence) * p.test.noncase.uc,
    num.noncase.tested.out = population * (1-incidence) * p.test.noncase.uc.out,
    
    # include propensity to forward in healthcare settings
    sen.PCR = sen.PCR * p.forward
  )]
  
  
  ## -------------------------------------------------------------------------------------------#   
  
  # Creating variables for Intensitve care (ICU)
  
  full[type == "ICU", `:=` ( 
    # keep track of for plots
    p.presentation = p.symp * p.presentation.ic,
    p.test.in   = p.testing.ic,
    p.test.out  = p.testing.ic.out,
    
    detection.time = detection.time.health,
    
    # Number of infected people tested (in and out of peak activity)
    num.case.tested.in  = population * incidence * p.symp * p.test.case.ic, 
    num.case.tested.out = population * incidence * p.symp * p.test.case.ic.out, 
    
    # Number of negative people tested (in and out of peak activity)
    num.noncase.tested.in  = population * (1-incidence) * p.test.noncase.ic,
    num.noncase.tested.out = population * (1-incidence) * p.test.noncase.ic.out,
    
    # include propensity to forward in healthcare settings
    sen.PCR = sen.PCR * p.forward
  )]
  
  
  ## -------------------------------------------------------------------------------------------#   
  
  # Creating variables for Community testing 
  
  full[type == "Community", `:=` ( 
    # keep track of for plots
    p.presentation = p.presentation.cm,
    p.test.in   = p.testing.cm,
    p.test.out  = p.testing.cm.out,
    
    detection.time = detection.time.commun,
    
    # Number of infected people tested (in and out of peak activity)
    num.case.tested.in  = population * incidence * p.symp * p.test.case.cm, 
    num.case.tested.out = population * incidence * p.symp * p.test.case.cm,
    
    # Number of negative people tested (in and out of peak activity)
    num.noncase.tested.in  = population * (1-incidence) * p.test.noncase.cm,
    num.noncase.tested.out = population * (1-incidence) * p.test.noncase.cm
  )]
  
  
  ## -------------------------------------------------------------------------------------------
  
  # Calculate all other variables
  
  full[, `:=` (
    
    # Total number tested
    num.tests.in  = num.case.tested.in  + num.noncase.tested.in,
    num.tests.out = num.case.tested.out + num.noncase.tested.out,
    
    # Probability of detection (in a given week)
    prob.detect.in    = 1 - (1 - sen.PCR * detection.time * num.case.tested.in /population)^population,
    prob.detect.out   = 1 - (1 - sen.PCR * detection.time * num.case.tested.out/population)^population,
    
    num.detected.in   =  num.case.tested.in  * sen.PCR * detection.time,
    num.detected.out  =  num.case.tested.out * sen.PCR * detection.time,
    
    num.cases     = (population * incidence),
    
    perc.detected.in  = (num.case.tested.in  * sen.PCR * detection.time) / (population * incidence),
    perc.detected.out = (num.case.tested.out * sen.PCR * detection.time) / (population * incidence), 
    
    # Probability of at least one false positive 
    prob.fp.in        = 1 - spec.PCR^num.noncase.tested.in,
    prob.fp.out       = 1 - spec.PCR^num.noncase.tested.out,
    
    # Expected number of false positives 
    expected.fp.in  = num.noncase.tested.in  * (1 - spec.PCR),
    expected.fp.out = num.noncase.tested.out * (1 - spec.PCR)
  )]
  
  # Calculate cases detected per 100k tests
  full[, `:=` ( detected.per.test.in  = num.detected.in  / num.tests.in  * 100000,
                detected.per.test.out = num.detected.out / num.tests.out * 100000)]
  
  # Keeping important variables
  full <- full[, list(type, population, incidence, num.cases,
                      p.presentation, p.test.in, p.test.out,
                      prob.detect.in, prob.detect.out, 
                      perc.detected.in, perc.detected.out,
                      num.tests.in, num.tests.out,
                      expected.fp.in, expected.fp.out, 
                      num.detected.in, num.detected.out,  
                      detected.per.test.in, detected.per.test.out)]
  
  # PPV
  full <- mutate(full, PPV.in  = num.detected.in  / (num.detected.in  + expected.fp.in),
                       PPV.out = num.detected.out / (num.detected.out + expected.fp.out))
  
  return(full)
}