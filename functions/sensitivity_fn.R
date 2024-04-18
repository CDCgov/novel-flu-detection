simulate <- function(p_symp,
                     h_testing,  uc_testing,  ic_testing, cm_testing,
                     h_careseek, uc_careseek, ic_careseek,
                     base_uc, hosp_icu_correction = FALSE, 
                     forward_min = 1, forward_max = 1) {
  
  full <- data.table(expand.grid(iter=1:n.samples, type=types, incidence = incidences, population=pops))
  
  if (hosp_icu_correction) {
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
      
      # Healthcare setting for cases - what % of cases seek care and are tested in each setting
      p.test.case.h     = 
        h_careseek *                      # What % symptomatic cases seek care at hospital
        h_testing,                        # What % of people in H are tested? (VISION)
      
      p.test.case.uc      = 
        uc_careseek *                     # What % symptomatic cases seek care at urgent care
        uc_testing,                       # What % of people in urgent care are tested?
      
      p.test.case.ic      = 
        ic_careseek *                      # What % symptomatic cases seek care at ICU
        ic_testing,                        # What % of people in urgent care are tested?
      
      p.test.case.cm      = cm_testing,   # What % of people in community are tested?
      
      # Healthcare setting for non-cases - what % of general population seek care and are tested in each setting
      p.test.noncase.h  = 
        runif(n.samples, .0003, .001) *    # What % general population present with ARI at hospital
        h_testing,                         # What % of people in H are tested? (VISION)
      
      p.test.noncase.uc = 
        runif(n.samples, .006, .06) *   # Population prevalence of ILI
        base_uc *                       # What % symptomatic cases seek care at urgent care
        uc_testing,                     # What % of people in urgent care are tested?
      
      p.test.noncase.ic = 
        runif(n.samples, .00019, .0003) *   # What % general population present with ARI at ICU?
        ic_testing,
      
      p.test.noncase.cm = cm_testing,       # What % general population present with ARI at ICU?
      
      # Placeholders: (for graphing assumptions)
      
      # chance of presenting in setting
      p.presentation.h  = h_careseek,
      p.presentation.uc = uc_careseek,
      p.presentation.ic = ic_careseek,
      p.presentation.cm = 1,
      
      # chance of testing in setting
      p.testing.h  = h_testing,
      p.testing.uc = uc_testing,
      p.testing.ic = ic_testing,
      p.testing.cm = cm_testing
    )
  } else {
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
      
      # Healthcare setting for cases - what % of cases seek care and are tested in each setting
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
      
      p.test.case.cm      = cm_testing,   # What % of people in community are tested?
      
      # Healthcare setting for non-cases - what % of general population seek care and are tested in each setting
      p.test.noncase.h  = 
        runif(n.samples, .0003, .001) *    # What % general population present with ARI at hospital
        h_testing,                         # What % of people in H are tested? (VISION)
      
      p.test.noncase.uc = 
        runif(n.samples, .006, .06) *   # Population prevalence of ILI
        base_uc *                       # What % symptomatic cases seek care at urgent care
        uc_testing,                     # What % of people in urgent care are tested?
      
      p.test.noncase.ic = 
        runif(n.samples, .00019, .0003) *   # What % general population present with ARI at ICU?
        ic_testing,
      
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
      p.testing.cm = cm_testing
    )
  }
 

  # Adding data on surveillance system specific assumptions
  
  full <- merge(full, testing.vars, by="iter")
  
  
  ## Analysis --------------------------------------------------------------------        
  
  
  # Creating variables for Hospital (H)
  
  full[type == "H", `:=` ( 
    
    p.presentation = p.symp * p.presentation.h,
    p.testing = p.testing.h,
    
    detection.time = detection.time.health,
    
    # Number of infected people tested 
    num.case.tested = population * incidence * p.symp * p.test.case.h, 
    
    # Number of negative people tested 
    num.noncase.tested = population * (1-incidence) * p.test.noncase.h,
    
    # include propensity to forward in healthcare settings
    sen.PCR = sen.PCR * p.forward
  )]
  
  
  
  ## -------------------------------------------------------------------------------------------#   
  
  # Creating variables for Urgent care (UC)
  
  full[type == "UC/ED", `:=` ( 
    
    p.presentation = p.symp * p.presentation.uc,
    p.testing = p.testing.uc,
    
    detection.time = detection.time.health,
    
    # Number of infected people tested 
    num.case.tested = population * incidence * p.symp * p.test.case.uc, 
    
    # Number of negative people tested 
    num.noncase.tested = population * (1-incidence) * p.test.noncase.uc,
    
    # include propensity to forward in healthcare settings
    sen.PCR = sen.PCR * p.forward
  )]
  
  
  ## -------------------------------------------------------------------------------------------#   
  
  # Creating variables for Intensive care (ICU)
  
  full[type == "ICU", `:=` ( 
    
    p.presentation = p.symp * p.presentation.ic,
    p.testing = p.testing.ic,
    
    detection.time = detection.time.health,
    
    # Number of infected people tested 
    num.case.tested = population * incidence * p.symp * p.test.case.ic, 
    
    # Number of negative people tested 
    num.noncase.tested = population * (1-incidence) * p.test.noncase.ic,
    
    # include propensity to forward in healthcare settings
    sen.PCR = sen.PCR * p.forward
  )]
  
  
  ## -------------------------------------------------------------------------------------------#   
  
  # Creating variables for Community testing 
  
  full[type == "Community", `:=` ( 
    
    p.presentation = p.presentation.cm,
    p.testing = p.testing.cm,
    
    detection.time = detection.time.commun,
    
    # Number of infected people tested 
    num.case.tested = population * incidence * p.test.case.cm, 
    
    # Number of negative people tested 
    num.noncase.tested = population * (1-incidence) * p.test.noncase.cm
  )]
  
  
  ## -------------------------------------------------------------------------------------------
  
  # Calculate all other variables
  
  full[, `:=` (
    
    # Total number tested
    num.tests = num.case.tested + num.noncase.tested,
    
    # Probability of detection 
    prob.detect   = 1 - (1 - sen.PCR * detection.time * num.case.tested/population)^population,
    
    num.detected  =  num.case.tested * sen.PCR * detection.time,
    num.cases     = (population * incidence),
    perc.detected = (num.case.tested * sen.PCR * detection.time) / (population * incidence) # num detected / total num

  )]
  
  # Calculate cases detected per 100k tests
  full[, `:=` ( detected.per.test = num.detected / num.tests * 100000)]
  
  # Keeping important variables
  full <- full[, list(type, population, incidence, p.presentation, p.testing,
                      prob.detect, num.tests, perc.detected,
                      num.detected, num.cases, detected.per.test)]
  
  return(full)
}
