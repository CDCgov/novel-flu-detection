## Renaming variables for plotting -----

rename_vars <- function(dat, rename_cases = FALSE, rename_tests = FALSE) {
  tmp <- dat %>% mutate(varname = recode(var, 
                                  p.presentation = "Probability of\npresentation",
                                  p.testing = "Proportion\ntested",
                                  p.test = "Proportion\ntested",
                                  prob.detect = "Probability of\ndetecting at least one", 
                                  num.detected = "Number detected cases",
                                  num.tests = "Number of tests\nper month", 
                                  expected.fp = "Expected false positives",
                                  detected.per.test = "Number detected\nper 100k tests",
                                  PPV = "Positive predictive value",
                                  perc.detected = "Percent\ndetected (%)")
                 )
  
  if (rename_cases) tmp <- tmp %>% mutate(num.cases = paste(num.cases, "cases"))
  if (rename_tests) tmp <- tmp %>% mutate(tests = paste(tests, "tests"))
  
  tmp
}


## Plotting theme --------


get_theme <- function(txt = 12, ...) {
  theme_bw() + 
    theme(axis.text    = element_text(size = txt),
          axis.title   = element_text(size = txt + 1),
          title        = element_text(size = txt),
          legend.title = element_text(size = txt + 1),
          legend.text  = element_text(size = txt),
          strip.text.x = element_text(size = txt),
          ...)
}
