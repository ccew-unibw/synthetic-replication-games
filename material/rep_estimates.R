############################################################
## R replication of conjoint AMCE analyses (Stata do-file)
## - Needs: haven, dplyr, fixest
## - Produces the same .txt files as the Stata code
############################################################

rm(list=ls())
library(haven)
library(dplyr)
library(fixest)
library(optparse)

############################################################
## Argument parsing
############################################################

parser <- OptionParser()
parser <- add_option(parser, c("-i", "--input"), action = "store",
                     type = "character", default = "repdata.dta",
                     help = "Input dta file to be processed. Defaults to `repdata.dta`.")
parser <- add_option(parser, c("-o", "--output"), action = "store",
                     type = "character", default = "orig",
                     help = "Output directory suffix. Defaults to `orig`.")
parser <- add_option(parser, c("-p", "--adjustprofiles"), action = "store_true",
                     default = FALSE, help = "Empirically adjust the profiles for Figure 3.")

if (interactive()) {
  # If running this script interactively, set the required arguments here
  arg <- parse_args(parser, args = c("--input=repdata.dta", "--output=orig"))
} else {
  arg <- parse_args(parser)
}

if (arg$input != "repdata.dta" && arg$output == "orig") {
  stop("If `input` is set to a dta other than the original study's file,
        `output` must be set to a value other than 'orig'!")
}

############################################################
## 0a. Output directory for txt files
############################################################

txt_dir <- paste("txt", arg$output, sep = "_")
if (!dir.exists(txt_dir)) dir.create(txt_dir)

############################################################
## 0. Load and prepare data
############################################################

data <- read_dta(
  arg$input
)

# Match Stata factor baselines ib6.FeatCountry, ib3.FeatPlans
data <- data %>%
  mutate(
    CaseID      = factor(CaseID),
    FeatGender  = factor(FeatGender),
    FeatEd      = factor(FeatEd),
    FeatJob     = factor(FeatJob),
    FeatLang    = factor(FeatLang),
    FeatCountry = relevel(factor(FeatCountry), ref = "6"),
    FeatReason  = factor(FeatReason),
    FeatExp     = factor(FeatExp),
    FeatPlans   = relevel(factor(FeatPlans), ref = "3"),
    FeatTrips   = factor(FeatTrips),
    weight2     = as.numeric(weight2)
  )

############################################################
## 1. Helpers: lincom, adjustmat, restrictamce, rebuild 50-row matrix
############################################################

## 1.1 Stata-style lincom: a'b and sqrt(a'Va)
lincom <- function(model, coefs) {
  b <- coef(model)
  V <- vcov(model)
  a <- rep(0, length(b))
  names(a) <- names(b)
  common <- intersect(names(coefs), names(b))
  a[common] <- coefs[common]
  est <- sum(a * b)
  se  <- sqrt(as.numeric(t(a) %*% V %*% a))
  list(est = est, se = se)
}

## 1.2 Extract main effects, drop intercept & interactions
adjustmat <- function(model) {
  b  <- coef(model)
  V  <- vcov(model)
  se <- sqrt(diag(V))
  df <- data.frame(
    est = as.numeric(b),
    se  = as.numeric(se),
    row.names = names(b),
    stringsAsFactors = FALSE
  )
  # drop intercept
  df <- df[rownames(df) != "(Intercept)", , drop = FALSE]
  # drop all interactions (colon)
  df <- df[!grepl(":", rownames(df)), , drop = FALSE]
  df
}

## 1.3 Full restrictamce(): Education, Profession, Country, Reason
restrictamce <- function(model, resmat) {
  # name builders
  main_ed        <- function(k) paste0("FeatEd", k)
  main_job       <- function(j) paste0("FeatJob", j)
  main_country   <- function(c) paste0("FeatCountry", c)
  main_reason    <- function(r) paste0("FeatReason", r)
  int_ed_job     <- function(k, j) paste0("FeatEd", k, ":FeatJob", j)
  int_country_rs <- function(c, r) paste0("FeatCountry", c, ":FeatReason", r)
  
  ## 1) Education AMCEs over low-skilled jobs
  for (x in 2:7) {
    jobs <- 1:11
    w    <- rep(0, length(jobs))
    names(w) <- jobs
    # 1/7 for jobs 1,2,3,4,6,7,9
    w[c(1,2,3,4,6,7,9)] <- 1/7
    w_total <- sum(w)
    
    coefs <- numeric(0)
    coefs[main_ed(x)] <- w_total
    for (j in jobs[w != 0]) {
      coefs[int_ed_job(x, j)] <- w[as.character(j)]
    }
    lc <- lincom(model, coefs)
    rn <- main_ed(x)
    if (rn %in% rownames(resmat)) {
      resmat[rn, "est"] <- lc$est
      resmat[rn, "se"]  <- lc$se
    }
  }
  
  ## 2) Profession AMCEs over education
  ## 2(a) low-skilled jobs: 2,3,4,6,7,9 (avg over all Ed 1..7)
  for (x in c(2,3,4,6,7,9)) {
    eds <- 1:7
    w   <- rep(1/7, length(eds))
    names(w) <- eds
    w_total <- sum(w)
    
    coefs <- numeric(0)
    coefs[main_job(x)] <- w_total
    for (k in 2:7) { # FeatEd1 has no interaction term
      coefs[int_ed_job(k, x)] <- w[as.character(k)]
    }
    lc <- lincom(model, coefs)
    rn <- main_job(x)
    if (rn %in% rownames(resmat)) {
      resmat[rn, "est"] <- lc$est
      resmat[rn, "se"]  <- lc$se
    }
  }
  
  ## 2(b) high-education-only jobs: 5,8,10,11 (Ed 5,6,7 only)
  for (x in c(5,8,10,11)) {
    eds <- c(5,6,7)
    w   <- rep(1/3, length(eds))
    names(w) <- eds
    w_total <- sum(w)
    
    coefs <- numeric(0)
    coefs[main_job(x)] <- w_total
    for (k in eds) {
      coefs[int_ed_job(k, x)] <- w[as.character(k)]
    }
    lc <- lincom(model, coefs)
    rn <- main_job(x)
    if (rn %in% rownames(resmat)) {
      resmat[rn, "est"] <- lc$est
      resmat[rn, "se"]  <- lc$se
    }
  }
  
  ## 3) Origin AMCEs over Reason (India is baseline, only reasons 1 & 2)
  for (x in c(1:5,7:10)) {
    coefs <- numeric(0)
    coefs[main_country(x)] <- 1      # main effect gets 1/2 + 1/2
    coefs[int_country_rs(x, 2)] <- 1/2
    # interaction with reason 3 weighted 0 -> ignore
    lc <- lincom(model, coefs)
    rn <- main_country(x)
    if (rn %in% rownames(resmat)) {
      resmat[rn, "est"] <- lc$est
      resmat[rn, "se"]  <- lc$se
    }
  }
  
  ## 4) Reason AMCEs over Origin
  ## Reason 2: all 10 countries, equal weights 1/10
  x <- 2
  coefs <- numeric(0)
  coefs[main_reason(x)] <- 1  # 10 * 1/10
  for (c in 2:10) {
    coefs[int_country_rs(c, x)] <- 1/10
  }
  lc <- lincom(model, coefs)
  rn <- main_reason(x)
  if (rn %in% rownames(resmat)) {
    resmat[rn, "est"] <- lc$est
    resmat[rn, "se"]  <- lc$se
  }
  
  ## Reason 3: only countries 7,8,9,10, weights 1/4
  x <- 3
  coefs <- numeric(0)
  coefs[main_reason(x)] <- 1  # 4 * 1/4
  for (c in c(7,8,9,10)) {
    coefs[int_country_rs(c, x)] <- 1/4
  }
  lc <- lincom(model, coefs)
  rn <- main_reason(x)
  if (rn %in% rownames(resmat)) {
    resmat[rn, "est"] <- lc$est
    resmat[rn, "se"]  <- lc$se
  }
  
  resmat
}

## 1.4 Build 50-row Stata-style matrix (add baselines, names like 1b.FeatEd etc.)
build_resmat_stata <- function(resmat) {
  add_row <- function(name, est, se) list(name = name, est = est, se = se)
  rows <- list()
  
  # Gender (2 levels, baseline 1)
  rows[[length(rows) + 1]] <- add_row("1b.FeatGender", 0, 0)
  rows[[length(rows) + 1]] <- add_row("2.FeatGender",
                                      resmat["FeatGender2","est"],
                                      resmat["FeatGender2","se"])
  # Education (1..7, baseline 1)
  for (k in 1:7) {
    if (k == 1) {
      rows[[length(rows) + 1]] <- add_row("1b.FeatEd", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatEd"),
        resmat[paste0("FeatEd",k),"est"],
        resmat[paste0("FeatEd",k),"se"]
      )
    }
  }
  # Language (1..4, baseline 1)
  for (k in 1:4) {
    if (k == 1) {
      rows[[length(rows) + 1]] <- add_row("1b.FeatLang", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatLang"),
        resmat[paste0("FeatLang",k),"est"],
        resmat[paste0("FeatLang",k),"se"]
      )
    }
  }
  # Origin (FeatCountry 1..10, baseline 6)
  for (k in 1:10) {
    if (k == 6) {
      rows[[length(rows) + 1]] <- add_row("6b.FeatCountry", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatCountry"),
        resmat[paste0("FeatCountry",k),"est"],
        resmat[paste0("FeatCountry",k),"se"]
      )
    }
  }
  # Job (FeatJob 1..11, baseline 1)
  for (k in 1:11) {
    if (k == 1) {
      rows[[length(rows) + 1]] <- add_row("1b.FeatJob", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatJob"),
        resmat[paste0("FeatJob",k),"est"],
        resmat[paste0("FeatJob",k),"se"]
      )
    }
  }
  # Reason (FeatReason 1..3, baseline 1)
  for (k in 1:3) {
    if (k == 1) {
      rows[[length(rows) + 1]] <- add_row("1b.FeatReason", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatReason"),
        resmat[paste0("FeatReason",k),"est"],
        resmat[paste0("FeatReason",k),"se"]
      )
    }
  }
  # Job experience (FeatExp 1..4, baseline 1)
  for (k in 1:4) {
    if (k == 1) {
      rows[[length(rows) + 1]] <- add_row("1b.FeatExp", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatExp"),
        resmat[paste0("FeatExp",k),"est"],
        resmat[paste0("FeatExp",k),"se"]
      )
    }
  }
  # Job plans (FeatPlans 1..4, baseline 3)
  for (k in 1:4) {
    if (k == 3) {
      rows[[length(rows) + 1]] <- add_row("3b.FeatPlans", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatPlans"),
        resmat[paste0("FeatPlans",k),"est"],
        resmat[paste0("FeatPlans",k),"se"]
      )
    }
  }
  # Prior trips (FeatTrips 1..5, baseline 1)
  for (k in 1:5) {
    if (k == 1) {
      rows[[length(rows) + 1]] <- add_row("1b.FeatTrips", 0, 0)
    } else {
      rows[[length(rows) + 1]] <- add_row(
        paste0(k,".FeatTrips"),
        resmat[paste0("FeatTrips",k),"est"],
        resmat[paste0("FeatTrips",k),"se"]
      )
    }
  }
  
  mat <- do.call(rbind, lapply(rows, function(z) c(z$est, z$se)))
  mat <- as.data.frame(mat)
  colnames(mat) <- c("pe","se")
  rownames(mat) <- sapply(rows, `[[`, "name")
  mat
}

############################################################
## 2. Common formulas
############################################################

mod_rhs <- "FeatGender + FeatEd * FeatJob + FeatLang + FeatCountry * FeatReason + FeatExp + FeatPlans + FeatTrips"

mod_formula <- as.formula(paste("Chosen_Immigrant ~", mod_rhs))

# Interaction model with subset for tests (mod2 in Stata)
mod2_rhs <- paste(
  "FeatGender * subset +",
  "FeatEd * FeatJob * subset +",
  "FeatLang * subset +",
  "FeatCountry * FeatReason * subset +",
  "FeatExp * subset +",
  "FeatPlans * subset +",
  "FeatTrips * subset"
)
mod2_formula <- as.formula(paste("Chosen_Immigrant ~", mod2_rhs))

# For "random effects" model modre (no weights in Stata)
modre_formula <- mod_formula  # RHS same; difference is weights

############################################################
## 3. Helper to run AMCE model and write txt
############################################################

# run_amce_and_save <- function(dat, fml, file, weights = TRUE, cluster = TRUE, fe_caseid = FALSE) {
#   if (nrow(dat) == 0) return(invisible(NULL))
#   
#   args <- list(
#     fml  = fml,
#     data = dat
#   )
#   if (weights) args$weights <- ~weight2
#   if (cluster) args$cluster <- ~CaseID
#   if (fe_caseid) args$fe <- ~CaseID
#   
#   mod <- do.call(feols, args)
#   
#   res <- adjustmat(mod)
#   res <- restrictamce(mod, res)
#   res50 <- build_resmat_stata(res)
#   
#   write.table(
#     res50,
#     file      = file,
#     quote     = FALSE,
#     sep       = "\t",
#     row.names = TRUE,
#     col.names = FALSE
#   )
# }
run_amce_and_save <- function(dat, fml, file, weights = TRUE, cluster = TRUE, fe_caseid = FALSE) {
  if (nrow(dat) == 0) return(invisible(NULL))
  
  args <- list(
    fml  = fml,
    data = dat
  )
  if (weights) args$weights <- ~weight2
  if (cluster) args$cluster <- ~CaseID
  if (fe_caseid) args$fe <- ~CaseID
  
  mod <- do.call(feols, args)
  
  res <- adjustmat(mod)
  res <- restrictamce(mod, res)
  res50 <- build_resmat_stata(res)
  
  write.table(
    res50,
    file      = file.path(txt_dir, file),  # <-- changed here
    quote     = FALSE,
    sep       = "\t",
    row.names = TRUE,
    col.names = FALSE
  )
}

############################################################
## 4. Figure 2: chosen.txt
############################################################

mod_bench <- feols(
  fml     = mod_formula,
  data    = data,
  weights = ~weight2,
  cluster = ~CaseID
)

resmat_df <- adjustmat(mod_bench)
resmat_df <- restrictamce(mod_bench, resmat_df)
resmat50  <- build_resmat_stata(resmat_df)

write.table(
  resmat50,
  # file      = "chosen.txt",
  file = file.path(txt_dir, "chosen.txt"),
  quote     = FALSE,
  sep       = "\t",
  row.names = TRUE,
  col.names = FALSE
)

############################################################
## 5. Figure 3: chosenPRs.txt (profile predictions)
############################################################

predict_with_se <- function(model, newdata) {
  pr <- predict(model, newdata = newdata, se.fit = TRUE)
  data.frame(pe = as.numeric(pr$fit), se = as.numeric(pr$se.fit))
}

if (arg$output != "orig" && arg$adjustprofiles) {
  library(Hmisc)
  # extract actual profiles from the data
  i_est <- obs(mod_bench) # get used observations         
  est_sample <- data[i_est, ]
  est_sample$estimates <- predict(mod_bench, newdata = est_sample) # calculate admission probability per profile
  quantiles <- c(.01, .25, .50, .75, .99)
  qs <- wtd.quantile(est_sample$estimates, weights = est_sample$weight2, probs = c(.01, .25, .50, .75, .99))
  # extract profiles closest to the respective quantile
  closest_rows <- sapply(qs, function(q) {
    which.min(abs(est_sample$estimates - q))
  })
  profiles <- est_sample[closest_rows, ]
  profiles$percentile <- quantiles
  profiles <- profiles[c("FeatGender", "FeatEd", "FeatLang", "FeatCountry", "FeatJob", "FeatExp", "FeatPlans", "FeatReason", "FeatTrips", "percentile")]
} else {
  # For the original study, stick to their profiles
  profiles <- tibble::tribble(
    ~FeatGender, ~FeatEd, ~FeatLang, ~FeatCountry, ~FeatJob, ~FeatExp, ~FeatPlans, ~FeatReason, ~FeatTrips, ~percentile,
    2,           2,       4,         8,            4,        2,         4,          3,           5,         0.01,
    2,           3,       3,         7,            6,        2,         2,          2,           1,         0.25,
    2,           4,       2,         6,            7,        2,         2,          2,           1,         0.50,
    2,           5,       2,         3,            9,        2,         2,          2,           3,         0.75,
    2,           7,       1,         1,            10,       3,         1,          2,           3,         0.99
  ) 
}

profiles <- profiles %>%
    mutate(
      FeatGender  = factor(FeatGender,  levels = levels(data$FeatGender)),
      FeatEd      = factor(FeatEd,      levels = levels(data$FeatEd)),
      FeatLang    = factor(FeatLang,    levels = levels(data$FeatLang)),
      FeatCountry = factor(FeatCountry, levels = levels(data$FeatCountry)),
      FeatJob     = factor(FeatJob,     levels = levels(data$FeatJob)),
      FeatExp     = factor(FeatExp,     levels = levels(data$FeatExp)),
      FeatPlans   = factor(FeatPlans,   levels = levels(data$FeatPlans)),
      FeatReason  = factor(FeatReason,  levels = levels(data$FeatReason)),
      FeatTrips   = factor(FeatTrips,   levels = levels(data$FeatTrips))
    )
preds <- predict_with_se(mod_bench, profiles)
store <- cbind(preds, profiles)

write.table(
  store,
  # file      = "chosenPRs.txt",
  file = file.path(txt_dir, "chosenPRs.txt"),
  quote     = FALSE,
  sep       = "\t",
  row.names = FALSE
)

############################################################
## 6. Figure 4: by respondent education (ppeducat)
############################################################

data <- data %>%
  mutate(subset = case_when(
    ppeducat %in% 1:2 ~ 1L,   # 1/2 -> 1
    ppeducat %in% 3:4 ~ 2L,   # 3/4 -> 2
    TRUE ~ NA_integer_
  ))

for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("ppeducat", i, ".txt"),
    weights = TRUE,
    cluster = TRUE
  )
}

data$subset <- NULL  # drop

############################################################
## 7. Figure 5: ethnocentrism (thermometer index)
############################################################

data <- data %>%
  mutate(
    ingroupTM = dplyr::case_when(
      ppethm == 1 ~ W1_Q8a,
      ppethm == 2 ~ W1_Q8b,
      TRUE ~ NA_real_
    )
  )

data <- data %>%
  mutate(
    outgroupTM = ifelse(ppethm %in% c(1,2),
                        rowMeans(cbind(W1_Q5, W1_Q6, W1_Q7), na.rm = TRUE),
                        NA_real_),
    ethnocentrism = ingroupTM - outgroupTM
  ) %>%
  select(-ingroupTM, -outgroupTM)

data <- data %>%
  mutate(
    subset = cut(ethnocentrism, breaks = quantile(ethnocentrism, probs = c(0, .5, 1), na.rm = TRUE),
                 include.lowest = TRUE, labels = FALSE)
  )

for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("ethnocentrism", i, ".txt"),
    weights = TRUE,
    cluster = TRUE
  )
}

data$subset <- NULL


## Figure 6: Effects by Party Identification of Respondent
## Stata: recode Party_ID (-1=.) (1/3=1) (4=.) (5/7=2), gen(subset)

data <- data %>%
  dplyr::mutate(
    subset_party = dplyr::case_when(
      Party_ID %in% 1:3 ~ 1L,   # Dem / Lean Dem
      Party_ID %in% 5:7 ~ 2L,   # Rep / Lean Rep
      TRUE             ~ NA_integer_  # -1, 4, others => NA
    )
  )

for (i in 1:2) {
  dat_i <- subset(data, subset_party == i)
  if (nrow(dat_i) == 0) next  # just in case
  
  # same model as baseline: Chosen_Immigrant ~ mod_rhs, with weights & cluster
  mod_i <- feols(
    fml     = mod_formula,
    data    = dat_i,
    weights = ~weight2,
    cluster = ~CaseID
  )
  
  # AMCE extraction + restrictions
  res_i <- adjustmat(mod_i)
  res_i <- restrictamce(mod_i, res_i)
  
  # add baselines & Stata-style names (1b.FeatEd, 2.FeatEd, etc.)
  res_i_50 <- build_resmat_stata(res_i)
  
  # write partyid1.txt / partyid2.txt in the same format as chosen.txt
  write.table(
    res_i_50,
    file      = file.path(txt_dir, paste0("partyid", i, ".txt")),
    quote     = FALSE,
    sep       = "\t",
    row.names = TRUE,
    col.names = FALSE
  )
}

# clean up
data$subset_party <- NULL

############################################################
## 8. Appendix B & C: other heterogeneity splits
##    Each block: define subset, loop, save txt
############################################################

## B.1: by percent immigrant workers in industry (FB09)
data <- data %>%
  mutate(subset = cut(FB09, breaks = quantile(FB09, probs = c(0, .5, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = FALSE))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("industryfb", i, ".txt")
  )
}
data$subset <- NULL

## B.2: by household income (ppincimp)
data <- data %>%
  mutate(subset = cut(ppincimp, breaks = quantile(ppincimp, probs = c(0, .5, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = FALSE))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("income", i, ".txt")
  )
}
data$subset <- NULL

## B.3: by fiscal exposure (fisexp2)
data <- data %>%
  mutate(subset = fisexp2)
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("fiscalexp2", i, ".txt")
  )
}
data$subset <- NULL

## B.4: by ZIP code demographics (censusgroup)
data <- data %>% mutate(subset = censusgroup)
for (i in 1:3) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("zipcodediversity", i, ".txt")
  )
}
data$subset <- NULL

## B.5: by ethnicity (white vs non-white)
data <- data %>%
  mutate(subset = dplyr::case_when(
    ppethm == 1        ~ 2L,  # whites -> 2
    ppethm %in% 2:4    ~ 1L,  # others -> 1
    TRUE               ~ NA_integer_
  ))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("whiteornot", i, ".txt")
  )
}
data$subset <- NULL

## B.6: by Hispanic ethnicity
data <- data %>%
  mutate(subset = dplyr::case_when(
    ppethm %in% 1:3 ~ 1L,
    ppethm == 4     ~ 2L,
    TRUE            ~ NA_integer_
  ))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("hispanicornot", i, ".txt")
  )
}
data$subset <- NULL

## B.7: by ideology
data <- data %>%
  mutate(subset = dplyr::case_when(
    Ideology %in% 1:3 ~ 1L,
    Ideology %in% 5:7 ~ 2L,
    TRUE              ~ NA_integer_
  ))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("ideology", i, ".txt")
  )
}
data$subset <- NULL

## B.8: by immigration attitudes (W1_Q4)
data <- data %>%
  mutate(subset = dplyr::case_when(
    W1_Q4 %in% 1:3 ~ 1L,
    W1_Q4 %in% 4:5 ~ 2L,
    TRUE           ~ NA_integer_
  ))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("opposeimmig", i, ".txt")
  )
}
data$subset <- NULL

## B.9: by gender of respondent (ppgender)
data <- data %>% mutate(subset = ppgender)
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("gender", i, ".txt")
  )
}
data$subset <- NULL

## B.10: by age of respondent (ppage)
data <- data %>%
  mutate(subset = cut(ppage, breaks = quantile(ppage, probs = c(0, .5, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = FALSE))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("age", i, ".txt")
  )
}
data$subset <- NULL

############################################################
## 9. Figure C.1: Support_Admission outcome (support.txt)
############################################################

support_formula <- as.formula(paste("Support_Admission ~", mod_rhs))

run_amce_and_save(
  dat   = data,
  fml   = support_formula,
  file  = "support.txt",
  weights = TRUE,
  cluster = TRUE
)

############################################################
## 10. Figure C.2: FE model (respondent fixed effects)
##     xtreg Chosen_Immigrant $mod , i(CaseID) fe cl(CaseID)
############################################################

run_amce_and_save(
  dat   = data,
  fml   = mod_formula,
  file  = "resfixedeffects.txt",
  weights = TRUE,
  cluster = TRUE,
  fe_caseid = TRUE
)

############################################################
## 11. Figure C.3: RE model (approximate, no weights)
##     xtreg Chosen_Immigrant $modre, re cl(CaseID)
############################################################

run_amce_and_save(
  dat   = data,
  fml   = modre_formula,
  file  = "resrandomeffects.txt",
  weights = FALSE,   # Stata RE had no weights
  cluster = TRUE
)

############################################################
## 12. Figure C.4: by panel tenure (Panel_Tenure_Months)
############################################################

data <- data %>%
  mutate(subset = cut(Panel_Tenure_Months, breaks = quantile(Panel_Tenure_Months,
                                                             probs = c(0, .5, 1),
                                                             na.rm = TRUE),
                      include.lowest = TRUE, labels = FALSE))
for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("paneltenure", i, ".txt")
  )
}
data$subset <- NULL

############################################################
## 13. Figure C.5: by pairing (contest_no)
############################################################

data <- data %>% mutate(subset = contest_no)
for (i in 1:5) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("pairingno", i, ".txt")
  )
}
data$subset <- NULL

############################################################
## 14. Figure C.6: by self-monitoring (SM_index)
############################################################

data <- data %>%
  mutate(
    W1_Q1r = dplyr::recode(as.numeric(W1_Q1), `5` = 1L, `4` = 2L, `2` = 4L, `1` = 5L, .default = as.integer(W1_Q1)),
    W1_Q2r = dplyr::recode(as.numeric(W1_Q2), `5` = 1L, `4` = 2L, `2` = 4L, `1` = 5L, .default = as.integer(W1_Q2)),
    W1_Q3r = dplyr::recode(as.numeric(W1_Q3), `5` = 1L, `4` = 2L, `2` = 4L, `1` = 5L, .default = as.integer(W1_Q3)),
    SM_index = (as.numeric(W1_Q1r) + as.numeric(W1_Q2r) + as.numeric(W1_Q3r))/3
  )

data <- data %>%
  mutate(subset = cut(SM_index, breaks = quantile(SM_index, probs = c(0, .5, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = FALSE))

for (i in 1:2) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("selfmonitor", i, ".txt")
  )
}
data$subset <- NULL

############################################################
## 15. Figure C.7: by number of countertypical profiles
############################################################

# Implement the same countertypical rules as Stata:
data <- data %>%
  mutate(
    countertypical = if_else(!is.na(FeatCountry), 0L, NA_integer_),
    countertypical = if_else(
      # Mexico + {some college or college degree or graduate degree}
      (FeatCountry == 3 & !is.na(FeatEd) & FeatEd >= 5) |
      # Mexico + {doctor or research scientist or computer programmer or financial analyst}
      (FeatCountry == 3 & FeatJob %in% c(11, 10, 8, 5)) |
      # Somalia + {some college or college degree or graduate degree}
      (FeatCountry == 9 & !is.na(FeatEd) & FeatEd >= 5) |
      # Somalia + {doctor or research scientist or computer programmer or financial analyst}
      (FeatCountry == 9 & FeatJob %in% c(11, 10, 8, 5)) |
      # Sudan + {research scientist or computer programmer or financial analyst}
      (FeatCountry == 9 & FeatJob %in% c(10, 8, 5)) |
      # Iraq + {research scientist or computer programmer or financial analyst}
      (FeatCountry == 10 & FeatJob %in% c(10, 8, 5)) |
      # German + {no formal education or 4th grade education or 8th grade education}
      (FeatCountry == 1 & !is.na(FeatEd) & FeatEd <= 3) |
      # German + {janitor or waiter or child care provider or gardener}
      (FeatCountry == 1 & FeatJob %in% c(1, 3, 4)) |
      # French + {no formal education or 4th grade education or 8th grade education}
      (FeatCountry == 2 & !is.na(FeatEd) & FeatEd <= 3) |
      # French + {janitor or waiter or child care provider or gardener}
      (FeatCountry == 2 & FeatJob %in% c(1, 3, 4)) |
      # Indian + {no formal education or 4th grade education or 8th grade education}
      (FeatCountry == 6 & !is.na(FeatEd) & FeatEd <= 3) |
      # Indian + {janitor or waiter or child care provider or gardener}
      (FeatCountry == 6 & FeatJob %in% c(1, 3, 4)) |
      # Indian + {tried English but unable or used interpreter}
      (FeatCountry == 6 & FeatLang == 3) |
      # German + unauthorized
      (FeatCountry == 1 & FeatTrips == 5) |
      # French + unauthorized
      (FeatCountry == 2 & FeatTrips == 5) |
      # Female + construction worker
      (FeatCountry == 1 & FeatJob == 6) |
      # Male + child care provider
      (FeatCountry == 2 & FeatJob == 3) |
      # Seek better job + No plans to look for work
      (FeatReason == 2 & FeatPlans == 4),
      1L,
      countertypical
    )
  )

# Then aggregate by CaseID
data <- data %>%
  group_by(CaseID) %>%
  mutate(sumct = ifelse(!is.na(Chosen_Immigrant), sum(countertypical, na.rm = TRUE), NA)) %>%
  ungroup() %>%
  mutate(
    subset = case_when(
      sumct %in% 0:3  ~ 1L,
      sumct %in% 4:5  ~ 2L,
      sumct %in% 6:9  ~ 3L,
      TRUE ~ NA_integer_
    )
  )

for (i in 1:3) {
  run_amce_and_save(
    dat   = subset(data, subset == i),
    fml   = mod_formula,
    file  = paste0("countertypical", i, ".txt")
  )
}
data$subset <- NULL
############################################################
## End of script
############################################################
