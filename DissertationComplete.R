####
##
## name: DissertationComplete.R
## author: Dennis F Langley
## date: 2017-10-25
## what: Dissertation Data Cleaning and Analysis
##
####


# Preamble ####
  rm(list=ls())
  dev.off()
  options(scipen=999)
  setwd("~/Documents/Dissertation/Data")
  library(tidyverse)
  library(foreign) # for read.dta() function
  library(plyr)
  library(gdata) # For keep() function
  library(texreg)
  cat("\014")

# Custom Functions ####

# FUNCTION TO MAKE BINARY VARIABLE, RECODING 'na_vals' TO NA
make_dummy <- function(var, on_vals, na_vals = NULL){
  case_when(
    is.na(var)       ~ NA_integer_,
    var %in% na_vals ~ NA_integer_,
    var %in% on_vals ~ 1L,
    TRUE             ~ 0L
  )
}

# FUNCTION TO REVERSE ORDER VARIABLE
reverse_code <- function(var, na_vals = NULL){
  var_nas <- ifelse(var %in% na_vals, NA_integer_, var)
  max_val <- max(var_nas, na.rm = TRUE)
  rev_var <-  as.integer(var_nas - max_val)  %>% abs()
  return(rev_var)
}


# Data Import ####
  fsu_module <- read.dta("~/Documents/Dissertation/Data/CCES/CCES16_FSU_OUTPUT_Feb2017.dta")
  load("~/Documents/Dissertation/Data/CCES/CCES16_Common_OUTPUT_Jul2017_VV.RData");common_content<-x;remove(x)
  anes_data <- read.dta("~/Documents/Dissertation/Data/ANES/anes_timeseries_2016_Stata12.dta")
  
# Data Cleaning ####
  
  # Clean ANES Data
  {
    anes_clean <- anes_data %>%
      ## Thermometer Variables
      
      # Liberals
      mutate(lib.therm = ifelse(V162097 %in% c(-9, -7, -6, 998, 999), NA, V162097)) %>%
      # Conservatives
      mutate(lib.therm = ifelse(V162101 %in% c(-9, -7, -6, 998, 999), NA, V162101)) %>%
      # Democrats
      mutate(lib.therm = ifelse(V161095 %in% c(-9, -7, -6, 998, 999), NA, V161095)) %>%
      # Republicans
      mutate(lib.therm = ifelse(V161096 %in% c(-9, -7, -6, 998, 999), NA, V161096)) %>%
      
      ## Political Variables
      
      # Ideology
      mutate(ideo7 = V161126)
  }
  
  # Clean FSU Module
  {
    fsu_clean <- fsu_module %>%
      # ID
      mutate(id = as.numeric(V101)) %>%
      # Age
      mutate(age = 2016 - as.numeric(birthyr))  %>%
      # Dummy to drop people who didn't receive the social identity questions
      mutate(drop = ifelse(pid7 %in% c("Not sure", "Skipped", "Not Asked") & CC16_340a %in% c("Not sure", "Skipped", "Not Asked"), 1, 0)) %>%
      # Dummies for Independents and Moderates, each of whom get 0 scores for psi and isi, respectively
      mutate(independent = make_dummy(pid7, on_vals = "Independent")) %>%
      mutate(moderate = make_dummy(CC16_340a, on_vals = "Middle of the Road")) %>%
      # Experiment Conditions
      mutate(exp.keep = make_dummy(FSU351_treat, on_vals = c("Condition 1", "Condition 2", "Condition 3"))) %>%
      mutate(exp.treatment1 = FSU351_treat)  %>%
      mutate(exp.treatment2 = FSU351_treat2) %>%
      mutate(exp.treated = make_dummy(exp.treatment1, on_vals = c("Condition 2", "Condition 3"))) %>%
      mutate(exp.response = recode(as.numeric(FSU351),
                                   `1` = 0L,
                                   `2` = 1L, 
                                   `3` = 2L,
                                   `4` = 3L,
                                   `5` = 4L,
                                   `6` = 5L,
                                   `7` = 6L,
                                   `8` = NA_integer_)) %>%
      mutate(exp.partysupport = recode(FSU351_treat2,
                                       "Rep support / Dem oppose" = "Republican",
                                       "Dem support / Rep oppose" = "Democrat")) %>%
      mutate(exp.partymatch = ifelse(as.character(exp.partysupport)==as.character(pid3), 1, 0)) %>%
      # Primary Participation
      mutate(fsu.pri.part = make_dummy(FSU345, on_vals = "Yes, I voted in a primary election or a caucus.", na_vals = c("Skipped"))) %>%
      # Primary Vote Choice
      mutate(fsu.pri.who = FSU346) %>%
      # Primary Choice - Establishment / Anti-Establishment
      mutate(fsu.pri.ae = make_dummy(fsu.pri.who, on_vals = c("Ted Cruz", "Bernie Sanders", "Donald Trump", "Ben Carson"), na_vals = c("Skipped"))) %>%
      mutate(fsu.pri.es = make_dummy(fsu.pri.who, on_vals = c("Hillary Clinton", "Jeb Bush", "John Kasich", "Marco Rubio"), na_vals = c("Skipped"))) %>%       
      # PSI questions
      mutate(p.insult  = as.numeric(mapvalues(FSU337, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      mutate(p.they    = as.numeric(mapvalues(FSU338, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      mutate(p.success = as.numeric(mapvalues(FSU339, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      mutate(p.praise  = as.numeric(mapvalues(FSU340, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      # ISI Questions
      mutate(i.insult  = as.numeric(mapvalues(FSU341, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      mutate(i.they    = as.numeric(mapvalues(FSU342, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      mutate(i.success = as.numeric(mapvalues(FSU343, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      mutate(i.praise  = as.numeric(mapvalues(FSU344, from=c("Skipped", "Not Asked"), to=c(NA, NA), warn_missing=FALSE)) -1) %>%
      # PSI and ISI Scales
      mutate(psi = p.insult + p.they + p.success + p.praise) %>%
      mutate(psi = replace(psi, pid7 == "Independent", 0)) %>%
      mutate(isi = i.insult + i.they + i.success + i.praise) %>%
      mutate(isi = replace(isi, CC16_340a == "Middle of the Road", 0)) %>%
      mutate(psi.high = ifelse(psi>=8, 1, 0)) %>%
      mutate(isi.high = ifelse(isi>=8, 1, 0)) %>%
      # Keep only some variables from FSU module
      dplyr::select(
        id:isi.high) #%>%
  }

  # Clean Common Content
  {
    cces_clean <- common_content %>%
      # ID
      mutate(id = as.numeric(V101)) %>%
      # Gender
      mutate(female = make_dummy(gender, on_vals = "Female"))  %>%
      # Education
      mutate(educ_cat = recode(as.numeric(educ),
                            `1` = 0L,
                            `2` = 1L,
                            `3` = 2L,
                            `4` = 2L,
                            `5` = 3L,
                            `6` = 4L,
                            .default = NA_integer_,
                            .missing = NA_integer_ ))  %>%
      # Race
      mutate(race_factor = recode(as.numeric(race),
                                  `1` = "1. White",
                                  `2` = "2. Black",
                                  `3` = "3. Hispanic",
                                  `4` = "4. Other",
                                  `5` = "4. Other",
                                  `6` = "4. Other",
                                  `7` = "4. Other",
                                  `8` = "4. Other",
                                  .default = NA_character_,
                                  .missing = NA_character_ ))  %>%
      mutate(minority = make_dummy(race_factor, on_vals=c("2. Black", "3. Hispanic", "4. Other"))) %>%
      # Married
      mutate(married = make_dummy(marstat, on_vals = "Married", na_vals = c(8,9)))  %>%
      # Party ID
      mutate(party.ID7 = recode(as.numeric(pid7),
                                `1` = 1L,
                                `2` = 2L,
                                `3` = 3L,
                                `4` = 4L, 
                                `5` = 5L,
                                `6` = 6L,
                                `7` = 7L,
                                `8` = NA_integer_,
                                `9` = NA_integer_)) %>%
      mutate(party.ID3 = pid3) %>%
      # Party Strength
      mutate(party.strength = case_when(
        pid7 %in% c("Strong Democrat", "Strong Republican") ~ 3L,
        pid7 %in% c("Not very strong Democrat", "Not very strong Republican") ~ 2L,
        pid7 %in% c("Lean Democrat", "Lean Republican") ~ 1L,
        pid7 %in% c("Independent") ~ 0L,
        TRUE ~ NA_integer_)) %>%
      mutate(party.strong.binary = ifelse(party.strength==3, 1, 0)) %>%
      # Ideology ID
      mutate(ideo.ID7 = recode(as.numeric(CC16_340a),
                               `1` = 1L,
                               `2` = 2L,
                               `3` = 3L,
                               `4` = 4L, 
                               `5` = 5L,
                               `6` = 6L,
                               `7` = 7L,
                               `8` = NA_integer_,
                               `9` = NA_integer_)) %>%
      mutate(ideo.ID5 = ideo5) %>%
      # Ideology Strength
      mutate(ideo.strength = case_when(
        CC16_340a %in% c("Very Liberal", "Very Conservative") ~ 3L,
        CC16_340a %in% c("Liberal", "Conservative") ~ 2L,
        CC16_340a %in% c("Somewhat Liberal", "Somewhat Conservative") ~ 1L,
        CC16_340a %in% c("Middle of the Road") ~ 0L,
        CC16_340a %in% c("Not sure", "Skipped", "Not Asked") ~ NA_integer_,
        TRUE ~ NA_integer_)) %>%
      # Born Again? 
      mutate(bornagain = make_dummy(pew_bornagain, on_vals = "Yes", na_vals = c(8,9))) %>%
      # Church Attendance
      mutate(church_freq = reverse_code(pew_churatd, na_vals = c(7, 8, 9))) %>%
      # Prayer Frequency
      mutate(prayer_freq = reverse_code(pew_prayer, na_vals = c(7, 8, 98, 99))) %>%
      # Political Interest
      mutate(interest = reverse_code(newsint, na_vals = c(7, 8, 9))) %>%
      # income
      mutate(income = recode(as.integer(faminc),
                             `97` = NA_integer_,
                             `98` = NA_integer_,
                             `99` = NA_integer_))  %>%
      mutate(income5 = ntile(income, 5))  %>%
      # General Election Participation
      mutate(cces.gen.part.self = ifelse(CC16_401 == "I definitely voted in the General Election.", 1, 0)) %>%
      mutate(cces.gen.part.self = replace(cces.gen.part.self, is.na(cces.gen.part.self)==TRUE, 0)) %>%
      # CCES Primary Questions
      mutate(cces.pri.part.self = make_dummy(CC16_327, on_vals = "Yes, voted in a primary or caucus", na_vals = c("Skipped", "Not Asked"))) %>%
      mutate(cces.pri.who = CC16_328)%>%
      mutate(cces.pri.ae = make_dummy(cces.pri.who, 
                                      on_vals=c("Bernie Sanders", "Donald Trump", "Ted Cruz"), 
                                      na_vals = c("Skipped"))) %>%
      mutate(cces.pri.es = make_dummy(cces.pri.who, 
                                      on_vals=c("Hillary Clinton", "Marco Rubio", "John Kasich"), 
                                      na_vals = c("Skipped"))) %>%
      # Opinion: Guns - Ban Assault Weapons. 1 = Support, 0 = Oppose
      mutate(guns = make_dummy(CC16_330d, on_vals = "Support",
                           na_vals = c('Skipped','Not Asked')))%>%
      # Opinion: Abortion - Always a choice. 1 = Support, 0 = Oppose
      mutate(abortion = make_dummy(CC16_332a, on_vals = "Support",
                               na_vals = c("Skipped", "Not Asked"))) %>%
      # Opinion: Environment: Let EPA regulate carbon dioxide. 1 = Support, 0 = Oppose
      mutate(environment = make_dummy(CC16_333a, on_vals = "Support",
                                      na_vals = c("Skipped", "Not Asked"))) %>%
      # Opinion: Gay Marriage
      mutate(gaymarriage = make_dummy(CC16_335, on_vals = "Favor",
                                  na_vals = c("Skipped", "Not Asked"))) %>%
      # Opinion: Repeal ACA
      mutate(aca = make_dummy(CC16_351I, on_vals = "Against",
                          na_vals=c("Skipped", "Not Asked"))) %>%
      # Sum of opinions
      mutate(demagree = guns+abortion+environment+gaymarriage+aca) %>%
      mutate(partyagree = ifelse(pid3=="Democrat", demagree, ifelse(pid3=="Republican", 5+(demagree*-1), 0))) %>%
      # Self-reported voters
      mutate(selfvoter = ifelse(CC16_401 == "I definitely voted in the General Election.", 1, 0))  %>%
      mutate(whovote = CC16_410a) %>%
      mutate(whovote.clinton = make_dummy(CC16_410a, on_vals="Hillary Clinton (Democrat)")) %>%
      mutate(whovote.trump   = make_dummy(CC16_410a, on_vals="Donald Trump (Republican)")) %>%
      mutate(whovote.clinton = replace(whovote.clinton, is.na(whovote.clinton)==TRUE, 0)) %>%
      mutate(whovote.trump   = replace(whovote.trump, is.na(whovote.trump)==TRUE, 0)) %>%
      # Participation
      mutate(part.meeting = make_dummy(CC16_417a_1, "Yes", na_vals = c("Skipped", "Not Asked"))) %>%
      mutate(part.work = make_dummy(CC16_417a_3, "Yes", na_vals = c("Skipped", "Not Asked"))) %>%
      mutate(part.donate = make_dummy(CC16_417a_4, "Yes", na_vals = c("Skipped", "Not Asked"))) %>%
      
      # Validated voters
      mutate(cces.gen.part.val = ifelse(CL_E2016GVM %in% c("absentee", 
                                                        "earlyVote", 
                                                        "mail", 
                                                        "polling", 
                                                        "unknown"),
                                 1,0)) %>%
      mutate(cces.pri.part.val = ifelse(CL_E2016PPVM %in% c("absentee", 
                                                            "earlyVote", 
                                                            "mail", 
                                                            "polling", 
                                                            "unknown"),
                                     1,0)) %>%
      # Keep only some variables from CCES common content
      dplyr::select(
        id:cces.pri.part.val)
  }

  # Merge FSU and Common Content by ID
  {
    d.full <- fsu_clean  %>%
      left_join(cces_clean, by = "id")  %>%
      as.data.frame()
    d<-subset(d.full, drop==0, select = -drop)
  }
  #keep(d, d.full, sure=TRUE)

# Models ####
{
# Linear Model predicting vote for establishment candidates
d.mod <- subset(d, select=c(whovote.clinton, whovote.trump, fsu.pri.part, fsu.pri.es, fsu.pri.ae, 
                            cces.gen.part.self, cces.gen.part.val, cces.pri.es, cces.pri.ae, cces.pri.part.self, cces.pri.part.val, 
                            psi, isi, party.strength, ideo.strength, partyagree, party.ID3, ideo.ID7, ideo.ID5,
                            female, minority, educ_cat, married, church_freq, interest, income))
d.mod <- d.mod[complete.cases(d.mod),]

mod.genpart.misreport1 <-glm(cces.gen.part.self ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                            data=d.mod[d.mod$cces.gen.part.val==0,], family = binomial(link = "logit"))
mod.genpart.misreport2 <-glm(cces.gen.part.self ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                             data=d.mod[d.mod$cces.gen.part.val==0,], family = binomial(link = "logit"))
  
mod.pripart.misreport1 <- glm(cces.pri.part.self ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                             data=d.mod[d.mod$cces.pri.part.val==0,], family = binomial(link = "logit"))
mod.pripart.misreport2 <- glm(cces.pri.part.self ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                              data=d.mod[d.mod$cces.pri.part.val==0,], family = binomial(link = "logit"))

mod.genpartval.1 <- glm(cces.gen.part.val ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.mod, family = binomial(link = "logit"))
mod.genpartval.2 <- glm(cces.gen.part.val ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.mod, family = binomial(link = "logit"))

mod.pripartval.1 <- glm(cces.pri.part.val ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.mod, family = binomial(link = "logit"))
mod.pripartval.2 <- glm(cces.pri.part.val ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.mod, family = binomial(link = "logit"))

mod.genpartself.1 <- glm(cces.gen.part.self ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                          data=d.mod, family = binomial(link = "logit"))
mod.genpartself.2 <- glm(cces.gen.part.self ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                          data=d.mod, family = binomial(link = "logit"))

mod.pripartself.1 <- glm(cces.pri.part.self ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                          data=d.mod, family = binomial(link = "logit"))
mod.pripartself.2 <- glm(cces.pri.part.self ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                          data=d.mod, family = binomial(link = "logit"))

mod.cces.es1 <- glm(cces.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))
mod.cces.es2 <- glm(cces.pri.es ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))

mod.cces.ae1 <- glm(cces.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))
mod.cces.ae2 <- glm(cces.pri.ae ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))

mod.cces.dem.es1 <- glm(cces.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Democrat",], family = binomial(link = "logit"))
mod.cces.dem.es2 <- glm(cces.pri.es ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Democrat",], family = binomial(link = "logit"))

mod.cces.dem.ae1 <- glm(cces.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Democrat",], family = binomial(link = "logit"))
mod.cces.dem.ae2 <- glm(cces.pri.ae ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Democrat",], family = binomial(link = "logit"))

mod.cces.rep.es1 <- glm(cces.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Republican",], family = binomial(link = "logit"))
mod.cces.rep.es2 <- glm(cces.pri.es ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Republican",], family = binomial(link = "logit"))

mod.cces.rep.ae1 <- glm(cces.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Republican",], family = binomial(link = "logit"))
mod.cces.rep.ae2 <- glm(cces.pri.ae ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod[d.mod$party.ID3=="Republican",], family = binomial(link = "logit"))

mod.cces.clinton1 <- glm(whovote.clinton ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                          data=d.mod, family = binomial(link = "logit"))
mod.cces.clinton2 <- glm(whovote.clinton ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                          data=d.mod, family = binomial(link = "logit"))
mod.cces.trump1 <- glm(whovote.trump ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.mod, family = binomial(link = "logit"))
mod.cces.trump2 <- glm(whovote.trump ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.mod, family = binomial(link = "logit"))

mod.fsu.es1 <- glm(fsu.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))
mod.fsu.es2 <- glm(fsu.pri.es ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))

mod.fsu.ae1 <- glm(fsu.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))
mod.fsu.ae2 <- glm(fsu.pri.ae ~ psi + isi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.mod, family = binomial(link = "logit"))

mod.test1 <- lm(ideo.strength ~ party.strength + partyagree + female + minority + educ_cat + married + church_freq + interest + income, data = d.mod)
mod.test2 <- lm(ideo.strength ~ psi + isi + party.strength + partyagree + female + minority + educ_cat + married + church_freq + interest + income, data = d.mod)

plot(x=d.mod$party.strength, y=d.mod$psi, xlab = "Party ID Strength", ylab = "Partisan Social Identity", xaxt='n', main = "Party ID Strength and Partisan Social Identity")
axis(side=1, at=c(0, 1, 2, 3), labels=TRUE)

plot(x=d.mod$ideo.strength, y=d.mod$isi, xlab = "Ideological Strength", ylab = "Ideological Social Identity", xaxt='n', main = "Ideological Strength and Ideological Social Identity")
axis(side=1, at=c(0, 1, 2, 3), labels=TRUE)

cor(d.mod$party.strength[d.mod$party.ID3!="Independent"], d.mod$psi[d.mod$party.ID3!="Independent"])
cor(d.mod$ideo.strength[d.mod$ideo.ID5!="Moderate"], d.mod$isi[d.mod$ideo.ID5!="Moderate"])


lr.genpart <- lrtest(mod.genpart.2, mod.genpart.1)
lr.pripart <- lrtest(mod.pripart.2, mod.pripart.1)

lr34 <- lrtest(mod4, mod3)
lr56 <- lrtest(mod6, mod5)

# Overreporting in General Elections and Primaries
texreg(l=list(mod.genpart.misreport1, mod.genpart.misreport2, mod.pripart.misreport1, mod.pripart.misreport2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Overreporting in the 2016 Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("General", "General", "Primary", "Primary"))


# Voting in General Elections, self-reported and validated
texreg(l=list(mod.genpartself.1, mod.genpartself.2, mod.genpartval.1, mod.genpartval.2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Voting in the 2016 General Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Self-Reported", "Self-Reported", "Validated", "Validated"))

# Voting in Primary Elections, self-reported and validated
texreg(l=list(mod.pripartself.1, mod.pripartself.2, mod.pripartval.1, mod.pripartval.2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Voting in the 2016 Primary Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Self-Reported", "Self-Reported", "Validated", "Validated"))

# Preference between Clinton and Trump, 2016 General Election
texreg(l=list(mod.cces.clinton1, mod.cces.clinton2, mod.cces.trump1, mod.cces.trump2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Clinton", "Clinton", "Trump", "Trump"))

# Candidate Preference in Primary Elections, establishment and anti-establishment
texreg(l=list(mod.cces.es1, mod.cces.es2, mod.cces.ae1, mod.cces.ae2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment", "Anti-Establishment", "Anti-Establishment"))


texreg(l=list(mod.test1, mod.test2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "TESTS",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Model 1", "Model 2"))




texreg(l=list(mod3, mod4), stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, caption = "Voting for an Anti-Establishment Candidate in the 2016 Presidential Primary",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label="",
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = paste("Model 2 is a better fit; p-value = ", round(lr34[[5]][2], 8)))


texreg(l=list(mod5, mod6), stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, caption = "Participating in the 2016 Presidential Primary",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label="",
       custom.coef.map = list('psi' = "Partisan Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = paste("Model 2 is not a better fit; p-value = ", round(lr56[[5]][2], 8)))


}


  
## Misc Stuff ####

# Subsets
{
  # Data sets with complete PSI or ISI observations
  d.psi <- subset(d, is.na(d$psi) == FALSE)
  d.isi <- subset(d, is.na(d$isi) == FALSE)
  # Data set with complete PSI & ISI observations
  d.both<- subset(d, is.na(d$psi) == FALSE & is.na(d$isi) == FALSE)
}

# Exploratory Data Analysis
{
  aggr(d.full, prop = F, numbers = T)
  
  ggplot(d, aes(x=psi, y=isi)) + geom_point(alpha = 0.3)
  
  ggplot(d.psi, aes(x=party.strength, y=psi)) + geom_point(alpha = 0.3)
  ggplot(d.psi, aes(x=party.ID7, y=psi)) + geom_point(alpha = 0.3)
  ggplot(d.isi, aes(x=ideo.ID7, y=isi)) + geom_point(alpha = 0.3)
  ggplot(d.isi, aes(x=ideo.strength, y=isi)) + geom_point(alpha = 0.3)
  
  cor(d$psi, d$isi)
  cor(d$psi, as.numeric(d$pid3))
  cor(d$psi, as.numeric(d$pid7))
  cor(as.numeric(d$ideo), d$isi)
}

# PCA
{
d.sub<-subset(d.mod, select=c(party.strength, partyagree, party.strength, interest, ideo.strength))
fit1<-princomp(d.sub, cor=TRUE)
fit2<-principal(d.sub, rotate="varimax")
fit3<-factanal(d.sub, 2, rotation="varimax")
ev<-eigen(cor(d.sub))
ap<-parallel(subject=nrow(d.sub), var=ncol(d.sub), rep=100, cent=0.05)
nS<-nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
fit4<-PCA(d.sub)
}

# Feature Selection
{
d.feature<-subset(d, select=c(cces.pri.part, psi, isi, female, minority, educ_cat, married, party.strength, ideo.strength, church_freq, interest, income, partyagree))
d.feature<-d.feature[complete.cases(d.feature),]
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(as.factor(cces.pri.part)~., data=d.feature, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, main = "Participation")

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(d.feature[,2:13], d.feature[,1], sizes=c(1:12), rfeControl=control)
}

# Experiment
{
d.experiment<-subset(d, select=c(exp.response, exp.treated, exp.partymatch, psi.high))
d.experiment<-d.experiment[complete.cases(d.experiment),]

test1 <- lm(exp.response ~ exp.partymatch*psi.high, data=d.experiment)

ggplot(d.experiment,aes(x=exp.partymatch,y=exp.response,colour=factor(psi.high)))+
  stat_smooth(method='lm',formula=y~x)
}


