## Title -----------------------------------------------------------------------
##
## name:    00cleandata.R
## author:  Dennis F Langley
## date:    2018-06-04
## what:    Import data from 2016 CCES, 2016 ANES, 
##               and ANES Panel Study file
##       
####

## Preamble --------------------------------------------------------------------
rm(list = ls())                           # Clears environment
dev.off()                                 # Clears plots
options(scipen = 999)                     # 'Disables' scientific notation
setwd("~/Documents/Dissertation/Data")    # Working directory

library(foreign)      # Load .dta files into R
library(readstata13)  # Load Stata13 files into R
library(tidyverse)    # Tidyverse packages for data cleaning; dplyr and ggplot2
library(plyr)         # mapvalues() command
library(gdata)        # keep() command

source("~/Documents/Dissertation/Analysis/00customfunctions.R")

## Data import -----------------------------------------------------------------
cces_fsu <- read.dta("./CCES/CCES16_FSU_OUTPUT_Feb2017.dta")

load("./CCES/CCES16_Common_OUTPUT_Jul2017_VV.RData")
cces_common <- x; remove(x)

data_anes <- read.dta("./ANES/anes_timeseries_2016_Stata12.dta")

#ata_anes_ts <- read.dta13("./ANES/anes_timeseries_cdf_stata12.dta",
#                           generate.factors = TRUE, nonint.factors = TRUE)

data_anes_panel <- read.dta13("./ANES/anes_mergedfile_2000to2004.dta",
                              nonint.factors = TRUE, generate.factors = TRUE)

cat("\014")

## Data Cleaning ---------------------------------------------------------------

## Clean Common Content 
{
  clean_cces <- 
    cces_common %>%
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
    mutate(minority = make_dummy(race_factor, 
                      on_vals=c("2. Black", "3. Hispanic", "4. Other"))) %>%
    # Married
    mutate(married = make_dummy(marstat, 
                     on_vals = "Married", na_vals = c(8,9)))  %>%
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
                              `9` = NA_integer_,
                              .default = NA_integer_,
                              .missing = NA_integer_)) %>%
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
                             `9` = NA_integer_,
                             .default = NA_integer_,
                             .missing = NA_integer_)) %>%
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
    mutate(bornagain = make_dummy(pew_bornagain, 
                                  on_vals = "Yes", na_vals = c(8,9))) %>%
    # Church Attendance
    mutate(church_freq = reverse_code(pew_churatd, 
                                      na_vals = c(7, 8, 9))) %>%
    # Prayer Frequency
    mutate(prayer_freq = reverse_code(pew_prayer, 
                                      na_vals = c(7, 8, 98, 99))) %>%
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
    mutate(cces.pri.part.self = make_dummy(CC16_327, 
            on_vals = "Yes, voted in a primary or caucus", 
            na_vals = c("Skipped", "Not Asked"))) %>%
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
    # Opinion: Environment: Let EPA regulate carbon dioxide. 
        # 1 = Support, 0 = Oppose
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
    mutate(partyagree = ifelse(pid3=="Democrat", demagree, 
                        ifelse(pid3=="Republican", reverse_code(demagree), 0))) %>%
    # Self-reported voters
    mutate(selfvoter = ifelse(CC16_401 == "I definitely voted in the General Election.", 1, 0))  %>%
    mutate(whovote = CC16_410a) %>%
    mutate(whovote.clinton = make_dummy(CC16_410a, 
                              on_vals="Hillary Clinton (Democrat)")) %>%
    mutate(whovote.trump   = make_dummy(CC16_410a, 
                              on_vals="Donald Trump (Republican)")) %>%
    mutate(whovote.clinton = replace(whovote.clinton, 
                                     is.na(whovote.clinton)==TRUE, 0)) %>%
    mutate(whovote.trump   = replace(whovote.trump, 
                                     is.na(whovote.trump)==TRUE, 0)) %>%
    # Participation
    mutate(part.meeting = make_dummy(CC16_417a_1, "Yes", 
                                     na_vals = c("Skipped", "Not Asked"))) %>%
    mutate(part.work = make_dummy(CC16_417a_3, "Yes", 
                                  na_vals = c("Skipped", "Not Asked"))) %>%
    mutate(part.donate = make_dummy(CC16_417a_4, "Yes", 
                                    na_vals = c("Skipped", "Not Asked"))) %>%
    
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

## Clean FSU Module
{
  clean_fsu <- 
    cces_fsu %>%
    # ID
    mutate(id = as.numeric(V101)) %>%
    # Age
    mutate(age = 2016 - as.numeric(birthyr))  %>%
    # Dummy to drop people who didn't receive the social identity questions
    mutate(drop = ifelse(pid7 %in% c("Not sure", "Skipped", "Not Asked") & 
                           CC16_340a %in% c("Not sure", "Skipped", "Not Asked"), 1, 0)) %>%
    # Dummies for Independents and Moderates, each of whom get 0 scores for psi and isi, respectively
    mutate(independent = make_dummy(pid7, on_vals = "Independent")) %>%
    mutate(moderate = make_dummy(CC16_340a, on_vals = "Middle of the Road")) %>%
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
    dplyr::select(-c(p.insult, p.they, p.success, p.praise, i.insult, i.they, i.success, i.praise)) %>%
    mutate(psi.high = ifelse(psi>=8, 1, 0)) %>%
    mutate(isi.high = ifelse(isi>=8, 1, 0)) %>%
    # Keep only some variables from FSU module
    dplyr::select(
      id:isi.high) #%>%
}

## Merge FSU and Common Content by ID
{
  d.cces <- clean_fsu  %>%
         left_join(clean_cces, by = "id")  %>%
         as.data.frame()
  d.cces<-subset(d.cces, drop==0, select = -drop)
}

## Clean ANES Data
{
  d.anes <- 
    data_anes %>%
    # age
    mutate(age = as.numeric(V161267)) %>%
    mutate(female = make_dummy(V161342, on_vals = "2. Female")) %>%
    mutate(educ_cat = case_when(
      as.numeric(V161270) %in% c(16:18) ~ 4L,            # Post-grad degree
      as.numeric(V161270) %in% c(15) ~ 3L,               # Bachelor's Degree
      as.numeric(V161270) %in% c(12:14) ~ 2L,            # Some college
      as.numeric(V161270) %in% c(11, 19) ~ 1L,           # High school diploma
      as.numeric(V161270) %in% c(3:10) ~ 0L,             # Less than HS diploma
      as.numeric(V161270) %in% c(1, 2, 20)~ NA_integer_, # Missing
      TRUE ~ NA_integer_)) %>%
    mutate(race_factor = recode(as.numeric(V161310x),
                                `2` = "1. White",
                                `3` = "2. Black",
                                `4` = "4. Other",
                                `5` = "4. Other",
                                `6` = "3. Hispanic",
                                `7` = "4. Other",
                                .default = NA_character_,
                                .missing = NA_character_)) %>%
    mutate(minority = make_dummy(race_factor, 
                                 on_vals = c("2. Black", "3. Hispanic", "4. Other"))) %>%
    dplyr::select(-race_factor) %>%
    mutate(married = make_dummy(as.numeric(V161268), 
                                on_vals = c(3, 4), na_vals = c(1, 2)))  %>%
    mutate(bornagain = make_dummy(V161263,
                                  on_vals = 1, na_vals = c(-9, -8, -4, -1))) %>%
    mutate(church_attend = make_dummy(as.numeric(V161244), 
                                      on_vals = 3, na_vals = c(1, 2))) %>%
    mutate(church_often = reverse_code(as.numeric(V161245),
                                       na_vals = c(1, 2, 3))) %>%
    mutate(church_moreweek = make_dummy(as.numeric(V161245a),
                                        on_vals = 1, na_vals = c(-9, -1))) %>%
    mutate(church_freq = case_when(
      church_attend == 1 & church_often == 4 & church_moreweek == 1 ~ 5L,
      church_attend == 1 & church_often == 4 & church_moreweek == 0 ~ 4L,
      church_attend == 1 & church_often == 3 ~ 3L,
      church_attend == 1 & church_often == 2 ~ 2L,
      church_attend == 1 & church_often == 1 ~ 1L,
      church_attend == 0 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    
    dplyr::select(-c(church_attend, church_often, church_moreweek)) %>%
    
    mutate(interest = reverse_code(as.numeric(V162256),
                                   na_vals = c(1, 2, 3, 4))) %>%
    mutate(income = recode(as.integer(V161361x),
                           `1` = NA_integer_,
                           `2` = NA_integer_,
                           .missing = NA_integer_) -2)  %>%
    #mutate(income5 = ntile(income, 5))  %>%
    mutate(party.ID7 = recode(as.integer(V161158x),
                              `-9` = NA_integer_,
                              `-8` = NA_integer_,
                              .missing = NA_integer_)) %>%
    mutate(guns = make_dummy(as.numeric(V161187),
                             on_vals = 3, na_vals = c(1,2))) %>%
    mutate(abortion = make_dummy(as.numeric(V161232),
                                 on_vals = 6, na_vals = c(1, 2, 7))) %>%
    mutate(gaymarriage = make_dummy(as.numeric(V161231),
                                    on_vals = 3, na_vals = c(1, 2))) %>%
    mutate(environment = make_dummy(as.numeric(V161224),
                                    on_vals = 3, na_vals = c(1, 2))) %>% 
    mutate(demagree = guns + abortion + gaymarriage + environment) %>%
    mutate(partyagree = ifelse(party.ID7%in%c(1,2,3), demagree, ifelse(party.ID7%in%c(5,6,7), 4+(demagree*-1), -99))) %>%
    dplyr::select(-c(guns, abortion, gaymarriage, environment, demagree)) %>%
    mutate(selfvoter = make_dummy(as.numeric(V162031x),
                                  on_vals = 1, na_vals = c(-8, -1))) %>%
    mutate(ideo.ID7 = recode(as.integer(V162171),
                             `-9` = NA_integer_,
                             `-7` = NA_integer_,
                             `-6` = NA_integer_,
                             `99` = NA_integer_,
                             .missing = NA_integer_)) %>%
    
    mutate(party.strength = case_when(
      party.ID7 %in% c(1,7) ~ 3L,
      party.ID7 %in% c(2,6) ~ 2L,
      party.ID7 %in% c(3,5) ~ 1L,
      party.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(ideo.strength = case_when(
      ideo.ID7 %in% c(1,7) ~ 3L,
      ideo.ID7 %in% c(2,6) ~ 2L,
      ideo.ID7 %in% c(3,5) ~ 1L,
      ideo.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(anes.pri.ae = make_dummy(V161021a, 
                                    on_vals=c("2. Bernie Sanders", "4. Donald Trump", "5. Ted Cruz"), 
                                    na_vals = c("-8. Don't know (FTF only)", "-9. Refused"))) %>%
    mutate(anes.pri.es = make_dummy(V161021a, 
                                    on_vals=c("1. Hillary Clinton", "7. Marco Rubio", "6. John Kasich"), 
                                    na_vals = c("-8. Don't know (FTF only)", "-9. Refused"))) %>%
    mutate(anes.pri.part = make_dummy(V161021,
                                      on_vals = c("1. Yes, voted in primary or caucus"),
                                      na_vals = c("-9. Refused", "-8. Don't know (FTF only)"))) %>%
    mutate(part.meeting = make_dummy(as.numeric(V162011), 
                                     on_vals = 4, na_vals = c(1, 2, 3))) %>%
    mutate(part.work = make_dummy(as.numeric(V162013), 
                                  on_vals = 4, na_vals = c(1, 2, 3))) %>%
    mutate(part.donate = make_dummy(as.numeric(V162014), 
                                    on_vals = 5, na_vals = c(1, 2, 3, 4))) %>%
    dplyr::select(
      age:part.donate) #%>%
}

## Clean ANES Panel Data

# 2000
{
  d.anes.p00 <- 
    data_anes_panel %>%
    filter(Years == "3. 2000 - 2002 - 2004") %>%
    mutate(female = make_dummy(as.numeric(M001029), 
                               on_vals = 2)) %>%
    mutate(minority = make_dummy(as.numeric(M001006a), 
                                 on_vals = 6)) %>%
    mutate(married = make_dummy(M000909, 
                                on_vals = "1. MARRIED")) %>%
    mutate(church_freq = case_when(
      as.numeric(M000879) == 2 & as.numeric(M000880) == 3 ~ 5L,
      as.numeric(M000879) == 2 & as.numeric(M000880) == 2 ~ 4L,
      as.numeric(M000879) == 3 ~ 3L,
      as.numeric(M000879) == 4 ~ 2L,
      as.numeric(M000879) == 5 ~ 1L,
      as.numeric(M000877) == 3 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(interest = reverse_code(as.numeric(M001367),
                                   na_vals = c(5, 6))) %>%
    mutate(income = as.numeric(M000997)) %>%
    mutate(educ_cat = case_when(
      as.numeric(M000913) == 8 ~ 4L,
      as.numeric(M000913) == 7 ~ 3L,
      as.numeric(M000913) %in% c(5, 6) ~ 2L,
      as.numeric(M000913) == 4 ~ 1L,
      as.numeric(M000913) %in% c(2, 3) ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(party.ID7 = recode(as.integer(M000523),
                              `1` = 1L, # Strong dem
                              `2` = 2L,
                              `3` = 3L,
                              `4` = 4L,
                              `5` = 5L,
                              `6` = 6L,
                              `7` = 7L, # Strong rep
                              `8` = NA_integer_,
                              .missing = NA_integer_,
                              .default = NA_integer_)) %>%
    mutate(party.ID3 = case_when(
      M000519 == "1. DEMOCRAT" ~ "Dem",
      M000519 == "2. REPUBLICAN" ~ "Rep",
      M000519 == "3. INDEPENDENT" ~ "Ind",
      M000519 == "5. NO PREFERENCE" ~ "None",
      M000519 == "4. OTHER PARTY [SPECIFY]" ~ "None",
      TRUE ~ NA_character_)) %>%
    mutate(party.strength = case_when(
      party.ID7 %in% c(1,7) ~ 3L,
      party.ID7 %in% c(2,6) ~ 2L,
      party.ID7 %in% c(3,5) ~ 1L,
      party.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(ideo.ID7 = as.numeric(M000446)-1) %>% #1 = strong lib, 7 = strong con
    mutate(ideo.strength = case_when(
      ideo.ID7 %in% c(1,7) ~ 3L,
      ideo.ID7 %in% c(2,6) ~ 2L,
      ideo.ID7 %in% c(3,5) ~ 1L,
      ideo.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(gun = as.numeric(M000731)-4) %>%
    mutate(abo = make_dummy(as.numeric(M000694),
                            on_vals=5)) %>%
    mutate(env = as.numeric(M000776)-4) %>%
    mutate(demagree = abo-gun-env) %>%
    mutate(demagree = ifelse(demagree<=0, 0, demagree)) %>%
    mutate(partyagree = ifelse(party.ID3=="Dem", demagree, ifelse(party.ID3=="Rep", 5+(demagree*-1), 0))) %>%
    mutate(repagree = (-1*demagree)+max(demagree, na.rm = TRUE)) %>%
    dplyr::select(-demagree, -gun, -abo, -env) %>%
    mutate(therm.reps.mean = (as.numeric(as.character(M000361)) + as.numeric(as.character(M000367)))/2) %>%
    mutate(therm.dems.mean = (as.numeric(as.character(M000360)) + as.numeric(as.character(M000368)) + as.numeric(as.character(M000359)))/3) %>%
    dplyr::select(female:therm.dems.mean)  
}

# 2002
{
  d.anes.p02 <-
    data_anes_panel %>%
    filter(Years == "3. 2000 - 2002 - 2004") %>%
    mutate(female = make_dummy(as.numeric(M001029), 
                               on_vals = 2)) %>%
    mutate(minority = make_dummy(as.numeric(M001006a), 
                                 on_vals = 6)) %>%
    mutate(married = make_dummy(M023127A,
                                on_vals = "1. Married")) %>%
    mutate(church_freq = case_when(
      as.numeric(M023085) == 2 & as.numeric(M023086) == 3 ~ 5L,
      as.numeric(M023085) == 2 & as.numeric(M023086) == 2 ~ 4L,
      as.numeric(M023085) == 3 ~ 3L,
      as.numeric(M023085) == 4 ~ 2L,
      as.numeric(M023085) == 5 ~ 1L,
      as.numeric(M023084) == 3 ~ 0L,
      TRUE ~ NA_integer_)) %>%  
    mutate(interest = reverse_code(as.numeric(M025084),
                                   na_vals = c(5, 6))) %>%
    mutate(income = recode(as.numeric(M023149),
                           `2` = 0L,
                           `3` = 1L,
                           `4` = 2L,
                           `5` = 3L,
                           `6` = 4L,
                           `7` = 5L,
                           `8` = 6L,
                           `9` = NA_integer_,
                           `10` = NA_integer_,
                           .missing = NA_integer_,
                           .default = NA_integer_)) %>%
    mutate(educ_cat = case_when(
      as.numeric(M023131) == 8 ~ 4L,
      as.numeric(M023131) == 7 ~ 3L,
      as.numeric(M023131) %in% c(5, 6) ~ 2L,
      as.numeric(M023131) == 4 ~ 1L,
      as.numeric(M023131) %in% c(2, 3) ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(party.ID7 = recode(as.integer(M023038X),
                              `1` = 1L, # Strong dem
                              `2` = 2L,
                              `3` = 3L,
                              `4` = 4L,
                              `5` = 5L,
                              `6` = 6L,
                              `7` = 7L, # Strong rep
                              `8` = NA_integer_,
                              .missing = NA_integer_,
                              .default = NA_integer_)) %>%
    mutate(party.ID3 = case_when(
      M023036 == "1. Democrat" ~ "Dem",
      M023036 == "2. Republican" ~ "Rep",
      M023036 == "3. Independent" ~ "Ind",
      M023036 == "5. No Preference {VOL}" ~ "None",
      M023036 == "4. Other Party {VOL} {SPECIFY}" ~ "None",
      TRUE ~ NA_character_)) %>%
    mutate(party.strength = case_when(
      party.ID7 %in% c(1,7) ~ 3L,
      party.ID7 %in% c(2,6) ~ 2L,
      party.ID7 %in% c(3,5) ~ 1L,
      party.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(ideo.ID7 = as.numeric(M023022)-1) %>% #1 = strong lib, 7 = strong con
    mutate(ideo.strength = case_when(
      ideo.ID7 %in% c(1,7) ~ 3L,
      ideo.ID7 %in% c(2,6) ~ 2L,
      ideo.ID7 %in% c(3,5) ~ 1L,
      ideo.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(env = make_dummy(as.numeric(M025113X),
                            on_vals=1)) %>%
    mutate(wel = make_dummy(as.numeric(M025107X),
                            on_vals=1)) %>%
    mutate(sch = ifelse(M025108X == "1. Increased" | M025108Y == "1. Increased", 1, 0)) %>%
    mutate(demagree = env+wel+sch) %>%
    mutate(partyagree = ifelse(party.ID3=="Dem", demagree, ifelse(party.ID3=="Rep", 4+(demagree*-1), 0))) %>%
    mutate(repagree = (-1*demagree)+max(demagree, na.rm = TRUE)) %>%
    dplyr::select(-env, -wel, -sch, -demagree) %>%
    mutate(therm.reps.mean = (as.numeric(as.character(M023010)) + as.numeric(as.character(M023011)))/2) %>%
    mutate(therm.dems.mean = (as.numeric(as.character(M023012)) + as.numeric(as.character(M023015)) + as.numeric(as.character(M023020)))/3) %>%
    dplyr::select(female:therm.dems.mean)  
}

# 2004
{
  d.anes.p04 <-
    data_anes_panel %>%
    filter(Years == "3. 2000 - 2002 - 2004") %>%
    mutate(female = make_dummy(as.numeric(M001029), 
                               on_vals = 2)) %>%
    mutate(minority = make_dummy(as.numeric(M001006a), 
                                 on_vals = 6)) %>%
    mutate(married = make_dummy(M045176, 
                                on_vals = "1. Married")) %>%
    mutate(church_freq = case_when(
      as.numeric(M023085) == 2 & as.numeric(M023086) == 3 ~ 5L,
      as.numeric(M023085) == 2 & as.numeric(M023086) == 2 ~ 4L,
      as.numeric(M023085) == 3 ~ 3L,
      as.numeric(M023085) == 4 ~ 2L,
      as.numeric(M023085) == 5 ~ 1L,
      as.numeric(M023084) == 3 ~ 0L,
      TRUE ~ NA_integer_)) %>% 
    mutate(interest = reverse_code(as.numeric(M045057),
                                   na_vals = c(5, 6))) %>%
    mutate(income = recode(as.numeric(M023149),
                           `2` = 0L,
                           `3` = 1L,
                           `4` = 2L,
                           `5` = 3L,
                           `6` = 4L,
                           `7` = 5L,
                           `8` = 6L,
                           `9` = NA_integer_,
                           `10` = NA_integer_,
                           .missing = NA_integer_,
                           .default = NA_integer_)) %>%
    mutate(educ_cat = case_when(
      as.numeric(M023131) == 8 ~ 4L,
      as.numeric(M023131) == 7 ~ 3L,
      as.numeric(M023131) %in% c(5, 6) ~ 2L,
      as.numeric(M023131) == 4 ~ 1L,
      as.numeric(M023131) %in% c(2, 3) ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(party.ID7 = recode(as.integer(M045058x),
                              `1` = 1L, # Strong dem
                              `2` = 2L,
                              `3` = 3L,
                              `4` = 4L,
                              `5` = 5L,
                              `6` = 6L,
                              `7` = 7L, # Strong rep
                              `8` = NA_integer_,
                              .missing = NA_integer_,
                              .default = NA_integer_)) %>%
    mutate(party.ID3 = case_when(
      M045058 == "2. Democrat" ~ "Dem",
      M045058 == "1. Republican" ~ "Rep",
      M045058 == "3. Independent" ~ "Ind",
      M045058 == "5. No preference {VOL}" ~ "None",
      M045058 == "4. Other party {VOL} {SPECIFY}" ~ "None",
      TRUE ~ NA_character_)) %>%
    mutate(party.strength = case_when(
      party.ID7 %in% c(1,7) ~ 3L,
      party.ID7 %in% c(2,6) ~ 2L,
      party.ID7 %in% c(3,5) ~ 1L,
      party.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(ideo.ID7 = as.numeric(M023022)-1) %>% #1 = strong lib, 7 = strong con
    mutate(ideo.strength = case_when(
      ideo.ID7 %in% c(1,7) ~ 3L,
      ideo.ID7 %in% c(2,6) ~ 2L,
      ideo.ID7 %in% c(3,5) ~ 1L,
      ideo.ID7 == 4 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    mutate(abo = make_dummy(as.numeric(M045110),
                            on_vals=5)) %>%
    mutate(env = make_dummy(as.numeric(M045068),
                            on_vals=1)) %>%
    mutate(wel = make_dummy(as.numeric(M045070),
                            on_vals=1)) %>%
    mutate(sch = make_dummy(as.numeric(M045071x),
                            on_vals=1)) %>%
    mutate(demagree = abo+env+wel+sch) %>%
    mutate(partyagree = ifelse(party.ID3=="Dem", demagree, ifelse(party.ID3=="Rep", 4+(demagree*-1), 0))) %>%
    mutate(repagree = (-1*demagree)+max(demagree, na.rm = TRUE)) %>%
    dplyr::select(-demagree, -abo, -env, -wel, -sch) %>%
    mutate(therm.reps.mean = (as.numeric(as.character(M045007)) + as.numeric(as.character(M045009)))/2) %>%
    mutate(therm.dems.mean = (as.numeric(as.character(M045010)) + as.numeric(as.character(M045013)) + as.numeric(as.character(M045018)))/3) %>%
    
    mutate(bush.approve.02 = reverse_code(as.numeric(M023006X))) %>%
    mutate(bush.approve.04 = reverse_code(as.numeric(M045005x))) %>%
    mutate(del.bush.job = case_when(
      bush.approve.04 > bush.approve.02 ~ 1L,
      bush.approve.04 < bush.approve.02 ~ -1L,
      bush.approve.04 == bush.approve.02 ~ 0L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(finance.00 = reverse_code(as.numeric(M000401))) %>%
    mutate(finance.02 = reverse_code(as.numeric(M023026))) %>%
    mutate(finance.04 = reverse_code(as.numeric(M045089x))) %>%
    mutate(del.finances = case_when(
      finance.04 > finance.02 ~ 1L,
      finance.04 == finance.02 ~ 0L,
      finance.04 < finance.02 ~ -1L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(nat.economy.00 = reverse_code(as.numeric(M000491))) %>%
    mutate(nat.economy.02 = reverse_code(as.numeric(M023028))) %>%
    mutate(nat.economy.04 = reverse_code(as.numeric(M045091x))) %>%
    mutate(del.nat.econ = case_when(
      nat.economy.04 > nat.economy.02 ~ 1L,
      nat.economy.04 == nat.economy.02 ~ 0L,
      nat.economy.04 < nat.economy.02 ~ -1L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(bush.economy.02 = reverse_code(as.numeric(M023042X))) %>%
    mutate(bush.economy.04 = reverse_code(as.numeric(M045006x))) %>%
    mutate(del.bush.econ = case_when(
      bush.economy.04 > bush.economy.02 ~ 1L,
      bush.economy.04 == bush.economy.02 ~ 0L,
      bush.economy.04 < bush.economy.02 ~ -1L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(inter.rep.02 = as.numeric(M023032)-2) %>%
    mutate(inter.rep.04 = as.numeric(M045142)-2) %>%
    mutate(del.inter.rep = case_when(
      inter.rep.04 > inter.rep.02 ~ 1L,
      inter.rep.04 == inter.rep.02 ~ 0L,
      inter.rep.04 < inter.rep.02 ~ -1L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(bush.terror.02 = reverse_code(as.numeric(M023119X))) %>%
    mutate(bush.terror.04 = reverse_code(as.numeric(M045085x))) %>%
    mutate(del.bush.terror = case_when(
      bush.terror.04 > bush.terror.02 ~ 1L,
      bush.terror.04 == bush.terror.02 ~ 0L,
      bush.terror.04 < bush.terror.02 ~ -1L,
      TRUE ~ NA_integer_)) %>%
    
    mutate(afghan.worth.02 = ifelse(M023120=="1. Worth it", 1, ifelse(M023120=="5. Not worth it", 0, NA))) %>%
    mutate(afghan.worth.04 = ifelse(M045086=="1. Worth it", 1, ifelse(M045086=="5. Not worth it", 0, NA))) %>%
    mutate(del.afghan = case_when(
      afghan.worth.04 > afghan.worth.02 ~ 1L,
      afghan.worth.04 == afghan.worth.02 ~ 0L,
      afghan.worth.04 < afghan.worth.02 ~ -1L,
      TRUE ~ NA_integer_)) %>%
    dplyr::select(female:del.afghan)  
}  

## Save stuff ------------------------------------------------------------------

keep(d.cces, d.anes, d.anes.p00, d.anes.p02, d.anes.p04, sure = TRUE)

save.image("~/Documents/Dissertation/Analysis/00cleaneddata.RData")


## Extra code, not run ---------------------------------------------------------

# ## Clean ANES TS Data
# {
#   d.anes.ts <-
#     data_anes_ts %>%
#     #year 
#     mutate(year = VCF0004) %>%
#     # pid strength 
#     mutate(party.strength = as.numeric(VCF0305)-2) %>%
#     mutate(party.strength = ifelse(party.strength==-1, NA, party.strength)) %>%
#     # ideology  
#     mutate(ideo.ID7 = as.numeric(VCF0803)) %>%
#     mutate(ideo.ID7 = ifelse(ideo.ID7 %in% c(1, 9), NA, ideo.ID7-1)) %>%
#     mutate(ideo.strength = case_when(
#       ideo.ID7 %in% c(1,7) ~ 3L,
#       ideo.ID7 %in% c(2,6) ~ 2L,
#       ideo.ID7 %in% c(3,5) ~ 1L,
#       ideo.ID7 == 4 ~ 0L,
#       TRUE ~ NA_integer_)) %>%
#     # party.agree ???
#     ## govinsurance VCF0806 1970-2012
#     ## jobs VCF0809 1972 - 2012
#     ## isolation VCF0823 1956 - 2012
#     ## abortion VCF0837 and 0838 1972 - 2012
#     # female dummy variable
#     mutate(female = make_dummy(as.numeric(VCF0104), on_vals = 3, na_vals = 1)) %>%
#     # minority dummy
#     mutate(minority = make_dummy(as.numeric(VCF0106), on_vals = c(3, 4),
#                                  na_vals = c(1, 5))) %>%
#     # married dummy
#     mutate(married = make_dummy(as.numeric(VCF0147), on_vals = 1,
#                                 na_vals = c(6, 7, 8))) %>%
#     # church frequency
#     mutate(church_freq = case_when(
#       as.numeric(VCF0130) %in% c(2,3) | as.numeric(VCF0131) == 2 ~ 3L,
#       as.numeric(VCF0130) %in% c(4) | as.numeric(VCF0131) == 3 ~ 2L,
#       as.numeric(VCF0130) %in% c(5) | as.numeric(VCF0131) == 4 ~ 1L,
#       as.numeric(VCF0130) %in% c(6) | as.numeric(VCF0131) == 5 ~ 0L,
#       TRUE ~ NA_integer_)) %>%
#     # interest 
#     mutate(interest = as.numeric(VCF0310)) %>%
#     mutate(interest = ifelse(interest %in% c(1, 5), NA, interest-1)) %>%
#     # income
#     mutate(income = as.numeric(VCF0114)) %>%
#     mutate(income = ifelse(income==1, NA, income-1)) %>%
#     # education category
#     mutate(educ_cat = recode(as.numeric(VCF0140a),
#                              `1` = 0L, 
#                              `2` = 0L,
#                              `3` = 1L,
#                              `4` = 1L,
#                              `5` = 2L,
#                              `6` = 3L,
#                              `7` = 4L,
#                              .default = NA_integer_,
#                              .missing = NA_integer_)) %>%
#     # pid seven pt
#     mutate(party.ID7 = as.numeric(VCF0301)-1) %>%
#     mutate(party.ID7 = ifelse(party.ID7 == 0, NA, party.ID7)) %>% # 1 = strong dem, 7 = strong rep
#     dplyr::select(
#       year:party.ID7)
# }

# Missing survey questions
# 
# # so as it turns out, some of the questions we need weren't asked through the 
# # entire history of the ANES. Let's see which questions we have in which years.
# questions <- as.data.frame(matrix(nrow = length(unique(d.anes.ts$year)), ncol = ncol(d.anes.ts)))
# colnames(questions) <- colnames(d.anes.ts)
# rownames(questions) <- unique(d.anes.ts$year)
# for (i in 1:nrow(questions)){
#   for (j in 1:ncol(questions)){
#     questions[i,j] <- ifelse(is.na(mean(as.numeric(d.anes.ts[d.anes.ts$year==unique(d.anes.ts$year)[i],j]), na.rm = TRUE)) == TRUE, "No", "Yes")
#   }
# }
# questions
# # so this is a bit unfortunate. two of the first three years (48, 54) are missing
# #  several variables. Some later years (72, 02) are missing one. 
# #  Let's drop those two years and leave the rest for imputation.
# 
# d.anes.ts <- subset(d.anes.ts, !(year%in%c(1948, 1952, 1954)))

## End ----------------------