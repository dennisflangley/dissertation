# Title ------------------------------------------------------------------------
##
## name: DissertationChapTwo.R
## author: Dennis F Langley
## date: 2018-04-06
## what: Dissertation Chapter Two
##       Data Import, Cleaning, and Analysis
##
####

# Preamble ---------------------------------------------------------------------
rm(list = ls())
dev.off()
options(scipen = 999)
setwd("~/Documents/Dissertation/Data")
library(tidyverse)
library(foreign)
library(plyr)
library(gdata)
library(texreg)
library(lmtest)
library(caret)
library(mice)
library(data.table)
library(reshape2)
library(VIM)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(grid)
library(lattice)
library(beepr)
cat("\014")

# Custom Functions -------------------------------------------------------------

## Make binary variable, recode 'na_vals' to NA
make_dummy <- function(var, on_vals, na_vals = NULL){
  case_when(
    is.na(var)       ~ NA_integer_,
    var %in% na_vals ~ NA_integer_,
    var %in% on_vals ~ 1L,
    TRUE             ~ 0L
  )
}

## Reverse order of a variable
reverse_code <- function(var, na_vals = NULL){
  var_nas <- ifelse(var %in% na_vals, NA_integer_, var)
  max_val <- max(var_nas, na.rm = TRUE)
  rev_var <-  as.integer(var_nas - max_val)  %>% abs()
  return(rev_var)
}

## Custom ggplot for visualization of missing data
## requires reshape2 library for melt function
ggplot_missing <- function(x){
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

## Gives the percent of observations of a variable that are NA
percent_missing <- function(x){sum(is.na(x))/length(x)*100}


# Data Import ------------------------------------------------------------------
cces_fsu <- read.dta("./CCES/CCES16_FSU_OUTPUT_Feb2017.dta")
load("./CCES/CCES16_Common_OUTPUT_Jul2017_VV.RData"); cces_common <- x;remove(x)
data_anes <- read.dta("./ANES/anes_timeseries_2016_Stata12.dta")
# Data Cleaning ----------------------------------------------------------------

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
      as.numeric(V161270) %in% c(16:18) ~ 4L,
      as.numeric(V161270) %in% c(15) ~ 3L,
      as.numeric(V161270) %in% c(12:14) ~ 2L,
      as.numeric(V161270) %in% c(11, 19) ~ 1L,
      as.numeric(V161270) %in% c(3:10) ~ 0L,
      as.numeric(V161270) %in% c(1, 2, 20)~ NA_integer_,
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
    mutate(minority = make_dummy(V161310x, 
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
    mutate(part.meeting = make_dummy(as.numeric(V162011), 
                                     on_vals = 4, na_vals = c(1, 2, 3))) %>%
    mutate(part.work = make_dummy(as.numeric(V162013), 
                                     on_vals = 4, na_vals = c(1, 2, 3))) %>%
    mutate(part.donate = make_dummy(as.numeric(V162014), 
                                     on_vals = 5, na_vals = c(1, 2, 3, 4))) %>%
    dplyr::select(
      age:part.donate) #%>%
}

# Keep only relevant things now
keep(d.cces, d.anes, percent_missing, ggplot_missing, sure=TRUE)

# Machine Learning -------------------------------------------------------------
d.cces.mod <- subset(d.cces, select = c(cces.pri.es, cces.pri.ae,
                                        psi, female, minority, married,
                                        party.strength, ideo.strength, 
                                        partyagree, church_freq, 
                                        interest, income, educ_cat))
d.anes.mod <- subset(d.anes, select = c(anes.pri.es, anes.pri.ae, 
                                        female, minority, married,
                                        party.strength, ideo.strength, 
                                        partyagree, church_freq, 
                                        interest, income, educ_cat))

# check missingness in each variable
apply(d.cces.mod,2,percent_missing)
apply(d.anes.mod,2,percent_missing)

# let's use MICE to impute
tempdata1 <- mice(d.cces.mod, m = 5, maxit = 50, meth = "rf", 
                 seed = 32308, print = FALSE)
mice_output1 <- complete(tempdata1)

tempdata2 <- mice(d.anes.mod, m = 5, maxit = 50, meth = "rf", 
                 seed = 32308, print = FALSE)
mice_output2 <- complete(tempdata2)
beep(3)

# check our variable of interest pre- and post-imputation
psi_orig <- ggplot(d.cces.mod, aes(x=psi)) +
  geom_histogram(color="black", fill=rgb(0.8, 0.2, 0.2, 0.9)) +
  theme_few() + ylim(0, 210) + 
  ggtitle("psi: Original Data")

psi_mice <- ggplot(mice_output1, aes(x=psi)) +
  geom_histogram(color="black", fill=rgb(0.6, 0.2, 0.2, 0.5)) +
  theme_few() +  ylim(0, 210) + 
  ggtitle("psi: MICE")

grid.arrange(psi_orig, psi_mice, ncol=2)

# looks fine, so let's fill in missing values
d.cces <- complete(tempdata1)
d.anes <- complete(tempdata2)

# keep only what we need, again
keep(d.cces, d.anes)

# we don't need these for building our machine learning models
omit <- c("cces.pri.es", "cces.pri.ae")
d.learn <- as.data.frame(dplyr::select(d.cces, -which(names(d.cces)%in%omit)))


# let's split data into training and test sets
set.seed(32308)
split1 <- createDataPartition(d.cces$psi, p = .75)[[1]]
train.data <- d.cces[split1,]
valid.data <- d.cces[-split1,]
train.x <- as.data.frame(dplyr::select(train.data, -psi))
train.y <- train.data$psi
test.x <- as.data.frame(dplyr::select(valid.data, -psi))
test.y <- valid.data$psi

ctrl<-trainControl(method = "repeatedcv",
                   n = 10, verboseIter = FALSE,
                   repeats = 5,
                   savePredictions = "final")

set.seed(32308)
train_lm <- train(y=train.y, x=train.x,
                  trControl=ctrl,
                  method="lm",
                  metric="RMSE")

set.seed(32308)
train_cart <- train(y=train.y, x=train.x,
                    trControl=ctrl,
                    tuneLength=10,
                    method="rpart",
                    metric="RMSE")

set.seed(32308)
nnetGrid <- expand.grid(.decay = c(0.001, 0.01, 0.1),
                        .size = seq(1, 27, by = 2),
                        .bag = FALSE)
train_nnet <- train(y=train.y, x=train.x,
                    tuneGrid = nnetGrid,
                    trControl = ctrl,
                    linout = TRUE,
                    trace = FALSE,
                    method = "avNNet",
                    metric = "RMSE"); beep(3)

set.seed(32308)
train_knn <- train(y=train.y, x=train.x,
                    trControl = ctrl,
                    method = "knn",
                    metric = "RMSE")

set.seed(32308)
train_rf <- train(y=train.y, x=train.x,
                  method = "rf",
                  tuneLength = 10,
                  ntrees = 1000,
                  metric = "RMSE",
                  trControl = ctrl,
                  importance = TRUE)

set.seed(32308)
train_cf <- train(y=train.y, x=train.x,
                  method = "cforest",
                  tuneLength = 10,
                  metric = "RMSE",
                  trControl = ctrl,
                  importance = TRUE)

train_models<-lapply(ls(pattern="train_"), get)
allResamples <- resamples(list("Linear Reg" = train_lm,
                               "CART" = train_cart,
                               "Neural Net" = train_nnet,
                               "K-Nearest" = train_knn,
                               "Random Forest" = train_rf,
                               "C Forest" = train_cf))

parallelplot(allResamples, metric = "RMSE")
parallelplot(allResamples, metric = "Rsquared")


preds.rf <- predict(train_rf, 
                      newdata = test.x,
                      type = "raw")

preds.cf <- predict(train_cf, newdata = test.x)

RMSE(preds.rf, test.y)
RMSE(preds.cf, test.y)


plot(preds.rf, test.y,
     xlim = c(0,16), ylim = c(0,16))
plot(preds.cf, test.y,
     xlim = c(0,16), ylim = c(0,16))

varImp(train_rf$finalModel)


## Misc ####

plot(jitter(d.anes$party.strength, 0.5), jitter(d.anes$psi, 1),
     xlab = "Party ID Strength", ylab = "Partisan Social Identity",
     main = "Jitter Plot of PID Strength and Partisan Social Identity")

plot(jitter(d.anes$ideo.strength, 0.5), jitter(d.anes$isi, 1),
     xlab = "Ideological ID Strength", ylab = "Ideological Social Identity",
     main = "Jitter Plot of Ideological Strength and Ideological Social Identity")

## Logit Models

### Main Results
mod.anes.es1 <- glm(anes.pri.es ~ female + minority + married + party.strength + 
                      ideo.strength + partyagree + church_freq + interest + 
                      income + educ_cat,
                    data=d.anes, family = binomial(link = "logit"))
mod.anes.es2 <- glm(anes.pri.es ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes, family = binomial(link = "logit"))

mod.anes.ae1 <- glm(anes.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes, family = binomial(link = "logit"))
mod.anes.ae2 <- glm(anes.pri.ae ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes, family = binomial(link = "logit"))

lr.es <- lrtest(mod.anes.es1, mod.anes.es2)
lr.ae <- lrtest(mod.anes.ae1, mod.anes.ae2)

texreg(l=list(mod.anes.es1, mod.anes.es2, mod.anes.ae1, mod.anes.ae2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Partisan Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment", "Anti-Establishment", "Anti-Establishment"))
