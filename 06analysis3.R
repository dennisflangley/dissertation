## Title -----------------------------------------------------------------------
##
## name:    06analysis3.R
## author:  Dennis F Langley
## date:    2018-06-04
## what:    Analysis for Chapter 3 after Machine Learning
##     
##
####

## Preamble --------------------------------------------------------------------
library(gdata)
library(dplyr)
library(tidyverse)
library(texreg)
rm(list = ls())
dev.off()
options(scipen = 999)
setwd("~/Documents/Dissertation/Data")

load("~/Documents/Dissertation/Analysis/03tuneddata.RData")

keep(d.anes.p00, d.anes.p02, d.anes.p04, sure=TRUE)

cat("\014")

## Data cleaning/merging -------------------------------------------------------

# 2000 Panel
{
d.anes.p00 <-
  d.anes.p00 %>%
  dplyr::select(therm.reps.00 = therm.reps.mean,
                therm.dems.00 = therm.dems.mean,
                party.ID7.00 = party.ID7,
                party.str.00 = party.strength,
                ideo.ID7.00 = ideo.ID7,
                ideo.str.00 = ideo.strength,
                repagree.00 = repagree,
                psi.00 = psi,
                party.ID3.00 = party.ID3) %>%
  mutate(psi.00 = ifelse(party.ID3.00=="Rep", psi.00, ifelse(party.ID3.00=="Dem", -psi.00, 0))) %>%
  mutate(therm.rep.diff.00 = therm.reps.00 - therm.dems.00)
}

# 2002 Panel
{
  d.anes.p02 <-
    d.anes.p02 %>%
    dplyr::select(therm.reps.02 = therm.reps.mean,
                  therm.dems.02 = therm.dems.mean,
                  party.ID7.02 = party.ID7,
                  party.str.02 = party.strength,
                  ideo.ID7.02 = ideo.ID7,
                  ideo.str.02 = ideo.strength,
                  repagree.02 = repagree,
                  psi.02 = psi,
                  party.ID3.02 = party.ID3) %>%
    mutate(psi.02 = ifelse(party.ID3.02=="Rep", psi.02, ifelse(party.ID3.02=="Dem", -psi.02, 0))) %>%
    mutate(therm.rep.diff.02 = therm.reps.02 - therm.dems.02)
}

# 2004 Panel
{
d.anes.p04 <-
  d.anes.p04 %>%
  dplyr::select(therm.reps.04 = therm.reps.mean,
                therm.dems.04 = therm.dems.mean,
                party.ID7.04 = party.ID7,
                party.str.04 = party.strength,
                ideo.ID7.04 = ideo.ID7,
                ideo.str.04 = ideo.strength,
                repagree.04 = repagree,
                psi.04 = psi,
                party.ID3.04 = party.ID3,
                finance.00, nat.economy.00,
                bush.approve.02,finance.02,nat.economy.02,
                bush.economy.02,inter.rep.02, bush.terror.02,
                afghan.worth.02, afghan.worth.04,
                bush.approve.04,finance.04,nat.economy.04,
                bush.economy.04,inter.rep.04,bush.terror.04,
                del.bush.job,del.finances,del.nat.econ,del.bush.econ,
                del.inter.rep,del.bush.terror,del.afghan) %>%
  mutate(psi.04 = ifelse(party.ID3.04=="Rep", psi.04, ifelse(party.ID3.04=="Dem", -psi.04, 0))) %>%
  mutate(therm.rep.diff.04 = therm.reps.04 - therm.dems.04)
}

# Merge them together
d.anes.p <- cbind(d.anes.p00, d.anes.p02, d.anes.p04)

# Creating remaining difference variables
{
d.anes.p <- d.anes.p %>%
  mutate(del.psi = psi.04 - psi.02) %>%
  mutate(del.party.str = party.str.04 - party.str.02) %>%
  mutate(del.party.ID7 = party.ID7.04 - party.ID7.02) %>%
  mutate(del.ideo.str = ideo.str.04 - ideo.str.02) %>%
  mutate(del.repagree = repagree.04 - repagree.02) %>%
  mutate(del.therm.dems = therm.dems.04 - therm.dems.02) %>%
  mutate(del.therm.reps = therm.reps.04 - therm.reps.02) %>%
  mutate(del.therm.diff = therm.rep.diff.04 - therm.rep.diff.02) #%>%
  # select(psi.00, psi.04, del.psi, party.str.00, party.str.04, del.party.str,
  #        ideo.str.00, ideo.str.04, del.ideo.str,
  #        party.ID7.00, party.ID7.04, del.party.ID7, ideo.ID7.00, ideo.ID7.04,
  #        therm.rep.00, therm.dem.00, del.therm.partydiff,
  #        therm.reps.00, therm.dems.00, therm.rep.diff.00,
  #        therm.reps.04, therm.dems.04, therm.rep.diff.04,
  #        agree.00, agree.04, del.therm.dems, del.therm.reps, del.therm.diff,
  #        bush.approve.02, bush.approve.04, del.bush.job, 
  #        finance.02, finance.04, del.finances, 
  #        nat.economy.02, nat.economy.04, del.nat.econ, 
  #        bush.economy.02, bush.economy.04, del.bush.econ, 
  #        inter.rep.02, inter.rep.04, del.inter.rep, 
  #        bush.terror.02, bush.terror.04, del.bush.terror, 
  #        afghan.worth.02, afghan.worth.04, del.afghan)
}

# minor cleaning
d.anes.p$del.psi[is.na(d.anes.p$del.psi)==TRUE] <- 0
#d.anes.p$del.psi[d.anes.p$del.psi>=10] <- 10
#d.anes.p$del.psi[d.anes.p$del.psi<=-10] <- -10

## Analysis models -------------------------------------------------------------

mod.pid.basic <- lm(del.party.ID7 ~ del.bush.job + del.finances + del.nat.econ +
                    del.bush.terror + del.afghan + del.inter.rep,
                    data = d.anes.p)

mod.psi.basic <- lm(del.psi ~ del.bush.job + del.finances + del.nat.econ +
                              del.bush.terror + del.afghan + del.inter.rep,
                              data = d.anes.p)

mod.pid.therm <- lm(del.party.ID7 ~ del.bush.job + del.finances + del.nat.econ + 
                      del.bush.terror + del.afghan + del.inter.rep + 
                      del.therm.diff, data = d.anes.p)

mod.psi.therm <- lm(del.psi ~ del.bush.job + del.finances + del.nat.econ + 
                       del.bush.terror + del.afghan + del.inter.rep + 
                       del.therm.diff, data = d.anes.p)

mod.pid.basic.psi <- lm(del.party.ID7 ~ del.bush.job + del.finances + del.nat.econ +
                        del.bush.terror + del.afghan + del.inter.rep + del.psi,
                        data = d.anes.p)
mod.pid.therm.psi <- lm(del.party.ID7 ~ del.bush.job + del.finances + del.nat.econ + 
                           del.bush.terror + del.afghan + del.inter.rep +
                           del.therm.diff + del.psi, data = d.anes.p)

mod.psi.basic.pid <- lm(del.psi ~ del.bush.job + del.finances + del.nat.econ + 
                          del.bush.terror + del.afghan + del.inter.rep + del.party.ID7,
                        data = d.anes.p)

mod.psi.therm.pid <- lm(del.psi ~ del.bush.job + del.finances + del.nat.econ + 
                           del.bush.terror + del.afghan + del.inter.rep +
                           del.therm.diff + del.party.ID7,
                         data = d.anes.p) 

mod.pid.therms <- lm(del.party.ID7 ~ del.bush.job + del.finances + del.nat.econ + 
                     del.bush.terror + del.afghan + del.inter.rep + 
                     del.therm.reps + del.therm.dems, data = d.anes.p)

mod.pid.therms.psi <- lm(del.party.ID7 ~ del.bush.job + del.finances + del.nat.econ + 
                       del.bush.terror + del.afghan + del.inter.rep + 
                       del.therm.reps + del.therm.dems + del.psi, data = d.anes.p)

mod.psi.therms <- lm(del.psi ~ del.bush.job + del.finances + del.nat.econ + 
                       del.bush.terror + del.afghan + del.inter.rep + 
                       del.therm.reps + del.therm.dems, data = d.anes.p)

mod.psi.therms.pid <- lm(del.psi ~ del.bush.job + del.finances + del.nat.econ + 
                           del.bush.terror + del.afghan + del.inter.rep + 
                           del.therm.reps + del.therm.dems + del.party.ID7, data = d.anes.p)

## LaTeX code ------------------------------------------------------------------

texreg(l=list(mod.pid.basic, mod.pid.basic.psi, mod.psi.basic, mod.psi.basic.pid), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Change in PID/PSI as a Function of Change in Evaluations",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('del.bush.job' = "Bush Job Approval", 'del.finances' = "Personal Finances",
                              'del.nat.econ' = "National Economy", 'del.bush.terror' = "Bush Terrorism Evaluation",
                              'del.afghan' = "Afghanistan Evaluation", 'del.inter.rep' = "International Reputation",
                              'del.therm.dems' = "Evaluation of Democrats", 'del.therm.reps' = "Evaluation of Republicans",
                              'del.psi' = "Party Social Identity", 'del.repagree' = "Agree w/ Republicans",
                              'del.party.ID7' = "Party identification",
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("PID", "PID","PSI", "PSI"))

texreg(l=list(mod.pid.therm, mod.pid.therm.psi, mod.psi.therm, mod.psi.therm.pid), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Change in PID/PSI as a Function of Change in Evaluations",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('del.bush.job' = "Bush Job Approval", 'del.finances' = "Personal Finances",
                              'del.nat.econ' = "National Economy", 'del.bush.terror' = "Bush Terrorism Evaluation",
                              'del.afghan' = "Afghanistan Evaluation", 'del.inter.rep' = "International Reputation",
                              'del.therm.diff' = "Change in Pro-Republican Differential",
                              'del.therm.dems' = "Evaluation of Democrats", 'del.therm.reps' = "Evaluation of Republicans",
                              'del.psi' = "Party Social Identity", 'del.repagree' = "Agree w/ Republicans",
                              'del.party.ID7' = "Party identification",
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("PID", "PID","PSI", "PSI"))

texreg(l=list(mod.pid.therms, mod.pid.therms.psi, mod.psi.therms.pid), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Change in PID/PSI as a Function of Change in Evaluations",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('del.bush.job' = "Bush Job Approval", 'del.finances' = "Personal Finances",
                              'del.nat.econ' = "National Economy", 'del.bush.terror' = "Bush Terrorism Evaluation",
                              'del.afghan' = "Afghanistan Evaluation", 'del.inter.rep' = "International Reputation",
                              'del.therm.diff' = "Change in Pro-Republican Differential",
                              'del.therm.dems' = "Evaluation of Democrats", 'del.therm.reps' = "Evaluation of Republicans",
                              'del.psi' = "Party Social Identity", 'del.repagree' = "Agree w/ Republicans",
                              'del.party.ID7' = "Party identification",
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("PID", "PID","PSI"))

## Extra models ----------------------------------------------------------------

texreg(l=list(mod.pid.basic, mod.psi.basic, mod.pid.basic, mod.psi.basic), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Change in PID/PSI as a Function of Change in Evaluations",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('del.bush.job' = "Bush Job Approval", 'del.finances' = "Personal Finances",
                              'del.nat.econ' = "National Economy", 'del.bush.terror' = "Bush Terrorism Evaluation",
                              'del.afghan' = "Afghanistan Evaluation", 'del.inter.rep' = "International Reputation",
                              'del.therm.dems' = "Evaluation of Democrats", 'del.therm.reps' = "Evaluation of Republicans",
                              'del.psi' = "Party Social Identity", 'del.repagree' = "Agree w/ Republicans",
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("PID", "PSI","PID", "PSI"))

texreg(l=list(mod.pid.basic, mod.pid.basic.psi, mod.pid.basic, mod.pid.basic.psi), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Change in PID as a Function of Change in Evaluations",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('del.bush.job' = "Bush Job Approval", 'del.finances' = "Personal Finances",
                              'del.nat.econ' = "National Economy", 'del.bush.terror' = "Bush Terrorism Evaluation",
                              'del.afghan' = "Afghanistan Evaluation", 'del.inter.rep' = "International Reputation",
                              'del.therm.dems' = "Evaluation of Democrats", 'del.therm.reps' = "Evaluation of Republicans",
                              'del.psi' = "Party Social Identity", 'del.repagree' = "Agree w/ Republicans",
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Basic", "B+PSI","basic", "T+PSI"))

texreg(l=list(mod.psi.basic, mod.psi.basic.pid, mod.psi.basic, mod.psi.basic.pid), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Change in PSI as a Function of Change in Evaluations",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('del.bush.job' = "Bush Job Approval", 'del.finances' = "Personal Finances",
                              'del.nat.econ' = "National Economy", 'del.bush.terror' = "Bush Terrorism Evaluation",
                              'del.afghan' = "Afghanistan Evaluation", 'del.inter.rep' = "International Reputation",
                              'del.therm.dems' = "Evaluation of Democrats", 'del.therm.reps' = "Evaluation of Republicans",
                              'del.psi' = "Party Social Identity", 'del.repagree' = "Agree w/ Republicans",
                              'del.party.ID7' = "Party identification",
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Basic", "B+PID","basic", "T+PID"))




# ## (Deprecated) Analysis -------------------------------------------------------------------
# 
# pid.rep.ts <- NULL
# pid.dem.ts <- NULL
# pid.ind.ts <- NULL
# psi.rep.ts <- NULL
# psi.dem.ts <- NULL
# psi.ind.ts <- NULL
# years <- unique(d.anes.ts$year)
# for (i in 1:length(years)){
#   year <- years[i]
#   subset <- subset(d.anes.ts, 
#                    d.anes.ts$year==years[i])
#   pid.dem.ts[i] <- sum(subset$party.ID7 %in% c(1,2))/nrow(subset)
#   pid.ind.ts[i] <- sum(subset$party.ID7 %in% c(3,4,5))/nrow(subset)
#   pid.rep.ts[i] <- sum(subset$party.ID7 %in% c(6,7))/nrow(subset)
#   psi.dem.ts[i] <- mean(subset$psi[subset$party.ID7 %in% c(2)])
#   psi.ind.ts[i] <- mean(subset$psi[subset$party.ID7 %in% c(4)])
#   psi.rep.ts[i] <- mean(subset$psi[subset$party.ID7 %in% c(6)])
# }
# plot(x = 1:length(pid.dem.ts), y = pid.dem.ts, type = "l", col = "blue", ylim = c(0,1))
# lines(x=1:length(pid.rep.ts), y = pid.rep.ts, type = "l", col = "red")
# lines(x=1:length(pid.ind.ts), y = pid.ind.ts, type = "l", col = "green")
# 
# plot(x=1:length(psi.dem.ts), y=psi.dem.ts, type = "l", col="blue", lty=2, ylim = c(5, 9))
# lines(x=1:length(psi.rep.ts), y=psi.rep.ts, type = "l", col="red", lty=2)
# 
# 
# ts.data <- data.frame(pid.dem.ts, pid.ind.ts, pid.rep.ts)