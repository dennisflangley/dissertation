## Title -----------------------------------------------------------------------
##
## name:    04analysis2.R
## author:  Dennis F Langley
## date:    2018-06-04
## what:    Replicate Chapter 1 analysis with ML data 
##
##       
####

## Preamble --------------------------------------------------------------------
rm(list = ls())
dev.off()
options(scipen = 999)
setwd("~/Documents/Dissertation/Data")
load("~/Documents/Dissertation/Analysis/03tuneddata.RData")

library(xtable)
library(texreg)
library(lmtest)
library(ggplot2)
library(tidyverse)
library(dplyr)

cat("\014")

## Data Visualization --------------------------------------------------------

png(file = "~/Documents/Dissertation/Dissertation/images/psipidstrengthknn.png")
p <- ggplot(d.anes, aes(x = as.factor(party.strength), y = psi.knn, fill = as.factor(party.strength))) +
  geom_violin() + 
  geom_boxplot(width = 0.1, fill = "white") + 
  labs(x = "Party ID Strength", y = "Party Social Identity", fill = "Party ID \nStrength") +
  scale_x_discrete(limits = c("1", "2", "3")) 
p + theme_classic() + scale_fill_brewer(palette="Blues") + guides(fill = FALSE)
dev.off()

png(file = "~/Documents/Dissertation/Dissertation/images/psipidstrengthbst.png")
p <- ggplot(d.anes, aes(x = as.factor(party.strength), y = psi.bst.boot, fill = as.factor(party.strength))) +
  geom_violin() + 
  geom_boxplot(width = 0.1, fill = "white") + 
  labs(x = "Party ID Strength", y = "Party Social Identity", fill = "Party ID \nStrength") +
  scale_x_discrete(limits = c("1", "2", "3"))
p + theme_classic() + scale_fill_brewer(palette="Blues") + guides(fill = FALSE)
dev.off()


## Empirical Models -------------------------------------------------------------
mod.anes.es1 <- glm(anes.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.es2 <- glm(anes.pri.es ~ psi.bst + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.es3 <- glm(anes.pri.es ~ psi.bst.boot + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.es4 <- glm(anes.pri.es ~ psi.cart + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.es5 <- glm(anes.pri.es ~ psi.cart.boot + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))


mod.anes.ae1 <- glm(anes.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.ae2 <- glm(anes.pri.ae ~ psi.bst + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.ae3 <- glm(anes.pri.ae ~ psi.bst.boot + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.ae4 <- glm(anes.pri.ae ~ psi.cart + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))
mod.anes.ae5 <- glm(anes.pri.ae ~ psi.cart.boot + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.anes[d.anes$anes.pri.part==1,], family = binomial(link = "logit"))


lr.es.bst <- lrtest(mod.anes.es1, mod.anes.es2)
lr.es.bst.boot <- lrtest(mod.anes.es1, mod.anes.es3)
lr.es.cart <- lrtest(mod.anes.es1, mod.anes.es4)
lr.es.cart.boot <- lrtest(mod.anes.es1, mod.anes.es5)

lr.ae.bst <- lrtest(mod.anes.ae1, mod.anes.ae2)
lr.ae.bst.boot <- lrtest(mod.anes.ae1, mod.anes.ae3)
lr.ae.cart <- lrtest(mod.anes.ae1, mod.anes.ae4)
lr.ae.cart.boot <- lrtest(mod.anes.ae1, mod.anes.ae5)

lr.es.bst;lr.es.bst.boot;lr.es.cart;lr.es.cart.boot
lr.ae.bst;lr.ae.bst.boot;lr.ae.cart;lr.ae.cart.boot

cors <- d.anes %>% select(female, educ_cat, minority, married, church_freq, 
                          interest, income, partyagree, party.strength,
                          ideo.strength, psi.bst.boot)
cor(cors)

## LaTeX Tables ----------------------------------------------------------------

texreg(l=list(mod.anes.es1, mod.anes.es2, mod.anes.ae1, mod.anes.ae2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections (bst Predictions)",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('psi.bst' = "Partisan Social Identity", 'psi.bst.boot' = "Partisan Social Identity",
                              'psi.cart' = "Partisan Social Identity", 'psi.cart.boot' = "Partisan Social Identity",
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment","Anti-Establishment", "Anti-Establishment"))


texreg(l=list(mod.anes.es1, mod.anes.es3, mod.anes.ae1, mod.anes.ae3), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections (bst boot Predictions)",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('psi.bst' = "Partisan Social Identity", 'psi.bst.boot' = "Partisan Social Identity",
                              'psi.cart' = "Partisan Social Identity", 'psi.cart.boot' = "Partisan Social Identity",
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment","Anti-Establishment", "Anti-Establishment"))


texreg(l=list(mod.anes.es1, mod.anes.es4, mod.anes.ae1, mod.anes.ae4), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections (cart Predictions)",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('psi.bst' = "Partisan Social Identity", 'psi.bst.boot' = "Partisan Social Identity",
                              'psi.cart' = "Partisan Social Identity", 'psi.cart.boot' = "Partisan Social Identity",
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment","Anti-Establishment", "Anti-Establishment"))


texreg(l=list(mod.anes.es1, mod.anes.es5, mod.anes.ae1, mod.anes.ae5), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections (cart boot Predictions)",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE,
       custom.coef.map = list('psi.bst' = "Partisan Social Identity", 'psi.bst.boot' = "Partisan Social Identity",
                              'psi.cart' = "Partisan Social Identity", 'psi.cart.boot' = "Partisan Social Identity",
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment","Anti-Establishment", "Anti-Establishment"))


