## Title -----------------------------------------------------------------------
##
## name:    02analysis1.R
## author:  Dennis F Langley
## date:    2018-06-04
## what:    Analysis for Chapter One
##       
####

## Preamble --------------------------------------------------------------------
rm(list = ls())
dev.off()
options(scipen = 999)
setwd("~/Documents/Dissertation/Data")

load("~/Documents/Dissertation/Analysis/01imputeddata.RData")

library(gdata)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(lmtest)
library(texreg)

keep(d.cces, d.cces.old, sure=TRUE)
cat("\014")

## Data Visualization ----------------------------------------------------------
png(file = "~/Documents/Dissertation/Dissertation/images/psipidstrength.png")
p <- ggplot(d.cces, aes(x = as.factor(party.strength), y = psi, fill = as.factor(party.strength))) +
              geom_violin() + 
              geom_boxplot(width = 0.1, fill = "white") + 
              labs(x = "Party ID Strength", y = "Party Social Identity", fill = "Party ID \nStrength") +
              scale_x_discrete(limits = c("1", "2", "3"))

p + theme_classic() + scale_fill_brewer(palette="Blues") + guides(fill = FALSE)
dev.off()

## Empirical Models ------------------------------------------------------------

t.test(d.cces$psi[d.cces$party.ID3=="Democrat"], d.cces$psi[d.cces$party.ID3=="Republican"])
t.test(d.cces$psi[d.cces$ideo.ID7%in%c(1,2,3)], d.cces$psi[d.cces$ideo.ID7%in%c(5,6,7)])

d.cces.old <- d.cces.old %>% dplyr::select(cces.pri.es, cces.pri.ae, female, minority, married, party.strength, ideo.strength,
                            educ_cat, partyagree, church_freq, interest, income, psi, party.ID3, cces.pri.part.self)
d.cces.old <- d.cces.old[complete.cases(d.cces.old),]

# Main tables
{
mod.cces.es1 <- glm(cces.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.cces.old[d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
mod.cces.es2 <- glm(cces.pri.es ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.cces.old[d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))

mod.cces.ae1 <- glm(cces.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.cces.old[d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
mod.cces.ae2 <- glm(cces.pri.ae ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                    data=d.cces.old[d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
}

lr.es1 <- lrtest(mod.cces.es1, mod.cces.es2)
lr.ae1 <- lrtest(mod.cces.ae1, mod.cces.ae2)


# Republican and Democratic subsamples
{
mod.cces.dem.es1 <- glm(cces.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Democrat"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
mod.cces.dem.es2 <- glm(cces.pri.es ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Democrat"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))

mod.cces.dem.ae1 <- glm(cces.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Democrat"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
mod.cces.dem.ae2 <- glm(cces.pri.ae ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Democrat"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))

mod.cces.rep.es1 <- glm(cces.pri.es ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Republican"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
mod.cces.rep.es2 <- glm(cces.pri.es ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Republican"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))

mod.cces.rep.ae1 <- glm(cces.pri.ae ~ female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Republican"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))
mod.cces.rep.ae2 <- glm(cces.pri.ae ~ psi + female + minority + married + party.strength + ideo.strength + partyagree + church_freq + interest + income + educ_cat,
                        data=d.cces.old[d.cces.old$party.ID3=="Republican"&d.cces.old$cces.pri.part.self==1,], family = binomial(link = "logit"))

lr.es.dem <- lrtest(mod.cces.dem.es1, mod.cces.dem.es2)
lr.ae.dem <- lrtest(mod.cces.dem.ae1, mod.cces.dem.ae2)

lr.es.rep <- lrtest(mod.cces.rep.es1, mod.cces.rep.es2)
lr.ae.rep <- lrtest(mod.cces.rep.ae1, mod.cces.rep.ae2)

}

# Chow test stuff
{
# establishment models
{
x1 <- d.cces %>% dplyr::select(c(psi, female, minority, married, 
                                 party.strength, ideo.strength, partyagree, 
                                 church_freq, interest, income, educ_cat))
y1 <- d.cces %>% dplyr::select(cces.pri.es)
}

# Clinton 
{
x2 <- d.cces %>% 
  filter(cces.pri.who!="Hillary Clinton") %>%
  dplyr::select(c(psi, female, minority, married, 
                  party.strength, ideo.strength, partyagree, 
                  church_freq, interest, income, educ_cat))
y2 <- d.cces %>% 
    filter(cces.pri.who!="Hillary Clinton") %>%
    dplyr::select(cces.pri.es)
chow.test(y1, x1, y2, x2)
}

# Trump
{
x2 <- d.cces %>% 
  filter(cces.pri.who!="Donald Trump") %>%
  dplyr::select(c(psi, female, minority, married, 
                  party.strength, ideo.strength, partyagree, 
                  church_freq, interest, income, educ_cat))
y2 <- d.cces %>% 
  filter(cces.pri.who!="Donald Trump") %>%
  dplyr::select(cces.pri.es)
chow.test(y1, x1, y2, x2)
}

# Cruz
{
  x2 <- d.cces %>% 
    filter(cces.pri.who!="Ted Cruz") %>%
    dplyr::select(c(psi, female, minority, married, 
                    party.strength, ideo.strength, partyagree, 
                    church_freq, interest, income, educ_cat))
  y2 <- d.cces %>% 
    filter(cces.pri.who!="Ted Cruz") %>%
    dplyr::select(cces.pri.es)
  chow.test(y1, x1, y2, x2)
}
}


## LaTeX results tables --------------------------------------------------------
texreg(l=list(mod.cces.es1, mod.cces.es2, mod.cces.ae1, mod.cces.ae2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Party Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment", "Anti-Establishment", "Anti-Establishment"))


texreg(l=list(mod.cces.dem.es1, mod.cces.dem.es2, mod.cces.dem.ae1, mod.cces.dem.ae2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections among Democrats",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Party Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment", "Anti-Establishment", "Anti-Establishment"))


texreg(l=list(mod.cces.rep.es1, mod.cces.rep.es2, mod.cces.rep.ae1, mod.cces.rep.ae2), 
       stars = numeric(0), bold = 0.05, single.row = TRUE, caption.above = TRUE, 
       caption = "Candidate Preference in the 2016 Primary Elections among Republicans",
       include.aic = FALSE, include.lr = TRUE, include.bic = FALSE, include.deviance = FALSE, label = NULL,
       custom.coef.map = list('psi' = "Party Social Identity", 'isi' = "Ideological Social Identity", 
                              'party.strength' = "Party ID Strength", 'ideo.strength' = "Ideology Strength", 'partyagree' = "Party Agreement",
                              'female' = "Female?", 'minority' = "Minority?", 'married' = "Married?", 
                              'church_freq' = "Church Frequency", 
                              'interest' = "Political Interest", 'income' = "Income", 'educ_cat' = 'Education', 
                              '(Intercept)' = "Intercept"),
       custom.note = "Coefficients significant at the p = 0.05 level are bolded.",
       custom.model.names = c("Establishment", "Establishment", "Anti-Establishment", "Anti-Establishment"))

