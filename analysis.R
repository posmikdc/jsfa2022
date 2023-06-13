################################################################################
################################################################################
################################################################################
###
### Title: Replication Code - "Predicting International Student Enrollment by Financial Aid"
###
### Author: Daniel Posmik (posmikdc@mail.uc.edu)
###
### Last Updated: November 15th, 2022
### 
### Recommended Citation: 
### Posmik, Daniel C. (2022) "Predicting International Student Enrollment by Institutional Aid: A Random and Fixed Effects Approach," Journal of Student Financial Aid, 51(3), Article 4.
### DOI: https://doi.org/10.55504/0884-9153.1802
### Available at: https://ir.library.louisville.edu/jsfa/vol51/iss3/4
###
################################################################################
################################################################################
################################################################################

# Libraries --------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(data.table)
library(scales)
library(ggpmisc)
library(xtable)
library(AER)
library(gmodels)
library(stargazer)
library(lmtest) 
library(plm) 

# Import Data ------------------------------------------------------------------
URL <- "https://github.com/posmikdc/jsfa2022/blob/cd115b3c89f9cfe2a7bbd29c4f8cc440bb2f14e0/dta_raw.csv"

################################################################################
###  I. Data Wrangling and Cleaning
################################################################################

# Inputs -----------------------------------------------------------------------
# Define Commonfund Institute's Higher Education Institutes Price Indices
# https://www.commonfund.org/hubfs/Institute/HEPI/Reports/2020-Commonfund-Higher-Education-Price-Index.pdf?hsCtaTracking=046d6d58-8f9f-476c-bf5c-9d6deb5636dc%7Cf0d7f42f-c13f-450b-a852-581331fc3c69

hepi_index <- data.frame(series = c("2012_13","2013_14",       #Define the series column
                                    "2014_15","2015_16",
                                    "2016_17","2017_18",
                                    "2018_19","2019_20"),
                         index = c("293.2", "297.8",           #Define the index column
                                   "306.7","312.9", 
                                   "317.7", "327.4",
                                   "336.1","346.0")
                         )

#Define the 2020 dollar conversion index separately
index_2020 <- as.numeric("352.7")                              #Separately create the 2020 index

# Adjust all financial variables to 2020-dollars -------------------------------
# Merge HEPI Indices into data frame
panel_df <- left_join(dta_raw,                                 #Raw data frame 
                      hepi_index,                              #Left join the HEPI index
                      by = c("series"))                        #By series

# Adjust all financial variables using the HEPI indices
panel_2020d <- panel_df %>%
  mutate(index = as.numeric(index)) %>%                        #Convert index to numeric format
  mutate(avg_aid_2020d = (avg_aid/index)*index_2020) %>%       #Avg. aid to 2020-$
  mutate(total_aid_2020d = (total_aid/index)*index_2020) %>%   #Total aid to 2020-$
  mutate(total_cost_2020d = (total_cost/index)*index_2020) %>% #Total cost to 2020-$
  mutate(udgr_avg_2020d = (udgr_avg/index)*index_2020) %>%     #Avg. udgr. aid to 2020-$
  mutate(udgr_aid_2020d = (udgr_aid/index)*index_2020)         #Total udgr. aid to 2020-$

# Apply logarithmic transformations to financial and enrollment variables ------
# Create all log values
panel_log <- panel_2020d %>%
  mutate(ln_avg_aid = log(avg_aid_2020d)) %>%                  #Log of avg. aid in 2020-$
  mutate(ln_total_aid = log(total_aid_2020d)) %>%              #Log of total aid in 2020-$
  mutate(ln_total_cost = log(total_cost_2020d)) %>%            #Log of total cost in 2020-$
  mutate(ln_udgr_avg = log(udgr_avg_2020d)) %>%                #Log of Avg. udgr. aid in 2020-$
  mutate(ln_udgr_aid = log(udgr_aid_2020d)) %>%                #Log of Total udgr. aid in 2020-$
  mutate(ln_first_enrollment = log(first_enrollment)) %>%      #Log of first-time, full-time enrollment
  mutate(ln_degree_enrollment = log(degree_enrollment)) %>%    #Log of total full-time enrollment
  mutate(ln_udgr_enrollment = log(udgr_enrollment))            #Log of total udgr. enrollment

# Compute additional variables -------------------------------------------------
# Compute aid concentration variable
panel_log %<>%
  mutate(aid_concentration =                                   #Create aid concentration variable
           number_recipients/degree_enrollment)                #Aid recipients over total degree-seeking ISE

# Final cleaning ---------------------------------------------------------------
# Remove all +Inf and -Inf values
dta_clean <- panel_log %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))         #Remove all non-finite values in numeric columns


################################################################################
###  II. EDA and Visualizations
################################################################################

# EDA --------------------------------------------------------------------------
# Violin Plots of total aid awards, by year
aid_violin_plot <- ggplot(dta_clean,                           #Select data
                          aes(series,total_aid,fill=series)) + #Define arguments
                   geom_violin() +                             #Define plot type               
                   labs(title = "Total Aid Awards from 2012/13 - 2019/20 \n(Inflation Adjusted to 2020 $)",
                   x="Academic Year",y="Total Aid [2020-$]") + #Customize labs, axes, and title
                   theme_bw() +                                #Define theme
                   scale_y_continuous(labels = scales::comma)  #Format y-axis with commas

# Violin plot of first-time enrollment, by year
ftenroll_violin_plot <- ggplot(dta_clean,                      #Select data
                        aes(series,first_enrollment,
                              fill=series)) +                  #Define arguments
                        geom_violin() +                        #Define plot type               
                        labs(title = "First-time Enrollment from 2012/13 - 2019/20",
                        x="Academic Year",
                        y="First-time Enrollment") +           #Customize labs, axes, and title
                        theme_bw() +                           #Define theme
                        scale_y_continuous(labels = 
                                             scales::comma)    #Format y-axis with commas

# Violin plot of aid recipients, by year
recipients_violin_plot <- ggplot(dta_clean,                    #Select data
                          aes(series,number_recipients,
                              fill=series)) +                  #Define arguments
  geom_violin() +                                              #Define plot type               
  labs(title = "Aid Recipients from 2012/13 - 2019/20",
       x="Academic Year",y="Aid Recipients") +                 #Customize labs, axes, and title
  theme_bw() +                                                 #Define theme
  scale_y_continuous(labels = scales::comma)                   #Format y-axis with commas



################################################################################
###  III. Modeling Aid and ISE
################################################################################

# Fixed Effects Model ----------------------------------------------------------
# TWFE Regression (1)
twfe_reg1 <- plm(ln_first_enrollment ~                         #Dependent variable
             ln_total_aid + aid_concentration +                #Variables of interest
             ln_total_cost  ,                                  #Controls
             data   = dta_clean,                               #Data frame           
             method = "within",                                #Fixed effects
             effect = "twoway",                                #Two-way fixed effects
             index  = c("institution", "series")               #Define fixed effects
             )
  
stargazer(twfe_reg1)                                           #Outputs LaTeX code
  
# TWFE Regression (2)
twfe_reg2 <- plm(ln_first_enrollment ~                         #Dependent variable
             ln_total_aid + aid_concentration +                #Variables of interest
             ln_total_cost + acceptance_rate,                  #Controls
             data   = dta_clean,                               #Data frame           
             method = "within",                                #Fixed effects
             effect = "twoway",                                #Two-way fixed effects
             index  = c("institution", "series")               #Define fixed effects
)

stargazer(twfe_reg2)                                           #Outputs LaTeX code

# TWFE Regression (3)
twfe_reg3 <- plm(ln_first_enrollment ~                         #Dependent variable
             ln_total_aid + aid_concentration +                #Variables of interest
             ln_total_cost + acceptance_rate +                 #Controls
             ln_udgr_enrollment,                               #Controls
             data   = dta_clean,                               #Data frame           
             method = "within",                                #Fixed effects
             effect = "twoway",                                #Two-way fixed effects
             index  = c("institution", "series")               #Define fixed effects
)

stargazer(twfe_reg3)                                           #Outputs LaTeX code

# Summary
stargazer(twfe_reg1, twfe_reg2, twfe_reg3,                     #Select models of interest
          type="text",align=TRUE,                              #Adjust 'type' for 'text'/'latex'
          omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
          report="vcsp",
          digit.separator = "",digits=2)  

# Other Models -----------------------------------------------------------------
# OLS
ols_reg <- lm(ln_first_enrollment ~                            #Dependent variable
           ln_total_aid + aid_concentration +                  #Variables of interest
           ln_total_cost + acceptance_rate +                   #Controls
           ln_udgr_enrollment +                                #Controls
           city + suburban + doctoral +                        #Controls
           masters + private,                                  #Controls
           data=dta_clean)                                     #Data frame

# Random Effects Model
random_reg <- plm(ln_first_enrollment ~                        #Dependent variable
                  ln_total_aid + aid_concentration +           #Variables of interest
                  ln_total_cost + acceptance_rate +            #Controls
                  ln_udgr_enrollment,                          #Controls
                  data   = dta_clean,                          #Data frame           
                  model="random",                              #Random effects
                  index  = c("institution", "series")          #Define random effects
)

# Summary
stargazer(ols_reg, random_reg, twfe_reg3,                      #Select models of interest
          type="text",align=TRUE,                              #Adjust 'type' for 'text'/'latex'
          omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
          report="vcsp",
          digit.separator = "",digits=2)  

# Heteroskedasticity test and White standard errors ----------------------------
# Breusch-Pagan Tests
bptest(ols_reg)                                                #Breusch-Pagan test
bptest(random_reg)                                             #Breusch-Pagan test
bptest(twfe_reg3)                                              #Breusch-Pagan test

# White standard errors
coeftest(ols_reg, vcov = vcovHC(ols, "HC1"))                   #White standard errors
coeftest(random_reg, vcov = vcovHC(ols, "HC1"))                #White standard errors
coeftest(twfe_reg3, vcov = vcovHC(ols, "HC1"))                 #White standard errors

# Statistical Testing ----------------------------------------------------------
# F-Test: OLS vs. FE
pFtest(reg3, ols)

# Hausman Test: RE vs. FE
phtest(reg3, random)

# Breusch-Pagan Lagrange Multiplier Test: 1-way vs. 2-way FE
plmtest(reg3, c("twoways"), type=("bp"))
  

################################################################################
###  IV. Modeling Marginal Enrollment Boosts
################################################################################

# Location Interactions --------------------------------------------------------
# City interaction
city_reg <- plm(ln_first_enrollment ~                          #Dependent variable
                city*ln_total_aid +                            #Interaction term
                aid_concentration + ln_total_cost +            #Controls
                acceptance_rate + ln_udgr_enrollment,          #Controls
                data   = dta_clean,                            #Data frame           
                model="random",                                #Random effects
                index  = c("institution", "series")            #Define random effects
)

# Suburban interaction
suburban_reg <- plm(ln_first_enrollment ~                      #Dependent variable
                    suburban*ln_total_aid +                    #Interaction term
                    aid_concentration + ln_total_cost +        #Controls
                    acceptance_rate + ln_udgr_enrollment,      #Controls
                    data   = dta_clean,                        #Data frame           
                    model="random",                            #Random effects
                    index  = c("institution", "series")        #Define random effects
)

# Town/Rural interaction
town_rural_reg <- plm(ln_first_enrollment ~                    #Dependent variable
                      town_rural*ln_total_aid +                #Interaction term
                      aid_concentration + ln_total_cost +      #Controls
                      acceptance_rate + ln_udgr_enrollment,    #Controls
                      data   = dta_clean,                      #Data frame           
                      model="random",                          #Random effects
                      index  = c("institution", "series")      #Define random effects
)

# Summary
stargazer(city_reg, suburban_reg, town_rural_reg,              #Select models of interest
          type="text",align=TRUE,                              #Adjust 'type' for 'text'/'latex'
          omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
          report="vcsp",
          digit.separator = "",digits=2)  

# Heteroskedasticity test and White standard errors
# Breusch-Pagan Tests
bptest(city_reg)                                               #Breusch-Pagan test
bptest(suburban_reg)                                           #Breusch-Pagan test
bptest(town_rural_reg)                                         #Breusch-Pagan test

# White standard errors
coeftest(city_reg, vcov = vcovHC(ols, "HC1"))                  #White standard errors
coeftest(suburban_reg, vcov = vcovHC(ols, "HC1"))              #White standard errors
coeftest(town_rural_reg, vcov = vcovHC(ols, "HC1"))            #White standard errors

# Research Activity Interactions -----------------------------------------------
# Doctoral interaction
doctoral_reg <- plm(ln_first_enrollment ~                      #Dependent variable
                    doctoral*ln_total_aid +                    #Interaction term
                    aid_concentration + ln_total_cost +        #Controls
                    acceptance_rate + ln_udgr_enrollment,      #Controls
                    data = dta_clean,                          #Data frame           
                    model="random",                            #Random effects
                    index = c("institution", "series")         #Define random effects
)

# Masters interaction
masters_reg <- plm(ln_first_enrollment ~                       #Dependent variable
                   masters*ln_total_aid +                      #Interaction term
                   aid_concentration + ln_total_cost +         #Controls
                   acceptance_rate + ln_udgr_enrollment,       #Controls
                   data = dta_clean,                           #Data frame           
                   model ="random",                            #Random effects
                   index = c("institution", "series")          #Define random effects
)

# Bachelor interaction
bachelor_reg <- plm(ln_first_enrollment ~                      #Dependent variable
                    bachelor*ln_total_aid +                    #Interaction term
                    aid_concentration + ln_total_cost +        #Controls
                    acceptance_rate + ln_udgr_enrollment,      #Controls
                    data = dta_clean,                          #Data frame           
                    model = "random",                          #Random effects
                    index = c("institution", "series")         #Define random effects
)

# Summary
stargazer(doctoral_reg, masters_reg, bachelor_reg,             #Select models of interest
          type="text",align=TRUE,                              #Adjust 'type' for 'text'/'latex'
          omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
          report="vcsp",
          digit.separator = "",digits=2)  

# Heteroskedasticity test and White standard errors
# Breusch-Pagan Tests
bptest(doctoral_reg)                                           #Breusch-Pagan test
bptest(masters_reg)                                            #Breusch-Pagan test
bptest(bachelor_reg)                                           #Breusch-Pagan test

# White standard errors
coeftest(doctoral_reg, vcov = vcovHC(ols, "HC1"))              #White standard errors
coeftest(masters_reg, vcov = vcovHC(ols, "HC1"))               #White standard errors
coeftest(bachelor_reg, vcov = vcovHC(ols, "HC1"))              #White standard errors

# Sector Interactions -----------------------------------------------
# Private interaction
private_reg <- plm(ln_first_enrollment ~                       #Dependent variable
                   private*ln_total_aid +                      #Interaction term
                   aid_concentration + ln_total_cost +         #Controls
                   acceptance_rate + ln_udgr_enrollment,       #Controls
                   data = dta_clean,                           #Data frame           
                   model="random",                             #Random effects
                   index = c("institution", "series")          #Define random effects
)

# Public interaction
public_reg <- plm(ln_first_enrollment ~                        #Dependent variable
                  public*ln_total_aid +                        #Interaction term
                  aid_concentration + ln_total_cost +          #Controls
                  acceptance_rate + ln_udgr_enrollment,        #Controls
                  data = dta_clean,                            #Data frame           
                  model="random",                              #Random effects
                  index = c("institution", "series")           #Define random effects
)

# Summary
stargazer(private_reg, public_reg,                             #Select models of interest
          type="text",align=TRUE,                              #Adjust 'type' for 'text'/'latex'
          omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
          report="vcsp",
          digit.separator = "",digits=2)  

# Heteroskedasticity test and White standard errors
# Breusch-Pagan Tests
bptest(private_reg)                                            #Breusch-Pagan test
bptest(public_reg)                                             #Breusch-Pagan test


# White standard errors
coeftest(private_reg, vcov = vcovHC(ols, "HC1"))               #White standard errors
coeftest(public_reg, vcov = vcovHC(ols, "HC1"))                #White standard errors
  
