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
### Posmik, Daniel C. (2022) "Predicting International Student Enrollment by Institutional Aid: A Random and Fixed Effects Approach," Journal of Student Financial Aid: Vol. 51 : Iss. 3 , Article 4.
### DOI: https://doi.org/10.55504/0884-9153.1802
### Available at: https://ir.library.louisville.edu/jsfa/vol51/iss3/4
###
################################################################################
################################################################################
################################################################################

# Libraries --------------------------------------------------------------------
library(AER)
library(gmodels)
library(haven)
library(jtools)
library(mfx)
library(pastecs)
library(psych)
library(stargazer)
library(summarytools)
library(readxl)
library(ggplot2)
library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)    # For hetoroskedasticity analysis
library(ggridges)
library(viridis)
library(sandwich)
library(gridExtra)
library(emmeans)
library(ggeffects)
library(tidyverse)

# Import Data ------------------------------------------------------------------
#GitHub Link: https://github.com/posmikdc/jsfa2022/blob/main/dta.csv


################################################################################
###  Section 1: EDA
################################################################################

# Visualize Distributions ------------------------------------------------------
# Violin Plots of aid awards, enrollment, and aid recipients by year
g1 <- ggplot(dta, aes(Series,total_aid,fill=Series)) + 
      geom_violin() +                                  
      labs(title = "Violin plot of Total Aid Awards from 2012-2013 - 2019-2020 \n(Inflation Adjusted to 2020 $)",
           x="Academic Year",y="Total Aid")            

g2 <- ggplot(dta, aes(Series,first_enrollment,fill=Series)) + 
      geom_violin() +
      labs(title = "Violin plot of first-time ISE from 2012-2013 - 2019-2020",
           x="Academic Year",y="First-time ISE")

g3 <- ggplot(dta, aes(Series,number_recipients,fill=Series)) + 
      geom_violin() +
      labs(title = "Violin plot of aid recipients from 2012-2013 - 2019-2020",
           x="Academic Year",y="Aid Recipients")
  
EDA.plot <- grid.arrange(g1,g2,g3,ncol = 1, nrow = 3)

# Examining aid over time ------------------------------------------------------
# Examining avg. aid over time
g4 <- ggplot(data=dta, aes(x=Series, y=log(avg_aid), fill= Institution, group = Institution)) +
  geom_smooth(se = FALSE, method = "lm", size = 0.5, ) +
  theme(legend.position = "none") +
  labs(x = "Academic Years", y = "Log of Average Aid [% Changes]") +
  ggtitle("Percentage Changes in Average Aid over time, by institution") 
g4

mod = lm(log(avg_aid) ~ Series, data = dta)

slopes = data.frame(c("Intercept","2013-2014",
                      "2014-2015","2015-2016",
                      "2016-2017","2017-2018", 
                      "2018-2019","2019-2020"),
                    mod$coefficients)

colnames(slopes) <- c("series", "coefficients")

slopes$coefficients <- as.numeric(slopes$coefficients)

slopes2 <- slopes %>%
  filter(!row_number() %in% c(1))

g5 <- ggplot(slopes2, aes(x= series, y= coefficients)) + 
  geom_hline(yintercept= mean(slopes2$coefficients), linetype="dashed", color = "red") +
  geom_point(size=3, shape=21, fill="lightgrey") + # 21 is filled circle
  ylab("Average Percentage Change [%]") +
  xlab("Time Periods") +
  scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                   breaks=c("OJ", "VC"),
                   labels=c("Orange juice", "Ascorbic acid"),
                   l=40) + # Use darker colors, lightness=40
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("The Average Percentage Changes of Average Aid over Time")

g5









################################################################################
###  Section 2: Variable Transformations
################################################################################

# Logarithmic transformations and remove NA, +Inf, -Inf
# First-time enrollment
dta$ln_first_enrollment = log(dta$first_enrollment)   
dta$ln_first_enrollment[which(dta$ln_first_enrollment==-Inf)] = NA 
dta$ln_first_enrollment[which(dta$ln_first_enrollment==Inf)] = NA

# Total aid
dta$ln_total_aid = log(dta$total_aid) 
dta$ln_total_aid[which(dta$total_aid== -Inf)] = NA 
dta$ln_total_aid[which(dta$total_aid==Inf)] = NA

# Aid recipients  
dta$ln_number_recipients = log(dta$number_recipients) 
dta$ln_number_recipients[which(dta$number_recipients==-Inf)] = NA 
dta$ln_number_recipients[which(dta$number_recipients==Inf)] = NA

# Total cost of attendance  
dta$ln_total_cost = log(dta$total_cost) 
dta$ln_total_cost[which(dta$total_cost==-Inf)] = NA 
dta$ln_total_cost[which(dta$total_cost==Inf)] = NA

# Total undergraduate enrollment
dta$ln_udgr_enrollment = log(dta$udgr_enrollment) 
dta$ln_udgr_enrollment[which(dta$udgr_enrollment==-Inf)] = NA 
dta$ln_udgr_enrollment[which(dta$udgr_enrollment==Inf)] = NA
  


#-----------------------------------------------------------------------------------------------------------------------
##### R1: Fixed Effects Regression Model #####
### I hypothesize that a 2-way Fixed Effects model is the best choice for my model
### Therefore, I begin by producing three iterations of a 2-way Fixed Effects model
  
## Regression (1)
  reg1 <- plm(ln_first_enrollment ~ ln_total_aid + aid_concentration + ln_total_cost  , 
              data   = dta,
              method = "within", 
              effect = "twoway", 
              index  = c("Institution", "Series")
  )
  
  stargazer(reg1)
  
## Regression (2)
  reg2 <- plm(ln_first_enrollment ~ ln_total_aid + aid_concentration + ln_total_cost + acceptance_rate  , 
              data   = dta,
              method = "within", 
              effect = "twoway", 
              index  = c("Institution", "Series")
  )

  stargazer(reg2)
  
## Regression (3)
  reg3 <- plm(ln_first_enrollment ~ ln_total_aid + aid_concentration + ln_total_cost + acceptance_rate +ln_udgr_enrollment , 
              data   = dta,
              method = "within", 
              effect = "twoway", 
              index  = c("Institution", "Series")
  )
  
  stargazer(reg3)

## Summary of the three 2-way FE models
  stargazer(reg1, reg2, reg3, type="latex",align=TRUE, #Adjust type for text vs. LaTeX output
            omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
            report="vcsp",
            digit.separator = "",digits=2)  

##### Other Regression Frameworks #####
### As described in the paper, I choose the predictors from Regression (3) as the preferred set
### Now, let us assess the modeling framework by comparing Regression (3) to an OLS, Random Effects, and 1-way FE model. 

## An OLS Model
  ols <- lm(ln_first_enrollment ~ ln_total_aid + aid_concentration 
            + ln_total_cost + acceptance_rate + ln_udgr_enrollment 
            + city + suburban + doctoral + masters + private, 
            data=dta)
  #bptest(ols)
  #coeftest(ols, vcov = vcovHC(ols, "HC1")) 

## A Random Effects Model
  random <- plm(ln_first_enrollment ~ ln_total_aid + aid_concentration 
                + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                data=dta, 
                model="random",
                index  = c("Institution", "Series")
  )
  #bptest(random)
  #coeftest(random, vcov = vcovHC(random, "white2")) 
  
## Summary of the OLS, Random Effects, and Regression (3) model
  stargazer(ols, random, reg3, type="latex",align=TRUE, #Adjust type for text vs. LaTeX output
            omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
            report="vcsp",
            digit.separator = "",digits=2) 

##### Validating the best Regression Framework #####
### Now, I compare model performance using hypothesis testing 

## Step 1: Test whether OLS or FE (F-Test: Test whether Coefficients_OLS = Coefficients_FE)
  pFtest(reg3, ols) # p<0.01 => OLS is unfit

## Step 2: Test whether FE or Random Eff Model (Hausman Test: Test whether Coefficients_RE = Coefficients_FE)
  phtest(reg3, random) # p<0.01 => RE is unfit

##Step 3: Test whether 2-way, 1-way_i, or 1-way_t (Breusch-Pagan LM Test: Test whether Coefficients_FE_i/t = Coefficients_FE_it)
  plmtest(reg3, c("twoways"), type=("bp")) # p<0.01 => FE_i/t is unfit

### Conclusion: The optimal set of predictors is Regression (3) and the optimal modeling framework is a 2-way Fixed Effects model. 

##### Testing for Heteroskedasticity #####

## Breusch-Pagan Test
  bptest(reg3, data = dta) # p<0.01 => Heteroskedasticity exists and I proceed with Robust ("White") standard errors

## Adjusting Standard Errors to robust Standard Errors
coeftest(reg1, vcov = vcovHC(reg1, "white2")) 
coeftest(reg2, vcov = vcovHC(reg2, "white2"))  
coeftest(reg3, vcov = vcovHC(reg3, "white2")) 

## Updated Summary (Robust Standard Errors)
stargazer(ols, random, reg3, type="text",align=TRUE,
          omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
          report="vcsp",
          digit.separator = "",digits=2)

#-----------------------------------------------------------------------------------------------------------------------
##### R2: Examining Interaction Terms #####
### Now, I will analyze R2 by introducing interaction terms into the regression framework
### It is important to note that we can no longer use a fixed effects regression framework due to intorducing time invariant dummy variables
### Therefore, these models are run in a random effects framework

##### Location Interactions #####
### Location interactions: City
  city_reg <- plm(ln_first_enrollment ~ city*ln_total_aid + aid_concentration 
                  + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                data=dta, 
                model="random",
                index  = c("Institution", "Series")
  )
  bptest(city_reg)
  coeftest(city_reg, vcov = vcovHC(city_reg, "white2"))
  
### Location interactions: Suburban
  suburban_reg <- plm(ln_first_enrollment ~ suburban*ln_total_aid + aid_concentration 
              + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
              data=dta, 
              model="random",
              index  = c("Institution", "Series")
  )
  bptest(suburban_reg)
  coeftest(suburban_reg, vcov = vcovHC(suburban_reg, "white2"))
  
### Location interactions: Rural
  town_rural_reg <- plm(ln_first_enrollment ~ town_rural*ln_total_aid + aid_concentration 
                  + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                  data=dta, 
                  model="random",
                  index  = c("Institution", "Series")
  )
  summary(town_rural_reg)
  bptest(town_rural_reg)
  coeftest(town_rural_reg, vcov = vcovHC(town_rural_reg, "white2"))
  
### Location interactions: Summary
  stargazer(city_reg, suburban_reg, town_rural_reg, type="latex",align=TRUE, #Adjust type for text vs. LaTeX output
            omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
            report="vcsp",
            digit.separator = "",digits=2)

##### Research Activity Interactions #####
### Research Activity Interactions: Doctoral (High Research Activity)
  doctoral_reg <- plm(ln_first_enrollment ~ doctoral*ln_total_aid + aid_concentration 
                  + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                  data=dta, 
                  model="random",
                  index  = c("Institution", "Series")
  )
  bptest(doctoral_reg)
  coeftest(doctoral_reg, vcov = vcovHC(doctoral_reg, "white2"))
  
### Research Activity Interactions: Masters (Medium Research Activity)
  masters_reg <- plm(ln_first_enrollment ~ masters*ln_total_aid + aid_concentration 
                      + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                      data=dta, 
                      model="random",
                      index  = c("Institution", "Series")
  )
  bptest(masters_reg)
  coeftest(masters_reg, vcov = vcovHC(masters_reg, "white2"))
  
### Research Activity Interactions: Bachelor (Low Research Activity)
  bachelor_reg <- plm(ln_first_enrollment ~ bachelor*ln_total_aid + aid_concentration 
                        + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                        data=dta, 
                        model="random",
                        index  = c("Institution", "Series")
  )
  bptest(bachelor_reg)
  coeftest(bachelor_reg, vcov = vcovHC(bachelor_reg, "white2"))
  
### Research Activity Interactions: Summary
  stargazer(doctoral_reg, masters_reg, bachelor_reg, type="latex",align=TRUE,
            omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
            report="vcsp",
            digit.separator = "",digits=2)

##### Sector Interactions #####
### Sector Interactions: Private
  private_reg <- plm(ln_first_enrollment ~ private*ln_total_aid + aid_concentration 
                      + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                      data=dta, 
                      model="random",
                      index  = c("Institution", "Series")
  )
  bptest(private_reg)
  coeftest(private_reg, vcov = vcovHC(private_reg, "white2"))
  
### Sector Interactions: Public
  public_reg <- plm(ln_first_enrollment ~ public*ln_total_aid + aid_concentration 
                     + ln_total_cost + acceptance_rate + ln_udgr_enrollment, 
                     data=dta, 
                     model="random",
                     index  = c("Institution", "Series")
  )
  bptest(public_reg)
  coeftest(public_reg, vcov = vcovHC(public_reg, "white2"))
  
### Sector Interactions: Summary
  stargazer(private_reg, public_reg, type="latex",align=TRUE,
            omit.stat=c("LL","adj.rsq","aic","bic"), no.space=FALSE,
            report="vcsp",
            digit.separator = "",digits=2)

##### Plotting interactions #####
### Plotting all average marginal effects by Category
  
  cat <- c("Location","Location","Location","Research","Research","Research","Sector")
  names <- c("City","Suburban","Town/Rural","Doctoral","Master","Bachelor","Private")
  values <- c("-0.017","-0.002","0.045","-0.034","0.009","0.022","0.038")
  values <- as.numeric(values)
  interactions <- data.frame(field, names, values)

  g6 <- ggplot(interactions, aes(x=names, y=values, 
                           colour = cat, group = cat, shape = cat)) + 
    geom_line(linetype = "dashed") +
    geom_point(size=3, shape=21, fill="white") + # 21 is filled circle
    scale_y_continuous(labels = scales::percent) +
    ylab("Average Marginal Effects") +
    xlab("") +
    scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                     breaks=c("OJ", "VC"),
                     labels=c("Orange juice", "Ascorbic acid"),
                     l=40) + # Use darker colors, lightness=40
    ggtitle("The Average Marginal Effects of Institutional Characteristics on Aid Effectiveness") +
    theme_bw() +
    facet_wrap(~ field, scales = "free")
  g6

  