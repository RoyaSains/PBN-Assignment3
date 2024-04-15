rm(list = ls())

library(haven)
library(arm)
library(tidyverse)
library(car)
library(plotfunctions)
library(kableExtra)
library(dplyr)
library(stargazer)

#Load voting file
load("cpln505_assignment3_voting_data_abb.rda")

#Create 

dat_clean <- dat.voting %>%
  filter(VCF0004 == 2012 | VCF0004 == 2016) %>%
  rename(candidate = VCF0704a, 
         year = VCF0004, 
         race = VCF0105a, 
         age = VCF0101, 
         gender = VCF0104, 
         income = VCF0114, 
         religion = VCF0128, 
         education = VCF0140a, 
         marital = VCF0147, 
         class = VCF0148a, 
         party = VCF0302, 
         hispanic = VCF0108, 
         south = VCF0113)

# Histograms were created to understand the distribution of variable categories to be able to clean and recategorize the data

hist(dat_clean$race)
hist(dat_clean$age)
hist(dat_clean$gender)
hist(dat_clean$income)
hist(dat_clean$religion)
hist(dat_clean$education)
hist(dat_clean$marital)
hist(dat_clean$class)
hist(dat_clean$party)
hist(dat_clean$hispanic)
hist(dat_clean$south)
hist(dat_clean$age)

# The variables were recategorized to better suit a logistical regression model. Yet to write about each variable 

dat <- dat.voting %>%
  filter(VCF0004 == 2012 | VCF0004 == 2016) %>%
  rename(candidate = VCF0704a, 
         year = VCF0004, 
         race = VCF0105a, 
         age = VCF0101, 
         gender = VCF0104, 
         income = VCF0114, 
         religion = VCF0128, 
         education = VCF0140a, 
         marital = VCF0147, 
         class = VCF0148a, 
         party = VCF0302, 
         hispanic = VCF0108, 
         south = VCF0113) %>%
  filter(age != 0, race != 9, candidate != 0, gender != 0, gender != 3, income != 0, religion != 0, education != 8, education != 9, class != 0, marital != 9, party != 0, class !=9, hispanic != 8, hispanic != 9, south != 0) %>%
  mutate(candidate = as.factor(case_when(
    candidate == "1" ~ "democrat",
    candidate == "2" ~ "republican",
    TRUE ~ as.character(candidate)
  ))) %>%
  mutate(race = as.factor(case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    TRUE ~ "other"
  ))) %>%
  mutate(age = as.factor(case_when(
    age >= 17 & age < 35 ~ "youth",
    age >= 35 & age < 65 ~ "middle age",
    age >= 65 ~ "retirees"
  ))) %>%
  mutate(gender = as.factor(case_when(
    gender == 1 ~ "male",
    gender == 2 ~ "female",
    TRUE ~ "other"
  ))) %>%
  mutate(income = as.factor(case_when(
    income %in% c(1, 2) ~ "low",
    income == 3 ~ "middle",
    income %in% c(4, 5) ~ "high",
    TRUE ~ "other"
  ))) %>%
  mutate(religion = as.factor(case_when(
    religion == 1 ~ "protestant",
    religion == 2 ~ "catholic",
    TRUE ~ "other"
  ))) %>%
  mutate(education = as.factor(case_when(
    education %in% c(1, 2) ~ "no hs",
    education %in% c(3, 4) ~ "hs",
    education %in% c(5, 6, 7) ~ "college",
    TRUE ~ "other"
  ))) %>%
  mutate(marital = as.factor(case_when(
    marital == 1 ~ "married",
    TRUE ~ "unmarried"
  ))) %>%
  mutate(class = as.factor(case_when(
    class %in% c(1, 2, 3) ~ "working",
    class %in% c(4, 5, 6) ~ "middle",
    TRUE ~ "other"
  ))) %>%
  mutate(party = as.factor(case_when(
    party == 1 ~ "republican",
    party == 5 ~ "democrat",
    TRUE ~ "other"
  ))) %>%
  mutate(hispanic = as.factor(case_when(
    hispanic == 1 ~ "hispanic",
    TRUE ~ "non-hispanic"
  ))) %>%
  mutate(south = as.factor(case_when(
    south == 1 ~ "south",
    TRUE ~ "non-south"
  )))

# Checking the new categories 
  
table(dat$candidate)
table(dat$race)
table(dat$age)
table(dat$gender)
table(dat$income)
table(dat$religion)
table(dat$education)
table(dat$marital)
table(dat$class)
table(dat$party)
table(dat$hispanic)
table(dat$south)  

#Separating 2012 and 2016 data
  
dat_2012 <- dat %>%
  filter(year == 2012) %>%
  select(-year)

dat_2016 <- dat %>%
  filter(year == 2016) %>%
  select(-year)

# Summarising each variable for 2012 and 2016

summary_cats_2012 <- dat_2012 %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  count(Category, Value) %>%
  group_by(Category) %>%
  mutate(Percentage = n / sum(n) * 100)

kable(summary_cats_2012)

summary_cats_2016 <- dat_2016 %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  count(Category, Value) %>%
  group_by(Category) %>%
  mutate(Percentage = n / sum(n) * 100)

kable(summary_cats_2016)

# Exploring distribution of categories for 2012 and 2016

par(mfrow = c(1, 2))

plot(dat_2012$candidate) 
plot(dat_2016$candidate)


plot(dat_2012$candidate, dat_2012$race, main = "Race Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$race, main = "Race Distribution in 2016", xlab = "", ylab = "")


plot(dat_2012$race, dat_2012$candidate, main = "Race Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$race, dat_2016$candidate, main = "Race Distribution in 2016", xlab = "", ylab = "")


plot(dat_2012$candidate, dat_2012$age, main = "Age Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$age, main = "Age Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$age, dat_2012$candidate, main = "Age Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$age, dat_2016$candidate, main = "Age Distribution in 2016", xlab = "", ylab = "")


plot(dat_2012$candidate, dat_2012$gender, main = "Age Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$gender, main = "Age Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$gender, dat_2012$candidate, main = "Age Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$gender, dat_2016$candidate, main = "Age Distribution in 2016", xlab = "", ylab = "")


plot(dat_2012$candidate, dat_2012$income, main = "Income Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$income, main = "Income Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$income, dat_2012$candidate, main = "Income Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$income, dat_2016$candidate, main = "Income Distribution in 2016", xlab = "", ylab = "")


plot(dat_2012$candidate, dat_2012$religion, main = "Religion Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$religion, main = "Religion Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$candidate, dat_2012$education, main = "Education Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$education, main = "Education Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$education, dat_2012$candidate, main = "Education Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$education, dat_2016$candidate, main = "Education Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$candidate, dat_2012$marital, main = "Marital Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$marital, main = "Marital Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$candidate, dat_2012$class, main = "Class Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$class, main = "Class Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$class, dat_2012$candidate, main = "Class Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$class, dat_2016$candidate, main = "Class Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$candidate, dat_2012$party, main = "Party Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$party, main = "Party Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$party, dat_2012$candidate, main = "Party Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$party, dat_2016$candidate, main = "Party Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$candidate, dat_2012$hispanic, main = "Hispanic Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$hispanic, main = "Hispanic Distribution in 2016", xlab = "", ylab = "")

plot(dat_2012$candidate, dat_2012$south, main = "South Distribution in 2012", xlab = "", ylab = "")
plot(dat_2016$candidate, dat_2016$south, main = "South Distribution in 2016", xlab = "", ylab = "")

# Setting reference categories for the binomial logistical regression analysis

dat_2012$candidate <- relevel(dat_2012$candidate, ref = "democratic candidate")
dat_2016$candidate <- relevel(dat_2016$candidate, ref = "democratic candidate")

dat_2012$education <- relevel(dat_2012$education, ref = "hs")
dat_2016$education <- relevel(dat_2016$education, ref = "hs")

dat_2012$income <- relevel(dat_2012$income, ref = "middle")
dat_2016$income <- relevel(dat_2016$income, ref = "middle")

# Model building for 2012

### Testing all variables

mod_12_a <- glm(candidate ~ race + age + gender + income + religion + education + marital + class + party + hispanic + south,
                data = dat_2012, 
                na.action = na.exclude, 
                family = binomial("logit"))
summary(mod_12_a)

### Income, Gender and Class are least significant variables in predicting a candidate.

#Additive method Without interaction

mod_12_b <- glm(candidate ~ race + age + religion + education + marital + party + hispanic + south,
                data = dat_2012,
                na.action = na.exclude, 
                family = binomial("logit"))
summary(mod_12_b)

### Income, Gender and Class fell out as variables in the model building process.

100*(exp(mod_12_b$coefficients) - 1)
logLik(mod_12_b)*(-2)

#With interaction

mod_12_c <- glm(candidate ~ age + education + marital + party + hispanic + south + race*religion,
                data = dat_2012,
                na.action = na.exclude, 
                family = binomial("logit"))
summary(mod_12_c)

100*(exp(mod_12_c$coefficients) - 1)
logLik(mod_12_c)*(-2)


# Model building for 2016

### Testing all variables

mod_16_a <- glm(candidate ~ race + age + gender + income + religion + education + marital + class + party + hispanic + south,
                data = dat_2016, 
                na.action = na.exclude, 
                family = binomial("logit"))
summary(mod_16_a)

### Remove gender and income
### Remove marital

mod_16_b <- glm(candidate ~ race + age + religion + education + class + party + hispanic + south,
                data = dat_2016, 
                na.action = na.exclude, 
                family = binomial("logit"))
summary(mod_16_b)

100*(exp(mod_16_b$coefficients) - 1)
logLik(mod_16_b)*(-2)

### Try adding interaction

mod_16_c <- glm(candidate ~ race + age + religion + education + class + party + hispanic + south + race*religion,
                data = dat_2016, 
                na.action = na.exclude, 
                family = binomial("logit"))
summary(mod_16_c)


100*(exp(mod_16_c$coefficients) - 1)
logLik(mod_16_c)*(-2)

### Race*religion is opposite in 2016 and is not significant
### Use mod_2016_b as final for now




#Q3
#HHTS####
setwd("D:/01UPENN/COURSES/CPLN5050_PlanningByNumbers/Assignment3")
hh <- read.csv("1_Household_Public.csv")
per <- read.csv("2_Person_Public.csv")
veh <- read.csv("3_Vehicle_Public.csv")
trip <- read.csv("4_Trip_Public.csv")

#filtering, recategorizing, renaming, and selecting variables#### 
#take trip dataset
#step 1. choose bike, auto, and transit trips that ended at work locations
#step 2. recode modes to bike, car, and transit (please name the modes as such)
#step 3. some people took multiple work trips. for each person, keep only the first work trip
#step 4. select household id, person id, parking costs, travel time (model simulated), and travel distance (model simulated)

trip_cleaned <- trip %>%
  filter(D_LOC_TYPE== 2) %>%
  filter(MODE_AGG == 2 | MODE_AGG == 3 | MODE_AGG == 5) %>%
  filter(TRIP_NUM == 1) %>%
  mutate(mode_cat = as.factor(recode(MODE_AGG,
                                          `2` = "Bike",
                                          `3` = "Car",
                                          `5` = "Transit"))) %>%
  select("HH_ID", "PERSON_ID", "Model_TravTime", "Model_TravDist", "mode_cat")

#take person dataset
#step 1. identify personal variables that might be associated with mode choice (be careful about the 988 value for race)
#step 2. for each variable, remove meaningless values
#step 3. recode values to more sensible categories
#step 4. select only the relevant variables

per_cleaned <- per %>%
  select(HH_ID, PERSON_ID, GEND, AGECAT, EDUCA, WK_MODE, LIC) %>%
  filter(!(GEND == 99 | AGECAT == 98 | AGECAT == 99 | EDUCA == 98 | EDUCA == 99 | WK_MODE == 9 | LIC == 98 | LIC == 99)) %>%
  filter(complete.cases(.))
per_cleaned <- per_cleaned %>%
  mutate(GEND = case_when(
      GEND == 1 ~ 0,
      GEND == 2 ~ 1))
per_cleaned <- per_cleaned %>%
  mutate(AGECAT = recode(AGECAT,
                    "4" = "UNDER 18YRS",
                    "5" = "18 to 44YRS",
                    "6" = "18 to 44YRS",
                    "7" = "18 to 44YRS",
                    "8" = "OVER 44YRS",
                    "9" = "OVER 44YRS",
                    "10" = "OVER 44YRS",
                    "11" = "OVER 44YRS",
                    "12" = "OVER 44YRS"))
per_cleaned <- per_cleaned %>%
  mutate(WK_MODE = as.factor(recode(WK_MODE,
                                          `1` = "Car",
                                          `2` = "Car",
                                          `3` = "Transit",
                                          `4` = "Transit",
                                          `5` = "Other",
                                          `6` = "Other",
                                          `7` = "Other",
                                          `8` = "Other")))
per_cleaned <- per_cleaned %>%
  mutate(EDUCA = as.factor(recode(EDUCA,
                                    `1` = "Did Not Graduate",
                                    `2` = "HSGrad",
                                    `3` = "HSGrad",
                                    `4` = "HSGrad",
                                    `5` = "HSGrad",
                                    `6` = "HSGrad",
                                    `97` = "Other")))

#take household dataset
#step 1. identify household variables that might be associated with mode choice
#step 2. for each variable, remove meaningless values
#step 3. recode values to more sensible categories
#step 4. select only the relevant variables

hh_cleaned <- hh %>%
  select(HH_ID, H_COUNTY, A_TYPE, HH_SIZE, HH_WORK, TOT_VEH, INCOME) %>%
  filter(!(HH_SIZE == 98 | HH_SIZE == 99 | HH_WORK == 98 | HH_WORK == 99 | TOT_VEH == 98 | TOT_VEH == 99 | INCOME == 98 | INCOME == 99)) %>%
  filter(complete.cases(.))
hh_cleaned <- hh_cleaned %>%
  mutate(INCOME = as.factor(recode(INCOME,
                                    `1` = "Low",
                                    `2` = "Low",
                                    `3` = "Middle",
                                    `4` = "Middle",
                                    `5` = "Middle",
                                    `6` = "Middle",
                                    `7` = "High",
                                    `8` = "High",
                                    `9` = "High",
                                    `10` = "High")))

#joining trip dataset to person dataset
#step 1. join trip, person, and household datasets
#step 2. examine variables, remove outliers (let's keep only people who travel < 10 miles, < 120 minutes, and paid <$50 for parking)
#step 3. remove NAs. if this step leaves you with a few observations (say, a few hundred), then inspect variables to see
#if certain variable has lots of NAs and whether it would be reasonable to remove that variable altogether in order to
#preserve sample size

dat <- merge(trip_cleaned, per_cleaned, by = "HH_ID", all.x = FALSE, all.y=FALSE, sort = FALSE) 
dat <- merge(hh_cleaned, dat, by = "HH_ID", all.x = FALSE, all.y=FALSE, sort = FALSE)
dat <- dat %>%
  filter(Model_TravDist < 10,
         Model_TravTime < 120)
any(is.na(dat))

#we will do the following in class####
#calculating average speed for each mode
ave.speed <- dat %>% group_by(mode_cat) %>%
  summarise(ave_speed = mean(Model_TravDist/(Model_TravTime/60)))

#calculating travel time for alternative modes
#need to do it one mode at a time
dat.car <- dat %>% filter(mode_cat == "Car")
dat.transit <- dat %>% filter(mode_cat == "Transit")
dat.bike <- dat %>% filter(mode_cat == "Bike")

dat.car$time.car <- dat.car$Model_TravTime
dat.car$time.transit <- 60*(dat.car$Model_TravDist/ave.speed$ave_speed[3]) + 10
dat.car$time.bike <- 60*(dat.car$Model_TravDist/ave.speed$ave_speed[1])

dat.transit$time.transit <- dat.transit$Model_TravTime + 10
dat.transit$time.car <- 60*(dat.transit$Model_TravDist/ave.speed$ave_speed[2])
dat.transit$time.bike <- 60*(dat.transit$Model_TravDist/ave.speed$ave_speed[1])

dat.bike$time.bike <- dat.bike$Model_TravTime
dat.bike$time.car <- 60*(dat.bike$Model_TravDist/ave.speed$ave_speed[2])
dat.bike$time.transit <- 60*(dat.bike$Model_TravDist/ave.speed$ave_speed[3]) + 10

#an overwhelming number of people drove
#let's take a subset so that the mode split is more balanced
set.seed(seed = 100)
rows <- sample(1:nrow(dat.car), 500)
dat.car <- dat.car[rows,]

dat <- rbind.data.frame(dat.car, dat.transit, dat.bike)

#calculating travel costs for alternative modes
#Dept. of Energy est. $0.6/mile for driving
dat$cost.car <- dat$Model_TravDist * 0.6 #+ dat$parking_cost #leave parking cost out for now

#multiply 0.5 for non-monetary costs, add $2 for ticket
dat$cost.transit <- dat$Model_TravDist * 0.5 + 2

#add $3 for non-monetary cost and wear and tear
#Logan et al. (2023) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10546027/
dat$cost.bike <- dat$Model_TravDist + 3 

#shaping data into correct format
library(mlogit)
dat.logit <- mlogit.data(dat, shape="wide", 
                      choice="mode_cat", 
                      varying=c(22:27)) 
#the 23:28 are column numbers of the alternative specific variables we created
#your column numbers might not be the same as mine

#notice the time and cost variables? we did not create them, 
#but mlogit was able to figure it out based on the naming
#scheme we used when creating the time.mode and cost.mode variables

#estimating multinomial logit model
#formula specification
#(dependent variable ~ alt. specific with generic coef. | individual specific | alt. specific with alt. specific coef.)

names(dat)
#fit a simple one for now
mod.1 <- mlogit (mode_cat ~ cost + income | income_cat, data = dat.logit)
summary(mod.1)

#predicting
mode.prob <- data.frame(fitted(mod.1, outcome = FALSE))
dat <- cbind.data.frame(dat, mode.prob)

dat$pred_mode <- 0

#use actual share in observed as thresholds
table(dat$mode_agg)/length(dat$mode_agg)

for (i in 1:length(dat$hh_id)) {
  if (dat$car[i] > ) {
    dat$pred_mode[i] = "car"
  } else if (dat$transit[i] > ) {
    dat$pred_mode[i] = "transit"
  } else if (dat$bike[i] > ) {
    dat$pred_mode[i] = "bike"
  }
}

#do not use this for now because it predicts bike poorly
for (i in 1:length(dat$hh_id)) {
  if (dat$car[i] > dat$bike[i] & dat$car[i] > dat$transit[i]) {
    dat$pred_mode[i] = "car"
  } else if (dat$transit[i] > dat$car[i] & dat$transit[i] > dat$bike[i]) {
    dat$pred_mode[i] = "transit"
  } else if (dat$bike[i] > dat$car[i] & dat$bike[i] > dat$transit[i]) {
    dat$pred_mode[i] = "bike"
  }
}

#calculate model fit statistics####
#count R squared
#misclassification error
#correct rate for car
#correct rate for transit
#correct rate for bike

#how changing variables affect mode choice####
#a few options to think about
#what if we increase cost of driving by xx%? how would that affect predicted average probabilities?
#what if we reduce travel time on transit by xx%? how would that affect predicted average probabilities?
#what if we increase the income of those who are currently at the bottom of the income brackets by xx%?

#increasing driving cost by 50%
dat.1 <- dat
dat.1$cost.car <- dat.1$cost.car * 1.5
dat.1.logit <- mlogit.data(dat.1, shape="wide", 
                           choice="mode_cat", varying=c(23:28))  
mod.2 <- mlogit (mode_cat ~ cost + time | income_cat, data = dat.1.logit)

#average predicted probability for using each mode before and after intervention
fitted(mod.2, outcome = FALSE)

apply(fitted(mod.2, outcome=FALSE), 2, mean)
apply(fitted(mod.1, outcome=FALSE), 2, mean)

#this is what the function is doing
len <- length(dat.logit$hh_id)/3
mean(fitted(mod.2, outcome = FALSE)[1:]) #plug in the value of len [1:len]
mean(fitted(mod.2, outcome = FALSE)[:]) #plug in the value of len to calculate [(len + 1):len*2]
mean(fitted(mod.2, outcome = FALSE)[:]) #plug in the value of len to calculate [(len*2+1):len*3]

mean(fitted(mod.1, outcome = FALSE)[1:]) #same as above
mean(fitted(mod.1, outcome = FALSE)[:])
mean(fitted(mod.1, outcome = FALSE)[:])
