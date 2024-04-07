rm(list = ls())

library(haven)
library(arm)
library(tidyverse)

#Load voting file
load("cpln505_assignment3_voting_data_abb.rda")

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

# Data exploration

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

#Data Cleaning
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
  filter(age != 0, race != 9, candidate != 0, gender != 0, gender != 3, income != 0, religion != 0, education != 8, education != 9, class != 0, marital != 9, party != 0, hispanic != 8, hispanic != 9, south != 0) %>%
  mutate(candidate = as.factor(recode(candidate,
                                      '1' = "democratic candidate",
                                      '2' = "republican candidate"))) %>%
  mutate(race = as.factor(recode(race,
                                 `1` = "white",
                                 `2` = "black",
                                 `3` = "other",
                                 `4` = "other",
                                 `5` = "other",
                                 `6` = "other"))) %>%
  mutate(gender = as.factor(recode(gender,
                                       `1` = "male", 
                                       `2` = "female"))) %>%
  mutate(income = as.factor(recode(income,
                                   '1' = "0 to 33rd percentile",
                                   '2' = "0 to 33rd percentile",
                                   `3` = "34th to 67th percentile",
                                   `4` = "68th to 100th percentile",
                                   `5` = "68th to 100th percentile"))) %>%
  mutate(religion = as.factor(recode(religion,
                                     '1' = "protestant",
                                     '2' = "catholic",
                                     `3` = "other",
                                     `4` = "other"))) %>%
  mutate(education = as.factor(recode(education,
                                 `1` = "high school or less",
                                 `2` = "high school or less",
                                 `3` = "high school or less",
                                 `4` = "some college",
                                 `5` = "some college",
                                 `6` = "grad", 
                                 '7' = "grad"))) %>%
  mutate(marital = as.factor(recode(marital,                               
                                    `1` = "married",
                                    `2` = "unmarried",
                                    `3` = "unmarried",
                                    `4` = "unmarried",
                                    `5` = "unmarried",
                                    `7` = "unmarried"))) %>%
  mutate(class = as.factor(recode(class,
                                  '1' = "working",
                                  `2` = "working",
                                  `3` = "working",
                                  `4` = "middle",
                                  `5` = "middle",
                                  `6` = "middle",
                                  '9' = "other"))) %>%
  mutate(party = as.factor(recode(party,
                                  '1' = "republican",
                                  `2` = "other",
                                  `3` = "other",
                                  `4` = "other",
                                  `5` = "democrat",
                                  `8` = "other",
                                  '9' = "other"))) %>%
  mutate(hispanic = as.factor(recode(hispanic,
                                     '1' = "hispanic",
                                     `2` = "non-hispanic"))) %>%
  mutate(south = as.factor(recode(south,
                                  '1' = "south",
                                  `2` = "non-south")))

#Data exploration
  
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

hist(dat$age)
plot(dat$candidate, dat$age)

#Separating 2012 and 2016 data
  
dat_2012 <- dat %>%
  filter(year == 2012) %>%
  select(-year)

dat_2016 <- dat %>%
  filter(year == 2016) %>%
  select(-year)

# Summaries

dat_2012_cats <- dat_2012 %>%
  select(-age)

summary_cats_2012 <- dat_2012_cats %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  count(Category, Value) %>%
  group_by(Category) %>%
  mutate(Percentage = n / sum(n) * 100)

write.csv(summary_cats_2012, "summary_cats_2012.csv")

summary_cont_2012 <- dat_2012 %>%
  as_tibble() %>%
  summarise(across(c(age), 
                   list(mean = mean, max = max, min = min, sd = sd))) %>% 
  pivot_longer(cols = everything(),
               names_to = c(".value", "variable"),
               names_pattern = "(.*)_(.*)")

write.csv(summary_cont_2012, "summary_cont_2012.csv")

dat_2016_cats <- dat_2016 %>%
  select(-age)

summary_cats_2016 <- dat_2016_cats %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value") %>%
  count(Category, Value) %>%
  group_by(Category) %>%
  mutate(Percentage = n / sum(n) * 100)

write.csv(summary_cats_2016, "summary_cats_2016.csv")

summary_cont_2016 <- dat_2016 %>%
  as_tibble() %>%
  summarise(across(c(age), 
                   list(mean = mean, max = max, min = min, sd = sd))) %>% 
  pivot_longer(cols = everything(),
               names_to = c(".value", "variable"),
               names_pattern = "(.*)_(.*)")

write.csv(summary_cont_2016, "summary_cont_2016.csv")

# Data Exploration

plot(dat_2012$candidate, dat_2012$age)

plot(dat_2016$candidate, dat_2016$age)

#Q2
# Model building

mod_12_a <- glm(candidate ~ race + age + gender + income,
                data = dat, 
                na.action = na.exclude, 
                family = binomial("logit"))

summary(mod_12_a)
100*(exp(mod_12_a$coefficients) - 1)
logLik(mod_12_a)*(-2)


#Q3
#HHTS####
rm(list = ls())
library(tidyverse)

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
  mutate(MODE_AGG_NAME = as.factor(recode(MODE_AGG,
                                          `2` = "Bike",
                                          `3` = "Car",
                                          `5` = "Transit"))) %>%
  select("HH_ID", "PERSON_ID", "Model_TravTime")

#take person dataset
#step 1. identify personal variables that might be associated with mode choice (be careful about the 988 value for race)
#step 2. for each variable, remove meaningless values
#step 3. recode values to more sensible categories
#step 4. select only the relevant variables

per_cleaned <- per %>%
  select("HH_ID", "PERSON_ID", "GEND", "AGECAT", "WK_STAT", "WK_MODE") %>%
  filter(GEND != 99 | AGECAT != 98 | AGECAT != 99 | WK_STAT != 98 | WK_STAT != 99 | WK_MODE != 99)
  
  

#take household dataset
#step 1. identify household variables that might be associated with mode choice
#step 2. for each variable, remove meaningless values
#step 3. recode values to more sensible categories
#step 4. select only the relevant variables

#joining trip dataset to person dataset
#step 1. join trip, person, and household datasets
#step 2. examine variables, remove outliers (let's keep only people who travel < 10 miles, < 120 minutes, and paid <$50 for parking)
#step 3. remove NAs. if this step leaves you with a few observations (say, a few hundred), then inspect variables to see
#if certain variable has lots of NAs and whether it would be reasonable to remove that variable altogether in order to
#preserve sample size

#we will do the following in class####
#calculating average speed for each mode
ave.speed <- dat %>% group_by(mode_cat) %>%
  summarise(ave_speed = mean(travel_dist/(travel_time/60)))

#calculating travel time for alternative modes
#need to do it one mode at a time
dat.car <- dat %>% filter(mode_cat == "car")
dat.transit <- dat %>% filter(mode_cat == "transit")
dat.bike <- dat %>% filter(mode_cat == "bike")

dat.car$time.car <- dat.car$travel_time
dat.car$time.transit <- 60*(dat.car$travel_dist/ave.speed$ave_speed[2]) + 10 #add 10 minutes for waiting and walking to station
dat.car$time.bike <- 60*(dat.car$travel_dist/ave.speed$ave_speed[3])

dat.transit$time.transit <- dat.transit$travel_time + 10
dat.transit$time.car <- 60*(dat.transit$travel_dist/ave.speed$ave_speed[1])
dat.transit$time.bike <- 60*(dat.transit$travel_dist/ave.speed$ave_speed[3])

dat.bike$time.bike <- dat.bike$travel_time
dat.bike$time.car <- 60*(dat.bike$travel_dist/ave.speed$ave_speed[1])
dat.bike$time.transit <- 60*(dat.bike$travel_dist/ave.speed$ave_speed[2]) + 10

#an overwhelming number of people drove
#let's take a subset so that the mode split is more balanced
set.seed(seed = 100)
rows <- sample(1:nrow(dat.car), 500)
dat.car <- dat.car[rows,]

dat <- rbind.data.frame(dat.car, dat.transit, dat.bike)

#calculating travel costs for alternative modes
#Dept. of Energy est. $0.6/mile for driving
dat$cost.car <- dat$travel_dist * 0.6 #+ dat$parking_cost #leave parking cost out for now

#multiply 0.5 for non-monetary costs, add $2 for ticket
dat$cost.transit <- dat$travel_dist * 0.5 + 2

#add $3 for non-monetary cost and wear and tear
#Logan et al. (2023) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10546027/
dat$cost.bike <- dat$travel_dist + 3 

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
