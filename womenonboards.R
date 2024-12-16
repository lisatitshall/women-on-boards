#load packages
library(tidyverse)

#data notes
#most data from 2018 except public sector rates from 2017 (wouldn't change much in a year)
#boardroom rates are from Deloitte (plus some quota information). Rest from OECD
#boardroom rates are for largest companies
#childcare enrolment rate is an average of two figures for 0-2 and 3-5 yr olds

#load data
women_on_boards <- read.csv("~/WomenOnBoards/Data/WomenOnBoards.csv", 
                            header = TRUE) 

#for now remove the fuzzy values
women_on_boards_raw <- women_on_boards %>% select(!contains("Fuzzy"))

#check datatypes are what we expect
glimpse(women_on_boards_raw)

#change datatypes (need to remove commas from childcare spending)
women_on_boards_raw$ChildcareSpending.<- 
  as.integer(str_replace_all(women_on_boards_raw$ChildcareSpending, ",", ""))

women_on_boards_raw$HardBoardroomQuota <- 
  as.factor(women_on_boards_raw$HardBoardroomQuota)

women_on_boards_raw$SoftBoardroomQuota <- 
  as.factor(women_on_boards_raw$SoftBoardroomQuota)


# Visualizations ----------------------

#plot the distribution of the boardroom rate
#12.5-17.5% and 22.5-27.5% most common
ggplot(women_on_boards_raw, aes(WomenBoardroomRate)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5) +
  stat_function(fun = dnorm, args = 
                  list(mean = mean(women_on_boards_raw$WomenBoardroomRate), 
                       sd = sd(women_on_boards_raw$WomenBoardroomRate)),
                colour = "red") +
  labs(x = "Percentage of women on company boards",
       y = "Density")

#quick plot of all variables (except country)
#boardroom rate is what we're predicting, first impressions
#some relationship: maternity leave weeks (outliers), childcare spending,
#  childcare enrolment, hard boardroom quota
#no relationship: maternity payment rate, public sector rate, 
#  soft boardroom quota??
plot(women_on_boards_raw %>% select(MaternityLeaveWeeks:WomenBoardroomRate))

#plot individual pairings
#plot maternity leave weeks vs boardroom rate
#a few outliers are skewing the data, without them would be positive relationship
ggplot(women_on_boards_raw, aes(x = MaternityLeaveWeeks, y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme (panel.background = element_blank()) +
  labs(x = "Maternity leave (weeks)",
       y = "Percentage of women on company boards")

#maternity payment rate vs boardroom rate
#no clear relationship
ggplot(women_on_boards_raw, aes(x = MaternityPaymentRate, y = WomenBoardroomRate)) +
  geom_point() + 
  theme (panel.background = element_blank()) +
  labs(x = "Maternity payment rate",
       y = "Percentage of women on company boards")

#childcare spending vs boardroom rate
#positive relationship but fewer data points with high spending
ggplot(women_on_boards_raw, aes(x = ChildcareSpending., y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme (panel.background = element_blank()) +
  labs(x = "Childcare spending ($)",
       y = "Percentage of women on company boards")

#childcare enrolment rate vs boardroom rate
ggplot(women_on_boards_raw, aes(x = ChildcareEnrolmentRate, y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme (panel.background = element_blank()) +
  labs(x = "Childcare enrolment rate",
       y = "Percentage of women on company boards")

#public sector rate vs boardroom rate
#positive relationship
ggplot(women_on_boards_raw, aes(x = PublicSectorRate, y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme (panel.background = element_blank()) +
  labs(x = "Public sector rate",
       y = "Percentage of women on company boards")

#plot hard boardroom quota vs boardroom rate 
#as expected clear difference but only 4 countries with hard quota
ggplot(women_on_boards_raw, aes (HardBoardroomQuota, WomenBoardroomRate)) +
  geom_boxplot() +
  theme (panel.background = element_blank()) +
  labs(x = "Hard boardroom quota",
       y = "Percentage of women on company boards")

#plot soft boardroom quota vs boardroom rate
#lots of cross over, no has smaller and larger percentages
ggplot(women_on_boards_raw, aes(WomenBoardroomRate, after_stat(density), colour = SoftBoardroomQuota)) +
  geom_density(linewidth = 1) +
  theme (panel.background = element_blank()) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  labs(x = "Percentage of women on company boards",
       y = "Density") 

#alternative to frequency distribution
ggplot(women_on_boards_raw, 
       aes(SoftBoardroomQuota, 
           WomenBoardroomRate, 
           fill = SoftBoardroomQuota)) +
  geom_violin(col = NA) +
  theme (panel.background = element_blank()) +
  labs(x = "Soft boardroom quota",
       y = "Percentage of women on company boards") 

#what is the number of hard/soft/no quotas
#soft no can mean hard yes...
#4 countries hard quota, 11 soft quota, 7 neither
ggplot(women_on_boards_raw, aes(x = HardBoardroomQuota, fill = SoftBoardroomQuota)) +
  geom_bar()

#add a new variable to say whether there's any quota, change to factor
women_on_boards_raw <- women_on_boards_raw %>%
  mutate(AnyQuota = case_when(HardBoardroomQuota == "Yes" ~ "Hard",
                               SoftBoardroomQuota == "Yes" ~ "Soft",
                               .default = "No quota"
  ))
women_on_boards_raw$AnyQuota <- as.factor(women_on_boards_raw$AnyQuota)

#plot new variable
#clearer relationship
ggplot(women_on_boards_raw, aes(x= reorder(AnyQuota, WomenBoardroomRate, 
                                           FUN =median),y =WomenBoardroomRate)) +
  geom_boxplot() +
  theme (panel.background = element_blank()) +
  labs(x = "Boardroom quota",
       y = "Percentage of women on company boards")

#try density plot on new variable
ggplot(women_on_boards_raw, aes(WomenBoardroomRate, after_stat(density), colour = AnyQuota)) +
  geom_density(linewidth = 1) +
  theme (panel.background = element_blank()) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  labs(x = "Percentage of women on company boards",
       y = "Density") 


#add bubble visuals to see if maternity and childcare variables have any links
#no clear relationship for maternity variables
ggplot(women_on_boards_raw, aes(
  x = MaternityLeaveWeeks, 
  y = WomenBoardroomRate,
  size = MaternityPaymentRate)) +
  geom_point(color = "blue") + 
  theme (panel.background = element_blank()) +
  labs(x = "Maternity leave (weeks)",
       y = "Percentage of women on company boards")

#childcare variables, no clear relationship
ggplot(women_on_boards_raw, aes(
  x = ChildcareSpending., 
  y = WomenBoardroomRate,
  size = ChildcareEnrolmentRate)) +
  geom_point(color = "blue") + 
  theme (panel.background = element_blank()) +
  labs(x = "Childcare Spending",
       y = "Percentage of women on company boards")


#Ideas:
#Are the highest maternity leave data points outliers?
#   How does removing them affect the relationship with boardroom rate?
#Assess correlation between numeric variables to see if the results
#   match the plots
#Use statistical test to assess whether quota groups differ in % on boards
#What method would be used to predict boardroom rate?
#Are there closely related variables which could be removed from the model? 
#Sampling would need to include hard/soft/no quota countries (encoded)
#Scaling could be needed especially for childcare spending
#Later: Are there clusters within data for similar countries?


