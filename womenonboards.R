#load packages
library(tidyverse)
library(tidymodels)

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

#is there any null data, no
women_on_boards_raw %>% 
  summarize(across(everything(), ~sum(is.na(.x))))

#function for repeated visualizations
plot_continuous <- function(col){
  plt <- women_on_boards_raw %>% 
    ggplot(aes(x={{col}}, y=WomenBoardroomRate)) +
    geom_point() +
    geom_smooth(method = "lm", se = F, color = "red") +
    theme (panel.background = element_blank())
  
  plt
}

#function for plotting new AnyQuota category variable - no/soft/hard quota
plot_categorical <- function(col){
  plt <- women_on_boards_raw %>% 
    ggplot(aes(x= AnyQuota, y={{col}})) +
    geom_boxplot() +
    theme (panel.background = element_blank())
  
  plt
}



# Visualizations ----------------------

#plot the distribution of the boardroom rate
#12.5-17.5% and 22.5-27.5% most common
#looks approx normal
ggplot(women_on_boards_raw, aes(WomenBoardroomRate)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5) +
  stat_function(fun = dnorm, args = 
                  list(mean = mean(women_on_boards_raw$WomenBoardroomRate), 
                       sd = sd(women_on_boards_raw$WomenBoardroomRate)),
                colour = "red") +
  labs(x = "Percentage of women on company boards",
       y = "Density")

median(women_on_boards_raw$WomenBoardroomRate)
mean(women_on_boards_raw$WomenBoardroomRate)

#quick plot of all variables (except country)
#boardroom rate is what we're predicting, first impressions
#some relationship: maternity leave weeks (outliers), childcare spending,
#  childcare enrolment, hard boardroom quota
#no relationship: maternity payment rate, public sector rate, 
#  soft boardroom quota??
plot(women_on_boards_raw %>% select(MaternityLeaveWeeks:WomenBoardroomRate))

#plot individual pairings using function
#plot maternity leave weeks vs boardroom rate
#a few outliers are skewing the data, without them positive relationship
plot_continuous(MaternityLeaveWeeks)

#maternity payment rate vs boardroom rate
#no clear relationship
plot_continuous(MaternityPaymentRate)

#childcare spending vs boardroom rate
#positive relationship but fewer data points with high spending
plot_continuous(ChildcareSpending.)

#childcare enrolment rate vs boardroom rate
#positive relationship
plot_continuous(ChildcareEnrolmentRate)

#public sector rate vs boardroom rate
#positive relationship
plot_continuous(PublicSectorRate)

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

# Statistics ----------------
#plot maternity leave box plot to look for outliers
#The four points we observed before are marked as outliers (1.5 * IQR)
ggplot(women_on_boards_raw, aes(MaternityLeaveWeeks)) +
  geom_boxplot()

#are the outliers more than 3 IQR away? Yes
outlier_min <- quantile(women_on_boards_raw$MaternityLeaveWeeks, probs = 0.75) + 
  3 * (IQR(women_on_boards_raw$MaternityLeaveWeeks))

#alternative dataset which removes those 4 rows
women_on_boards_raw_no_outliers <- women_on_boards_raw %>% 
  filter(MaternityLeaveWeeks < outlier_min)

#calculate correlation between all numeric variables
correlations <- cor(women_on_boards_raw %>% select(MaternityLeaveWeeks:PublicSectorRate, 
                                   WomenBoardroomRate))

#correlation with outcome (boardroom rate)
#weak: maternity leave weeks, maternity payment rate (negative)
#moderate: childcare enrolment, public sector rate (positive)
#strong: childcare spending (positive)

#see what the p-values are
#0.007
cor.test(women_on_boards_raw$ChildcareSpending., 
         women_on_boards_raw$WomenBoardroomRate)
#0.03
cor.test(women_on_boards_raw$ChildcareEnrolmentRate, 
         women_on_boards_raw$WomenBoardroomRate)
#0.02
cor.test(women_on_boards_raw$PublicSectorRate, 
         women_on_boards_raw$WomenBoardroomRate)

#correlation between variables (only moderate/high listed)
#moderate: maternity leave and childcare enrolment (-ive), 
  #maternity leave/payment rate and public sector rate (+ive)
#strong: childcare enrolment and spending (+ive), 
  #childcare spending and public sector rate (+ve)

#see what the p-values are
#0.001
cor.test(women_on_boards_raw$ChildcareSpending., 
         women_on_boards_raw$ChildcareEnrolmentRate)
#0.007
cor.test(women_on_boards_raw$ChildcareSpending., 
         women_on_boards_raw$PublicSectorRate)
#0.03
cor.test(women_on_boards_raw$PublicSectorRate, 
         women_on_boards_raw$MaternityLeaveWeeks)
#0.07, don't reject null, correlation is equal to 0
cor.test(women_on_boards_raw$ChildcareEnrolmentRate, 
         women_on_boards_raw$MaternityLeaveWeeks)
#0.13, don't reject null, correlation is equal to 0
cor.test(women_on_boards_raw$PublicSectorRate, 
         women_on_boards_raw$MaternityPaymentRate)

#how different are the correlations without the extreme maternity values
#obviously the maternity leave ones are different
#maternity leave / boardroom rate now moderately positive
#only two other pairs are significantly different:
  #public sector rate and boardroom rate (already significant)
  #public sector rate and child enrolment rate (now significant wasn't before)
correlations_2 <- cor(women_on_boards_raw_no_outliers %>% 
                        select(MaternityLeaveWeeks:PublicSectorRate, 
                                                   WomenBoardroomRate))

#plot new maternity leave relationship, moderate positive relationship
ggplot(women_on_boards_raw_no_outliers, aes(x = MaternityLeaveWeeks, y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme (panel.background = element_blank()) +
  labs(x = "Maternity leave (weeks)",
       y = "Percentage of women on company boards")

#is correlation between boardroom rate and maternity leave weeks significant
  #with outliers removed? Not at 0.05 level... p 0.08 borderline
cor.test(women_on_boards_raw_no_outliers$WomenBoardroomRate, 
         women_on_boards_raw_no_outliers$MaternityLeaveWeeks)

#can we conclude statistically that boardroom rate differs among quota?
#assumptions for ANOVA:
#independent populations - yes, different quotas
#populations have equal standard deviation - yes, see below
#populations follow normal - no, from earlier plot
no_quota <- women_on_boards_raw %>% filter(AnyQuota == "No quota")
soft_quota <- women_on_boards_raw %>% filter(AnyQuota == "Soft")
hard_quota <- women_on_boards_raw %>% filter(AnyQuota == "Hard")

#another way to assess normality
qqnorm(hard_quota$WomenBoardroomRate, pch = 1, frame = FALSE)
qqline(hard_quota$WomenBoardroomRate, col = "blue", lwd = 2)

#Kruskal Wallis instead of ANOVA. Assumptions:
#Dependent variable is ordinal or continuous - yes
#Independent variable has more than two independent groups - yes
#Observations within and between groups are independent - yes
#shapes of distributions aren't the same so compare mean ranks

#there's a significant difference in values among groups
kruskal.test(WomenBoardroomRate ~ AnyQuota, data = women_on_boards_raw)
kruskal.test(WomenBoardroomRate ~ AnyQuota, 
             data = women_on_boards_raw_no_outliers)

#is there a relationship between AnyQuota and other numeric variables
#yes for childcare/maternity leave, no for public sector rate
plot_categorical(ChildcareSpending.)
plot_categorical(ChildcareEnrolmentRate)
plot_categorical(PublicSectorRate)
women_on_boards_raw_no_outliers %>% 
  ggplot(aes(x= AnyQuota, y=MaternityLeaveWeeks)) +
  geom_boxplot() +
  theme (panel.background = element_blank())

# Model ----------------
#Compare results with and without maternity leave outliers

#With maternity leave outliers use:
  # ChildcareSpending., ChildcareEnrolmentRate, PublicSectorRate, AnyQuota
  # some of these variables are correlated so depends on method we use
#Without maternity leave outliers use:
  # As above but include MaternityLeaveWeeks


#To start try linear regression on ChildcareSpending and AnyQuota
#Boardroom Rate looks approximately normal
#No outliers for chosen variables
#Only one numerical independent variable so leave scaling for now
#Try test/train for now but would prefer cross validation on small set

#split into test train
set.seed(1353)
split <- initial_split(women_on_boards_raw, strata = AnyQuota)
train <- training(split)
test <- testing(split)

#create recipe, no preprocessing for now but get used to structure
recipe <- recipe(
  WomenBoardroomRate ~ ChildcareSpending. + AnyQuota,
  data = train
) 

#set up linear regression model
linear_model <- linear_reg() 

#set up workflow
workflow <- workflow() %>% 
  add_model(linear_model) %>%
  add_recipe(recipe)

#fit model on training set
model_fit <- workflow %>% fit(data = train)

#review model, childcare spending not statistically significant at 5% level
model_fit %>% extract_fit_parsnip() %>% tidy()

#add predictions to training set
train_augment <-augment(model_fit, train)

#calculate r2, 65% of variation explained by model
rsq(train_augment, truth = WomenBoardroomRate, estimate = .pred)

#run model on test set and add predictions to test data
test_augment <- augment(model_fit, test)
test_augment %>% 
  select(Country, ChildcareSpending., AnyQuota, WomenBoardroomRate,
         .pred, .resid)

#plot predictions against actual, same amount above/below but not close to 0
ggplot(test_augment, aes(x = .pred, y=WomenBoardroomRate)) +
  geom_point(aes(color = AnyQuota)) +
  geom_abline(intercept = 0, slope = 1)

#plot distribution of residuals, not too bad
qqnorm(test_augment$.resid, pch = 1, frame = FALSE)
qqline(test_augment$.resid, col = "blue", lwd = 2)

#overall model doesn't explain enough variation in the data

#Ideas:

#What method would be used to predict boardroom rate?
#Are there closely related variables which could be removed from the model? 
#Sampling would need to include hard/soft/no quota countries (encoded)
#Scaling could be needed especially for childcare spending
#Later: Use cross validation because of small dataset and see how it compares
#Later: Are there clusters within data for similar countries?


