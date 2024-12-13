#load packages
library(tidyverse)

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


#quick plot of all variables (except country)
#boardroom rate is what we're predicting, first impressions
#some relationship: maternity leave weeks (outliers), childcare spending,
#  childcare enrolment, hard boardroom quota
#no relationship: maternity payment rate, public sector rate, 
#  soft boardroom quota??
plot(women_on_boards_raw %>% select(MaternityLeaveWeeks:WomenBoardroomRate))

#plot maternity leave weeks vs boardroom rate
# a few outliers are skewing the data
ggplot(women_on_boards_raw, aes(x = MaternityLeaveWeeks, y = WomenBoardroomRate)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F, col = "red") +
  theme (panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank()) +
  labs(x = "Maternity leave (weeks)",
       y = "Percentage of women on company boards")


#plot hard boardroom quota vs boardroom rate 
#as expected clear difference
ggplot(women_on_boards_raw, aes (HardBoardroomQuota, WomenBoardroomRate)) +
  geom_boxplot() +
  theme (panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank()) +
  labs(x = "Hard boardroom quota",
       y = "Percentage of women on company boards")

#plot soft boardroom quota vs boardroom rate
#lots of cross over, no has smaller and larger percentages
ggplot(women_on_boards_raw, aes(WomenBoardroomRate, after_stat(density), colour = SoftBoardroomQuota)) +
  geom_freqpoly(binwidth = 10,
                closed = "left") +
  theme (panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank()) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  labs(x = "Percentage of women on company boards",
       y = "Density") 


#alternative to frequency distribution
ggplot(women_on_boards_raw, 
       aes(SoftBoardroomQuota, 
           WomenBoardroomRate, 
           fill = SoftBoardroomQuota)) +
  geom_violin(col = NA) +
  theme (panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank()) +
  labs(x = "Soft boardroom quota",
       y = "Percentage of women on company boards") 



