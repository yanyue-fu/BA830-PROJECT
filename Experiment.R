library(tidyverse)
library(readxl)
library(factoextra)
library(tidytext)
ex<-read_xlsx("Documents/BA830/Experiment/experiment.xlsx")
ex %>% 
  select(-...1,-`How many times had any CBD in past 2 weeks`)->ex1 
ex_c<-ex1 %>% 
  filter(complete.cases(ex1)==TRUE)
ex_c
ex_c$difference<-ex_c$after-ex_c$before
ex_c
## replace A with 0 and B with 1, B is the treatment, A is the placebo
ex_c$treatment[ex_c$treatment=="A"]<-0
ex_c$treatment[ex_c$treatment=="B"]<-1
## we want to know if the treatment would effect on the people's concentration
## null hypothesis is there is no affect on grade
summary(lm(difference ~ treatment, data=ex_c))
library(lfe)
## we fail to reject the H0, thus we are 95% confidence that 
## the CBD has no affect on the grade
summary(felm(difference ~ treatment, data=ex_c))
