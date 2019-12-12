library(readr)
library(tidyverse)
library(lfe)
library(cluster)
library(factoextra)

data = read_csv("experiment.csv")
data = na.omit(data)
glimpse(data)
data$X1 = NULL
data$`Unique ID` = NULL
data$diff = data$before - data$after
data$id = 1:40
data = data %>% select(id, everything())
# write_csv(data, "cbd-cleaned.csv")

# EDA
# The "difference" is the difference between the before reaction time and the 
# after reation time.
data %>% ggplot(aes(x= factor(`gender (1/0)`), y = diff, fill = treatment))+
  geom_boxplot() +
  labs(
       title = "The difference in reaction time upon gender",
       y = "difference",
       x = "female                                         male")


data$ageRange <- findInterval(data$AGE, c(15, 20, 25, 30, 35))
data %>% 
  group_by(ageRange,treatment) %>% 
  summarize(totaldiff=mean(diff)) %>% 
  ggplot(aes(x= as.factor(ageRange), y=totaldiff,fill = treatment)) +
  geom_col(position = "dodge") +
  labs(title = "Average difference For Each Age Bin", 
       x = "Age Bin By 10 (1 = 15-20, 2 = 20-25, 3 = 25-30, 4 = 30-35)",
       y = "Average difference of reaction time", 
       fill = "Treatment") 

# regression
reg1 = felm(diff ~ treatment + AGE + `gender (1/0)`, data = data)
summary(reg1)

# reg2 = felm(log(diff) ~ treatment + AGE + `gender (1/0)`, data = data)
# summary(reg2)

reg1 = felm(diff ~ treatment | AGE + `gender (1/0)`, data = data)
summary(reg1)

# reg2 = felm(diff ~ treatment*AGE , data = data)
# summary(reg2)

reg3 = felm(diff ~ treatment*`gender (1/0)` | AGE , data = data)
summary(reg3)

reg3 = felm(diff ~ treatment*AGE | `gender (1/0)` , data = data)
summary(reg3)

# reg4 = felm(diff ~ treatment*`gender (1/0)`*AGE, data = data)
# summary(reg4)

# clustering 
data1 <- data %>% select(AGE, `gender (1/0)`, before)
b = scale(data1)
x = 1:20
k_wss = function(k) {
  km = kmeans(b, k, nstart=25, iter=25)
  kwss = km$tot.withinss
  return(kwss)
}

wss = map_dbl(x, k_wss)
plot(x, wss, type = "b", main = "Judges select K-wss")

data_dist <- dist(data1, method = "euclidean")
hfit <- hclust(data_dist, method = "ward.D")
plot(hfit)

hclust <- cutree(hfit, k=3)
rect.hclust(hfit, k = 3, border ="green")
data <- cbind(hclust, data)
data %>% count(hclust)

k = kmeans(b, centers = 3, iter.max = 25, nstart = 25)
fviz_cluster(k,data = data1)
table(k$cluster)












