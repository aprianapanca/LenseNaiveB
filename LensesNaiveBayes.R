# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
data <- lenses1.csv(file.choose(), header = T)
str(data)
xtabs(~age+tear, data = data)
data$tear <- as.factor(data$tear)
data$age <- as.factor(data$age)

# Visualization
pairs.panels(data[-1])
data %>%
  ggplot(aes(x=age, y=astigmatic, fill = age)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>% ggplot(aes(x=astigmatic, fill = age)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(age ~ ., data = train, usekernel = T)
model

train %>%
  filter(age == "1") %>%
  summarise(mean(spectacle), sd(spectacle))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$age))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$age))
1 - sum(diag(tab2)) / sum(tab2)
