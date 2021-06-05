setwd("G:\data_vis_project\Red-Wine-Data-Analysis-by-R")


library("ggplot2")
library("dplyr")
library("gridExtra")
library(Simpsons)
library(GGally)
library(memisc)
library(pander)
library(corrplot)

wine <- read.csv('wineQualityReds.csv')

#Converting Wine quality into a ordered factor
wine$quality <- factor(wine$quality, ordered = T)

#Creating a new 'rating' variable into the dataframe for different quality range

wine$rating <- ifelse(wine$quality < 5, 'bad', ifelse(
  wine$quality < 7, 'average', 'good'))

wine$rating <- ordered(wine$rating,
                       levels = c('bad', 'average', 'good'))

wine$X = factor(wine$X)
#Structure of the Dataframe

str(wine)

#Summary of the dataframe

summary(wine)

#Univariate plots

#Quality and rating
ggplot(data = wine, aes(x = quality)) +
  stat_count(width = 1, color = 'black',fill = I('orange'))

ggplot(data = wine, aes(x = rating)) +
  stat_count(width = 1, color = 'black',fill = I('blue'))


#Fixed acidity (Positively skewed)
p1 <- ggplot(data = wine, aes(x = fixed.acidity)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange'))

summary(wine$fixed.acidity)  #Median = 7.9 but some outliers dragged the mean upto 8.32

#Volatile acidity(Maybe a little bimodality)

summary(wine$volatile.acidity)

p2 <- ggplot(data = wine, aes(x = volatile.acidity)) +
  geom_histogram(binwidth = 0.05, color = 'black',fill = I('orange'))

#Citric acid(Positively skewed)
summary(wine$citric.acid)

p3 <- ggplot(data = wine, aes(x = citric.acid)) +
  geom_histogram(binwidth = 0.08, color = 'black',fill = I('orange')) +
  scale_x_continuous(breaks = seq(0,1,0.1), lim = c(0,1))

#Residual sugar (Strong freq around median with a few outliers)
summary(wine$residual.sugar)

p4 <- ggplot(data = wine, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

#Chlorides(Similar dist like Residual sugar)
summary(wine$chlorides)

p5 <- ggplot(data = wine, aes(x = chlorides)) +
  geom_histogram(binwidth = 0.01, color = 'black',fill = I('orange'))

#Free Sulphur dioxide(Large peak at 7. Positively skewed)

summary(wine$free.sulfur.dioxide)

p6 <- ggplot(data = wine, aes(x = free.sulfur.dioxide)) +
  geom_histogram(binwidth = 1, color = 'black',fill = I('orange')) +
  scale_x_continuous(breaks = seq(0,80,5))

#Total Sulphur dioxide(Similar to last one)

summary(wine$total.sulfur.dioxide)

p7 <- ggplot(data = wine, aes(x = total.sulfur.dioxide)) +
  geom_histogram(binwidth = 5, color = 'black',fill = I('orange'))

#Density(Has a very normal distribution)

summary(wine$density)

p8 <- ggplot(data = wine, aes(x = density)) +
  geom_histogram(binwidth = 0.001, color = 'black',fill = I('orange'))

#pH(Has a very normal distribution)

summary(wine$pH)

p9 <- ggplot(data = wine, aes(x = pH)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

#Sulphates(Positively skewed. Similar to Chlorides and residual sugar)

summary(wine$sulphates)

p10 <- ggplot(data = wine, aes(x = sulphates)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

#Alcohol(Positively skewed)

summary(wine$alcohol)

p11 <- ggplot(data = wine, aes(x = alcohol)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = I('orange'))

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, ncol = 4)


#Bivariate analysis
#Correlation table
c <- cor(
  wine %>%
    # first we remove unwanted columns
    dplyr::select(-X) %>%
    dplyr::select(-rating) %>%
    mutate(
      # now we translate quality to a number
      quality = as.numeric(quality)
    )
)

pandoc.table(c)


#Fixed acidity : Doesn't seem to have much effect
ggplot(data = wine, aes(x = quality, y = fixed.acidity)) +
  geom_boxplot()

#Volatile Acidity : Seems to have negative effect. With increase, quality seems to go down
ggplot(data=wine, aes(x = quality, y = volatile.acidity)) +
  geom_boxplot()

#Citric acid (Better wines tend to have higher citric acid)
ggplot(data=wine, aes(x=quality, y=citric.acid)) +
  geom_boxplot()

#Residual Sugar(Almost has no effect to quality. This is contrary to previous assumption)

ggplot(data=wine, aes(x=quality, y=residual.sugar)) +
  geom_boxplot()

#Chlorides

ggplot(data=wine, aes(x=quality, y=chlorides)) +
  geom_boxplot()

#Free SO2(We see too little and we get a poor wine and too much : we get an average wine)

ggplot(data=wine, aes(x=quality, y=free.sulfur.dioxide)) +
  geom_boxplot()

#Total SO2(Just like free SO2)

ggplot(data=wine, aes(x=quality, y=total.sulfur.dioxide)) +
  geom_boxplot()

#Density(Better wines tend to have lower densities but is it due to alcohol content?)

ggplot(data=wine, aes(x=quality, y=density)) +
  geom_boxplot()

#pH(Better wines seems to be more acidic. Now let's see contribution of each acid on pH)

ggplot(data=wine, aes(x=quality, y=pH)) +
  geom_boxplot()


#Contribution of each acid to pH(We see all of them has negative correlation on pH except 
#volatile acidity. But how's that possible! Is it possible that there is a Simson's effect?)


ggplot(data = wine, aes(x = fixed.acidity, y = pH)) +
  geom_point() +
  scale_x_log10(breaks=seq(5,15,1)) +
  xlab("log10(fixed.acidity)") +
  geom_smooth(method="lm")

ggplot(data = wine, aes(x = volatile.acidity, y = pH)) +
  geom_point() +
  scale_x_log10(breaks=seq(.1,1,.1)) +
  xlab("log10(volatile.acidity)") +
  geom_smooth(method="lm")

ggplot(data = subset(wine, citric.acid > 0), aes(x = citric.acid, y = pH)) +
  geom_point() +
  scale_x_log10() +
  xlab("log10(citric.acid)") +
  geom_smooth(method="lm")

#Test for Simpson's paradox(Test Result = Positive. So presence of underlying variables which distorts
#the shape of the whole curve and makes it positive.)

simpsons <- Simpsons(volatile.acidity, pH, data=wine)


#Sulphates(better wines seems to have higher sulphates. Although medium wines have many outliers)

ggplot(data=wine, aes(x=quality, y=sulphates)) +
  geom_boxplot()

#Alcohol(Better wines have higher alcohol)

ggplot(data=wine, aes(x=quality, y=alcohol)) +
  geom_boxplot()


#Linear model test(From R squared value, it seems alcohol contributes only 22% to the quality variance)
alcoholQualityLM <- lm(as.numeric(quality) ~ alcohol,
                       data = wine)
summary(alcoholQualityLM)
df = data.frame(wine$quality )
df$predictions <- predict(alcoholQualityLM, wine)
df$error <- (df$predictions - as.numeric(wine$quality))/as.numeric(wine$quality)

ggplot(data=df, aes(x=wine.quality, y=error)) +
  geom_boxplot()

#Putting a Cor test together

simple_cor_test <- function(x, y) {
  return(cor.test(x, as.numeric(y))$estimate)
}

correlations <- c(
  simple_cor_test(wine$fixed.acidity, wine$quality),
  simple_cor_test(wine$volatile.acidity, wine$quality),
  simple_cor_test(wine$citric.acid, wine$quality),
  simple_cor_test(log10(wine$residual.sugar), wine$quality),
  simple_cor_test(log10(wine$chlorides), wine$quality),
  simple_cor_test(wine$free.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$total.sulfur.dioxide, wine$quality),
  simple_cor_test(wine$density, wine$quality),
  simple_cor_test(wine$pH, wine$quality),
  simple_cor_test(log10(wine$sulphates), wine$quality),
  simple_cor_test(wine$alcohol, wine$quality))
names(correlations) <- c('fixed.acidity', 'volatile.acidity', 'citric.acid',
                         'log10.residual.sugar',
                         'log10.chlordies', 'free.sulfur.dioxide',
                         'total.sulfur.dioxide', 'density', 'pH',
                         'log10.sulphates', 'alcohol')

correlations

#Multivariate

#With constant alcohol density does not seem to have much effect, confirming our old suspicion
v1 <- ggplot(data = wine,
       aes(y = density, x = alcohol,
           color = quality)) +
  geom_point() +
  scale_color_brewer()


# Seems like for wines with higher alcohol content, having higher sulphate makes better wines
v2 <- ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point() +
  scale_y_continuous(limits=c(0.3,1.5)) +
  facet_wrap(~rating) +
  scale_color_brewer()

#If volatile acidity os less, better wine
v3 <- ggplot(data = wine,
       aes(y = volatile.acidity, x = alcohol,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


#Low pH and higher alcohol percent produces better wines
v4 <- ggplot(data = wine,
       aes(y = pH, x = alcohol,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


#Lower residual sugar produces better wine
v5 <- ggplot(data = wine,
       aes(y = residual.sugar, x = alcohol,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


#In general lower SO2 produces better wine even though some high outliers for better wine with high SO2
v6 <- ggplot(data = wine,
       aes(y = total.sulfur.dioxide, x = alcohol,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()

#Comparing the acids

#Higher citric acid and low volatile acidity produces better wines
v7 <- ggplot(data = wine,
       aes(y = citric.acid, x = volatile.acidity,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()

#citric acid and fixed acidity may be correlated. But the quality does not seem to be dependent here
v8 <- ggplot(data = wine,
       aes(y = citric.acid, x = fixed.acidity,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()

#Cannot really distinguish Average from Good wine based on these two factors
v9 <- ggplot(data = wine,
       aes(y = fixed.acidity, x = volatile.acidity,
           color = quality)) +
  geom_point() +
  facet_wrap(~rating) +
  scale_color_brewer()


grid.arrange(v1,v2,v3,v4,v5,v6,v7,v8,v9,ncol = 3)


#Making the linear model

set.seed(1221)
training_data <- sample_frac(wine, .6)
test_data <- wine[ !wine$X %in% training_data$X, ]
m1 <- lm(as.numeric(quality) ~ alcohol, data = training_data)
m2 <- update(m1, ~ . + sulphates)
m3 <- update(m2, ~ . + volatile.acidity)
m4 <- update(m3, ~ . + citric.acid)
m5 <- update(m4, ~ . + fixed.acidity)
m6 <- update(m2, ~ . + pH)
mtable(m1,m2,m3,m4,m5,m6)


df <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(df) <- c("quality", "error")
ggplot(data=df, aes(x=quality,y=error)) +
  geom_point()


#Final plots
ggplot(data=wine, aes(y=alcohol, x=quality)) + 
  geom_boxplot() +
  xlab("alcohol concentration (% by volume)") +
  ggtitle("Influence of alcohol on wine quality")


ggplot(data = wine,
       aes(y = sulphates, x = alcohol,
           color = quality)) +
  geom_point() +
  scale_y_continuous(limits=c(0.3,1.5)) +
  ylab("potassium sulphate (g/dm3)") +
  xlab("alcohol (% by volume)") +
  scale_color_brewer() +
  ggtitle("Alcohol and sulphates over wine quality")


df <- data.frame(
  test_data$quality,
  predict(m5, test_data) - as.numeric(test_data$quality)
)
names(df) <- c("quality", "error")
ggplot(data=df, aes(x=quality,y=error)) +
  geom_point() +
  ggtitle("Linear model errors over expected quality")
