iris
names(iris)
str(iris)
summary(iris)
dim(iris)
plot(iris, col=iris$Species)
library(caret)
        
sam = caret::createDataPartition(iris$Species, p=.7, list=FALSE)
train = iris[sam,]
test = iris[-sam,]
dim(iris)
nrow(train)
nrow(test)

ggplot(iris, aes(x = Petal.Length, y = Sepal.Length, colour = Species)) + 
  geom_point() +
  ggtitle('Iris Species by Petal and Sepal Length')

iris[['Is.Versicolor']] <- as.numeric(iris[['Species']] == 'versicolor')
fit.lm <- lm(Is.Versicolor ~ Petal.Length + Sepal.Length, data = iris)
summary(fit.lm)
iris[['Predict.Versicolor.lm']] <- as.numeric(predict(fit.lm) > 0.5)
table(iris[, c('Is.Versicolor', 'Predict.Versicolor.lm')])

fit.logit <- glm(Is.Versicolor ~ Petal.Length + Sepal.Length, data = iris,
                 family = binomial(link = 'logit'))
summary(fit.logit)
iris[['Predict.Versicolor.logit']] <- as.numeric(predict(fit.logit) > 0.5)
table(iris[, c('Is.Versicolor', 'Predict.Versicolor.logit')])

cor(iris[, c('Petal.Length', 'Sepal.Length')])

