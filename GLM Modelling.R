# Get rid of following variables because test set has more levels
df.train$Parch = NULL
df.test$Parch = NULL
df.train$Title = NULL
df.test$Title = NULL
# Visual check for multicollinearity
pairs(df.train)
glm_1 = glm(data = df.train, Survived~., family = binomial())
summary(glm_1)

# Getting rid of Family size because glm recognises this is a linear combination of SibSp and Parch
glm_2 = glm(data = df.train, Survived~.-FamilySize, family = binomial())
summary(glm_2)

# Getting rid of title because least significant
glm_3 = glm(data=df.train, Survived~.-FamilySize-LifeStage, family = binomial())
summary(glm_3)

# Getting rid of CabinSection which is least significant
glm_4 = glm(data = df.train, Survived~.-FamilySize-LifeStage-CabinSection, family = binomial())
summary(glm_4)

# Getting rid of fare which is least significant
glm_5 = glm(data = df.train, Survived~.-FamilySize-LifeStage-CabinSection-Fare, family = binomial())
summary(glm_5)

# Choose glm_1 which has lower AIC and least variables

