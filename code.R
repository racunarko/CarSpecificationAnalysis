# Loading data
cardata = read.csv('/Users/dominik/Desktop/CarSpecificationAnalysis/datasets/car_specifications.csv')

require(nortest)
lillie.test(cardata$horsepower)

lillie.test(cardata$horsepower[cardata$drive.wheels == 'fwd'])
lillie.test(cardata$horsepower[cardata$drive.wheels == 'rwd'])
lillie.test(cardata$horsepower[cardata$drive.wheels == '4wd'])


hist(cardata$horsepower[cardata$drive.wheels=='fwd'])
hist(cardata$horsepower[cardata$drive.wheels=='rwd'])
hist(cardata$horsepower[cardata$drive.wheels=='4wd'])

bartlett.test(cardata$horsepower ~ cardata$drive.wheels)

var(cardata$horsepower[cardata$drive.wheels == 'fwd'])
var(cardata$horsepower[cardata$drive.wheels == 'rwd'])
var(cardata$horsepower[cardata$drive.wheels == '4wd'])

boxplot(cardata$horsepower ~ cardata$drive.wheels)

a = aov(cardata$horsepower ~ cardata$drive.wheels)
summary(a)

model = lm(horsepower ~ drive.wheels, data = cardata)
summary(model)

anova(model)

# Assuming 'a' is the ANOVA model you created
a = aov(cardata$horsepower ~ cardata$drive.wheels)

# Perform a post-hoc test (Tukey's test is commonly used)
posthoc = TukeyHSD(a)

# Print the results
print(posthoc)

