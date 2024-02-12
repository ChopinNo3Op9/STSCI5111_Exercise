

d <- read.csv("data/titanic.csv")

# Does female have a higher chance of survival?

# find how many unqiue prefixes
unique(d$Prefix)

these <- which(d$Prefix == "Dr")
these
d[these, ]

n <- nrow(d)
d$Gender <- rep(NA, n)

d$Gender[d$Prefix %in% c("Miss", "Mrs", "Mme.", "Sr.")] <- "F"
d$Gender[d$Prefix %in% c("Mr", "Sir")] <- "M"
table(d$Gender)
table(d$Gender, d$Survived)  # from the table, we can see female has a higher chance of survival


# to analyze statistically
prop.table(table(d$Gender, d$Survived))  # percentage in contingency table
prop.table(table(d$Gender, d$Survived), 1)  # 1 indicates that proportions should be calculated across the rows (margins). 
prop.table(table(d$Gender, d$Survived), 2)  # 2 indicates that proportions should be calculated across the columns (margins). 

# plot
plot(prop.table(table(d$Gender, d$Survived))) # mosaic plot, the relative frequencies of different categories in a contingency table

# correlation
cor(d$Gender == "F", d$Survived == 1)  # lots of NAs
# only complete cases (i.e., rows with non-missing values for both variables) should be considered when calculating the correlation.
cor(d$Gender == "F", d$Survived == 1, use = "pairwise.complete.obs") # 0.48, positive correlation, but not correct/adjust for the sample size
cor(d$Gender == "M", d$Survived == 1, use = "pairwise.complete.obs")  # -0.48

# create a dataset
fake.d <- data.frame(Gender = c("F", "F", "F", "M", "M", "M"), Survived = c(1, 1, 1, 1, 0, 0))
cor(fake.d$Gender == "F", fake.d$Survived == 1, use = "pairwise.complete.obs")  # 0.7, since small size, not confident to say there is a correlation.

# to quantify the statistical evidence
lm1 <- lm((d$Survived == 1) ~ as.factor(d$Gender))
summary(lm1)  # p-value = <2e-16 ***, reject the null hypothesis, female and male's survival rate is different.

lm2 <- lm((fake.d$Survived == 1) ~ as.factor(fake.d$Gender))
summary(lm2)  # p-value: 0.1161, fail to reject the null hypothesis at the 0.05 significance level, not enough evidence, even if the female survivor rate is more in this small dataset.


# Task for today
# 1. Repeat the above to Class and Survived variables 
# -> data cleaning: 1st, 2nd, 3rd,
# -> calculate correlation, plot, and two-way table between class and survived
# -> linear regression, get p-value, quantify statistical evidence
# Data cleaning: Convert class to standard notation
unique(d$Class)
unique_classes <- unique(d$Class)

# Filter out only 1st, 2nd, and 3rd class
filtered_classes <- unique_classes[grep("1st|2nd|3rd", unique_classes)]
dd <- subset(d, Class %in% filtered_classes)

# Calculate correlation between class and survival
dd$Survived <- factor(dd$Survived)
correlation <- cor(dd$Class == "1st Class Passenger", dd$Survived == 1, use = "pairwise.complete.obs")
correlation

correlation <- cor(dd$Class == "2nd Class Passenger", dd$Survived == 1, use = "pairwise.complete.obs")
correlation

correlation <- cor(dd$Class == "3rd Class Passenger", dd$Survived == 1, use = "pairwise.complete.obs")
correlation

# Plot the relationship between class and survival
cont_table <- table(dd$Class, dd$Survived)
cont_table
# Perform chi-square test for independence
chi_square_test <- chisq.test(cont_table)  # reject the null hypothesis and conclude that there is a significant association between 'Class' and 'Survived'.
chi_square_test

# Create a two-way table between class and survival
two_way_table <- table(dd$Class, dd$Survived)
two_way_table

# Linear regression to quantify statistical evidence
lm_class_survived1 <- lm(Survived ~ as.factor(Class), data = dd)  
summary(lm_class_survived1) # '1st Class' is not included in the coefficients table is because it's used as a reference level in the linear regression model.

lm_class_survived2 <- lm((Survived == 1) ~ as.factor(Class), data = dd)  # classes' survival rate are different.
summary(lm_class_survived2)

lm_class_survived3 <- lm((Survived == 0) ~ as.factor(Class), data = dd)  # all sifniticant
summary(lm_class_survived3)

# Convert Class to a factor and change the reference level
dd$Class <- relevel(factor(dd$Class), ref = "2nd Class Passenger")
# Linear regression to quantify statistical evidence
lm_class_survived <- lm(Survived ~ Class, data = dd)
summary(lm_class_survived)
