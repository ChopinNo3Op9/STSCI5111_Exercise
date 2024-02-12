

d <- read.csv("titanic.csv")

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
prop.table(table(d$Gender, d$Survived))  # percentage
prop.table(table(d$Gender, d$Survived), 1)
prop.table(table(d$Gender, d$Survived), 2)

# plot
plot(prop.table(table(d$Gender, d$Survived))) # mosaic plot, the relative frequencies of different categories in a contingency table


