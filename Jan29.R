library(stringr)

x <- scan("titanic_survivor.html", what = "character", sep = "\n")

for (i in 1:length(x)) {
  print(paste(i, nchar(x[i])))
}

y <- x[11]

substr(y, 1, 100)
substr(y, 1000, 2000)
substr(y, 5000, 6000)
substr(y, 10000, 11000)

z <- str_split(y, "<span style=[^<>]+>")
z <- z[[1]]

head(z)

z <- z[-1]

tail(z)

length(z)

n <- length(z)
d <- data.frame(FamilyName = rep("", n), GivenName = rep("", n),
                Prefix = rep("", n), Age = rep("", n),
                Class  = rep("", n), Join  = rep("", n),
                url = rep("", n), Price  = rep("", n))

# focus on passenger 1
# grab the age of the passenger

str_extract(z[1], "Age (\\d+)", group = 1)

for (i in 1:n) {
  d$Age[i] <- str_extract(z[i], "Age (\\d+)", group = 1)
}

d$Age

z[9]

# Task for today
# 1. Use regular expression to extract all age information
# 2. Grab the other covariates, starting from Family Name and Given Name. 
# The class variable is more complicated.

