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
  d$Class[i] <- str_extract(z[i], "<a title=\"([0-9][a-z]{2}\\sClass\\sPassenger|[A-Z][a-z]+\\sCrew|[A-Z][a-z]+\\sStaff)\"", group = 1)
}

d$Age

z[9]

# Task for today
# 1. Use regular expression to extract all age information
# 2. Grab the other covariates, starting from Family Name and Given Name. 
# 3. The class variable is more complicated. Write a regular
# expression that is more elegant than mine.
# 4. Try to get Prefix, Joined, url, Price

for (i in 1:n) {
  # family name
  d$FamilyName[i] <- str_extract(z[i], "(?<=familyName>)[^<>]+(?=<)")
  
  # given name
  d$GivenName[i] <- str_extract(z[i], "givenName>([^<>]+)<", group = 1)
  
  # prefix
  d$Prefix[i] <- str_extract(z[i], "Prefix>([^<>]+)<", group = 1)
  
  # gender
  gender <- NA
  # Check if prefix contains Mr. or Mrs.
  if (!is.na(d$Prefix[i])) {
    if (grepl("^Mr", d$Prefix[i])) {
      gender <- "Male"
    } else if (grepl("^Mrs", d$Prefix[i])) {
      gender <- "Female"
    } else if (grepl("^Miss", d$Prefix[i])) {
      gender <- "Female"
    } else if (grepl("^Ms", d$Prefix[i])) {
      gender <- "Female"
    }
  }
  # Assign gender to the "gender" variable
  d$Gender[i] <- gender
  
  # Age
  # Solution 1: use of if, else logic
  age_str <- str_extract(z[i], "Age (\\d+)", group = 1)
  if (is.na(age_str)) {
    age_str <- str_extract(z[i], "nowrap>([^<>]+)<", group = 1)
  }
  
  # Clean the age
  if (!is.na(age_str)) {
    if (grepl("m", age_str)) {
      # Age is in months
      age_numeric <- as.numeric(gsub("[^0-9]", "", age_str)) / 12
    } else {
      # Age is in years
      age_numeric <- as.numeric(age_str)
    }
    # Convert to fraction if not integer
    if (age_numeric %% 1 != 0) {
      age_numeric <- as.character(round(age_numeric, 2))
    }
    d$Age[i] <- age_numeric
  }
  
  # Solution 2: use of \\D,\\d,?,+,etc.
  # If age >= 1, text looks like nowrap><a title=\"Age 39
  # If age < 1, text looks like nowrap>10m
  # d$Age[i] <- str_extract(z[i], "nowrap>[\\D]*([\\d]+[m]?)",group = 1)
  
  # Solution 3: use of the "or" logic |
  # d$Age[i] <- str_extract(z[i], "[Age |nowrap>](\\d+m?)", group = 1)
  
  # class
  d$Class[i] <- str_extract(z[i], "<a title=\"([0-9][a-z]{2}\\sClass\\sPassenger|[A-Z][a-z]+\\sCrew|[A-Z][a-z]+\\sStaff)\"", group = 1)
  # d$Class[i] <- str_extract(z[i], "<span>(.+(Passenger|Crew|Staff))</span>)", group = 1)
  # d$Class[i] <- str_extract(z[i], "<a title=\"([^<>]+) (Passenger|Crew|Staff)\" href", group = 1) 
  # d$Class[i] <- str_extract(z[i], 'title=\"([^<>]+)\" href=[^<>]+/>', group = 1)
  # d$Class[i] <- str_extract(z[i], "<span>(.[^<>]+)", group = 1)
  # d$Class[i] <-str_extract(z[i], "<span>(\\d?\\D+)</span>", group=1)
  
  # These solutions only grab the "... Class Passenger" pattern and do not get the staff and crew
  # d$Class[i] <- str_extract(z[i], "(\\d)[a-z]{2} Class", group = 1)
  # d$Class[i] <- str_extract(z[i], "\"([/d/w+/sClass]\\d\\w+\\sClass Passenger)", group = 1)
  
  # Another solution using "if" statements
  # d$Class[i] <- str_extract(z[i], "\"([/d/w+/sClass]\\d\\w+\\sClass Passenger)", group = 1)
  # if (is.na(d$Class[i])){
  #   d$Class[i] <- str_extract(z[i], "([/w+/sCrew]\\w+\\sCrew)", group = 1)
  # }
  # if (is.na(d$Class[i])){
  #   d$Class[i] <- str_extract(z[i], "([/w+/sStaff]\\w+\\sStaff)", group = 1)
  # }
  
  # joined
  d$Joined[i] <- str_extract(z[i], 'Titanic in ([^<>]+)\"', group = 1)
  
  # url
  d$url[i] <- paste0("https://www.encyclopedia-titanica.org/", str_extract(z[i], "url href=(.[^>]+)", group = 1))
  
  # ticket price
  d$Price[i] <- str_extract(z[i], "£.[^<>]+")
  
  # temp <- str_split(z[1], "<[^<>]*>")[[1]]
  # temp
  # temp <- temp[nchar(temp)>0]
  # d[i,] <- c(temp[1], temp[5], temp[3], temp[6], temp[7], temp[10])
  
  #Alternative solution (that does not work here)
  # Can be risky if you cannot check if mistakes are made.
  # d <- data.frame(FamilyName = rep('', n), GivenName = rep('', n),
  #                  Prefix = rep('', n), Age = rep('', n),
  #                  Class = rep('', n), Joined = rep('', n))
  #  for (i in 1:n) {
  #    temp <- str_split(z[i], "<[^<>]*>")[[1]]
  #    temp <- temp[nchar(temp)>0]
  #    d[i,] <- c(temp[1], temp[5], temp[3], temp[6], temp[7], temp[10])
  #  }
}



