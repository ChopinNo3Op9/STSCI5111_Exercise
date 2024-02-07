library(stringr)

# Task for today
# 1. Scrape the victims data as well. Create a big dataframe with 
# both survivors and victims. Create a "survived" variable (binary, 1 for survivors,
# 0 for victims). Hint: use the function "rbind"
# 2. Clean the age. Make it numeric. If age >= 1, leave it, otherwise, divide 
# the number by 12 (e.g. 10m should become 10/12).
# 3. Create a "gender" variable using the "prefix" variable.
# 4. Clean the Price variable: e.g. just take the number after the pound sign, or specify.
# 4. Ask one simple question, and test your hypothesis, by tables or pictures.
# e.g. * Do children have higher chances of survival?
# * How much more expensive are first class tickets?

# Read survivor data
x_survivor <- scan("titanic_survivor.html", what = "character", sep = "\n")
y_survivor <- x_survivor[11]
z_survivor <- str_split(y_survivor, "<span style=[^<>]+>")[[1]]
z_survivor <- z_survivor[-1]

# Read victim data
x_victim <- scan("titanic_victim.html", what = "character", sep = "\n")
y_victim <- x_victim[11]
z_victim <- str_split(y_victim, "<span style=[^<>]+>")[[1]]
z_victim <- z_victim[-1]

# Function to clean data
clean_data <- function(z_data, survived) {
  n <- length(z_data)
  d <- data.frame(FamilyName = rep("", n), GivenName = rep("", n),
                  Prefix = rep("", n), Gender = rep("", n), Age = rep("", n),
                  Class  = rep("", n), Joined  = rep("", n),
                  url = rep("", n), Price  = rep("", n), Survived = rep(survived, n))
  
  for (i in 1:n) {
    # family name
    d$FamilyName[i] <- str_extract(z_data[i], "(?<=familyName>)[^<>]+(?=<)")
    
    # given name
    d$GivenName[i] <- str_extract(z_data[i], "givenName>([^<>]+)<", group = 1)
    
    # prefix
    d$Prefix[i] <- str_extract(z_data[i], "Prefix>([^<>]+)<", group = 1)
    
    # gender
    gender <- NA
    # Check if prefix contains Mr. or Mrs.
    if (!is.na(d$Prefix[i])) {
      if (grepl("^Mr", d$Prefix[i])) {
        gender <- "Male"
      } else if (grepl("^Mrs", d$Prefix[i]) || grepl("^Miss", d$Prefix[i]) || grepl("^Ms", d$Prefix[i])) {
        gender <- "Female"
      }
    }
    # Assign gender to the "gender" variable
    d$Gender[i] <- gender
    
    # Age
    age_str <- str_extract(z_data[i], "Age (\\d+)", group = 1)
    if (is.na(age_str)) {
      age_str <- str_extract(z_data[i], "nowrap>([^<>]+)<", group = 1)
    }
    if (!is.na(age_str)) {
      if (grepl("m", age_str)) {
        age_numeric <- as.numeric(gsub("[^0-9]", "", age_str)) / 12
      } else {
        age_numeric <- as.numeric(age_str)
      }
      if (age_numeric < 1) {
        age_numeric <- age_numeric / 12
      }
      d$Age[i] <- age_numeric
    }
    
    # class
    d$Class[i] <- str_extract(z_data[i], "<a title=\"([0-9][a-z]{2}\\sClass\\sPassenger|[A-Z][a-z]+\\sCrew|[A-Z][a-z]+\\sStaff)\"", group = 1)
    
    # joined
    d$Joined[i] <- str_extract(z_data[i], 'Titanic in ([^<>]+)\"', group = 1)
    
    # url
    d$url[i] <- paste0("https://www.encyclopedia-titanica.org/", str_extract(z_data[i], "url href=(.[^>]+)", group = 1))
    
    # ticket price
    d$Price[i] <- str_extract(z_data[i], "£.[^<>]+")
  }
  return(d)
}

# Clean survivor data
survivor_data <- clean_data(z_survivor, 1)

# Clean victim data
victim_data <- clean_data(z_victim, 0)

# Combine survivor and victim data
titanic_data <- rbind(survivor_data, victim_data)

# Display the first few rows of the combined data
head(titanic_data)

table(titanic_data$Survived)

# "Did passengers with higher passenger class (such as first class) have better chances of survival compared to those in lower classes?" "Yes"
# Load necessary libraries
library(ggplot2)

# Calculate survival rates by passenger class
survival_rates <- aggregate(Survived ~ Class, titanic_data, function(x) sum(x) / length(x))

# Plot the survival rates by passenger class
ggplot(survival_rates, aes(x = Class, y = Survived)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Survival Rates by Passenger Class",
       x = "Passenger Class",
       y = "Survival Rate") +
  theme_minimal()


