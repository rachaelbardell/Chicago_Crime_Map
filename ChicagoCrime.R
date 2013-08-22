library(ggplot2)

data <- read.csv("http://data.cityofchicago.org/views/x2n5-8w5q/rows.csv", stringsAsFactors = F)

names(data) <- tolower(names(data))

data <- subset(data, !is.na(longitude) & !is.na(latitude))

ggplot(data)+geom_point(aes(x=longitude,y=latitude), alpha = .2, size =.4)

#highest crimes
crimes <-  table(data$primary.description) # 31 crimes
crimes <- as.list(names(crimes[crimes > 100])) # only 25 with over 100 observations
data_new <- subset(data, primary.description %in% crimes)
ggplot(data_new)+geom_bar(aes(x=primary.description, fill = primary.description))

#by month
month <- substring(data$date..of.occurrence, 1, 2)
data_new$month <- substring(data_new$date..of.occurrence, 1, 2)
table(data_new$month)

ggplot(data_new, aes(x=primary.description, fill = month))+geom_bar(position="stack")
# ggplot(data_new)+geom_bar(aes(x=primary.description, fill = month), position="stack")

#by hour
data$hour <- substring(data$date..of.occurrence, 21, 22)
ggplot(data, aes(x=primary.description, fill = hour))+geom_bar(position="stack")
ggplot(data_new, aes(x=primary.description, fill = hour))+geom_bar(position="dodge")
# ggplot(data)+geom_bar(aes(x=primary.description, fill = hour), position="stack")
# ggplot(data_new)+geom_bar(aes(x=primary.description, fill = hour), position="dodge")

# create a column to categorize crimes as violent or not violent

violent <- c("ASSUALT", "BATTERY", "CRIM SEXUAL ASSAULT", "KIDNAPPING", "SEX OFFENSE", "HOMICIDE", "INTIMIDATION")
# 1. Return TRUE or FALSE
data$violent <- data$primary.description %in% violent
# 2. make a column with all not violent and then change if in violent column
data$violent_str <- rep("not violent", nrow(data))
data$violent_str[data$primary.description %in% violent] <- "violent"
data$violent_str[data$violent=="not violent"] <- "Not Violent"
# 3. if esle statement doing the same thing
data$violent_str <- ifelse(data$violent_str %in% violent, "Violent", "Not Violent")
# filter data by violent crime
