#Topic 4
install.packages("arules")
library(arules)

visits <- read.transactions("C:/Users/dimitris/Documents/ΕΑΠ_b_8_24/1st year/DAMA 51/HW/4/countries.csv", format="basket", 
                            header=FALSE, sep=",", rm.duplicates=FALSE)
#a
typeof(visits)

summary(visits)
str(visits)

visits@itemInfo

labels(visits)

#b
rules <- apriori(visits,parameter = list(minlen=2,supp = 0.2, conf = 0.8))

inspect(rules)

#c
# Initialize a vector to store the number of rules for each confidence value
num_rules <- numeric(length = 6)  # Adjust the length based on the desired range

# Loop through different confidence values and mine rules
x <- seq(0.125, 0.25, by=0.025)

cat(x)
i <- 0
for (val in x) {
  supp_value <- val # Adjust the multiplier based on your desired range
  rules <- apriori(visits, parameter = list(supp=supp_value, conf=0.8, minlen=2, target= "rules"))
  i <- i+1
  num_rules[i] <- length(rules)
}

# Plot
plot(x, num_rules, type = "o", col = "blue", pch = 16, main = "Plot of number of rules",
     xlab = "Support value", ylab = "Number of association rules")

#d
# Check for Cyprus
rules <- apriori(visits, parameter = list(supp=0.2, conf=0.8, minlen=2, target= "rules"))
Cyprus_rules <- subset(rules, lhs %in% "Cyprus")
inspect(Cyprus_rules)
