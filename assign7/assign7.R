communities <- read.table("C:/Users/carrc4/Documents/ITWS4600/DataAnalytics2024/assign7/communities.data", sep = ",", header = FALSE)

# Define the path to the file
file_path <- "C:/Users/carrc4/Documents/ITWS4600/DataAnalytics2024/assign7/headers.txt"
# Define the file paths

# Step 1: Extract attribute names from the .names file
lines <- readLines(file_path)

# Initialize a vector to store the extracted attribute names
attribute_names <- c()

# Loop through each line in the file
for (line in lines) {
  # Check if the line starts with '@attribute'
  if (startsWith(line, "@attribute")) {
    # Extract the attribute name using regex
    matches <- regmatches(line, regexec("@attribute ([^ ]+)", line))
    if (length(matches[[1]]) > 1) {
      attribute_names <- c(attribute_names, matches[[1]][2])
    }
  }
}

# Step 3: Assign the extracted attribute names as column names
colnames(communities) <- attribute_names



#########################
# Introductory Analysis #
#########################

boxplot(communities[, c("racepctblack", "racePctWhite", "racePctAsian", "racePctHisp")])
hist(1-communities$racePctWhite, main = "Percent minority population", breaks = 100)

boxplot(communities[, c("agePct12t21", "agePct12t29", "agePct16t24", "agePct65up")])
summary(communities$agePct12t21)
summary(communities$agePct12t29)
summary(communities$agePct16t24)
summary(communities$agePct65up)

hist(communities$pctUrban, breaks = 100, main = "Percent of People in Urban Areas")
hist(communities$medIncome, breaks = 100)

lm0 <- lm(ViolentCrimesPerPop ~ racePctHisp, data = communities)
lm0
plot(communities$racePctHisp, communities$ViolentCrimesPerPop, main = "Violent Crime Per 100k by Percent Hispanic", xlab="Percent Hispanic", ylab="Violent Crimes per 100k (percentile)")
abline(0.1957, 0.2937)
cor(communities$racePctHisp, communities$ViolentCrimesPerPop)

lm1 <- lm(ViolentCrimesPerPop ~ agePct65up, data = communities)
lm1
plot(communities$agePct65up, communities$ViolentCrimesPerPop, main = "Violent Crime Per 100k by 16-24yo", xlab="Percent16-24 year olds", ylab="Violent Crimes per 100k (percentile)")
abline(0.20102, 0.08734)
cor(communities$agePct65up, communities$ViolentCrimesPerPop)

lm1 <- lm(ViolentCrimesPerPop ~ pctUrban, data = communities)
lm1
plot(communities$pctUrban, communities$ViolentCrimesPerPop, main = "Violent Crime by Urban Population", xlab="Percent Urban Population", ylab="Violent Crimes per 100k (percentile)")
abline(0.20806, 0.04296)
cor(communities$pctUrban, communities$ViolentCrimesPerPop)

lm3 <- lm(ViolentCrimesPerPop ~ PctRecImmig10, data = communities)
lm3
plot(communities$PctRecImmig10, communities$ViolentCrimesPerPop, main = "Violent Crime by Percent Immigrants in Past 10 Years", xlab="Percent Immigrants in Past 10 Years", ylab="Violent Crimes per 100k (percentile)")
abline(0.1900, 0.2622)
cor(communities$PctRecImmig10, communities$ViolentCrimesPerPop)

communities$PctPolicMinor <- as.numeric(communities$PctPolicMinor)
communities_clean <- na.omit(communities[, c("PctPolicMinor", "ViolentCrimesPerPop")])
lm4 <- lm(ViolentCrimesPerPop ~ PctPolicMinor, data = communities_clean)
lm4
plot(communities_clean$PctPolicMinor, communities_clean$ViolentCrimesPerPop, main = "Violent Crime by Percent Minority Police", xlab="Percent White Minority Officers", ylab="Violent Crimes per 100k (percentile)")
abline(0.2867, 0.5959  )
cor(communities_clean$PctPolicMinor, communities_clean$ViolentCrimesPerPop)








communities.clean <- data.frame(lapply(communities.clean, function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x)) # Convert factors to characters, then to numeric
  } else if (is.character(x)) {
    as.numeric(x) # Convert characters directly to numeric
  } else {
    x # Leave numeric columns as they are
  }
}))

communities.clean <- communities.clean[, colSums(is.na(communities.clean)) == 0]

communities.clean.pc <- prcomp(communities.clean[,-121], center = TRUE, scale. = TRUE)

attributes(communities.clean.pc)
summary(communities.clean.pc)

plot(communities.clean.pc)



communities.clean$CrimeRate <- cut(
  communities$ViolentCrimesPerPop, 
  breaks = c(-Inf, 0.2, 0.4, 0.6, 0.8, 2), # Define breakpoints
  labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"), # Descriptive labels
  right = FALSE # Make intervals left-inclusive
)


k=5
knn.pred <- knn(train = communities.clean.pc$x[,c(1:61)], test = communities.clean.pc$x[,c(1:61)], cl = communities.clean$CrimeRate, k = k)

## evaluate
cm <- table(Predicted=knn.pred, Actual = communities.clean$CrimeRate, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)

