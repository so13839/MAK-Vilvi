# Load necessary libraries
library(tidyverse) # For data manipulation and plotting
library(data.table) # For efficient data handling
library(fs) # For file system operations
library(ggplot2) # For advanced plotting

# List all files under the input directory
#dir_ls("/Users/opiyo.1/Downloads", recurse = TRUE)

# Load the data with fread for efficiency
merged_data = fread("merged_dataset.csv")

# Display the first few rows of the dataset
head(merged_data)

#Select Africa countries from the merged data
merged_data <- merged_data %>%
  filter(Country %in% c("Cameroon", "Egypt", "Ghana", "Ivory Coast", "Kenya",
                        "Malawi", "Mauritius", "Morocco", "Namibia", "Nigeria",
                        "South Africa", "Tunisia", "Uganda"))




# Print column names
columns <- colnames(merged_data)
print(columns)

# Drop the "Isolate Id" column
merged_data <- merged_data[, !"Isolate Id"]

# List of columns to drop
columns_to_drop <- c(
  'Amikacin', 'Amoxycillin clavulanate', 'Ampicillin', 'Azithromycin', 'Cefepime', 'Cefoxitin', 'Ceftazidime',
  'Ceftriaxone', 'Clarithromycin', 'Clindamycin', 'Erythromycin', 'Imipenem', 'Levofloxacin', 'Linezolid',
  'Meropenem', 'Metronidazole', 'Minocycline', 'Penicillin', 'Piperacillin tazobactam', 'Tigecycline', 'Vancomycin',
  'Ampicillin sulbactam', 'Aztreonam', 'Aztreonam avibactam', 'Cefixime', 'Ceftaroline', 'Ceftaroline avibactam',
  'Ceftazidime avibactam', 'Ciprofloxacin', 'Colistin', 'Daptomycin', 'Doripenem', 'Ertapenem', 'Gatifloxacin',
  'Gentamicin', 'Moxifloxacin', 'Oxacillin', 'Quinupristin dalfopristin', 'Sulbactam', 'Teicoplanin', 'Tetracycline',
  'Trimethoprim sulfa', 'Ceftolozane tazobactam', 'Cefoperazone sulbactam', 'Meropenem vaborbactam', 'Cefpodoxime',
  'Ceftibuten', 'Ceftibuten avibactam', 'Tebipenem'
)

# Drop the specified columns
merged_data <- merged_data[, !columns_to_drop, with = FALSE]

# Analyze categorical columns
for (col in names(merged_data)[sapply(merged_data, is.character)]) {
  cat("Column:", col, "\n")
  cat("Unique values:", unique(merged_data[[col]]), "\n")
  cat("Value counts:\n")
  print(table(merged_data[[col]]))
  cat("******************************************\n")
}

# Count missing values in numeric columns
numeric_missing <- sapply(merged_data, function(x) if (is.numeric(x)) sum(is.na(x)) else 0)
print(numeric_missing)

# Count missing values in character columns
character_missing <- sapply(merged_data, function(x) if (is.character(x)) sum(is.na(x)) else 0)
print(character_missing)

# Handling missing values
merged_data2 <- copy(merged_data)

# Plot histograms
ggplot(merged_data2, aes(x = Precipitation)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  ggtitle("Histogram of Precipitation") +
  theme_minimal()

ggplot(merged_data2, aes(x = Temperature)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  ggtitle("Histogram of Temperature") +
  theme_minimal()

ggplot(merged_data2, aes(x = `CO2 Emissions`)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  ggtitle("Histogram of CO2 Emissions") +
  theme_minimal()

ggplot(merged_data2, aes(x = Humidity)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  ggtitle("Histogram of Humidity") +
  theme_minimal()

ggplot(merged_data2, aes(x = `Wind Speed`)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  ggtitle("Histogram of Wind Speed") +
  theme_minimal()

# Load necessary libraries
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(tidyr)       # For reshaping data
library(data.table)  # For using data.table functionalities

# Convert the data frame to a data table for efficient manipulation
setDT(merged_data2)

# Calculate the number of NAs in each categorical column
na_counts <- sapply(merged_data2[, "Cefixime_I", with = FALSE], function(x) sum(is.na(x)))

# Ensure na_counts is a data frame
na_counts_df <- data.frame(Column = names(na_counts), NaN_Count = na_counts)

# Plotting the number of NaNs in categorical columns
ggplot(na_counts_df, aes(x = Column, y = NaN_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of NaNs in Categorical Columns", x = "Categorical Columns", y = "Number of NaNs")

# Select only the categorical columns
categorical_columns <- merged_data2 %>% select(where(is.character))

# Plot value counts for each categorical column
for (column_name in names(categorical_columns)) {
  value_counts <- table(categorical_columns[[column_name]], useNA = "ifany")
  
  value_counts_df <- data.frame(Value = names(value_counts), Count = as.vector(value_counts))
  
  ggplot(value_counts_df, aes(x = Value, y = Count)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Counts of", column_name), x = column_name, y = "Counts") +
    print()
}

# Impute missing values in numerical columns with the median
numeric_columns <- c('Temperature', 'CO2 Emissions', 'Sea Level Rise', 'Precipitation', 'Humidity', 'Wind Speed')

for (col in numeric_columns) {
  median_value <- median(merged_data2[[col]], na.rm = TRUE)
  merged_data2[[col]][is.na(merged_data2[[col]])] <- median_value
}

# Count missing values in numerical columns
na_numeric_counts <- sapply(merged_data2 %>% select(where(is.numeric)), function(x) sum(is.na(x)))

# Count missing values in categorical columns
na_categorical_counts <- sapply(merged_data2 %>% select(where(is.character)), function(x) sum(is.na(x)))

# Check the structure of the data
str(merged_data2)

# List of columns to drop
columns_to_drop_1 <- c(
  'Aztreonam avibactam_I','Cefixime_I','Ceftaroline avibactam_I','Ciprofloxacin_I',
  'Gatifloxacin_I','Sulbactam_I','Tetracycline_I','Ceftolozane tazobactam_I',
  'Cefoperazone sulbactam_I','Meropenem vaborbactam_I','Cefpodoxime_I',
  'Ceftibuten_I','Ceftibuten avibactam_I','Tebipenem_I'
)

# Drop the first set of columns
merged_data2 <- merged_data2[, !columns_to_drop_1, with = FALSE]

# List of columns to drop
columns_to_drop_2 <- c(
  'AMPC','SHV','TEM', 'CTXM1','CTXM2','CTXM825','CTXM9','VEB','PER','GES','ACC',
  'CMY1MOX','CMY11','DHA','FOX','ACTMIR','KPC','OXA','NDM','IMP','VIM','SPM','GIM'
)

# Drop the second set of columns
merged_data2 <- merged_data2[, !columns_to_drop_2, with = FALSE]

# Check the structure of the data after dropping columns
str(merged_data2)

# Print the remaining columns
print(names(merged_data2))

#Select Acinetobacter baumannii bacteria
Acinetobacter_baumannii <- subset(merged_data2, Species == "Acinetobacter baumannii")

#Select Acinetobacter baumannii bacteria
Acinetobacter_baumannii <- subset(merged_data2, Species == "Acinetobacter baumannii")

#Final dataset
Acinetobacter_baumannii_Final <- data.frame(Acinetobacter_baumannii[,c(6,8:9,13,24,27,48:53)])

#Impute catergorcal variables

#For multiple columns
# Load necessary library
library(dplyr)
library(mice)

# Function to impute missing categorical variables in a data frame
impute_categorical_proportion <- function(df) {
  # Identify categorical columns (factors and character vectors)
  categorical_cols <- names(df)[sapply(df, function(col) is.factor(col) || is.character(col))]
  
  # Iterate over each categorical column
  for (column_name in categorical_cols) {
    # Calculate proportions for the current column
    proportions <- df %>%
      dplyr::filter(!is.na(.data[[column_name]])) %>%
      dplyr::count(.data[[column_name]]) %>%
      dplyr::mutate(proportion = n / sum(n))
    
    # Print proportions for debugging
    cat("Proportions for column", column_name, ":\n")
    print(proportions)
    
    # Set a random seed for reproducibility
    set.seed(123)
    
    # Generate random imputed values based on proportions
    imputed_values <- sample(
      proportions[[column_name]],
      size = sum(is.na(df[[column_name]])),
      replace = TRUE,
      prob = proportions$proportion
    )
    
    # Replace NA values with imputed values
    df[[column_name]][is.na(df[[column_name]])] <- imputed_values
  }
  
  return(df)
}

df <- Acinetobacter_baumannii_Final
df[df == ""] <- NA
# Impute missing values in all categorical columns
df_Acinetobacter_baumannii_Final <-impute_categorical_proportion(df)

#Impute continuous variable
imputed_data_Acinetobacter_baumannii_Final <- mice(df_Acinetobacter_baumannii_Final, m = 5, method = 'pmm', maxit = 50, seed = 123)
Acinetobacter_regression <- complete(imputed_data_Acinetobacter_baumannii_Final)

# Convert relevant columns to factors
Acinetobacter_regression$Gender <- as.factor(Acinetobacter_regression$Gender)
Acinetobacter_regression$Speciality <- as.factor(Acinetobacter_regression$Speciality)
Acinetobacter_regression$Source <- as.factor(Acinetobacter_regression$Source)
Acinetobacter_regression$Amikacin_I <- as.factor(Acinetobacter_regression$Amikacin_I)
Acinetobacter_regression$Imipenem_I <- as.factor(Acinetobacter_regression$Imipenem_I)
Acinetobacter_regression$Meropenem_I <- as.factor(Acinetobacter_regression$Meropenem_I)

# Checking the distribution of Specialities
specialty_distribution <- table(Acinetobacter_regression$Speciality)
barplot(specialty_distribution, main = "Distribution of Specialities", col = "lightblue", las = 2, cex.names = 0.8)

# Checking the distribution of Genders
gender_distribution <- table(Acinetobacter_regression$Gender)
barplot(gender_distribution, main = "Distribution of Genders", col = "pink", las = 2, cex.names = 0.8)

# Distribution of Antibiotic Resistance Levels
resistance_levels <- df %>% 
  summarise(Amikacin = table(Amikacin_I),
            Imipenem = table(Imipenem_I),
            Meropenem = table(Meropenem_I))


#Prediction
#install.packages("xgboost")
library(xgboost)

Acinetobacter_regression$Source <- as.numeric(as.factor(Acinetobacter_regression$Source))

Acinetobacter_regression$Imipenem_I <- as.numeric(as.factor(Acinetobacter_regression$Imipenem_I))

Acinetobacter_regression$Meropenem_I <- as.numeric(as.factor(Acinetobacter_regression$Meropenem_I))

Acinetobacter_regression$Source <- as.numeric(as.factor(Acinetobacter_regression$Source))

Acinetobacter_regression$Gender <- as.numeric(as.factor(Acinetobacter_regression$Gender))

Acinetobacter_regression$Speciality <- as.numeric(as.factor(Acinetobacter_regression$Speciality))

Acinetobacter_regression$Amikacin_I <- as.numeric(as.factor(Acinetobacter_regression$Amikacin_I
)) - 1

indexes <- caret::createDataPartition(Acinetobacter_regression$Amikacin_I
                                      , p = .75, list = F)
train_data <- Acinetobacter_regression[indexes, ]
test_data <- Acinetobacter_regression[-indexes, ]

xgb.train <- xgb.DMatrix(data = as.matrix(train_data), label = train_data$Amikacin_I)
xgb.test <- xgb.DMatrix(data = as.matrix(test_data), label = test_data$Amikacin_I)

params = list("objective" = "multi:softprob", 
              "eval_metric" = "mlogloss",
              "num_class" = 3)

xgb.model <- xgboost::xgb.train(params = params, data = xgb.train, nrounds = 1000)
pred<- predict(xgb.model, newdata = xgb.test)
pred <- predict(xgb.model, newdata = xgb.test, type = "prob")

#Convert to matrix
pred <- predict(xgb.model, newdata = xgb.test, type = "prob", reshape = TRUE)
pred <- data.frame(pred)
names(pred) <- c("Intermediate", "Resistant", "Susceptible")

pred_Final <- apply(pred, 1L, which.max)
pred_Final[pred_Final == 1] <- "Intermediate"
pred_Final[pred_Final == 2] <- "Resistant"
pred_Final[pred_Final == 3] <- "Susceptible"
pred_Final

#Roc curve
library(pROC)
library(ROCR)
library(caret)
# Plot ROC curve using pROC
preds <- predict(xgb.model, newdata = xgb.test, type = "prob", reshape = TRUE)
preds <- apply(preds, 1L, which.max)
roc_obj <- roc(test_data$Amikacin_I, preds)
plot(roc_obj, col = "#1c61b6", main = "ROC Curve for XGBoost Model (pROC)", legacy.axes = TRUE)
auc(roc_obj) # Calculate AUC

# Plot ROC curve using ROCR
pred <- prediction(preds, test_data$Amikacin_I)
perf <- performance(pred, "tpr", "fpr")

# Plot
plot(perf, col = "#1c61b6", main = "ROC Curve for XGBoost Model (ROCR)")
abline(a = 0, b = 1, col = "gray", lty = 2)


#Random Forest
library(randomForest)
library(pROC)

rf = randomForest(Amikacin_I~., data = Acinetobacter_regression , ntree = 1000)
# predict(.., type = 'prob') returns a probability matrix
predictions <- as.numeric(predict(rf, Acinetobacter_regression, type = 'response'))
roc.multi <- multiclass.roc(Acinetobacter_regression$Amikacin_I, predictions)
auc(roc.multi)
rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

#Select Escherichia colibacteria bacteria
Escherichia_coli <- subset(merged_data2, Species == "Escherichia coli")

#Select Klebsiella pneumoniae  bacteria
Klebsiella_pneumoniae  <- subset(merged_data2, Species == "Klebsiella pneumoniae")

#Final dataset
Klebsiella_pneumoniae_Final <- data.frame(Klebsiella_pneumoniae[,c(6,8:10,13,15,20,24,48:53)])
names(Klebsiella_pneumoniae_Final)
