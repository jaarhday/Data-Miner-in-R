# Function to load required libraries
load_libraries <- function() {
  library(arules)
}

# Function to read the CSV file and return a dataframe
read_transactions <- function(file_path) {
  data <- read.csv(file_path, header = TRUE)
  return(data)
}

# Function to preprocess data
preprocess_data <- function(data) {
  # Remove duplicated rows
  data <- unique(data)

  # Convert Item column to factors
  data$Item <- as.factor(data$Item)

  return(data)
}

# Function to convert dataframe into transactions format
convert_to_transactions <- function(data) {
  transactions <- as(data, "transactions")
  return(transactions)
}

# Function to run Apriori algorithm and return rules
run_apriori <- function(transactions, min_support = 0.05, 
                        min_confidence = 0.7) {
  rules <- apriori(transactions, parameter = list(support = min_support,
                                                  confidence = min_confidence))
  return(rules)
}

# Function to test association rules
test_association_rules <- function(transactions, rules, itemset, outcome) {
  # Convert itemset and outcome to lowercase to ensure case insensitivity
  itemset <- tolower(itemset)
  outcome <- tolower(outcome)

  # Check if itemset and outcome are present in transactions
  if (!all(itemset %in% colnames(as(transactions, "matrix")))) {
    stop("Itemset contains items not present in transactions.")
  }
  if (!outcome %in% colnames(as(transactions, "matrix"))) {
    stop("Outcome item not present in transactions.")
  }

  # Filter transactions containing itemset
  itemset_transactions <- transactions[rowSums(transactions[, itemset]) == length(itemset), ]

  # Filter transactions containing itemset and outcome
  outcome_transactions <- itemset_transactions[rowSums(itemset_transactions[, outcome]) > 0, ]

  # Calculate support for itemset
  support_itemset <- nrow(itemset_transactions) / nrow(transactions)

  # Calculate support for outcome
  support_outcome <- nrow(outcome_transactions) / nrow(transactions)

  # Calculate support for itemset and outcome
  support_itemset_outcome <- nrow(outcome_transactions) / nrow(transactions)

  # Calculate confidence
  confidence <- support_itemset_outcome / support_itemset

  # Print results
  cat("Support for", paste(itemset, collapse = " & ")
      , ":", support_itemset, "\n")

  cat("Support for", outcome, ":", support_outcome, "\n")
  cat("Support for", paste(itemset, collapse = " & "), "and", 
      outcome, ":", support_itemset_outcome, "\n")
  cat("Confidence for", paste(itemset, collapse = " & ")
      , "->", outcome, ":", confidence, "\n")
}

# Main function to orchestrate the entire process
main <- function(file_path, min_support = 0.05, min_confidence = 0.7,
                 itemset = c("Bread", "Coffee"), outcome = "Pastry") {
  # Load libraries
  load_libraries()

  # Read transactions
  data <- read_transactions(file_path)

  # Preprocess data
  data <- preprocess_data(data)

  # Convert to transactions
  transactions <- convert_to_transactions(data)

  # Run Apriori algorithm
  rules <- run_apriori(transactions, min_support, min_confidence)

  # Test association rules
  test_association_rules(transactions, rules, itemset, outcome)
}

# Call the main function with the file path
main("BreadBasket_DMS.csv")