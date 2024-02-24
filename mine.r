# Load required library
library(arules)

# Read the CSV file
data <- read.csv("BreadBasket_DMS.csv")

# Remove duplicated rows
data <- unique(data)

# Convert Item column to factors
data$Item <- as.factor(data$Item)

# Convert data into transactions format
transactions <- as(data, "transactions")

# Inspect the transactions
summary(transactions)

# Run Apriori algorithm
min_support <- 0.05
min_confidence <- 0.7
rules <- apriori(transactions, parameter = list(support = min_support, 
                                                confidence = min_confidence))

# Print summary of discovered rules
summary(rules)

# Display top rules
inspect(head(rules))


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
  cat("Support for", paste(itemset, collapse = " & "), ":", 
      support_itemset, "\n")

  cat("Support for", outcome, ":", support_outcome, "\n")

  cat("Support for", paste(itemset, collapse = " & "), "and", 
      outcome, ":", support_itemset_outcome, "\n")

  cat("Confidence for", paste(itemset, collapse = " & "), "->", 
      outcome, ":", confidence, "\n")
}

# Test association rules
test_association_rules(transactions, rules, c("Bread", "Coffee"), "Pastry")