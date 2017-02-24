# install.packages('dplyr')
# install.packages('tidyr')
# install.packages('readr')
library(dplyr)
library(tidyr)
library(readr)

data <- read_csv('./refine_original.csv')
data <- tbl_df(data)

# Mapper function for product code to product category
get_product_category <- function(product_code) {
  category <- switch(product_code, p = "Smartphone", v = "TV", x = "Laptop", q = "Tablet")
  return(category)
}

# Fix misspellings in the company name with regex
clean_company_name <- function(company_name) {
  name <- tolower(company_name)
  if (grepl("^(ph|f)[il]+ps$", name)) {
    name <- sub("^(ph|f)[il]+ps$", "philips", name)
  } else if (grepl("^ak", name)) {
    name <- sub("^ak.+$", "akzo", name)
  } else if (grepl("^unil", name)) {
    name <- sub("^unil.+$", "unilever", name)
  }
  return(name)
}

data <- data %>%
  separate("Product code / number", c("product_code", "product_number"), sep = "-") %>%
  unite("full_address", address, city, country, sep = ",") %>%
  mutate(company = sapply(company, clean_company_name), product_category = sapply(product_code, get_product_category))

# Create a one-hot encoded matrix for the product category
one_hot_product_category <- tbl_df(model.matrix(~ product_category + 0, data = data))
names(one_hot_product_category) <- sub('^product_category', 'product_', tolower(names(one_hot_product_category)))

# Create a one-hot encoded matrix for the company name
one_hot_company <- tbl_df(model.matrix(~ company + 0, data = data))
names(one_hot_company) <- sub('^company', 'company_', sub(' ', '_', tolower(names(one_hot_company))))

# Merge the columns from the one-hot encoded matrices into our data frame
data <- data %>%
  bind_cols(one_hot_product_category) %>%
  bind_cols(one_hot_company)

write_csv(data, './refine_clean.csv')
