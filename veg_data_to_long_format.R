pq <- read.csv("~/Desktop/Projects/veg_data_code/veg_data_code/dfrb_point_quarter.csv") #import wide point-qaurter data

library(tidyverse)
library(tidyr)
library(dplyr)


# remove the area columns (redundant with DBH)
pq <- pq %>%
  select(-starts_with("Area"))  



# Identify tree columns that match the pattern
tree_to_rename <- grep("^Center\\.tree\\.|^Rand\\.tree\\.", current_names, value = TRUE)

# Create new names for these columns
new_tree_names <- paste0("Tree.", seq_along(tree_to_rename))

# Rename only the matched columns
names(pq)[colnames(pq) %in% tree_to_rename] <- new_tree_names


# Identify DBH columns that match the pattern
dbh_to_rename <- grep("^DBH", current_names, value = TRUE)

# Create new names for these columns
new_dbh_names <- paste0("DBH.", seq_along(dbh_to_rename))

# Rename only the matched columns
names(pq)[colnames(pq) %in% dbh_to_rename] <- new_dbh_names


# Identify Dist columns that match the pattern
dist_to_rename <- grep("^Dist", current_names, value = TRUE)

# Create new names for these columns
new_dist_names <- paste0("Dist.", seq_along(dist_to_rename))

# Rename only the matched columns
names(pq)[colnames(pq) %in% dist_to_rename] <- new_dist_names








# Define column patterns for Tree, DBH, and Distance
tree_columns <- grep("^Tree\\.", colnames(pq), value = TRUE)
dbh_columns <- grep("^DBH", colnames(pq), value = TRUE)
dist_columns <- grep("^Dist", colnames(pq), value = TRUE)


# Extract numeric index from column names
extract_index <- function(colname) {
  as.integer(gsub(".*?(\\d+)$", "\\1", colname))
}

# Reshape the tree columns into long format
long_data_trees <- pq %>%
  pivot_longer(
    cols = all_of(tree_columns),
    names_to = "Tree_Type",
    values_to = "Tree"
  ) %>%
  mutate(Tree_Index = extract_index(Tree_Type)) %>%
  select(Point.ID, Tree_Index, Tree)

# Reshape the DBH columns into long format
long_data_dbh <- pq %>%
  pivot_longer(
    cols = all_of(dbh_columns),
    names_to = "DBH_Type",
    values_to = "DBH"
  ) %>%
  mutate(DBH_Index = extract_index(DBH_Type)) %>%
  select(Point.ID, DBH_Index, DBH)

# Reshape the Distance columns into long format
long_data_dist <- pq %>%
  pivot_longer(
    cols = all_of(dist_columns),
    names_to = "Dist_Type",
    values_to = "Distance"
  ) %>%
  mutate(Dist_Index = extract_index(Dist_Type)) %>%
  select(Point.ID, Dist_Index, Distance)

# Combine all long data frames by matching indexes
combined_data <- long_data_trees %>%
  left_join(long_data_dbh, by = c("Point.ID", "Tree_Index" = "DBH_Index")) %>%
  left_join(long_data_dist, by = c("Point.ID", "Tree_Index" = "Dist_Index")) %>%
  filter(!is.na(Tree)) %>%  # Remove rows where Tree is NA (indicating no valid tree data)
  select(Point.ID, Tree, DBH, Distance) %>%
  distinct()


View(combined_data)

