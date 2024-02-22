#Annika Reinholdt

#### TO DO ####

# Note: pls make it into tidyvers in the future 
# Note: pls change the collum names, so it makes more sense 

# Note: Not all code chuncks was relevant from the script laura provided, so i have used some of it 
# and added new things


# load libraries
library("dplyr")
library("ggplot2")
library("stringr")
library("tidyr")
library("reshape2")

# read CSV file with semicolon delimiter
LV_SS <- read.csv("C:/Users/45282/OneDrive/Skrivebord/dorsal_2022_all_R.csv", 
                  sep = ";",
                  header = TRUE,
                  colClasses = c("Date.Original" = "character"))


#### RE-SIGHTING ###
# Note: only using the left side catalouge here 
# Note: OBS: check with the catalouge.. by counting and select folder


# ID in more than 1 folder = 1 encounter (obs: all folder is a new encounter)
results <- LV_SS %>%
  distinct(Title, Folder) %>%
  group_by(Title) %>%
  filter(grepl("left", Keyword.export) %>%
  summarise(num_folders = n()) %>%
  filter(num_folders > 1)

num_titles <- nrow(results)

print(num_titles)


# remove duplicates and count number of encounters
results_2 <- LV_SS %>%
  distinct(Title, Folder) %>%
  group_by(Title) %>%
  summarise(num_encounters = n()) %>%
  filter(num_encounters > 1)

print(results_2)


### DYNAMICS ###
# do whales go together ? - ID LIST AND NUMBER 

results_3 <- LV_SS %>%
  group_by(Folder) %>%
  summarise(num_ids = n_distinct(Title), 
            ids = paste(unique(Title), collapse = ",")) %>%
  group_by(num_ids, ids) %>%
  summarise(num_folders = n())

print(results_3)


#KK

#results_4 <- LV_SS %>%
#  distinct(Title, Folder) %>%
#  group_by(Title) %>%
#  filter(n() > 1) %>%
#  group_by(Folder) %>%
#  summarise(num_titles = n_distinct(Title), 
#            titles = paste(unique(Title), collapse = ",")) %>%
#  group_by(num_titles, titles) %>%
#  summarise(num_folders = n())

#print(results_4)



### Reliable marks - Notch and indent ###

# this is for both side - right and left.
# if only looking at one side add:   filter(grepl("left", Keyword.export) %>%


# calculate proportion of notches 
LV_SS <- LV_SS %>% 
  mutate(QRATE = ifelse(grepl("\\* \\* \\* \\*", Rating), "4", 
                        ifelse(grepl("\\* \\* \\*", Rating), "3",
                               ifelse(grepl("\\* \\*", Rating), "2", 
                                      ifelse(grepl("\\*", Rating), "1", "??"))))) %>%
  filter(as.numeric(QRATE) >= 3)


if (nrow(LV_SS) == 0) {
  prop_with_notch <- 0
} else {
  prop_with_notch <- LV_SS %>% 
    filter(grepl("notch", Keyword.export)) %>% 
    nrow() / nrow(LV_SS)
}

print('this is the proportion of notches', prop_with_notch)



# calculate proportion of indent
# OBS!: Next time you have to use the same keyword, whenever there is a indent. 


LV_SS <- LV_SS %>% 
  mutate(QRATE = ifelse(grepl("\\* \\* \\* \\*", Rating), "4", 
                        ifelse(grepl("\\* \\* \\*", Rating), "3",
                               ifelse(grepl("\\* \\*", Rating), "2", 
                                      ifelse(grepl("\\*", Rating), "1", "??"))))) %>%
  filter(as.numeric(QRATE) >= 3)

if (nrow(LV_SS) == 0) {
  prop_with_notch <- 0
} else {
  prop_with_notch <- LV_SS %>% 
    filter(grepl("indent", Keyword.export)) %>% 
    nrow() / nrow(LV_SS)
}

print('this is the proportion of indent', prop_with_notch)



### Social network analyses ###

## Note: not complete or running yet!! 

LV_SS2 <- LV_SS %>% 
  mutate_all(function(x) as.numeric(as.character(x))) # convert all columns to numeric

LV_SS_wide <- LV_SS2 %>% pivot_wider(names_from = Title, values_from = Title, values_fill = 0)

# Compute the covariance matrix for Title (ID)  vs Folder (ENCOUNTER)
LV_SS_cov <- cov(LV_SS_wide[, c("Folder", colnames(LV_SS_wide)[-1])])

# Set diagonal elements to 1
diag(LV_SS_cov) <- 1

# Write the covariance matrix to a CSV file
write.csv(LV_SS_cov, "LV_SS_Title_Folder_cov_matrix.csv")

# Subset the data to only include the ID and Folder columns
LV_SS_subset <- LV_SS[, c("Title", "Folder")]

# Create a binary matrix indicating whether each ID appears in each Folder
LV_SS_matrix <- table(LV_SS_subset)
LV_SS_matrix_binary <- as.matrix((LV_SS_matrix > 0) * 1)

# Compute the covariance matrix
LV_SS_cov <- cov(LV_SS_matrix_binary)

# Set diagonal elements to 1
diag(LV_SS_cov) <- 1

# Convert the covariance matrix back to a data frame and add the ID names as row and column names...
LV_SS_cov_df <- data.frame(LV_SS_cov)
rownames(LV_SS_cov_df) <- colnames(LV_SS_matrix_binary)
colnames(LV_SS_cov_df) <- colnames(LV_SS_matrix_binary)

# Write the data frame to a CSV file
write.csv(LV_SS_cov_df, "LV_SS_cov_matrix.csv", row.names = TRUE)

# Create a new data frame with only the Folder and Title columns
LV_SS_adj <- LV_SS[, c("Folder", "Title")]

# Remove duplicate rows
LV_SS_adj <- distinct(LV_SS_adj)

# Pivot the data to create a wide matrix
LV_SS_wide <- LV_SS_adj %>%
  pivot_wider(names_from = Title, values_from = Title, values_fn = list(Title = ~as.numeric(length(.))), values_fill = list(Title = 0))

# Transpose the matrix
LV_SS_transpose <- t(LV_SS_wide[, -1])

# create the adjacency matrix
LV_SS_adj_matrix <- LV_SS_transpose %*% LV_SS_wide[, -1]

# adjacency matrix to a CSV file
write.csv(LV_SS_adj_matrix, "LV_SS_adj_matrix.csv")


#library(igraph)
#LV_SS_adj <- as_adjacency_matrix(graph_from_adjacency_matrix(as.matrix(LV_SS_wide[, -1])), sparse = FALSE)

#LV_SS_wide <- LV_SS %>% 
#  pivot_wider(names_from = Title, values_from = Folder, values_fill = 0, values_fn = length)

