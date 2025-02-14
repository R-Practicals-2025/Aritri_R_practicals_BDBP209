#(5) Practice with subsets.
# Form a vector with elements
vector <- c(8, 10, 12, 7, 14, 16, 2, 4, 9, 19, 20, 3, 6)
# (a) Values greater than 12
filtered_a <- vector[vector > 12]
print(filtered_a)
# (b) Values greater than 10 and less than 20
filtered_b <- vector[vector > 10 & vector < 20]
print(filtered_b)
# Form an array with the given elements
A <- c(2, 7, 29, 32, 41, 11, 15, NA, NA, 55, 32, NA, 42, 109)
# Create a new array without NAs and keeping values less than 100
A_filtered <- A[!is.na(A) & A < 100]
print(A_filtered)
# Replace NA with 0 in array A
A_no_na <- ifelse(is.na(A), 0, A)
print(A_no_na)
# Create a vector of gene names
genes <- paste("gene", 1:7, sep="-")
# Create a vector for gender
gender <- c("M", "M", "F", "M", "F", "F", "M")
print(genes)
print(gender)
# Define 7 result vectors
result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)
datframe=data.frame(genes = genes, gender = gender, result1 = result1, result2 = result2, result3 = result3, result4 = result4, result5 = result5, result6 = result6, result7 = result7)
print(datframe)
# Add new column names to the dataframe
colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")
print(datframe)
# Create a subset where expt2 > 20
subset_expt2_gt_20 <- subset(datframe, expt2 > 20)
print(subset_expt2_gt_20)
# Create a subset where gender is Female
subset_female <- subset(datframe, Gender == "F")
print(subset_female)
# Create a subset where gender is Male and expt2 < 30
subset_male_expt2_lt_30 <- subset(datframe, Gender == "M" & expt2 < 30)
print(subset_male_expt2_lt_30)















