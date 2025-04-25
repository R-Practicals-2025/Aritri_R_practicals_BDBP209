# (1) ANOVA test on people on the Titanic ship
# (a) Read in the data file called titanic.csv. Make histogram plots of groups of
# people marked as ‘1st’, ‘2nd’ and ‘3rd’ (use about 30 bins) to check whether the
# three samples have approximately the same variance and are looking approximately
# normal. Then we are justified in using ANOVA. Make the plots in a 3x1 grid. Our
# null hypothesis is that the mean age is same among the 3 groups of people.

# (1a) Read data and create histograms
titanicData <- read.csv("/home/ibab/Downloads/titanic.csv")

# Create histograms in 3x1 grid
par(mfrow = c(3, 1))  # Set up 3x1 plotting grid

hist(titanicData$Age[titanicData$Pclass == 1], breaks = 30, 
     main = "Age Distribution - 1st Class", xlab = "Age", col = "lightblue")
hist(titanicData$Age[titanicData$Pclass == 2], breaks = 30, 
     main = "Age Distribution - 2nd Class", xlab = "Age", col = "lightgreen")
hist(titanicData$Age[titanicData$Pclass == 3], breaks = 30, 
     main = "Age Distribution - 3rd Class", xlab = "Age", col = "lightpink")

par(mfrow = c(1, 1))  # Reset plotting grid



# (b) To quantitatively verify the equal variance assumption, we are going to determine
# the mean and standard deviations from each group. Load the package dplyr, we
# will use two functions group_by() and summarise(). Study the output of the
# following commands:
# titanic_by_passenger_class<- group_by(titanicData,passenger_class)
# summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),group_sd=sd(age,na.rm=TRUE)
# What do you find? Are the standard deviations similar between the groups? Print a statement showing the conclusion of this comparison.
          
# (1b) Check group means and standard deviations
library(dplyr)

titanic_by_passenger_class <- group_by(titanicData, Pclass)
group_stats <- summarise(titanic_by_passenger_class, 
                         group_mean = mean(Age, na.rm = TRUE),
                         group_sd = sd(Age, na.rm = TRUE))

print(group_stats)

# Print conclusion about variances
if (max(group_stats$group_sd)/min(group_stats$group_sd) < 2) {
  print("The standard deviations are similar enough between groups (ratio < 2) to proceed with ANOVA.")
} else {
  print("The standard deviations differ substantially between groups, violating ANOVA's equal variance assumption.")
}





# (c) We fit the ANOVA model to the data using lm() function. This function takes
# a formula and data frame as arguments. A model formula takes the form of a
# response variable followed by a tilde( ) and then at least one explanatory variable.
# Here we will give age~passenger_class which tells R to ‘fit’ a model in which
# age of passengers are grouped by the variable passenger_class. The command therefore is
# lmresults <- lm(age~passenger_class, data=titanicData)
# anova(lmresults)
# The anova() function returns the ANOVA table as output. What is your statistical
# inference/decision from the table, and therefore what is the statistical conclusion?
  
# (1c) Perform ANOVA
lmresults <- lm(Age ~ factor(Pclass), data = titanicData)
anova_results <- anova(lmresults)
print(anova_results)

# Print conclusion
if (anova_results$`Pr(>F)`[1] < 0.05) {
  print("We reject the null hypothesis - there is significant evidence that mean ages differ between passenger classes.")
} else {
  print("We fail to reject the null hypothesis - there is no significant evidence of difference in mean ages between passenger classes.")
}
  
# (d) The ANOVA tells us that at least one group has a mean different from the others,
#             but does not tell us which group means are actually different. A Tukey-Kramer’s
#             test tests the null hypothesis that there is no difference between the population
#             means of all pairs of groups. This is invoked in R by using TukeyHSD() function.
#             Execute the following command;
#             TukeyHSD(aov(lmresults))
#             Look at the columns labeled ‘diff’ and ‘p adj’. The p-values are calculated using a
#             95% confidence interval, and ‘lwr’ and ‘upr’ denote lower and upper bounds of the
#             interval. From the output you should be able to see that the CIs do not include
#             zero, and since the p-value is less than 0.05 in all the cases, the H0 is rejected for all
#             pairs, and we will conclude that the means of the three populations are significantly
#             different from each other.
#            
            
 # (1d) Tukey-Kramer post-hoc test
            tukey_results <- TukeyHSD(aov(lmresults))
            print(tukey_results)
            
            
            
            
            
# (e) Let us also perform a Kruskal-Wallis test for the above data since the test does not
#             need us to assume normal distribution like ANOVA. Execute
#             kruskal.test(age~passenger,data=titanidData). Check whether the p value
#             leads to the same conclusion as the parametric test above.
#             
            
            # (1e) Kruskal-Wallis test
            kruskal_results <- kruskal.test(Age ~ factor(Pclass), data = titanicData)
            print(kruskal_results)
            
            # Compare with ANOVA conclusion
            if (kruskal_results$p.value < 0.05) {
              print("Kruskal-Wallis test agrees with ANOVA - significant differences exist between groups.")
            } else {
              print("Kruskal-Wallis test disagrees with ANOVA - no significant differences found.")
            }
            
################################################################################################################################
            
            
# (2) Cuckoo egg size problem:
# (a) The European cuckoo does not look after its own eggs, but instead lays them in
# the nests of birds of other species. Previous studies showed that cuckoos sometimes
# have evolved to lay eggs that are colored similarly to the host bird species’ eggs.
# Is the same true of egg size – do cuckoos lay eggs similar in size to the size of the
# eggs of their hosts? The data file “cuckooeggs.csv” contains data on the lengths
# of cuckoo eggs laid in the nests of a variety of host species. Here we compare the
# mean size of cuckoo eggs found in the nests of different host species. Plot a multiple
# histogram showing cuckoo egg lengths by host species.
            
# Load required packages
library(ggplot2)
library(dplyr)
            
            cuckooData <- read.csv("/home/ibab/Downloads/cuckooeggs.csv")
            
            # Create multiple histogram
            ggplot(cuckooData, aes(x = egg_length, fill = host_species)) +
              geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
              facet_wrap(~ host_species, ncol = 1) +
              labs(title = "Cuckoo Egg Lengths by Host Species",
                   x = "Egg Length (mm)", y = "Count") +
              theme(legend.position = "none")
            
           
            
           
            
            
            
            
            
# (b) Calculate a table that shows the mean and standard deviation of length of cuckoo
# eggs for each host species. Look at the graph and the table. For these data, would
# ANOVA be a valid method to test for differences between host species in the lengths
# of cuckoo eggs in their nests?
            
            
            host_stats <- cuckooData %>%
              group_by(host_species) %>%
              summarise(
                n = n(),
                mean_length = mean(egg_length, na.rm = TRUE),
                sd_length = sd(egg_length, na.rm = TRUE),
                se_length = sd_length/sqrt(n)
              )
            
            print(host_stats)
            
            # Check ANOVA assumptions
            sd_ratio <- max(host_stats$sd_length)/min(host_stats$sd_length)
            if (sd_ratio < 2) {
              cat("\nThe standard deviations are similar enough between groups (ratio =", 
                  round(sd_ratio, 2), "< 2) to proceed with ANOVA.\n")
            } else {
              cat("\nWarning: The standard deviations differ substantially between groups (ratio =",
                  round(sd_ratio, 2), "≥ 2), which may violate ANOVA's equal variance assumption.\n")
            }            
            
# (c) Use ANOVA to test for a difference between host species in the mean size of the
# cuckoo eggs in their nests. What is your conclusion?
# 

            anova_model <- lm(egg_length ~ host_species, data = cuckooData)
            anova_results <- anova(anova_model)
            print(anova_results)
            
            # Print conclusion
            if (anova_results$`Pr(>F)`[1] < 0.05) {
              cat("\nReject the null hypothesis (p =", anova_results$`Pr(>F)`[1], 
                  "): There is significant evidence that mean cuckoo egg lengths differ between host species.\n")
            } else {
              cat("\nFail to reject the null hypothesis (p =", anova_results$`Pr(>F)`[1], 
                  "): No significant evidence of difference in mean cuckoo egg lengths between host species.\n")
            }           
            
# (d) Assuming that ANOVA rejected the null hypotheses of no mean differences, use a
# Tukey-Kramer test to decide which pairs of host species are significantly different
# from each other in cuckoo egg mean length. What is your conclusion?
            
            if (anova_results$`Pr(>F)`[1] < 0.05) {
              tukey_results <- TukeyHSD(aov(anova_model))
              print(tukey_results)
              
              # Visualize the pairwise comparisons
              plot(tukey_results, las = 2)
              
              # Count significant pairs
              sig_pairs <- sum(tukey_results$host_species[, "p adj"] < 0.05)
              total_pairs <- nrow(tukey_results$host_species)
              cat("\n", sig_pairs, "out of", total_pairs, 
                  "host species pairs show statistically significant differences in mean cuckoo egg length.\n")
              
              # Print significant pairs
              sig_results <- as.data.frame(tukey_results$host_species) %>%
                filter(`p adj` < 0.05) %>%
                arrange(`p adj`)
              
              if (nrow(sig_results) > 0) {
                cat("\nSignificant pairs (ordered by p-value):\n")
                print(sig_results)
              } else {
                cat("\nNo pairs show statistically significant differences after adjustment.\n")
              }
            } else {
              cat("\nANOVA was not significant (p =", anova_results$`Pr(>F)`[1], 
                  "), so no post-hoc tests are needed.\n")
            }   
            
            
#########################################################################################################################
            
            
# (3) Maize and malaria problem:
# (a) The pollen of the maize (corn) plant is a source of food to larval mosquitoes of
# the species Anopheles arabiensis, the main vector of malaria in Ethiopia. The
# production of maize has increased substantially in certain areas of Ethiopia recently,
# and over the same time period, malaria has entered in to new areas where it was
# previously rare. This raises the question, is the increase of maize cultivation partly
# responsible for the increase in malaria?
# One line of evidence is to look for an association between maize production and
# malaria incidence at different geographically dispersed sites (Kebede et al. 2005).
# The data set “malaria vs maize.csv” contains information on several high-altitude
# sites in Ethiopia, with information about the level of cultivation of maize (low,
# medium or high in the variable maize yield) and the rate of malaria per 10,000
# people (incidence rate per ten thousand).
# Plot a multiple histogram to show the relationship between level of maize produc-tion and the incidence of malaria.
            
            
# Load required packages
library(ggplot2)
library(dplyr)
# library(multcomp)  # For post-hoc tests
malaria_data <- read.csv("/home/ibab/Downloads/malaria vs maize.csv")
# Convert maize_yield to ordered factor
malaria_data$maize_yield <- factor(malaria_data$maize_yield, levels = c("Low", "Medium", "High"),ordered = TRUE)
# Plot multiple histogram
ggplot(malaria_data, aes(x = incidence_rate_per_ten_thousand, fill = maize_yield)) +
geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
facet_wrap(~ maize_yield, ncol = 1) +
            labs(title = "Malaria Incidence Rate by Maize Yield Level",
            x = "Malaria Incidence Rate (per 10,000)", y = "Count") +
theme_minimal() +theme(legend.position = "none")
            
            
            
           
            
           
# (b) ANOVA is a logical choice of method to test differences in the mean rate of malaria
# between sites differing in level of maize production. Calculate the standard devi-ation of the incidence rate for each level of maize yield. Do these data seem to
# conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.

# Calculate summary statistics by maize yield level
yield_stats <- malaria_data %>%
group_by(maize_yield) %>%
summarise(n = n(),mean_rate = mean(incidence_rate_per_ten_thousand),sd_rate = sd(incidence_rate_per_ten_thousand),se_rate = sd_rate/sqrt(n))
print(yield_stats)
# Check variance assumption
sd_ratio <- max(yield_stats$sd_rate)/min(yield_stats$sd_rate)
if (sd_ratio < 2) {
cat("\nStandard deviation ratio:", round(sd_ratio, 2), "(< 2) - Variance assumption may be acceptable.\n")
} else {
cat("\nWarning: Standard deviation ratio:", round(sd_ratio, 2), "(≥ 2) - Variance assumption may be violated.\n")
}            
            
            
            
            
# (c) Compute the log of the incidence rate and redraw the multiple histograms for
# different levels of maize yield. Calculate the standard deviation of the log incidence
# rate for each level of maize yield. Does the log-transformed data better meet the
# assumptions of ANOVA than did the untransformed data?
            

# Add log-transformed incidence rate
malaria_data$log_incidence <- log10(malaria_data$incidence_rate_per_ten_thousand + 1)
# Plot log-transformed data
ggplot(malaria_data, aes(x = log_incidence, fill = maize_yield)) +
geom_histogram(position = "identity", alpha = 0.6, bins = 10) +
facet_wrap(~ maize_yield, ncol = 1) +
labs(title = "Log-Transformed Malaria Incidence by Maize Yield Level",
                   x = "Log10(Malaria Incidence Rate + 1)", y = "Count") +
theme_minimal() +theme(legend.position = "none")
# Calculate stats for log-transformed data
log_stats <- malaria_data %>%
group_by(maize_yield) %>%
summarise(n = n(),mean_log = mean(log_incidence),
                sd_log = sd(log_incidence),
                se_log = sd_log/sqrt(n)
              )
            
 print(log_stats)
            
# Check variance assumption after transformation
log_sd_ratio <- max(log_stats$sd_log)/min(log_stats$sd_log)
if (log_sd_ratio < 2) {
cat("\nAfter log transformation - Standard deviation ratio:", round(log_sd_ratio, 2), 
                  "(< 2) - Variance assumption improved.\n")
} else {
cat("\nAfter log transformation - Standard deviation ratio:", round(log_sd_ratio, 2), 
                  "(≥ 2) - Variance assumption still problematic.\n")
}            
            
# (d) Test for an association between maize yield and malaria incidence.
            
# Using original data (with potential assumption violations)
anova_original <- aov(incidence_rate_per_ten_thousand ~ maize_yield, data = malaria_data)
cat("\nANOVA results for original data:\n")
print(summary(anova_original))
# Using log-transformed data
anova_log <- aov(log_incidence ~ maize_yield, data = malaria_data)
cat("\nANOVA results for log-transformed data:\n")
print(summary(anova_log))
# Post-hoc tests if ANOVA is significant
if (summary(anova_log)[[1]]$'Pr(>F)'[1] < 0.05) {
cat("\nPost-hoc pairwise comparisons (Tukey HSD) for log-transformed data:\n")
              tukey_results <- TukeyHSD(anova_log)
print(tukey_results)
              
# Visualize pairwise comparisons
plot(tukey_results, las = 2)
# Count significant pairs
sig_pairs <- sum(tukey_results$maize_yield[, "p adj"] < 0.05)
cat("\nNumber of significant pairwise comparisons:", sig_pairs, "\n")
              
# Print significant pairs
if (sig_pairs > 0) {
cat("\nSignificant differences between:\n")
print(tukey_results$maize_yield[tukey_results$maize_yield[, "p adj"] < 0.05, ])
              }
            }
# Alternative non-parametric test (Kruskal-Wallis)
cat("\nKruskal-Wallis test results (non-parametric alternative):\n")
print(kruskal.test(incidence_rate_per_ten_thousand ~ maize_yield, data = malaria_data))

#####################################################################################################################################
            
# (4) Circadian rhythms of diseased animals:
#   (a) Animals that are infected with a pathogen often have disturbed circadian rhythms.
# (A circadian rhythm is an endogenous daily cycle in a behavior or physiological
#   trait that persists in the absence of time cues.) Shirasu-Hiza et al. (2007) wanted
# to know whether it was possible that the circadian timing mechanism itself could
# have an effect on disease. To test this idea they sampled from three groups of fruit
# flies: one “normal”, one with a mutation in the timing gene tim01, and one group
# that had the tim01 mutant in a heterozygous state. They exposed these flies to
# a dangerous bacteria, Streptococcus pneumoniae, and measured how long the flies
# lived afterwards, in days. The date file “circadian mutant health.csv” shows some
# of their data.
# Plot a histogram of each of the three groups. Do these data match the assumptions
# of an ANOVA?
  
  # Load required packages
  library(ggplot2)
library(dplyr)
library(ggpubr)  # For adding statistical comparisons to plots

# (4a) Read data and create histograms
circadian_data <- read.csv("circadian mutant health.csv")

# Clean genotype names (remove extra spaces)
circadian_data$genotype <- trimws(circadian_data$genotype)

# Create histograms for each genotype
ggplot(circadian_data, aes(x = days_to_death, fill = genotype)) +
  geom_histogram(bins = 20, color = "black") +
  facet_wrap(~ genotype, ncol = 1) +
  labs(title = "Days to Death by Genotype",
       x = "Days to Death", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Check ANOVA assumptions
# 1. Normality within each group
shapiro_tests <- circadian_data %>%
  group_by(genotype) %>%
  summarise(p_value = shapiro.test(days_to_death)$p.value)

print("Normality tests (Shapiro-Wilk):")
print(shapiro_tests)

# 2. Homogeneity of variances
levene_test <- car::leveneTest(days_to_death ~ genotype, data = circadian_data)
print("Homogeneity of variances test (Levene's test):")
print(levene_test)

# Visual assessment of assumptions
# Q-Q plots for normality check
ggqqplot(circadian_data, x = "days_to_death", facet.by = "genotype") +
  ggtitle("Q-Q Plots by Genotype")

# Boxplot for variance visualization
ggplot(circadian_data, aes(x = genotype, y = days_to_death, fill = genotype)) +
  geom_boxplot() +
  labs(title = "Days to Death by Genotype",
       x = "Genotype", y = "Days to Death") +
  theme_minimal()


  
  
#   
#   (b) Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups
# of flies.            
#             
            
# (4b) Kruskal-Wallis test
kruskal_result <- kruskal.test(days_to_death ~ genotype, data = circadian_data)
print("Kruskal-Wallis test results:")
print(kruskal_result)

# If significant, perform pairwise comparisons
if (kruskal_result$p.value < 0.05) {
  print("Pairwise Wilcoxon rank sum tests with Holm adjustment:")
  pairwise_results <- pairwise.wilcox.test(circadian_data$days_to_death, 
                                           circadian_data$genotype,
                                           p.adjust.method = "holm")
  print(pairwise_results)
  
  # Add significance levels to plot
  comparison_plot <- ggplot(circadian_data, aes(x = genotype, y = days_to_death, fill = genotype)) +
    geom_boxplot() +
    stat_compare_means(method = "kruskal.test", label.y = max(circadian_data$days_to_death) * 1.1) +
    stat_compare_means(comparisons = list(c("tim01", "tim01 (rescued)"), 
                                          c("tim01", "wild type"),
                                          c("tim01 (rescued)", "wild type")),
                       method = "wilcox.test") +
    labs(title = "Days to Death by Genotype with Statistical Comparisons",
         x = "Genotype", y = "Days to Death") +
    theme_minimal()
  
  print(comparison_plot)
}          
            
            
            
            
            
            
            
            
            
            