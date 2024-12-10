# ----------------------------------------------------
# Cannabis Voter Turnout Project
# Starter Script
# Author: Angela Scott
# Date: December 11, 2024
# ----------------------------------------------------

# Set up the working directory
setwd("C:/Users/angelascott/OneDrive - NOVA GROUP GBC/Documents/ANGIE/Scott School/PSCI Research Project/Cannabis_Voter_Turnout")

# Install required packages
install.packages(c("dplyr", "readxl", "ggplot2", "lubridate", "DBI"))
install.packages(c("RSQLite"))
install.packages(c("tidyr"))
install.packages("reshape2")
install.packages ("broom")
install.packages("stargazer")
install.packages("modelsummary")
install.packages("webshot")

# Load required libraries
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files
library(ggplot2)   # For data visualization
library(lubridate) # For date handling
library(DBI)
library(RSQLite)
library(readr)
library(tidyr)
library(reshape2)
library(broom)
library(stargazer)
library(modelsummary)
library(webshot)

# Snapshot the environment
if ("renv" %in% installed.packages()) {
  renv::snapshot()
}

# Create an SQLite database
con <- dbConnect(SQLite(), "data/project_database.sqlite")

# Confirm connection
print(con)

# Reinitialize Git repository in the project folder
system("git init")

# Initialize renv
renv::init()

# New session 12.11.24 to complete final project/paper

# Read all data files into dataframes
legalization_data <- read.csv("legalization_data.csv")
states_gen_data <- read.csv("states_gen_data.csv")
analyses_event_group <- read.csv("analyses_event_group.csv")
control_2020_pres_event <- read.csv("control_2020_pres_event.csv")
control_2016_pres_event <- read.csv("control_2016_pres_event.csv")
control_2012_pres_event <- read.csv("control_2012_pres_event.csv")
pres_turnout_2008 <- read.csv("pres_turnout_2008.csv")
pres_turnout_2012 <- read.csv("pres_turnout_2012.csv")
pres_turnout_2016 <- read.csv("pres_turnout_2016.csv")
pres_turnout_2020 <- read.csv("pres_turnout_2020.csv")
pres_turnout_2024 <- read.csv("pres_turnout_2024.csv")
mid_turnout_2006 <- read.csv("mid_turnout_2006.csv")
mid_turnout_2010 <- read.csv("mid_turnout_2010.csv")
mid_turnout_2014 <- read.csv("mid_turnout_2014.csv")
mid_turnout_2018 <- read.csv("mid_turnout_2018.csv")
mid_turnout_2022 <- read.csv("mid_turnout_2022.csv")
master_turnout <- read.csv("master_turnout.csv")

# Check the structure of the tables
str(master_turnout)
str(legalization_data)
str(analyses_event_group)
str(pres_turnout_2008)
str(pres_turnout_2012)
str(pres_turnout_2016)
str(pres_turnout_2020)
str(pres_turnout_2024)
str(mid_turnout_2006)
str(mid_turnout_2010)
str(mid_turnout_2014)
str(mid_turnout_2018)
str(mid_turnout_2022)
str(control_2020_pres_event)
str(control_2016_pres_event)
str(control_2012_pres_event)

# Preview the first few rows
head(master_turnout)
head(legalization_data)

# Remove the 'X' from column names
colnames(master_turnout) <- gsub("^X", "", colnames(master_turnout))

# Check the updated column names
colnames(master_turnout)

# 87-400 INITIAL ANALYSES for presentation
# Drop 8 rows in analyses_event_group (voter turnout data pending for odd years)
analyses_event_group <- analyses_event_group %>%
  drop_na()

# Check the first few rows
head(analyses_event_group)

# Count the rows
nrow(analyses_event_group)

# Verify the updated dataframe
summary(analyses_event_group)

# Add group identifiers to each dataframe
event_data <- analyses_event_group %>% mutate(group_type = "Event")
control_2012 <- control_2012_pres_event %>% mutate(group_type = "Control 2012")
control_2016 <- control_2016_pres_event %>% mutate(group_type = "Control 2016")
control_2020 <- control_2020_pres_event %>% mutate(group_type = "Control 2020")

# Combine all into one dataframe
combined_data <- bind_rows(event_data, control_2012, control_2016, control_2020)

# H1 visual
# Step 1: Add a column to classify Event vs. Control
combined_data$binary_group <- ifelse(combined_data$group_type == "Event", "Event", "Control")

# Step 2: Create a boxplot comparing primary_event_turnout
library(ggplot2)  # Load ggplot2 for plotting

ggplot(combined_data, aes(x = binary_group, y = primary_event_turnout, fill = binary_group)) +
  geom_boxplot() +
  labs(
    title = "Voter Turnout: Event vs. Control States",
    x = "Group Type",
    y = "Primary Event Turnout"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "none")

# Statistical support for H1 - T-Test
t_test <- t.test(primary_event_turnout ~ binary_group, data = combined_data)
print(t_test)

# H1 - T-Test visualization
# Step 1: Calculate group means and confidence intervals
library(dplyr)

t_test_summary <- combined_data %>%
  group_by(binary_group) %>%
  summarise(
    mean_turnout = mean(primary_event_turnout, na.rm = TRUE),
    sd_turnout = sd(primary_event_turnout, na.rm = TRUE),
    n = n(),
    se_turnout = sd_turnout / sqrt(n),  # Standard error
    lower_ci = mean_turnout - qt(1 - 0.05 / 2, n - 1) * se_turnout,
    upper_ci = mean_turnout + qt(1 - 0.05 / 2, n - 1) * se_turnout
  )

print(t_test_summary)

# Step 2: Plot the means with error bars
ggplot(t_test_summary, aes(x = binary_group, y = mean_turnout, fill = binary_group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(
    title = "Mean Voter Turnout with Confidence Intervals",
    x = "Group Type",
    y = "Mean Primary Event Turnout",
    fill = "Group Type"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

t_test <- t.test(primary_event_turnout ~ binary_group, data = combined_data)
print(t_test)

t_test <- t.test(primary_event_turnout ~ binary_group, data = combined_data)
t_test$p.value

# Data for plotting
t_test_plot_data <- data.frame(
  group = c("Control", "Event"),
  mean_turnout = c(0.6116667, 0.5862857),  # Mean turnouts
  lower_ci = c(0.5959605, 0.5379812),      # Lower bounds of 95% CI
  upper_ci = c(0.6273728, 0.6345902)       # Upper bounds of 95% CI
)

# Perform the t-test
t_test <- t.test(primary_event_turnout ~ binary_group, data = combined_data)

# Extract the p-value
p_value <- t_test$p.value

# Plot with annotation
ggplot(t_test_plot_data, aes(x = group, y = mean_turnout, fill = group)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +  # Bar chart
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +  # Error bars
  labs(
    title = "Mean Turnout with Confidence Intervals",
    x = "Group Type",
    y = "Mean Primary Event Turnout"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(
    aes(
      x = 1.5, y = 0.65,  # Position the annotation dynamically above bars
      label = paste0(
        "t-test results:\n",
        "Control Mean = 61.2%\n",
        "Event Mean = 58.6%\n",
        "p-value = ", round(p_value, 4)  # Round p-value to 4 decimal places
      )
    ),
    size = 4, hjust = 0  # Text size and alignment
  )

# H2 - Boxplot for comparison and t-test
# Filter the data for Voter Engagement and Legislative groups
filtered_data_h2 <- analyses_event_group %>%
  filter(legalization_type %in% c(1, 2))  # Keep Voter Engagement (1) and Legislative (2)

# Create the boxplot
ggplot(filtered_data_h2, aes(x = factor(legalization_type), y = primary_event_turnout, fill = factor(legalization_type))) +
  geom_boxplot() +
  labs(
    title = "Turnout by Legalization Type (H2)",
    x = "Legalization Type",
    y = "Primary Event Turnout",
    fill = "Legalization Type"
  ) +
  scale_x_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative")) +  # Correctly label the groups
  theme_minimal() +
  theme(legend.position = "none")

# Calculate group means and confidence intervals
summary_h2 <- filtered_data_h2 %>%
  group_by(legalization_type) %>%
  summarize(
    mean_turnout = mean(primary_event_turnout, na.rm = TRUE),
    se_turnout = sd(primary_event_turnout, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_turnout - qt(0.975, df = n() - 1) * se_turnout,
    upper_ci = mean_turnout + qt(0.975, df = n() - 1) * se_turnout
  )

# Create the bar chart
ggplot(summary_h2, aes(x = factor(legalization_type), y = mean_turnout, fill = factor(legalization_type))) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(
    title = "Mean Turnout with Confidence Intervals (H2)",
    x = "Legalization Type",
    y = "Mean Primary Event Turnout",
    fill = "Legalization Type"
  ) +
  scale_x_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative")) +  # Correctly label the groups
  theme_minimal() +
  theme(legend.position = "none")

# Conduct the t-test for Voter Engagement (1) vs Legislative (2)
t_test_h2 <- t.test(
  primary_event_turnout ~ factor(legalization_type), 
  data = filtered_data_h2, 
  var.equal = FALSE  # Welch's t-test (default), assuming unequal variance
)

# Print t-test results
print(t_test_h2)

# Calculate group means and confidence intervals
summary_h2 <- filtered_data_h2 %>%
  group_by(legalization_type) %>%
  summarize(
    mean_turnout = mean(primary_event_turnout, na.rm = TRUE),
    se_turnout = sd(primary_event_turnout, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean_turnout - qt(0.975, df = n() - 1) * se_turnout,
    upper_ci = mean_turnout + qt(0.975, df = n() - 1) * se_turnout
  )

# Conduct the t-test for H2
t_test_h2 <- t.test(
  primary_event_turnout ~ factor(legalization_type), 
  data = filtered_data_h2, 
  var.equal = FALSE  # Welch's t-test (default)
)

# Create the bar chart with t-test results
ggplot(summary_h2, aes(x = factor(legalization_type), y = mean_turnout, fill = factor(legalization_type))) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_text(
    aes(label = paste0("Mean: ", round(mean_turnout, 3))),
    vjust = -0.5,  # Position above the bars
    size = 4
  ) +
  labs(
    title = "Mean Turnout by Legalization Type (H2)",
    subtitle = paste("T-Test p-value:", format(t_test_h2$p.value, digits = 3)),
    x = "Legalization Type",
    y = "Mean Primary Event Turnout",
    fill = "Legalization Type"
  ) +
  scale_x_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative")) +
  theme_minimal() +
  theme(legend.position = "none")

# H3
# Filter the data for voter engagement (legalization_type = 1) and legislative action (legalization_type = 2)
filtered_data_h3 <- analyses_event_group %>%
  filter(legalization_type %in% c(1, 2)) # Include only relevant legalization types

# Violin plot for H3
ggplot(filtered_data_h3, aes(x = factor(legalization_type), y = primary_event_turnout, fill = factor(legalization_type))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.6) +
  labs(
    title = "Turnout Distribution by Legalization Type",
    x = "Legalization Type",
    y = "Primary Event Turnout",
    fill = "Legalization Type"
  ) +
  scale_x_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative Action")) +
  theme_minimal()

# Perform t-test comparing primary_event_turnout for the two legalization types
t_test_h3 <- t.test(
  primary_event_turnout ~ legalization_type,
  data = filtered_data_h3
)

# Print t-test results
print(t_test_h3)

# Summarize means and standard errors
library(dplyr)
summary_data <- filtered_data_h3 %>%
  group_by(legalization_type) %>%
  summarize(
    mean_turnout = mean(primary_event_turnout, na.rm = TRUE),
    se_turnout = sd(primary_event_turnout, na.rm = TRUE) / sqrt(n())
  )

# Dot-and-whisker plot for H3
ggplot(summary_data, aes(x = factor(legalization_type), y = mean_turnout)) +
  geom_point(size = 4, aes(color = factor(legalization_type))) +
  geom_errorbar(aes(ymin = mean_turnout - se_turnout, ymax = mean_turnout + se_turnout), width = 0.2, color = "black") +
  geom_text(aes(label = round(mean_turnout, 3)), nudge_x = 0.3, size = 4, color = "black") +
  labs(
    title = "Mean Turnout with Error Bars (H3)",
    x = "Legalization Type",
    y = "Mean Primary Event Turnout"
  ) +
  scale_x_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative Action")) +
  annotate("text", x = 1.5, y = max(summary_data$mean_turnout) + 0.02, label = paste0("p-value: ", round(t_test_h3$p.value, 3)), size = 5, color = "red") +
  theme_minimal()

# Alternative T-Test visual for H3
# Density plot for H3
# Calculate the means for annotation
mean_values <- filtered_data_h3 %>%
  group_by(legalization_type) %>%
  summarize(mean_turnout = mean(primary_event_turnout, na.rm = TRUE))

# Density plot with means and p-value
ggplot(filtered_data_h3, aes(x = primary_event_turnout, fill = factor(legalization_type))) +
  geom_density(alpha = 0.6) +
  geom_vline(data = mean_values, aes(xintercept = mean_turnout, color = factor(legalization_type)),
             linetype = "dashed", size = 1) +
  annotate("text", x = mean_values$mean_turnout[1], y = 0.6, 
           label = paste0("Mean (Voter): ", round(mean_values$mean_turnout[1], 3)), 
           color = "blue", size = 4, hjust = -0.1) +
  annotate("text", x = mean_values$mean_turnout[2], y = 0.6, 
           label = paste0("Mean (Legislative): ", round(mean_values$mean_turnout[2], 3)), 
           color = "red", size = 4, hjust = 1.1) +
  annotate("text", x = max(filtered_data_h3$primary_event_turnout) - 0.1, y = 0.2, 
           label = paste0("p-value: ", round(t_test_h3$p.value, 3)), 
           color = "black", size = 5) +
  labs(
    title = "Density of Turnout by Legalization Type (H3)",
    x = "Primary Event Turnout",
    y = "Density",
    fill = "Legalization Type"
  ) +
  scale_fill_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative Action")) +
  theme_minimal()

mean_values <- filtered_data_h3 %>%
  group_by(legalization_type) %>%
  summarize(mean_turnout = mean(primary_event_turnout, na.rm = TRUE))

t_test_h3 <- t.test(primary_event_turnout ~ factor(legalization_type), data = filtered_data_h3)

ggplot(filtered_data_h3, aes(x = primary_event_turnout, fill = factor(legalization_type))) +
  geom_density(alpha = 0.6) +
  geom_vline(data = mean_values, aes(xintercept = mean_turnout, color = factor(legalization_type)),
             linetype = "dashed", size = 1) +
  annotate("text", x = mean_values$mean_turnout[1], y = 0.6, 
           label = paste0("Mean (Voter): ", round(mean_values$mean_turnout[1], 3)), 
           color = "blue", size = 4, hjust = -0.1) +
  annotate("text", x = mean_values$mean_turnout[2], y = 0.6, 
           label = paste0("Mean (Legislative): ", round(mean_values$mean_turnout[2], 3)), 
           color = "red", size = 4, hjust = 1.1) +
  annotate("text", x = max(filtered_data_h3$primary_event_turnout, na.rm = TRUE) - 0.1, y = 0.2, 
           label = paste0("p-value: ", round(t_test_h3$p.value, 3)), 
           color = "black", size = 5) +
  labs(
    title = "Density of Turnout by Legalization Type (H3)",
    x = "Primary Event Turnout",
    y = "Density",
    fill = "Legalization Type"
  ) +
  scale_fill_discrete(labels = c("1" = "Voter Engagement", "2" = "Legislative Action")) +
  theme_minimal()

#402-440 PRIOR SESSIONS - Exploration of SQL database setup

# Check
dbListTables(con)

# Verify table structure
dbGetQuery(con, "PRAGMA table_info(legalization_data);")

# Read the CSV file into R and check
legalization_data <- read.csv("legalization_data_master.csv")

# Clean column names
names(legalization_data) <- gsub(" ", "_", names(legalization_data))

# Check the first few rows of the data
head(legalization_data)

# Remove the unwanted 'X' column
legalization_data <- legalization_data[, !names(legalization_data) %in% c("X")]

# Check the structure of the data
str(legalization_data)

# Insert the cleaned data into the 'legalization_data' table
dbWriteTable(con, "legalization_data", legalization_data, append = TRUE, row.names = FALSE)

# Check the structure of the 'legalization_data' table
dbGetQuery(con, "PRAGMA table_info(legalization_data);")

# Checks to ensure data_frame matches SQL table
names(legalization_data)
dbGetQuery(con, "PRAGMA table_info(legalization_data);")
str(legalization_data)
db_data <- dbGetQuery(con, "SELECT * FROM legalization_data LIMIT 5")
print(db_data)
head(legalization_data)
nrow(legalization_data)
db_data_count <- dbGetQuery(con, "SELECT COUNT(*) FROM legalization_data")
print(db_data_count)

# DiD with OLS

# Load the master_turnout data
master_turnout <- read.csv("master_turnout.csv", check.names = FALSE)

# Preview the data
head(master_turnout)

# Reshape the data into long format
melted_data <- master_turnout %>%
  pivot_longer(
    cols = c(`1998`, `2000`, `2002`, `2004`, `2006`, `2008`, `2010`, 
             `2012`, `2014`, `2016`, `2018`, `2020`, `2022`, `2024`), 
    names_to = "year", 
    values_to = "turnout"
  )

# Convert year to numeric
melted_data$year <- as.numeric(melted_data$year)

# Preview the reshaped data
head(melted_data)

# Filter for presidential elections
presidential_data <- melted_data %>%
  filter(year %% 4 == 0)  # Years divisible by 4 are presidential elections

# Filter for midterm elections
midterm_data <- melted_data %>%
  filter(year %% 4 != 0)  # Years not divisible by 4 are midterm elections

# Preview the filtered data
head(presidential_data)
head(midterm_data)

# Filter pre-treatment data for 2012 cohort
pre_treatment_2012 <- presidential_data %>%
  filter(cohort == 2012 & year < 2012)

# Preview the filtered data
head(pre_treatment_2012)

# Filter pre-treatment data for 2014 cohort
pre_treatment_2014 <- midterm_data %>%
  filter(cohort == 2014 & year < 2014)

# Preview the filtered data
head(pre_treatment_2014)

# Filter pre-treatment data for 2016 cohort
pre_treatment_2016 <- presidential_data %>%
  filter(cohort == 2016 & year < 2016)

# Preview the filtered data
head(pre_treatment_2016)

# Aggregate mean turnout for 2012 cohort
agg_2012 <- pre_treatment_2012 %>%
  group_by(year) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE))

# Plot pre-treatment trend for 2012 cohort
ggplot(agg_2012, aes(x = year, y = mean_turnout)) +
  geom_line(linewidth = 1.2, color = "blue") +  # Thicker line, add color
  geom_point(size = 3, color = "blue") +        # Add points at data values
  theme_minimal() +
  theme(
    text = element_text(size = 14),            # Increase font size
    plot.title = element_text(face = "bold", hjust = 0.5),  # Bold centered title
    axis.title = element_text(face = "bold")   # Bold axis labels
  ) +
  labs(
    title = "Pre-Treatment Turnout Trends for 2012 Cohort (Presidential)",
    subtitle = "Average turnout rates from 2000 to 2008",
    x = "Year",
    y = "Mean Turnout"
  )

# Export 2012 pre-treatment visual
ggsave("2012_cohort_trend.png", bg = "white", width = 8, height = 6, dpi = 300)

# Aggregate mean turnout for 2014 cohort
agg_2014 <- pre_treatment_2014 %>%
  group_by(year) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE))

# Plot pre-treatment trend for 2014 cohort
ggplot(agg_2014, aes(x = year, y = mean_turnout)) +
  geom_line(linewidth = 1.2, color = "red") +  # Use red for 2014 cohort
  geom_point(size = 3, color = "red") +        # Add points with matching color
  theme_minimal() +
  theme(
    text = element_text(size = 14),            # Increase font size
    plot.title = element_text(face = "bold", hjust = 0.5),  # Bold centered title
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text.x = element_text(size = 12)      # Adjust axis text size
  ) +
  labs(
    title = "Pre-Treatment Turnout Trends for 2014 Cohort (Midterm)",
    subtitle = "Average turnout rates from 1998 to 2010",
    x = "Year",
    y = "Mean Turnout"
  ) +
  scale_x_continuous(breaks = seq(1998, 2010, by = 4))  # Ensure clean x-axis labels

# Save the plot
ggsave("2014_cohort_trend.png", bg = "white", width = 8, height = 6, dpi = 300)

# Aggregate mean turnout for 2016 cohort
agg_2016 <- pre_treatment_2016 %>%
  group_by(year) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE))

# Plot pre-treatment trend for 2016 cohort
ggplot(agg_2016, aes(x = year, y = mean_turnout)) +
  geom_line(linewidth = 1.2, color = "green") +  # Use standard green for 2016 cohort
  geom_point(size = 3, color = "green") +        # Add points with matching color
  theme_minimal() +
  theme(
    text = element_text(size = 14),            # Increase font size
    plot.title = element_text(face = "bold", hjust = 0.5),  # Bold centered title
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text.x = element_text(size = 12)      # Adjust axis text size
  ) +
  labs(
    title = "Pre-Treatment Turnout Trends for 2016 Cohort (Presidential)",
    subtitle = "Average turnout rates from 2000 to 2012",
    x = "Year",
    y = "Mean Turnout"
  ) +
  scale_x_continuous(breaks = seq(2000, 2012, by = 4))  # Ensure clean x-axis labels

# Save the plot
ggsave("2016_cohort_trend.png", bg = "white", width = 8, height = 6, dpi = 300)

# ANALYZE POTENTIAL CONTROL STATES for each cohort to identify states to be included in control group for each cohort

# Filter pre-treatment data for control states in presidential elections
control_presidential <- presidential_data %>%
  filter(cohort == 0)

# Filter pre-treatment data for control states in midterm elections
control_midterm <- midterm_data %>%
  filter(cohort == 0)

# Filter control states for 2012 pre-treatment period
control_states_2012 <- control_presidential %>%
  filter(year %in% c(2000, 2004, 2008))

# Filter pre-treatment data for control states in midterm elections (2014 cohort)
control_states_2014 <- control_midterm %>%
  filter(year %in% c(1998, 2002, 2006, 2010))  # Pre-treatment years for 2014 cohort

# Filter pre-treatment data for control states in presidential elections (2016 cohort)
control_states_2016 <- control_presidential %>%
  filter(year %in% c(2000, 2004, 2008, 2012))  # Pre-treatment years for 2016 cohort

# Plot individual trends for potential control states (2012 cohort)
ggplot(control_states_2012, aes(x = year, y = turnout, group = state_abbr, color = state_abbr)) +
  geom_line(alpha = 0.7, size = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White overall background
    text = element_text(size = 14, color = "black"),             # Non-shadowed text
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold centered title
    axis.title = element_text(face = "bold"),                   # Bold axis labels
    axis.text = element_text(color = "black"),                  # Black axis text
    legend.position = "none"                                    # Suppress legend
  ) +
  labs(
    title = "Turnout Trends for Potential 2012 Control States (Presidential)",
    subtitle = "Pre-treatment years: 2000, 2004, 2008",
    x = "Year",
    y = "Turnout Percentage"
  )

# Export with a white background
ggsave("appendix_2012_control_states.png", bg = "white", width = 8, height = 6, dpi = 300)

# Plot individual trends for potential control states (2014 cohort)
ggplot(control_states_2014, aes(x = year, y = turnout, group = state_abbr, color = state_abbr)) +
  geom_line(alpha = 0.7, size = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White overall background
    text = element_text(size = 14, color = "black"),             # Non-shadowed text
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold centered title
    axis.title = element_text(face = "bold"),                   # Bold axis labels
    axis.text = element_text(color = "black"),                  # Black axis text
    legend.position = "none"                                    # Suppress legend
  ) +
  labs(
    title = "Turnout Trends for Potential 2014 Control States (Midterm)",
    subtitle = "Pre-treatment years: 1998, 2002, 2006, 2010",
    x = "Year",
    y = "Turnout Percentage"
  )

# Export with a white background
ggsave("appendix_2014_control_states.png", bg = "white", width = 8, height = 6, dpi = 300)

# Plot individual trends for potential control states (2016 cohort)
ggplot(control_states_2016, aes(x = year, y = turnout, group = state_abbr, color = state_abbr)) +
  geom_line(alpha = 0.7, size = 1) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # White plot background
    plot.background = element_rect(fill = "white", color = NA),  # White overall background
    text = element_text(size = 14, color = "black"),             # Non-shadowed text
    plot.title = element_text(face = "bold", hjust = 0.5),       # Bold centered title
    axis.title = element_text(face = "bold"),                   # Bold axis labels
    axis.text = element_text(color = "black"),                  # Black axis text
    legend.position = "none"                                    # Suppress legend
  ) +
  labs(
    title = "Turnout Trends for Potential 2016 Control States (Presidential)",
    subtitle = "Pre-treatment years: 2000, 2004, 2008, 2012",
    x = "Year",
    y = "Turnout Percentage"
  )

# Export with a white background
ggsave("appendix_2016_control_states.png", bg = "white", width = 8, height = 6, dpi = 300)

# DEFINE CONTROL GROUPS FOR EACH COHORT
# Define control groups using pre-treatment mean comparison and t-tests

# 2012 CONTROL GROUP
# Filter pre-treatment data for treatment cohort (2012)
pre_treatment_2012_treatment <- presidential_data %>%
  filter(cohort == 2012, year %in% c(2000, 2004, 2008))

# Calculate mean turnout for the treatment cohort
treatment_mean_turnout_2012 <- mean(pre_treatment_2012_treatment$turnout, na.rm = TRUE)

# Calculate mean turnout and differences for each control state
control_means_2012 <- pre_treatment_2012_control %>%
  group_by(state_abbr) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE)) %>%
  mutate(mean_diff = abs(mean_turnout - treatment_mean_turnout_2012))

# Perform t-tests for each control state
t_test_results_2012 <- control_means_2012 %>%
  rowwise() %>%
  mutate(
    t_test_p_value = t.test(
      pre_treatment_2012_treatment$turnout,
      pre_treatment_2012_control %>%
        filter(state_abbr == state_abbr) %>%
        pull(turnout)
    )$p.value
  )

# Filter control states with p-value > 0.05
selected_control_states_2012 <- t_test_results_2012 %>%
  filter(t_test_p_value > 0.05) %>%
  arrange(mean_diff)

# View the final selected control states for 2012
print(selected_control_states_2012)

# Filter control states with a more relaxed p-value threshold (p > 0.01)
selected_control_states_2012_relaxed <- t_test_results_2012 %>%
  filter(t_test_p_value > 0.01) %>%
  arrange(mean_diff)

# Filter control states with a more relaxed p-value threshold (p > 0.01)
selected_control_states_2012_relaxed <- t_test_results_2012 %>%
  filter(t_test_p_value > 0.01) %>%
  arrange(mean_diff)

# View the relaxed selection
print(selected_control_states_2012_relaxed)

# Select the top 5 control states with the smallest mean differences
final_control_states_2012 <- selected_control_states_2012_relaxed %>%
  slice_head(n = 5)

# Filter the data for these selected control states
selected_control_trends_2012 <- pre_treatment_2012_control %>%
  filter(state_abbr %in% final_control_states_2012$state_abbr)

# Combine treatment cohort and selected control states
combined_trends_2012 <- bind_rows(
  pre_treatment_2012_treatment %>% mutate(group = "Treatment Cohort"),
  selected_control_trends_2012 %>% mutate(group = "Control States")
)

# Filter the data to include only the treatment cohort and the top 5 control states
# Select the top 5 control states based on mean_diff
top5_control_states_2012 <- final_control_states_2012 %>%
  slice_head(n = 5)

# Ensure only the treatment cohort and top 5 control states are included
filtered_trends_2012 <- combined_trends_2012 %>%
  filter(
    group == "Treatment Cohort" | 
      (group == "Control States" & state_abbr %in% top5_control_states_2012$state_abbr)
  )

# Confirm the filtering worked
print(unique(filtered_trends_2012$state_abbr))

# DEBUG
str(final_control_states_2012)

# Ensure the data is ungrouped before selecting the top 5
top5_control_states_2012 <- final_control_states_2012 %>%
  ungroup() %>%  # Remove rowwise grouping
  arrange(mean_diff) %>%  # Sort by mean_diff
  slice_head(n = 5)  # Select the top 5 rows

# Verify the result
print(top5_control_states_2012)

# 2014 CONTROL GROUP
# Step 1: Filter pre-treatment data for 2014 cohort (Midterm Elections)
pre_treatment_2014_treatment <- midterm_data %>%
  filter(cohort == 2014, year %in% c(1998, 2002, 2006, 2010))

pre_treatment_2014_control <- midterm_data %>%
  filter(cohort == 0, year %in% c(1998, 2002, 2006, 2010))

# Step 2: Compute mean turnout for treatment cohort
treatment_mean_turnout_2014 <- mean(pre_treatment_2014_treatment$turnout, na.rm = TRUE)

# Step 3: Compute mean turnout and differences for each control state
control_means_2014 <- pre_treatment_2014_control %>%
  group_by(state_abbr) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE)) %>%
  mutate(mean_diff = abs(mean_turnout - treatment_mean_turnout_2014))

# Step 4: Perform t-tests for each control state
t_test_results_2014 <- control_means_2014 %>%
  rowwise() %>%
  mutate(
    t_test_p_value = t.test(
      pre_treatment_2014_treatment$turnout,
      pre_treatment_2014_control %>%
        filter(state_abbr == state_abbr) %>%
        pull(turnout)
    )$p.value
  )

# Step 5: Select top 5 control states based on mean_diff and p-value > 0.05
final_control_states_2014 <- t_test_results_2014 %>%
  ungroup() %>%  # Remove rowwise grouping
  filter(t_test_p_value > 0.05) %>%
  arrange(mean_diff) %>%
  slice_head(n = 5)

# View the selected control states
print(final_control_states_2014)

# 2016 CONTROL GROUP
# Step 1: Filter pre-treatment data for 2016 cohort (Presidential Elections)
pre_treatment_2016_treatment <- presidential_data %>%
  filter(cohort == 2016, year %in% c(2000, 2004, 2008, 2012))

pre_treatment_2016_control <- presidential_data %>%
  filter(cohort == 0, year %in% c(2000, 2004, 2008, 2012))

# Step 2: Compute mean turnout for treatment cohort
treatment_mean_turnout_2016 <- mean(pre_treatment_2016_treatment$turnout, na.rm = TRUE)

# Step 3: Compute mean turnout and differences for each control state
control_means_2016 <- pre_treatment_2016_control %>%
  group_by(state_abbr) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE)) %>%
  mutate(mean_diff = abs(mean_turnout - treatment_mean_turnout_2016))

# Step 4: Perform t-tests for each control state
t_test_results_2016 <- control_means_2016 %>%
  rowwise() %>%
  mutate(
    t_test_p_value = t.test(
      pre_treatment_2016_treatment$turnout,
      pre_treatment_2016_control %>%
        filter(state_abbr == state_abbr) %>%
        pull(turnout)
    )$p.value
  )

# Step 5: Select top 5 control states based on mean_diff and p-value > 0.05
final_control_states_2016 <- t_test_results_2016 %>%
  ungroup() %>%  # Remove rowwise grouping
  filter(t_test_p_value > 0.05) %>%
  arrange(mean_diff) %>%
  slice_head(n = 5)

# View the selected control states
print(final_control_states_2016)

# PLOTS FOR COHORTS WITH CONTROL STATES
#2012
# Combine treatment and top 5 control states data for 2012 cohort
combined_trends_2012 <- pre_treatment_2012_treatment %>%
  mutate(group = "Treatment Cohort") %>%
  bind_rows(
    pre_treatment_2012_control %>%
      filter(state_abbr %in% top5_control_states_2012$state_abbr) %>%
      mutate(group = "Control States")
  )

# Calculate mean turnout for each group
mean_trends_2012 <- combined_trends_2012 %>%
  group_by(year, group) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop")

# Simplified ggplot for 2012 cohort
ggplot(data = combined_trends_2012, aes(x = year, y = turnout, group = interaction(state_abbr, group), color = group)) +
  geom_line(alpha = 0.7, size = 1) +
  geom_line(data = mean_trends_2012, aes(x = year, y = mean_turnout, color = group),
            size = 1.2, linetype = "dashed", inherit.aes = FALSE) +
  theme_minimal() +
  labs(
    title = "2012 Pre-Treatment Turnout Trends: Treatment vs. Control States",
    x = "Year",
    y = "Turnout Percentage",
    color = "Group"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  scale_color_manual(values = c("Treatment Cohort" = "blue", "Control States" = "orange"))

# Save the plot
ggsave("2012_treatment_vs_control_trends_orange_fixed.png", width = 8, height = 6, dpi = 300, bg = "white")

# 2014
# Combine treatment and top 5 control states data for 2014 cohort
combined_trends_2014 <- pre_treatment_2014_treatment %>%
  mutate(group = "Treatment Cohort") %>%
  bind_rows(
    pre_treatment_2014_control %>%
      filter(state_abbr %in% final_control_states_2014$state_abbr) %>%
      mutate(group = "Control States")
  )

# Calculate mean turnout for each group
mean_trends_2014 <- combined_trends_2014 %>%
  group_by(year, group) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop")

# Plot turnout trends for 2014 cohort
ggplot(data = combined_trends_2014, aes(x = year, y = turnout, group = state_abbr, color = group)) +
  geom_line(alpha = 0.7, size = 1) +
  geom_line(data = mean_trends_2014, aes(x = year, y = mean_turnout, color = group),
            size = 1.2, linetype = "dashed", inherit.aes = FALSE) +
  theme_minimal() +
  labs(
    title = "2014 Pre-Treatment Turnout Trends: Treatment vs. Control States",
    x = "Year",
    y = "Turnout Percentage",
    color = "Group"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_continuous(breaks = seq(1998, 2010, by = 2)) +  # Explicitly set year breaks
  scale_color_manual(values = c("Treatment Cohort" = "red", "Control States" = "orange"))

# Save the plot
ggsave("2014_treatment_vs_control_trends.png", width = 8, height = 6, dpi = 300, bg = "white")

# 2016
# Combine treatment and top 5 control states data for 2016 cohort
combined_trends_2016 <- pre_treatment_2016_treatment %>%
  mutate(group = "Treatment Cohort") %>%
  bind_rows(
    pre_treatment_2016_control %>%
      filter(state_abbr %in% final_control_states_2016$state_abbr) %>%
      mutate(group = "Control States")
  )

# Calculate mean turnout for each group
mean_trends_2016 <- combined_trends_2016 %>%
  group_by(year, group) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop")

# Plot turnout trends for 2016 cohort
ggplot(data = combined_trends_2016, aes(x = year, y = turnout, group = state_abbr, color = group)) +
  geom_line(alpha = 0.7, size = 1) +
  geom_line(data = mean_trends_2016, aes(x = year, y = mean_turnout, color = group),
            size = 1.2, linetype = "dashed", inherit.aes = FALSE) +
  theme_minimal() +
  labs(
    title = "2016 Pre-Treatment Turnout Trends: Treatment vs. Control States",
    x = "Year",
    y = "Turnout Percentage",
    color = "Group"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_continuous(breaks = seq(2000, 2012, by = 4)) +  # Explicitly set year breaks
  scale_color_manual(values = c("Treatment Cohort" = "green", "Control States" = "orange"))

# Save the plot
ggsave("2016_treatment_vs_control_trends.png", width = 8, height = 6, dpi = 300, bg = "white")

# 2012 DiD
# Filter data for the 2012 cohort and presidential election years
did_data_2012 <- melted_data %>%
  filter(
    (state_abbr %in% c("CO", "WA") & cohort == 2012) |  # Treatment states
      (state_abbr %in% top5_control_states_2012$state_abbr & cohort == 0)  # Control states
  ) %>%
  filter(year %% 4 == 0) %>%  # Include only presidential election years
  mutate(
    treatment = ifelse(state_abbr %in% c("CO", "WA"), 1, 0),  # Mark treatment states
    post = ifelse(year >= 2012, 1, 0)  # Mark post-treatment years
  )

# Verify the included years
unique(did_data_2012$year)

# Check the breakdown of treatment and post-treatment
table(did_data_2012$treatment, did_data_2012$post)

# Run the DiD regression model
did_model_2012 <- lm(turnout ~ treatment * post, data = did_data_2012)

# Summarize the regression results
summary(did_model_2012)

# Plot turnout trends for treatment and control states
ggplot(did_data_2012, aes(x = year, y = turnout, color = as.factor(treatment))) +
  geom_line(aes(group = state_abbr), alpha = 0.7, size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = interaction(treatment, post), linetype = as.factor(post))) +
  theme_minimal() +
  labs(
    title = "Turnout Trends for 2012 Cohort: Treatment vs Control States",
    x = "Year",
    y = "Turnout Percentage",
    color = "Group",
    linetype = "Period"
  ) +
  scale_color_manual(values = c("blue", "orange"), labels = c("Control", "Treatment")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pre-Treatment", "Post-Treatment")) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

ggsave("turnout_trends_2012.png", width = 8, height = 6, dpi = 300)

# Extract regression coefficients for inclusion in a table or report
regression_table_2012 <- broom::tidy(did_model_2012)

# Create a visual representation of coefficients
coef_plot <- ggplot(regression_table_2012, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Regression Coefficients for 2012 Cohort (DiD Model)",
    x = "Model Term",
    y = "Estimate",
    fill = "Term"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Print the plot
print(coef_plot)

# Save the plot as a PNG
ggsave("did_model_2012_coefficients.png", coef_plot, width = 8, height = 6, dpi = 300)

# Save the output as an HTML file
stargazer(did_model_2012, type = "html", title = "Regression Results for 2012 Cohort",
          out = "regression_results_2012.html")

# 2014 DiD
# Calculate mean turnout for treatment and control states for 2014
pre_treatment_2014_treatment <- melted_data %>%
  filter(cohort == 2014, year %in% c(1998, 2002, 2006, 2010))

pre_treatment_2014_control <- melted_data %>%
  filter(cohort == 0, year %in% c(1998, 2002, 2006, 2010))

treatment_mean_turnout_2014 <- mean(pre_treatment_2014_treatment$turnout, na.rm = TRUE)

# Compute mean turnout and differences for each control state
control_means_2014 <- pre_treatment_2014_control %>%
  group_by(state_abbr) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE)) %>%
  mutate(mean_diff = abs(mean_turnout - treatment_mean_turnout_2014))

# Perform t-tests
t_test_results_2014 <- control_means_2014 %>%
  rowwise() %>%
  mutate(
    t_test_p_value = t.test(
      pre_treatment_2014_treatment$turnout,
      pre_treatment_2014_control %>%
        filter(state_abbr == state_abbr) %>%
        pull(turnout)
    )$p.value
  )

# Select top 5 control states
top5_control_states_2014 <- t_test_results_2014 %>%
  ungroup() %>%
  filter(t_test_p_value > 0.05) %>%
  arrange(mean_diff) %>%
  slice_head(n = 5)

# Confirm the top 5 control states
print(top5_control_states_2014)

# Filter data for the 2014 cohort and midterm election years
did_data_2014 <- melted_data %>%
  filter(
    (state_abbr %in% c("AK", "DC", "OR") & cohort == 2014) |  # Treatment states
      (state_abbr %in% top5_control_states_2014$state_abbr & cohort == 0)  # Control states
  ) %>%
  filter(year %% 4 != 0) %>%  # Include only midterm election years
  mutate(
    treatment = ifelse(state_abbr %in% c("AK", "DC", "OR"), 1, 0),  # Mark treatment states
    post = ifelse(year >= 2014, 1, 0)  # Mark post-treatment years
  )

# Verify the included years
unique(did_data_2014$year)

# Run the DiD regression model
did_model_2014 <- lm(turnout ~ treatment * post, data = did_data_2014)

# Summarize the regression results
summary(did_model_2014)

# Plot turnout trends
ggplot(did_data_2014, aes(x = year, y = turnout, color = as.factor(treatment))) +
  geom_line(aes(group = state_abbr), alpha = 0.7, size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = interaction(treatment, post), linetype = as.factor(post))) +
  theme_minimal() +
  labs(
    title = "Turnout Trends for 2014 Cohort: Treatment vs Control States",
    x = "Year",
    y = "Turnout Percentage",
    color = "Group",
    linetype = "Period"
  ) +
  scale_color_manual(values = c("red", "orange"), labels = c("Control", "Treatment")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pre-Treatment", "Post-Treatment")) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Save the plot as a PNG
ggsave("turnout_trends_2014.png", width = 8, height = 6, dpi = 300)

# Extract regression coefficients for inclusion in a table or report
regression_table_2014 <- broom::tidy(did_model_2014)

# Create a visual representation of coefficients
coef_plot <- ggplot(regression_table_2014, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Regression Coefficients for 2014 Cohort (DiD Model)",
    x = "Model Term",
    y = "Estimate",
    fill = "Term"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Print the plot
print(coef_plot)

# Save the plot as a PNG
ggsave("did_model_2014_coefficients.png", coef_plot, width = 8, height = 6, dpi = 300)

# Save the regression output as an HTML file
stargazer(did_model_2014, type = "html", title = "Regression Results for 2014 Cohort",
          out = "regression_results_2014.html")

#2016 DiD
# Calculate mean turnout for treatment and control states for 2016
pre_treatment_2016_treatment <- melted_data %>%
  filter(cohort == 2016, year %in% c(2000, 2004, 2008, 2012))

pre_treatment_2016_control <- melted_data %>%
  filter(cohort == 0, year %in% c(2000, 2004, 2008, 2012))

treatment_mean_turnout_2016 <- mean(pre_treatment_2016_treatment$turnout, na.rm = TRUE)

# Compute mean turnout and differences for each control state
control_means_2016 <- pre_treatment_2016_control %>%
  group_by(state_abbr) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE)) %>%
  mutate(mean_diff = abs(mean_turnout - treatment_mean_turnout_2016))

# Perform t-tests
t_test_results_2016 <- control_means_2016 %>%
  rowwise() %>%
  mutate(
    t_test_p_value = t.test(
      pre_treatment_2016_treatment$turnout,
      pre_treatment_2016_control %>%
        filter(state_abbr == state_abbr) %>%
        pull(turnout)
    )$p.value
  )

# Select top 5 control states
top5_control_states_2016 <- t_test_results_2016 %>%
  ungroup() %>%
  filter(t_test_p_value > 0.05) %>%
  arrange(mean_diff) %>%
  slice_head(n = 5)

# Confirm the top 5 control states
print(top5_control_states_2016)

# Filter data for the 2016 cohort and presidential election years
did_data_2016 <- melted_data %>%
  filter(
    (state_abbr %in% c("AZ", "CA", "ME", "MA", "NV") & cohort == 2016) |  # Treatment states
      (state_abbr %in% top5_control_states_2016$state_abbr & cohort == 0)  # Control states
  ) %>%
  filter(year %% 4 == 0) %>%  # Include only presidential election years
  mutate(
    treatment = ifelse(state_abbr %in% c("AZ", "CA", "ME", "MA", "NV"), 1, 0),  # Mark treatment states
    post = ifelse(year >= 2016, 1, 0)  # Mark post-treatment years
  )

# Verify the included years
unique(did_data_2016$year)

# Run the DiD regression model
did_model_2016 <- lm(turnout ~ treatment * post, data = did_data_2016)

# Summarize the regression results
summary(did_model_2016)

# Plot turnout trends
ggplot(did_data_2016, aes(x = year, y = turnout, color = as.factor(treatment))) +
  geom_line(aes(group = state_abbr), alpha = 0.7, size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = interaction(treatment, post), linetype = as.factor(post))) +
  theme_minimal() +
  labs(
    title = "Turnout Trends for 2016 Cohort: Treatment vs Control States",
    x = "Year",
    y = "Turnout Percentage",
    color = "Group",
    linetype = "Period"
  ) +
  scale_color_manual(values = c("green", "orange"), labels = c("Control", "Treatment")) +
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Pre-Treatment", "Post-Treatment")) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Save the plot as a PNG
ggsave("turnout_trends_2016.png", width = 8, height = 6, dpi = 300)

# Extract regression coefficients for inclusion in a table or report
regression_table_2016 <- broom::tidy(did_model_2016)

# Create a visual representation of coefficients
coef_plot_2016 <- ggplot(regression_table_2016, aes(x = term, y = estimate, fill = term)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Regression Coefficients for 2016 Cohort (DiD Model)",
    x = "Model Term",
    y = "Estimate",
    fill = "Term"
  ) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Print the plot
print(coef_plot_2016)

# Save the plot as a PNG
ggsave("did_model_2016_coefficients.png", coef_plot_2016, width = 8, height = 6, dpi = 300)

# Save the regression output as an HTML file
stargazer(did_model_2016, type = "html", title = "Regression Results for 2016 Cohort",
          out = "regression_results_2016.html")


# ADDITIONAL ANALYSES

# Aggregated DiD Model Across Cohorts
# Combine all cohorts into a single dataset
did_data_combined <- melted_data %>%
  filter(
    (state_abbr %in% c("CO", "WA", "AK", "DC", "OR", "AZ", "CA", "ME", "MA", "NV")) |
      (state_abbr %in% c(top5_control_states_2012$state_abbr, 
                         top5_control_states_2014$state_abbr, 
                         top5_control_states_2016$state_abbr))
  ) %>%
  mutate(
    treatment = ifelse(cohort %in% c(2012, 2014, 2016), 1, 0),
    post = ifelse((cohort == 2012 & year >= 2012) |
                    (cohort == 2014 & year >= 2014) |
                    (cohort == 2016 & year >= 2016), 1, 0),
    cohort_year = factor(cohort, levels = c(2012, 2014, 2016))
  )

# Run the aggregated DiD regression
did_model_combined <- lm(turnout ~ treatment * post * cohort_year, data = did_data_combined)

# Summarize the results
summary(did_model_combined)

# Save regression output
stargazer(did_model_combined, type = "html", title = "Aggregated DiD Results",
          out = "aggregated_did_results.html")

# Pre-vs. Post-Treatment Trends for Control States
# Filter control states only
control_states_data <- melted_data %>%
  filter(cohort == 0 & state_abbr %in% c(
    top5_control_states_2012$state_abbr,
    top5_control_states_2014$state_abbr,
    top5_control_states_2016$state_abbr
  )) %>%
  mutate(post = ifelse(year >= 2012, 1, 0))

# Compare pre- and post-treatment mean turnout
control_means <- control_states_data %>%
  group_by(post) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE))

# Visualize pre- vs. post-treatment trends for control states
control_trends_plot <- ggplot(control_states_data, aes(x = year, y = turnout, group = state_abbr)) +
  geom_line(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Turnout Trends for Control States",
    x = "Year",
    y = "Turnout Percentage"
  )

# Save the plot as a PNG
ggsave("control_states_trends.png", bg = "white", control_trends_plot, width = 8, height = 6, dpi = 300)

# Perform t-tests
pre_turnout <- control_states_data %>% filter(post == 0) %>% pull(turnout)
post_turnout <- control_states_data %>% filter(post == 1) %>% pull(turnout)
t_test_control <- t.test(pre_turnout, post_turnout)
print(t_test_control)

# Create a summary data frame for the visual
summary_data <- data.frame(
  Period = c("Pre-Treatment", "Post-Treatment"),
  MeanTurnout = c(mean(pre_turnout, na.rm = TRUE), mean(post_turnout, na.rm = TRUE))
)

# Create the bar chart
bar_plot <- ggplot(summary_data, aes(x = Period, y = MeanTurnout, fill = Period)) +
  geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
  theme_minimal() +
  labs(
    title = "Mean Turnout: Pre- vs. Post-Treatment for Control States",
    x = "Period",
    y = "Mean Turnout Percentage"
  ) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

# Save the bar chart as a PNG
ggsave("control_states_pre_post_treatment.png", bar_plot, width = 8, height = 6, dpi = 300)

# Print the plot
print(bar_plot)

# Create a line graph
# Calculate mean turnout for pre- and post-treatment periods
control_states_means <- control_states_data %>%
  group_by(post) %>%
  summarize(mean_turnout = mean(turnout, na.rm = TRUE)) %>%
  mutate(Period = ifelse(post == 0, "Pre-Treatment", "Post-Treatment"))

# Reorder the Period column to ensure proper order on the x-axis
control_states_means$Period <- factor(control_states_means$Period, levels = c("Pre-Treatment", "Post-Treatment"))

# Create the line plot with a white background
plot <- ggplot(control_states_means, aes(x = Period, y = mean_turnout, group = 1)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "blue", size = 3) +
  theme_minimal() +
  labs(
    title = "Mean Turnout: Pre- vs. Post-Treatment for Control States",
    x = "Period",
    y = "Mean Turnout Percentage"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Print the plot
print(plot)

# Save the corrected plot as PNG with a white background
ggsave("control_states_pre_post_line_corrected.png", plot = plot, width = 8, height = 6, dpi = 300)



# Comparing the Three Cohorts' Results
# Extract the treatment:post coefficients and confidence intervals
cohort_results <- tibble(
  cohort = c("2012", "2014", "2016"),
  estimate = c(coef(did_model_2012)["treatment:post"], 
               coef(did_model_2014)["treatment:post"], 
               coef(did_model_2016)["treatment:post"]),
  conf_low = c(confint(did_model_2012)["treatment:post", 1], 
               confint(did_model_2014)["treatment:post", 1], 
               confint(did_model_2016)["treatment:post", 1]),
  conf_high = c(confint(did_model_2012)["treatment:post", 2], 
                confint(did_model_2014)["treatment:post", 2], 
                confint(did_model_2016)["treatment:post", 2])
)

# Create the plot
cohort_plot <- ggplot(cohort_results, aes(x = cohort, y = estimate, ymin = conf_low, ymax = conf_high)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(width = 0.2, color = "blue") +
  theme_minimal() +
  labs(
    title = "Comparison of Treatment Effects Across Cohorts",
    x = "Cohort",
    y = "Estimated Effect (Turnout)"
  )

# Print the plot
print(cohort_plot)

# Save the plot as a PNG
ggsave("cohort_comparison.png", bg = "white", cohort_plot, width = 8, height = 6, dpi = 300)

# CREATE SQL DATABASE

# Create a connection to a new SQLite database
conn <- dbConnect(RSQLite::SQLite(), "project_database.sqlite")

# Drop all existing tables if they exist
dbExecute(conn, "DROP TABLE IF EXISTS states_gen_data;")
dbExecute(conn, "DROP TABLE IF EXISTS master_turnout;")
dbExecute(conn, "DROP TABLE IF EXISTS legalization_data;")
dbExecute(conn, "DROP TABLE IF EXISTS analyses_event_group;")
dbExecute(conn, "DROP TABLE IF EXISTS control_event_data;")

# Create tables
# States General Data
dbExecute(conn, "
CREATE TABLE states_gen_data (
    state_id INT PRIMARY KEY,
    state_name TEXT,
    state_abbr TEXT
);
")

# Master Turnout
dbExecute(conn, "
CREATE TABLE master_turnout (
    state_id INT,
    state_abbr TEXT,
    cohort TEXT,
    turnout_1998 DECIMAL(5,3),
    turnout_2000 DECIMAL(5,3),
    turnout_2002 DECIMAL(5,3),
    turnout_2004 DECIMAL(5,3),
    turnout_2006 DECIMAL(5,3),
    turnout_2008 DECIMAL(5,3),
    turnout_2010 DECIMAL(5,3),
    turnout_2012 DECIMAL(5,3),
    turnout_2014 DECIMAL(5,3),
    turnout_2016 DECIMAL(5,3),
    turnout_2018 DECIMAL(5,3),
    turnout_2020 DECIMAL(5,3),
    turnout_2022 DECIMAL(5,3),
    turnout_2024 DECIMAL(5,3),
    PRIMARY KEY (state_id)
);
")

# Legalization Data
dbExecute(conn, "
CREATE TABLE legalization_data (
    state_id INT,
    event_id INT,
    legalization_status INT,
    legalization_type INT,
    legalization_year INT,
    year_election_type INT,
    PRIMARY KEY (state_id, event_id)
);
")

# Analyses Event Group
dbExecute(conn, "
CREATE TABLE analyses_event_group (
    state_id INT,
    event_id INT,
    primary_event_turnout DECIMAL(5,3),
    pre_event_turnout DECIMAL(5,3),
    post_event_turnout DECIMAL(5,3),
    avg_turnout_change DECIMAL(5,3),
    PRIMARY KEY (state_id, event_id)
);
")

# Control Event Data
dbExecute(conn, "
CREATE TABLE control_event_data (
    state_id INT,
    year INT,
    voter_turnout DECIMAL(5,3),
    PRIMARY KEY (state_id, year)
);
")

# Populate tables
# States General Data
states_gen_data <- read.csv("states_gen_data.csv")
dbWriteTable(conn, "states_gen_data", states_gen_data, overwrite = TRUE)

# Master Turnout
master_turnout <- read.csv("master_turnout.csv")
dbWriteTable(conn, "master_turnout", master_turnout, overwrite = TRUE)

# Legalization Data
legalization_data <- read.csv("legalization_data.csv")
dbWriteTable(conn, "legalization_data", legalization_data, overwrite = TRUE)

# Analyses Event Group
analyses_event_group <- read.csv("analyses_event_group.csv")
dbWriteTable(conn, "analyses_event_group", analyses_event_group, overwrite = TRUE)

# Control Event Data
control_2012 <- read.csv("control_2012_pres_event.csv")
control_2016 <- read.csv("control_2016_pres_event.csv")
control_2020 <- read.csv("control_2020_pres_event.csv")

dbWriteTable(conn, "control_event_data", control_2012, overwrite = TRUE)
dbWriteTable(conn, "control_event_data", control_2016, append = TRUE)
dbWriteTable(conn, "control_event_data", control_2020, append = TRUE)

# Verify tables and data
dbListTables(conn)
dbGetQuery(conn, "SELECT * FROM states_gen_data LIMIT 5;")
dbGetQuery(conn, "SELECT * FROM master_turnout LIMIT 5;")
dbGetQuery(conn, "SELECT * FROM legalization_data LIMIT 5;")
dbGetQuery(conn, "SELECT * FROM analyses_event_group LIMIT 5;")
dbGetQuery(conn, "SELECT * FROM control_event_data LIMIT 5;")

# Disconnect from the database
dbDisconnect(conn)


# GENERATE SQL FILE IN R
# Connect to the SQLite database
conn <- dbConnect(RSQLite::SQLite(), "project_database.sqlite")

# Get the schema of all tables
schema <- lapply(dbListTables(conn), function(table) {
  paste("CREATE TABLE", table, "AS\n", dbGetQuery(conn, paste("SELECT sql FROM sqlite_master WHERE name = '", table, "';"))$sql)
})

# Combine schema into one script
schema_text <- paste(unlist(schema), collapse = "\n\n")

# Write the schema to a .sql file
writeLines(schema_text, "create_database.sql")

# Disconnect from the database
dbDisconnect(conn)

# Verify that the file was created
file.exists("create_database.sql")



