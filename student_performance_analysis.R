# R Script – Student Performance Analysis
# Keep the CSV file in the same folder as this R script.
# Change the filename below if your CSV has a different name.

X <- read.csv("student_performance_dataset_updated.csv")
View(X)

# Select numeric columns
num_cols <- c(
  "study_time_hours",
  "attendance_percent",
  "sleep_hours",
  "previous_grade",
  "final_exam_score"
)

# Calculate mode
get_mode <- function(x) {
  x <- x[!is.na(x)]
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Calculate summary statistics
summary_stats <- data.frame(
  Variable = num_cols,
  Mean = sapply(X[num_cols], function(x) round(mean(x, na.rm = TRUE), 2)),
  Median = sapply(X[num_cols], function(x) round(median(x, na.rm = TRUE), 2)),
  P25 = sapply(X[num_cols], function(x) round(quantile(x, 0.25, na.rm = TRUE), 2)),
  P50 = sapply(X[num_cols], function(x) round(quantile(x, 0.50, na.rm = TRUE), 2)),
  P75 = sapply(X[num_cols], function(x) round(quantile(x, 0.75, na.rm = TRUE), 2)),
  SD = sapply(X[num_cols], function(x) round(sd(x, na.rm = TRUE), 2)),
  Variance = sapply(X[num_cols], function(x) round(var(x, na.rm = TRUE), 2)),
  Range = sapply(X[num_cols], function(x) round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 2)),
  IQR = sapply(X[num_cols], function(x) round(IQR(x, na.rm = TRUE), 2)),
  Min = sapply(X[num_cols], function(x) round(min(x, na.rm = TRUE), 2)),
  Max = sapply(X[num_cols], function(x) round(max(x, na.rm = TRUE), 2))
)

# Display summary table
print(summary_stats)

# Gender mode
gender_mode <- get_mode(X$gender)
gender_mode

# Parental education mode
parent_mode <- get_mode(X$parental_education)
parent_mode

# Final grade mode
grade_mode <- get_mode(X$final_grade)
grade_mode

# Histograms
library(ggplot2)
library(gridExtra)

p1 <- ggplot(X, aes(x = study_time_hours)) +
  geom_histogram(bins = 20, fill = "skyblue") +
  theme_minimal() +
  labs(title = "Study Time Distribution")

p2 <- ggplot(X, aes(x = attendance_percent)) +
  geom_histogram(bins = 20, fill = "skyblue") +
  theme_minimal() +
  labs(title = "Attendance % Distribution")

p3 <- ggplot(X, aes(x = sleep_hours)) +
  geom_histogram(bins = 20, fill = "skyblue") +
  theme_minimal() +
  labs(title = "Sleep Hours Distribution")

p4 <- ggplot(X, aes(x = previous_grade)) +
  geom_histogram(bins = 20, fill = "skyblue") +
  theme_minimal() +
  labs(title = "Previous Grade Distribution")

p5 <- ggplot(X, aes(x = final_exam_score)) +
  geom_histogram(bins = 20, fill = "skyblue") +
  theme_minimal() +
  labs(title = "Final Exam Score Distribution")

# Arrange the graphs
grid.arrange(p1, p2, p3, p4, p5, ncol = 3)

# Bar graph
ggplot(X, aes(x = final_grade)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Final Grade Distribution", x = "Grade", y = "Count")

# Pie chart
ggplot(X, aes(x = "", fill = gender)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  labs(title = "Gender Composition")

# Scatter plot
ggplot(X, aes(x = study_time_hours, y = final_exam_score)) +
  geom_point(color = "skyblue") +
  labs(
    title = "Study Time vs Exam Score",
    x = "Study Hours",
    y = "Score"
  )

# Q-Q plot
qqnorm(X$final_exam_score, main = "Q-Q Plot of Final Exam Scores", col = "skyblue")
qqline(X$final_exam_score, col = "red", lwd = 2)

# Box plot
boxplot(
  final_exam_score ~ gender,
  data = X,
  main = "Final Exam Scores by Gender",
  xlab = "Gender",
  ylab = "Final Exam Score",
  col = "skyblue"
)
