# Load Required Libraries
library(dplyr)
library(readxl)
library(esquisse)
library(ggplot2)
library(tidyr)
library(scales)
library(reshape2)
library(corrplot)

# Data Download and Preparation
links <- list(
  "2015" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2015%20NFL%20season.xltx",
  "2016" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2016%20NFL%20season.xltx",
  "2017" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2017%20NFL%20season.xltx",
  "2018" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2018%20NFL%20season.xltx",
  "2019" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2019%20NFL%20season.xltx",
  "2020" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2020%20NFL%20season.xltx",
  "2021" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2021%20NFL%20season.xltx",
  "2022" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2022%20NFL%20season.xltx",
  "2023" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2023%20NFL%20season.xltx",
  "2024" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2024%20NFL%20season.xltx"
)

all_data <- lapply(names(links), function(year) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(links[[year]], temp_file, mode = "wb")
  read_excel(temp_file) %>%
    mutate(Year = as.integer(year))
}) %>%
  bind_rows()

# Define standard column names
standard_columns <- c(
  "Rank", "Team", "Games", "Points_Scored", "Yards", "Plays", "Yards_Per_Play",
  "Turnovers", "Fumbles_Lost", "First_Downs", "Pass_Completions", "Pass_Attempts",
  "Pass_Yards", "Pass_Touchdowns", "Pass_Interceptions", "Net_Yards_Per_Pass_Attempt",
  "First_Downs_Passing", "Rush_Attempts", "Rush_Yards", "Rush_Touchdowns",
  "Rush_Yards_Per_Attempt", "First_Downs_Rushing", "Penalties", "Penalties_In_Yards",
  "First_Downs_By_Penalty", "%_Drives_Ending_In_Score", "%_Drives_Ending_In_Turnover",
  "Expected_Points_Contributed", "Year"
)

colnames(all_data) <- standard_columns[1:ncol(all_data)]

# Data Cleaning
cleaned_data <- all_data %>%
  filter(!is.na(Rank)) %>%
  filter(!grepl("Avg|League|Total|Tm", Team)) %>%
  mutate(across(!c("Team"), as.numeric)) %>%
  mutate(
    SuperBowl_Winner = case_when(
      (Team == "Kansas City Chiefs" & Year == 2024) ~ TRUE,
      (Team == "Kansas City Chiefs" & Year == 2023) ~ TRUE,
      (Team == "Los Angeles Rams" & Year == 2022) ~ TRUE,
      (Team == "Tampa Bay Buccaneers" & Year == 2021) ~ TRUE,
      (Team == "Kansas City Chiefs" & Year == 2020) ~ TRUE,
      (Team == "New England Patriots" & Year == 2019) ~ TRUE,
      (Team == "Philadelphia Eagles" & Year == 2018) ~ TRUE,
      (Team == "New England Patriots" & Year == 2017) ~ TRUE,
      (Team == "Denver Broncos" & Year == 2016) ~ TRUE,
      (Team == "New England Patriots" & Year == 2015) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Ranking Within Each Year (Optional)
ranked_data <- cleaned_data %>%
  group_by(Year) %>%
  mutate(across(where(is.numeric), ~ rank(-., ties.method = "min"))) %>%
  ungroup() %>%
  select(-Games, -Rank)

# Basic Statistical Analysis
overall_summary <- cleaned_data %>%
  select(where(is.numeric)) %>%
  summary()

print(overall_summary)

grouped_summary <- cleaned_data %>%
  group_by(SuperBowl_Winner) %>%
  summarise(across(where(is.numeric),
                   list(mean = mean, sd = sd, median = median, min = min, max = max),
                   na.rm = TRUE), .groups = "drop")

# Visualizations: Distribution of Key Stats
ggplot(cleaned_data, aes(x = Points_Scored, fill = SuperBowl_Winner)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "darkgoldenrod2")) +
  labs(title = "Distribution of Points Scored",
       subtitle = "Super Bowl Winners vs. Non-Winners",
       x = "Points Scored", y = "Count") +
  theme_minimal()

ggplot(cleaned_data, aes(x = SuperBowl_Winner, y = Yards, fill = SuperBowl_Winner)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "darkgoldenrod2")) +
  labs(title = "Distribution of Total Yards", x = "", y = "Yards") +
  theme_minimal() +
  theme(legend.position = "none")

# Yearly Trends
yearly_stats <- cleaned_data %>%
  group_by(Year, SuperBowl_Winner) %>%
  summarise(
    avg_points = mean(Points_Scored, na.rm = TRUE),
    avg_yards = mean(Yards, na.rm = TRUE),
    avg_turnovers = mean(Turnovers, na.rm = TRUE),
    avg_yards_per_play = mean(Yards_Per_Play, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(yearly_stats, aes(x = Year, y = avg_points, color = SuperBowl_Winner)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "darkgoldenrod2"),
                     labels = c("Non-Winners", "Super Bowl Winners")) +
  scale_x_continuous(breaks = yearly_stats$Year) +
  labs(title = "Average Points Scored Over Time",
       subtitle = "Super Bowl Winners vs. Non-Winners",
       x = "Year", y = "Average Points") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Example with avg_yards
ggplot(yearly_stats, aes(x = Year, y = avg_yards, color = SuperBowl_Winner)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "darkgoldenrod2")) +
  scale_x_continuous(breaks = yearly_stats$Year) +
  labs(title = "Average Yards Over Time",
       subtitle = "Super Bowl Winners vs. Non-Winners",
       x = "Year", y = "Average Yards") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Trends in Winners Only
winners_over_time <- cleaned_data %>%
  filter(SuperBowl_Winner == TRUE) %>%
  group_by(Year) %>%
  summarise(
    median_points = median(Points_Scored, na.rm = TRUE),
    median_yards = median(Yards, na.rm = TRUE),
    median_turnovers = median(Turnovers, na.rm = TRUE),
    median_pass_tds = median(Pass_Touchdowns, na.rm = TRUE),
    .groups = "drop"
  )

winners_long <- winners_over_time %>%
  pivot_longer(cols = c("median_points", "median_yards", "median_turnovers", "median_pass_tds"), 
               names_to = "Statistic", values_to = "Value") %>%
  mutate(Statistic = recode(Statistic, 
                            "median_points" = "Points",
                            "median_yards" = "Yards",
                            "median_turnovers" = "Turnovers",
                            "median_pass_tds" = "Pass Touchdowns"))

ggplot(winners_long, aes(x = Year, y = Value)) +
  geom_line(color = "darkgoldenrod2", size = 1.2) +
  geom_point(color = "darkgoldenrod2") +
  facet_wrap(~Statistic, scales = "free_y") +
  scale_x_continuous(breaks = winners_long$Year) +
  labs(title = "Key Performance Metrics for Super Bowl Winners Over Time",
       x = "Year", y = "Value") +
  theme_minimal()

# Correlation Analysis
# Convert SuperBowl_Winner to numeric for correlation
cleaned_data <- cleaned_data %>%
  mutate(SuperBowl_Winner = as.numeric(SuperBowl_Winner))

numeric_data <- cleaned_data %>%
  select(where(is.numeric))

corr_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

# Correlation Heatmap
melted_corr <- melt(corr_matrix)
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Correlation Heatmap Including SuperBowl_Winner")

# Focus on correlations involving SuperBowl_Winner
sbw_correlations <- melted_corr %>%
  filter(Var1 == "SuperBowl_Winner" | Var2 == "SuperBowl_Winner") %>%
  filter(Var1 != Var2) %>%
  arrange(desc(abs(value)))

View(sbw_correlations)
