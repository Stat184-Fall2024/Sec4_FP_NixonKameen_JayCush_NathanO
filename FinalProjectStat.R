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

# Read all data and combine
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

# Rename columns to standard
colnames(all_data) <- standard_columns[1:ncol(all_data)]

# Data Cleaning
cleaned_data <- all_data %>%
  filter(!is.na(Rank)) %>%
  filter(!grepl("Avg|League|Total|Tm", Team)) %>%
  mutate(across(!c("Team"), as.numeric)) %>%
  mutate(
    SuperBowl_Winner = case_when(
      (Team == "Kansas City Chiefs" & Year == 2024) ~ 1,
      (Team == "Kansas City Chiefs" & Year == 2023) ~ 1,
      (Team == "Los Angeles Rams" & Year == 2022) ~ 1,
      (Team == "Tampa Bay Buccaneers" & Year == 2021) ~ 1,
      (Team == "Kansas City Chiefs" & Year == 2020) ~ 1,
      (Team == "New England Patriots" & Year == 2019) ~ 1,
      (Team == "Philadelphia Eagles" & Year == 2018) ~ 1,
      (Team == "New England Patriots" & Year == 2017) ~ 1,
      (Team == "Denver Broncos" & Year == 2016) ~ 1,
      (Team == "New England Patriots" & Year == 2015) ~ 1,
      TRUE ~ 0
    )
  )

# Ranking Within Each Year
ranked_data <- cleaned_data %>%
  group_by(Year) %>%
  mutate(across(
    where(is.numeric) & !c(SuperBowl_Winner), # Exclude SuperBowl_Winner
    ~ rank(-., ties.method = "min")
  )) %>%
  ungroup() %>%
  select(-Games, -Rank) 

# Basic summary statistics of numeric columns
overall_summary <- cleaned_data %>%
  select(where(is.numeric)) %>%
  summary()

overall_summary


# Create a column to indicate if a team has ever won a Super Bowl
aggregated_data <- cleaned_data %>%
  group_by(Team) %>%
  summarise(
    Total_Points_Scored = sum(Points_Scored, na.rm = TRUE),
    Ever_SuperBowl_Winner = ifelse(any(SuperBowl_Winner == "1"), "Winner", "Non-Winner"),
    .groups = "drop"
  )

# Plot with the updated column
ggplot(aggregated_data, aes(x = reorder(Team, Total_Points_Scored), y = Total_Points_Scored, fill = Ever_SuperBowl_Winner)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(
    values = c("Non-Winner" = "grey70", "Winner" = "darkgoldenrod2")
  ) +
  labs(
    title = "Total Points Scored by Team",
    subtitle = "Super Bowl Winners vs. Non-Winners",
    x = "Team",
    y = "Total Points Scored",
    fill = "Super Bowl Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(cleaned_data, aes(x = factor(SuperBowl_Winner), y = Yards, fill = factor(SuperBowl_Winner))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c("0" = "grey70", "1" = "darkgoldenrod2"),
    labels = c("Non-Winner", "Winner")
  ) +
  scale_x_discrete(
    labels = c("0" = "Non-Winner", "1" = "Winner")
  ) +
  labs(
    title = "Distribution of Total Yards",
    x = "",
    y = "Yards"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


yearly_stats <- cleaned_data %>%
  group_by(Year, SuperBowl_Winner) %>%
  summarise(avg_points = mean(Points_Scored, na.rm = TRUE), .groups = "drop")

ggplot(yearly_stats, aes(x = Year, y = avg_points, color = factor(SuperBowl_Winner))) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("0" = "grey70", "1" = "darkgoldenrod2"), 
                     labels = c("Non-Winners", "Winners")) +
  scale_x_continuous(breaks = yearly_stats$Year) +
  labs(title = "Average Points Scored Over Time",
       subtitle = "Super Bowl Winners vs. Non-Winners",
       x = "Year",
       y = "Average Points") +
  theme_minimal() +
  theme(legend.title = element_blank())


yearly_stats <- cleaned_data %>%
  group_by(Year, SuperBowl_Winner) %>%
  summarise(avg_yards = mean(Yards, na.rm = TRUE), .groups = "drop")

ggplot(yearly_stats, aes(x = Year, y = avg_yards, color = factor(SuperBowl_Winner))) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("0" = "grey70", "1" = "darkgoldenrod2"),
                     labels = c("Non-Winners", "Winners")) +
  scale_x_continuous(breaks = yearly_stats$Year) +
  labs(title = "Average Yards Over Time",
       subtitle = "Super Bowl Winners vs. Non-Winners",
       x = "Year",
       y = "Average Yards") +
  theme_minimal() +
  theme(legend.title = element_blank())



winners_over_time <- cleaned_data %>%
  filter(SuperBowl_Winner == 1) %>%
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
               names_to = "Statistic",
               values_to = "Value") %>%
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
       x = "Year", 
       y = "Value") +
  theme_minimal()


# Convert SuperBowl_Winner to numeric if not already done
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


correlationMat <- cor(cleaned_data[, sapply(cleaned_data, is.numeric)], use = "complete.obs")
correlation_with_SB_Winner <- correlationMat["SuperBowl_Winner", ]
correlation_with_SB_Winner <- sort(correlation_with_SB_Winner, decreasing = TRUE)
print(correlation_with_SB_Winner)

# Filter data for the 2024 season
teams_2024 <- cleaned_data %>%
  filter(Year == 2024) %>%
  group_by(Team) %>%
  summarise(
    Avg_Yards = mean(Yards, na.rm = TRUE),
    Avg_Points = mean(Points_Scored, na.rm = TRUE)
  )

# Create scatter plot with unique colors for each team
# Create bar chart
ggplot(teams_2024, aes(x = reorder(Team, Avg_Yards), y = Avg_Yards, fill = Team)) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  scale_fill_viridis_d() + # Optional: Use a visually appealing color palette
  labs(
    title = "Average Yards for NFL Teams (2024)",
    x = "Team",
    y = "Average Yards"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Rotate team names for readability
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )


# Create horizontal bar chart 
ggplot(teams_2024, aes(x = reorder(Team, Avg_Points), y = Avg_Points, fill = Team)) +
  geom_bar(stat = "identity", alpha = 0.8, show.legend = FALSE) +
  scale_fill_viridis_d() +
  labs(
    title = "Average Points for NFL Teams (2024)",
    x = "Average Points",
    y = "Team"
  ) +
  theme_minimal() +
  coord_flip() + # Flips the axes
  theme(
    axis.text.y = element_text(size = 10, margin = margin(r = 10), lineheight = 1.2), # Adjust text size, margin, and spacing
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )
