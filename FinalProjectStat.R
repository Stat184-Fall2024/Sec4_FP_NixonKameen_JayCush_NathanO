library(dplyr)
library(readxl)

# Define the URLs for the data
links <- list(
  "2021" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2021%20NFL%20season.xltx",
  "2022" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2022%20NFL%20season.xltx",
  "2023" = "https://raw.githubusercontent.com/Stat184-Fall2024/Sec4_FP_NixonKameen_JayCush_NathanO/main/2023%20NFL%20season.xltx"
)

# Load and combine data
all_data <- lapply(names(links), function(year) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(links[[year]], temp_file, mode = "wb")
  read_excel(temp_file) %>%
    mutate(Year = as.integer(year))  # Add a Year column
}) %>%
  bind_rows()

# View combined data
View(all_data)

# Define standard column names
standard_columns <- c(
  "Rank", "Team", "Games", "Points_Scored", "Yards", "Plays", "Yards_Per_Play",
  "Turnovers", "Fumbles_Lost", "First_Downs", "Pass_Completions",
  "Pass_Attempts", "Pass_Yards", "Pass_Touchdowns", "Pass_Interceptions",
  "Net_Yards_Per_Pass_Attempt", "First_Downs_Passing", "Rush_Attempts", "Rush_Yards",
  "Rush_Touchdowns", " Rush_Yards_Per_Attempt", "First_Downs_Rushing", "Penalties", "Penalties_In_Yards",
  "First_Downs_By_Penalty", "%_Drives_Ending_In_Score", "%_Drives_Ending_In_Turnover",
  "Expected_Points_Contributed", "Year"
)
# Rename columns
colnames(all_data) <- standard_columns[1:ncol(all_data)]

cleaned_data <- all_data %>%
  filter(!is.na(Rank)) %>%
  filter(!grepl("Avg|League|Total|Tm", Team)) %>%
  mutate(across(!c("Team"), as.numeric))

# Rank teams in each stat within each year, replacing original values with ranks
ranked_data <- cleaned_data %>%
  group_by(Year) %>%  # Group by Year
  mutate(
    across(
    where(is.numeric),  # Apply ranking to numeric columns
    ~ rank(-., ties.method = "min")  # Rank in descending order (highest value = rank 1)
  )) %>%
  ungroup() %>%
  select(-("Games"), -("Rank")) %>%
  mutate(
    case_when(
      (Team == "Kansas City Chiefs") & (Year == 2023) ~ TRUE,
      (Team == "Los Angeles Rams") & (Year == 2022) ~ TRUE,
      (Team == "Tampa Bay Buccaneers") & (Year == 2021) ~ TRUE,
      .default = FALSE)) %>%
  rename("SuperBowl_Winner" = "case_when(...)")




