

Sys.setenv(CFBD_API_KEY = "")
# Load the cfbfastR library
library(cfbfastR)
library(dplyr)

# Define the year and team (or use NULL to retrieve data for all teams)
year <- 2023
team <- NULL

# Get the roster data for players and coaches for the specified year and team
players_data <- cfbd_team_roster(year, team)
coaches_data <- cfbd_coaches(year = year)

# Define the variations of the name "Tom"
target_names <- c("Tom", "Thomas", "Tommy", "Tomás", "Thom", "Thompson")

# Filter players with the target names
players_with_target_names <- players_data %>%
  filter(grepl(paste(target_names, collapse = "|"), first_name))

# Filter coaches with the target names
coaches_with_target_names <- coaches_data %>%
  filter(grepl(paste(target_names, collapse = "|"), first_name))

# Group players by team and count the number of Toms
players_grouped <- players_with_target_names %>%
  group_by(team) %>%
  summarize(TomCount = n(), TomNames = paste(first_name, last_name))

# Group coaches by team and count the number of Toms
coaches_grouped <- coaches_with_target_names %>%
  group_by(school) %>%
  summarize(TomCount = n(), TomNames = toString(last_name))

# Print the table for players with the number of Toms and their names
cat("Players with 'Tom' Names Grouped by Team:\n")
print(players_grouped)

# Print the table for coaches with the number of Toms and their names
cat("\nCoaches with 'Tom' Names Grouped by Team:\n")
print(coaches_grouped)

library(dplyr)

# Combine the players and coaches groups into one super table
combined_table <- full_join(players_grouped, coaches_grouped, by = c("team" = "school"))

# Fill missing values with 0
combined_table <- combined_table %>%
  mutate(
    TomCount.x = coalesce(TomCount.x, 0),
    TomNames.x = coalesce(TomNames.x, ""),
    TomCount.y = coalesce(TomCount.y, 0),
    TomNames.y = coalesce(TomNames.y, "")
  )

# Calculate the total Tom count (sum of players and coaches)
combined_table$TotalTomCount <- combined_table$TomCount.x + combined_table$TomCount.y

# Print the combined table
cat("Combined Table:\n")
print(combined_table)
library(dplyr)

# Sort the combined table by Tom count
combined_table_sorted <- combined_table %>%
  arrange(desc(TotalTomCount))

# Sort the coaches' table by Tom count
coaches_grouped_sorted <- coaches_grouped %>%
  arrange(desc(TomCount))

# Sort the players' table by Tom count
players_grouped_sorted <- players_grouped %>%
  arrange(desc(TomCount))

# Print the top 15 results for combined_table
cat("Top 15 Teams in Combined Table (by Tom Count):\n")
print(head(combined_table_sorted, 15))

# Print the top 15 teams for coaches' table
cat("\nTop 15 Teams in Coaches' Table (by Tom Count):\n")
print(head(coaches_grouped_sorted, 15))

# Print the top 15 teams for players' table
cat("\nTop 15 Teams in Players' Table (by Tom Count):\n")
print(head(players_grouped_sorted, 35), n=35)


