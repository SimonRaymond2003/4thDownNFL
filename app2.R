# app2
  
# global
  
# First, modify your library and data loading section at the top:
library(readr)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(DT)
library(shinyWidgets) # Add this for materialSwitch
library(shinyjs)
library(lubridate)
library(data.table)
library(later)

# Load data using data.table for better performance
df_pbp <- fread("df_pbp.csv")
pff_stats_no_grades <- fread("pff_stats_no_grades.csv")
data_all <- fread("C:/Users/simon/Dropbox/My_Projects/Honours_Data/all_data.csv")
decisions_data <- fread("C:/Users/simon/Dropbox/My_Projects/Honours_Data/decisions_data.csv")
# Utils

# Function to check column variance (excluding NAs)
check_zero_variance <- function(x) {
  if (all(is.na(x))) return(TRUE)
  unique_vals <- unique(x[!is.na(x)])
  return(length(unique_vals) <= 1)
}

# Function to clean position-specific dataframe
clean_position_df <- function(df, na_threshold = 0.95) {
  initial_cols <- ncol(df)
  
  # Get columns to remove based on NA percentage
  na_percentages <- colMeans(is.na(df))
  high_na_cols <- names(which(na_percentages >= na_threshold))
  
  # Get columns with zero variance
  zero_var_cols <- names(which(sapply(df, check_zero_variance)))
  
  # Combine columns to remove, ensuring 'position' is kept
  cols_to_remove <- unique(c(high_na_cols, zero_var_cols))
  cols_to_remove <- setdiff(cols_to_remove, "position")
  
  # Remove identified columns
  df_cleaned <- df[, setdiff(names(df), cols_to_remove), with = FALSE]
  
  return(df_cleaned)
}

# Split by position and clean each position's data
# After loading data and before the position processing loop, add:
position_dfs <- list()

# Split by position and clean each position's data
positions <- unique(pff_stats_no_grades$position)
positions <- positions[positions != ""]  # Remove empty position value

for(pos in positions) {
  # Filter for position
  pos_df <- pff_stats_no_grades[position == pos]
  
  # Clean the position-specific dataframe
  pos_df_cleaned <- clean_position_df(pos_df)
  
  # Store in list
  position_dfs[[pos]] <- pos_df_cleaned
}

# Create individual dataframes in the environment
for(pos in positions) {
  clean_name <- tolower(gsub(" ", "_", pos))
  assign(paste0("pff_", clean_name), position_dfs[[pos]])
}

# NFL Field Constants and Formation Functions
FIELD_LENGTH <- 120  # Total length including endzones
FIELD_WIDTH <- 53.3  # Official NFL width (53 1/3 yards)
ENDZONE_LENGTH <- 10 # Each endzone is 10 yards
PLAYING_FIELD_LENGTH <- 100 # Length between endzones

# Function to create formation data frames with more specific formations
create_formation_data <- function(formation_type, personnel, los_position, ydstogo, going_right) {
  # Adjust LOS position for players based on direction
  player_los_position <- if (going_right) los_position else abs(100 - los_position)
  
  # Define offensive formations with specific alignments
  offensive_players <- list(
    "SHOTGUN" = tibble(
      position = c("QB", "RB", "LT", "LG", "C", "RG", "RT", "WR1", "WR2", "WR3", "TE"),
      x = c(-5, -5, 0, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, 2, -3, -1.5, 0, 1.5, 3, -12, 12, -8, 5),
      type = "offense"
    ),
    "EMPTY" = tibble(
      position = c("QB", "LT", "LG", "C", "RG", "RT", "WR1", "WR2", "WR3", "WR4", "WR5"),
      x = c(-5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, -3, -1.5, 0, 1.5, 3, -15, -8, 8, 15, 0),
      type = "offense"
    ),
    "I_FORM" = tibble(
      position = c("QB", "FB", "RB", "LT", "LG", "C", "RG", "RT", "WR1", "WR2", "TE"),
      x = c(-2, -4, -6, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, 0, 0, -3, -1.5, 0, 1.5, 3, -12, 12, 5),
      type = "offense"
    ),
    "SINGLEBACK" = tibble(
      position = c("QB", "RB", "LT", "LG", "C", "RG", "RT", "WR1", "WR2", "TE1", "TE2"),
      x = c(-2, -5, 0, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, 0, -3, -1.5, 0, 1.5, 3, -12, 12, 5, -5),
      type = "offense"
    ),
    "PISTOL" = tibble(
      position = c("QB", "RB", "LT", "LG", "C", "RG", "RT", "WR1", "WR2", "WR3", "TE"),
      x = c(-3, -6, 0, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, 0, -3, -1.5, 0, 1.5, 3, -12, 12, -8, 5),
      type = "offense"
    ),
    "JUMBO" = tibble(
      position = c("QB", "FB", "RB", "LT", "LG", "C", "RG", "RT", "TE1", "TE2", "TE3"),
      x = c(-2, -4, -6, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, 0, 0, -3, -1.5, 0, 1.5, 3, 5, -5, 2),
      type = "offense"
    ),
    "WILDCAT" = tibble(
      position = c("RB1", "RB2", "LT", "LG", "C", "RG", "RT", "WR1", "WR2", "TE1", "TE2"),
      x = c(0, -5, 0, 0, 0, 0, 0, 0, 0, 0, 0) + player_los_position,
      y = c(0, 2, -3, -1.5, 0, 1.5, 3, -12, 12, 5, -5),
      type = "offense"
    )
  )
  
  # Define defensive formations based on coverage type
  defensive_formations <- list(
    "COVER_0" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 8, 8, 5, 5) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -12, 12, -6, 6),
      type = "defense"
    ),
    "COVER_1" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 8, 8, 5, 12) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -12, 12, 2, 0),
      type = "defense"
    ),
    "COVER_2" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 8, 8, 12, 12) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -10, 10, -6, 6),
      type = "defense"
    ),
    "COVER_3" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 8, 8, 12, 15) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -10, 10, 0, 0),
      type = "defense"
    ),
    "COVER_4" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 12, 12, 12, 12) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -8, 8, -3, 3),
      type = "defense"
    ),
    "COVER_6" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 12, 8, 12, 15) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -8, 8, 4, 0),
      type = "defense"
    ),
    "2_MAN" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 8, 8, 12, 15) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -12, 12, 0, 0),
      type = "defense"
    ),
    "PREVENT" = tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "LB1", "LB2", "LB3", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 8, 8, 8, 15, 15, 15, 15) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -10, 10, -5, 5),
      type = "defense"
    )
  )
  
  # Get the appropriate offensive formation or default to SHOTGUN if not found
  off_players <- offensive_players[[formation_type]]
  if(is.null(off_players)) {
    off_players <- offensive_players[["SHOTGUN"]]
  }
  
  # Get the appropriate defensive formation based on coverage type
  # If coverage type is NA or not found, use a default 4-3 base alignment
  def_formation <- if(!is.null(personnel) && grepl("4 DL", personnel)) {
    tibble(
      position = c("DE1", "DT1", "DT2", "DE2", "WLB", "MLB", "SLB", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 2, 5, 5, 5, 8, 8, 8, 12) + player_los_position,
      y = c(-4, -1.5, 1.5, 4, -3, 0, 3, -10, 10, 2, 0),
      type = "defense"
    )
  } else {
    tibble(
      position = c("DE1", "NT", "DE2", "WLB", "ILB1", "ILB2", "SLB", "CB1", "CB2", "SS", "FS"),
      x = c(2, 2, 2, 5, 5, 5, 5, 8, 8, 8, 12) + player_los_position,
      y = c(-3, 0, 3, -4, -1.5, 1.5, 4, -10, 10, 2, 0),
      type = "defense"
    )
  }
  
  # Combine offensive and defensive players
  formation_data <- bind_rows(off_players, def_formation)
  
  # Flip coordinates if going left
  if(!going_right) {
    formation_data <- formation_data %>%
      mutate(
        x = 100 - x,
        y = -y
      )
  }
  
  return(formation_data)
}

# Function to get position summary
get_position_summary <- function(position_df) {
  list(
    total_rows = nrow(position_df),
    total_cols = ncol(position_df),
    numeric_cols = sum(sapply(position_df, is.numeric)),
    categorical_cols = sum(sapply(position_df, is.factor)) + sum(sapply(position_df, is.character)),
    na_percentage = mean(is.na(position_df)) * 100
  )
}

calculate_player_averages <- function(player_data) {
  # Columns to exclude completely
  exclude_cols <- c("player", "player_id", "year", "week", "team_name", "position")
  
  # Find percentage columns to exclude from totals
  percentage_cols <- grep("percent|percentage|%", names(player_data), value = TRUE)
  
  player_data %>%
    select(-all_of(exclude_cols)) %>%
    summarise(
      across(where(is.numeric),
             list(
               avg = ~round(mean(.x, na.rm = TRUE), 2),
               na_count = ~sum(is.na(.x)),
               total = ~if(cur_column() %in% percentage_cols) NULL
               else round(sum(.x, na.rm = TRUE), 2)
             ),
             .names = "{.col}_{.fn}")) %>%
    select(-matches("percent|percentage|%.*_total$"))
}

plot_formation <- function(play_data) {
  # Determine play direction based on possession team type
  going_right <- play_data$posteam_type == "away"
  
  # Adjust LOS and first down line based on play direction
  los_position <- play_data$LOS
  first_down_position <- if (going_right) los_position + play_data$ydstogo else los_position - play_data$ydstogo
  
  # Create formation data with LOS position for players
  formation_data <- create_formation_data(
    play_data$offense_formation, 
    play_data$defense_personnel,
    los_position,
    play_data$ydstogo,
    going_right
  )
  
  # Create yard line labels
  yard_lines <- data.frame(
    x = seq(0, 100, 10),
    label = c("G", "10", "20", "30", "40", "50", "40", "30", "20", "10", "G")
  )
  
  # Create endzone text data (home always right)
  endzone_text <- data.frame(
    x = c(-5, 105),
    y = 0,
    label = c(play_data$away_team, play_data$home_team),
    angle = c(90, -90)
  )
  
  # Create grass texture data
  x_coords <- seq(-ENDZONE_LENGTH, FIELD_LENGTH - ENDZONE_LENGTH, 5)
  y_coords <- seq(-FIELD_WIDTH/2, FIELD_WIDTH/2, 1)
  
  grass_data <- expand.grid(
    x = x_coords[1:(length(x_coords)-1)],
    y = y_coords
  ) %>%
    mutate(
      xend = x + 5,
      yend = y
    )
  
  # Create hash marks data
  nfl_hash_distance <- 70.75/3
  hash_marks <- data.frame(
    x = rep(seq(0, 100, 1), each = 4),
    xend = rep(seq(0, 100, 1), each = 4),
    y = rep(c(-nfl_hash_distance, -(nfl_hash_distance-0.5), 
              nfl_hash_distance-0.5, nfl_hash_distance), length(seq(0, 100, 1))),
    yend = rep(c(-(nfl_hash_distance-0.5), -nfl_hash_distance, 
                 nfl_hash_distance, nfl_hash_distance-0.5), length(seq(0, 100, 1)))
  )
  
  # Get quarter and time remaining
  minutes <- floor(play_data$game_seconds_remaining/60)
  seconds <- play_data$game_seconds_remaining %% 60
  
  # Create main plot title
  main_title <- paste0(
    play_data$posteam, " vs ", 
    ifelse(play_data$posteam == play_data$home_team, play_data$away_team, play_data$home_team),
    "  |  Q", play_data$qtr, " (", sprintf("%d:%02d", minutes, seconds), ")"
  )
  
  # Create defensive formation description
  defense_desc <- if(grepl("4 DL", play_data$defense_personnel)) {
    "4-3 Defense"
  } else {
    "3-4 Defense"
  }
  
  # Add coverage type to defensive description
  coverage_desc <- if(is.na(play_data$defense_coverage_type)) {
    "Unknown Coverage"
  } else {
    gsub("_", " ", play_data$defense_coverage_type)
  }
  
  # Create subtitle with formation and situation including coverage
  sub_title <- sprintf("%s Formation vs %s (%s)  |  %s Ball on %s  |  %d yard(s) to go",
                       play_data$offense_formation,
                       defense_desc,
                       coverage_desc,
                       play_data$posteam,
                       play_data$side_of_field,
                       play_data$ydstogo)
  
  # Create the base plot
  p <- ggplot(formation_data, aes(x = x, y = y, color = type, shape = type)) +
    # Field background
    annotate("rect", xmin = -ENDZONE_LENGTH, xmax = FIELD_LENGTH - ENDZONE_LENGTH,
             ymin = -FIELD_WIDTH/2, ymax = FIELD_WIDTH/2,
             fill = "#2E7D32", color = "white") +
    
    # Add grass texture
    geom_segment(data = grass_data,
                 aes(x = x, y = y, xend = xend, yend = y),
                 inherit.aes = FALSE,
                 color = "#235B24", 
                 alpha = 0.2) +
    
    # Endzones
    annotate("rect", xmin = -ENDZONE_LENGTH, xmax = 0,
             ymin = -FIELD_WIDTH/2, ymax = FIELD_WIDTH/2,
             fill = "#1B5E20", color = "white") +
    annotate("rect", xmin = PLAYING_FIELD_LENGTH, xmax = FIELD_LENGTH - ENDZONE_LENGTH,
             ymin = -FIELD_WIDTH/2, ymax = FIELD_WIDTH/2,
             fill = "#1B5E20", color = "white") +
    
    # Endzone team names
    geom_text(data = endzone_text,
              aes(x = x, y = y, label = label, angle = angle),
              color = "white",
              size = 10,
              fontface = "bold",
              inherit.aes = FALSE) +
    
    # Yard lines
    geom_vline(xintercept = seq(0, 100, 5),
               color = "white", alpha = 0.3) +
    
    # NFL Hash marks
    geom_segment(data = hash_marks,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 inherit.aes = FALSE,
                 color = "white",
                 alpha = 0.5) +
    
    # Yard numbers
    geom_text(data = yard_lines,
              aes(x = x, y = FIELD_WIDTH/2 - 5, label = label),
              color = "white",
              size = 3,
              inherit.aes = FALSE) +
    
    # Line of scrimmage
    geom_vline(xintercept = los_position,
               color = "blue", linewidth = 1, alpha = 0.8) +
    
    # First down line
    geom_vline(xintercept = first_down_position,
               color = "yellow", size = 1, alpha = 0.8) +
    
    # Players
    geom_point(size = 4, na.rm = TRUE) +
    geom_text(aes(label = position), 
              color = "white", 
              size = 3,
              vjust = -1.5,
              na.rm = TRUE) +
    
    # Formatting
    scale_color_manual(values = c("offense" = "red", "defense" = "blue")) +
    scale_shape_manual(values = c("offense" = 16, "defense" = 17)) +
    scale_x_continuous(limits = c(-ENDZONE_LENGTH, FIELD_LENGTH - ENDZONE_LENGTH)) +
    scale_y_continuous(limits = c(-FIELD_WIDTH/2 - 2, FIELD_WIDTH/2 + 2)) +
    
    # Theme
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "#2E7D32"),
      plot.title = element_text(
        color = "black",
        hjust = 0.5,
        size = 14,
        face = "bold",
        margin = margin(t = 10, b = 5)
      ),
      plot.subtitle = element_text(
        color = "black",
        hjust = 0.5,
        size = 12,
        margin = margin(b = 10)
      )
    ) +
    
    # Maintain proper NFL field aspect ratio
    coord_fixed(ratio = 1) +
    
    # Title and subtitle
    labs(
      title = main_title,
      subtitle = sub_title
    )
  
  return(p)
}
# Update the plot_fourth_down_distance function
plot_fourth_down_distance <- function(df, selected_year, selected_weeks) {
  df_filtered <- df %>%
    filter(
      season == selected_year,
      season_type == "REG",
      week >= selected_weeks[1],
      week <= selected_weeks[2]
    ) %>%
    mutate(distance_group = case_when(
      ydstogo <= 2 ~ "1-2 yards",
      ydstogo <= 4 ~ "3-4 yards",
      ydstogo <= 6 ~ "5-6 yards",
      ydstogo <= 8 ~ "7-8 yards",
      TRUE ~ "9+ yards"
    ))
  
  df_summary <- df_filtered %>%
    group_by(posteam, distance_group) %>%
    summarise(attempts = n(), .groups = 'drop') %>%
    group_by(posteam) %>%
    mutate(total_attempts = sum(attempts)) %>%
    ungroup() %>%
    arrange(desc(total_attempts))
  
  team_order <- df_summary %>%
    select(posteam, total_attempts) %>%
    distinct() %>%
    arrange(desc(total_attempts)) %>%
    pull(posteam)
  
  ggplot(df_summary, aes(x = attempts, y = factor(posteam, levels = team_order), 
                         fill = factor(distance_group, 
                                       levels = c("9+ yards", "7-8 yards", 
                                                  "5-6 yards", "3-4 yards",
                                                  "1-2 yards")))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("9+ yards" = "#4682B4",     
                                 "7-8 yards" = "#6BB5FF",      
                                 "5-6 yards" = "#FFB347",      
                                 "3-4 yards" = "#FF8C42",      
                                 "1-2 yards" = "#FF4B4B"),     
                      name = "Distance to Go") +
    labs(title = paste("Fourth Down Attempts by Distance -", selected_year,
                       "\nWeeks", selected_weeks[1], "to", selected_weeks[2]),
         x = "Number of Attempts",
         y = "Team") +
    theme_minimal()
}

# Update the plot_fourth_down_success function
plot_fourth_down_success <- function(df, selected_year, selected_weeks) {
  df_filtered <- df %>%
    filter(
      season == selected_year,
      season_type == "REG",
      week >= selected_weeks[1],
      week <= selected_weeks[2]
    ) %>%
    mutate(success = if_else(fourth_down_converted == 1, 
                             "Successful", "Unsuccessful"))
  
  df_summary <- df_filtered %>%
    group_by(posteam, success) %>%
    summarise(attempts = n(), .groups = 'drop') %>%
    group_by(posteam) %>%
    mutate(total_attempts = sum(attempts)) %>%
    ungroup() %>%
    arrange(desc(total_attempts))
  
  team_order <- df_summary %>%
    select(posteam, total_attempts) %>%
    distinct() %>%
    arrange(desc(total_attempts)) %>%
    pull(posteam)
  
  ggplot(df_summary, aes(x = attempts, y = factor(posteam, levels = team_order), 
                         fill = success)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Successful" = "#A8E6CF",     
                                 "Unsuccessful" = "#FFB7B2"),    
                      name = "Attempt Result") +
    labs(title = paste("Fourth Down Success Rate -", selected_year,
                       "\nWeeks", selected_weeks[1], "to", selected_weeks[2]),
         x = "Number of Attempts",
         y = "Team") +
    theme_minimal()
}
# Fourth Down Decisions Tab Functions
create_decision_type_plot <- function(data, selected_teams) {
  team_decisions <- data %>%
    filter(posteam %in% selected_teams) %>%
    group_by(posteam, decision) %>%
    summarise(count = n(), .groups = 'drop')
  
  team_totals <- team_decisions %>%
    group_by(posteam) %>%
    summarise(total = sum(count)) %>%
    arrange(desc(total))
  
  team_decisions$posteam <- factor(team_decisions$posteam, 
                                   levels = team_totals$posteam)
  
  ggplot(team_decisions, aes(x = count, y = posteam, fill = decision)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c("Go For It" = "#2ecc71", 
                                 "Field Goal" = "#3498db", 
                                 "Punt" = "#e74c3c")) +
    labs(title = "Fourth Down Decisions by Team",
         x = "Number of Decisions",
         y = "Team",
         fill = "Decision Type") +
    theme_minimal()
}

create_yards_plot <- function(data, selected_teams) {
  team_yards_summary <- data %>%
    filter(posteam %in% selected_teams) %>%
    group_by(posteam, yards_category) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(yards_category = factor(yards_category, 
                                   levels = c("1 or less", "2-3", "4-5", "6-10", "10+", "Unknown")))
  
  team_totals <- team_yards_summary %>%
    group_by(posteam) %>%
    summarise(total = sum(count)) %>%
    arrange(desc(total))
  
  team_yards_summary$posteam <- factor(team_yards_summary$posteam, 
                                       levels = team_totals$posteam)
  
  ggplot(team_yards_summary, 
         aes(x = count, y = posteam, fill = yards_category)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = c(
      "1 or less" = "#1a9850",
      "2-3" = "#91cf60",
      "4-5" = "#d9ef8b",
      "6-10" = "#fee08b",
      "10+" = "#d73027",
      "Unknown" = "grey"
    )) +
    labs(title = "Fourth Down Plays by Distance",
         x = "Number of Fourth Downs",
         y = "Team",
         fill = "Yards to Go") +
    theme_minimal()
}

create_rates_plot <- function(data, selected_teams, color_by_success = TRUE) {
  team_rates <- data %>%
    filter(posteam %in% selected_teams) %>%
    group_by(posteam) %>%
    summarise(
      total_fourth_downs = n(),
      go_for_it_attempts = sum(decision == "Go For It"),
      attempt_rate = go_for_it_attempts / total_fourth_downs,
      successes = sum(fourth_down_converted == 1, na.rm = TRUE),
      success_rate = successes / go_for_it_attempts,
      avg_yards_to_go = mean(ydstogo[decision == "Go For It"], na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(attempt_rate))
  
  team_rates$posteam <- factor(team_rates$posteam, levels = team_rates$posteam)
  
  if(color_by_success) {
    p <- ggplot(team_rates, aes(x = attempt_rate, y = posteam, fill = success_rate))
    fill_scale <- scale_fill_gradient(low = "#e74c3c", high = "#2ecc71", 
                                      labels = scales::percent)
    fill_label <- "Success Rate"
    subtitle <- "Teams ordered by attempt rate, colored by success rate"
  } else {
    p <- ggplot(team_rates, aes(x = attempt_rate, y = posteam, fill = avg_yards_to_go))
    fill_scale <- scale_fill_gradient(low = "#2ecc71", high = "#e74c3c",
                                      labels = scales::number_format(accuracy = 0.1))
    fill_label <- "Avg Yards to Go"
    subtitle <- "Teams ordered by attempt rate, colored by average yards to go"
  }
  
  p + geom_bar(stat = "identity") +
    fill_scale +
    scale_x_continuous(labels = scales::percent) +
    labs(
      title = "Fourth Down Attempt Rates by Team",
      subtitle = subtitle,
      x = "Attempt Rate (Go For It / Total 4th Downs)",
      y = "Team",
      fill = fill_label
    ) +
    theme_minimal()
}

# Helper functions for statistics
create_summary_stats <- function(data) {
  data %>%
    summarise(
      total_plays = n(),
      go_for_it = sum(decision == "Go For It"),
      punts = sum(decision == "Punt"),
      field_goals = sum(decision == "Field Goal"),
      success_rate = mean(fourth_down_converted == 1, na.rm = TRUE),
      avg_distance = mean(ydstogo, na.rm = TRUE)
    )
}

create_team_stats <- function(data) {
  data %>%
    group_by(posteam) %>%
    summarise(
      Total_4th_Downs = n(),
      Go_For_It = sum(decision == "Go For It"),
      Go_For_It_Pct = sprintf("%.1f%%", 100 * mean(decision == "Go For It")),
      Success_Rate = sprintf("%.1f%%", 100 * mean(fourth_down_converted == 1, na.rm = TRUE)),
      Avg_Distance = sprintf("%.1f", mean(ydstogo)),
      Punts = sum(decision == "Punt"),
      Field_Goals = sum(decision == "Field Goal"),
      .groups = 'drop'
    ) %>%
    arrange(desc(Total_4th_Downs))
}

# Function to create player table (continued)
create_player_table <- function(play_data, type = "offense") {
  player_indices <- 1:11
  
  table_data <- lapply(player_indices, function(i) {
    prefix <- paste0(type, "_player_", i)
    
    if(is.na(play_data[[paste0(prefix, "_first_name")]])) {
      return(NULL)
    }
    
    data.frame(
      Position = play_data[[paste0(prefix, "_position")]],
      Name = paste(play_data[[paste0(prefix, "_first_name")]],
                   play_data[[paste0(prefix, "_last_name")]]),
      Height = sprintf("%d'%d\"", 
                       floor(play_data[[paste0(prefix, "_height")]]/12),
                       play_data[[paste0(prefix, "_height")]] %% 12),
      Weight = sprintf("%d lbs", play_data[[paste0(prefix, "_weight")]]),
      Experience = ifelse(
        !is.na(play_data[[paste0(prefix, "_rookie_year")]]),
        play_data$season - play_data[[paste0(prefix, "_rookie_year")]],
        NA
      ),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, table_data)
}

# UI

# Define UI
ui <- navbarPage(
  "NFL Analysis (2016-2023)",
  useShinyjs(),
  tags$head(
    tags$script("
      $(document).ready(function(){
        // Show info modal on tab load
        $('a[data-value=\"Player Statistics\"]').on('shown.bs.tab', function(){
          $('#info_modal').show();
        });
        
        // Close modal when clicking X
        $(document).on('click', '#close_modal', function(){
          $('#info_modal').hide();
        });
        
        // Close modal when clicking outside
        $(window).click(function(event) {
          if ($(event.target).is('#info_modal')) {
            $('#info_modal').hide();
          }
        });
      });
    ")
  ),
  
  # Tab 1: Formation Viewer
  tabPanel("Formation Viewer",
           sidebarLayout(
             sidebarPanel(
               selectInput("year", "Season:", choices = NULL),
               selectInput("week", "Week:", choices = NULL),
               selectInput("game", "Game:", choices = NULL),
               selectInput("play", "Play:", choices = NULL),
               verbatimTextOutput("play_details"),
               width = 3
             ),
             mainPanel(
               plotOutput("formation_plot", height = "500px"),
               htmlOutput("play_desc"),
               fluidRow(
                 column(6,
                        h4("Offensive Players", style = "background-color: #f8f9fa; padding: 10px;"),
                        DT::dataTableOutput("offense_table")
                 ),
                 column(6,
                        h4("Defensive Players", style = "background-color: #f8f9fa; padding: 10px;"),
                        DT::dataTableOutput("defense_table")
                 )
               )
             )
           )
  ),
  # tab 2
  
  # UI Tab replacement:
  tabPanel("Player Statistics",
           div(
             id = "info_modal",
             style = "display: none; position: fixed; z-index: 1000; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgba(0,0,0,0.4);",
             div(
               style = "background-color: #fefefe; margin: 15% auto; padding: 20px; border: 1px solid #888; width: 80%; max-width: 700px; position: relative;",
               tags$span(
                 id = "close_modal",
                 HTML("&times;"),
                 style = "position: absolute; right: 10px; top: 5px; font-size: 28px; font-weight: bold; cursor: pointer;"
               ),
               h3("About Player Statistics", style = "margin-top: 10px;"),
               p("This tab allows you to explore detailed player statistics from the PFF database. 
        You can view both individual week performance and aggregated statistics over selected time periods."),
               h4("Features:"),
               tags$ul(
                 tags$li("Select specific weeks and years to analyze player performance"),
                 tags$li("View detailed weekly statistics with horizontal scrolling"),
                 tags$li("See aggregated statistics over custom time periods"),
                 tags$li("Calculate averages excluding NA values"),
                 tags$li("Filter by position, team, and player")
               )
             )
           ),
           fluidRow(
             column(3,
                    wellPanel(
                      actionButton("show_info", "Show Information", class = "btn-info"),
                      br(), br(),
                      selectInput("player_position", "Select Position:", 
                                  choices = c("Select Position" = "",
                                              "K", "QB", "WR", "ED", "CB", "TE", "DI", "S", 
                                              "LB", "P", "LS", "T", "HB", "G", "C", "FB"),
                                  selected = ""),
                      selectInput("player_team", "Select Team:", 
                                  choices = c("Select Team" = ""),
                                  selected = ""),
                      selectInput("player_name", "Select Player:", 
                                  choices = c("Select Player" = ""),
                                  selected = ""),
                      sliderInput("year_range", "Select Years:",
                                  min = 2016, max = 2023,
                                  value = c(2016, 2023),
                                  step = 1,
                                  sep = ""),
                      sliderInput("week_range", "Select Weeks:",
                                  min = 1, max = 18,
                                  value = c(1, 18),
                                  step = 1)
                    )
             ),
             column(9,
                    div(
                      style = "background-color: #f8f9fa; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                      h4("Position Summary", style = "margin-top: 0;"),
                      verbatimTextOutput("position_summary")
                    ),
                    tabsetPanel(id = "stats_tabs",
                                tabPanel("Weekly Stats",
                                         div(
                                           style = "overflow-x: auto;",
                                           DT::dataTableOutput("weekly_stats")
                                         )
                                ),
                                tabPanel("Aggregated Stats",
                                         div(
                                           style = "overflow-x: auto; width: 100%;",
                                           DT::dataTableOutput("aggregated_stats", width = "100%")
                                         )
                                )
                    )
             )
           )
  ),
  # Tab 3: Fourth Down Analysis
  tabPanel("Fourth Down Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput("analysis_year", "Select Year:",
                           choices = 2016:2023,
                           selected = 2023),
               
               sliderInput("analysis_weeks", "Select Weeks:",
                           min = 1, max = 18,
                           value = c(1, 18),
                           step = 1),
               
               checkboxInput("select_all_teams", "Select/Deselect All Teams", TRUE),
               
               div(
                 style = "max-height: 300px; overflow-y: auto;",
                 checkboxGroupInput("selected_teams", "Select Teams:",
                                    choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                "SF", "TB", "TEN", "WAS"),
                                    selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                 "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                 "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                 "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                 "SF", "TB", "TEN", "WAS"))
               ),
               width = 3
             ),
             mainPanel(
               plotOutput("distance_plot", height = "400px"),
               br(),
               plotOutput("success_plot", height = "400px"),
               wellPanel(
                 verbatimTextOutput("analysis_summary")
               )
             )
           )
  ),
  
  # Tab 4: Fourth Down Decisions
  tabPanel("Fourth Down Decisions",
           sidebarLayout(
             sidebarPanel(
               selectInput("decisions_year", "Select Year:",
                           choices = 2016:2023,
                           selected = 2023),
               
               selectInput("conference", "Conference:",
                           choices = c("All", "AFC", "NFC"),
                           selected = "All"),
               
               selectInput("division", "Division:",
                           choices = c("All", "North", "South", "East", "West"),
                           selected = "All"),
               
               checkboxInput("select_all_teams_decisions", 
                             "Select/Deselect All Teams", 
                             TRUE),
               
               div(
                 style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
                 checkboxGroupInput("selected_teams_decisions", "Select Teams:",
                                    choices = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                "SF", "TB", "TEN", "WAS"),
                                    selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                                 "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                                 "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                                 "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                                 "SF", "TB", "TEN", "WAS"))
               ),
               
               div(
                 style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
                 h4("Summary Statistics", style = "margin-top: 0;"),
                 verbatimTextOutput("decisions_summary")
               ),
               
               width = 3
             ),
             mainPanel(
               fluidRow(
                 column(12,
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          div(
                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                            h4("Decision Distribution", style = "margin: 0;"),
                            materialSwitch("toggle_top_plot", "Switch View", 
                                           status = "primary", right = TRUE)
                          ),
                          plotOutput("decisions_plot_top", height = "400px")
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          div(
                            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                            h4("Attempt Rates", style = "margin: 0;"),
                            materialSwitch("toggle_bottom_plot", "Switch View", 
                                           status = "primary", right = TRUE)
                          ),
                          plotOutput("decisions_plot_bottom", height = "400px")
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        div(
                          style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                          h4("Team Statistics"),
                          DT::dataTableOutput("decisions_table")
                        )
                 )
               )
             )
           )
  )
)

# Server

# Define server
server <- function(input, output, session) {
  
  
  # player stats add #########################3
  # In the position_data reactive within the server function, update to:
  # Server code between hashtags:
  
  position_data <- reactive({
    req(input$player_position)
    if(input$player_position == "") return(NULL)
    
    pff_stats_no_grades %>%
      filter(position == input$player_position) %>%
      arrange(year, week)
  })
  
  # Initialize position data
  position_data <- reactive({
    req(input$player_position)
    switch(input$player_position,
           "K" = pff_k,
           "QB" = pff_qb,
           "WR" = pff_wr,
           "ED" = pff_ed,
           "CB" = pff_cb,
           "TE" = pff_te,
           "DI" = pff_di,
           "S" = pff_s,
           "LB" = pff_lb,
           "P" = pff_p,
           "LS" = pff_ls,
           "T" = pff_t,
           "HB" = pff_hb,
           "G" = pff_g,
           "C" = pff_c,
           "FB" = pff_fb)
  })
  
  # Update team choices
  observe({
    req(position_data())
    teams <- unique(position_data()$team_name)
    updateSelectInput(session, "player_team",
                      choices = c("Select Team" = "", sort(teams)))
  })
  
  # Update player choices
  observe({
    req(input$player_team, position_data())
    players <- position_data() %>%
      filter(team_name == input$player_team) %>%
      pull(player) %>%
      unique()
    updateSelectInput(session, "player_name",
                      choices = c("Select Player" = "", sort(players)))
  })
  
  # Show modal
  observeEvent(input$show_info, {
    shinyjs::runjs("$('#info_modal').show();")
  })
  
  # Position summary output
  output$position_summary <- renderText({
    req(position_data())
    summary <- get_position_summary(position_data())
    sprintf("Total Players: %d\nTotal Variables: %d\nNumeric Variables: %d\nCategorical Variables: %d\nOverall NA Percentage: %.2f%%",
            summary$total_rows, summary$total_cols, summary$numeric_cols,
            summary$categorical_cols, summary$na_percentage)
  })
  
  # Weekly stats table
  # Server modification - Update the weekly_stats and aggregated_stats outputs:
  output$weekly_stats <- DT::renderDataTable({
    req(input$player_position, input$player_team, input$player_name)
    
    player_stats <- position_data() %>%
      filter(
        player == input$player_name,
        year >= input$year_range[1],
        year <= input$year_range[2],
        week >= input$week_range[1],
        week <= input$week_range[2]
      ) %>%
      select_if(~!all(is.na(.)))
    
    DT::datatable(
      player_stats,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        autoWidth = TRUE
      )
    )
  })
  
  output$aggregated_stats <- DT::renderDataTable({
    req(input$player_position, input$player_team, input$player_name)
    
    player_stats <- position_data() %>%
      filter(
        player == input$player_name,
        year >= input$year_range[1],
        year <= input$year_range[2],
        week >= input$week_range[1],
        week <= input$week_range[2]
      )
    
    agg_stats <- calculate_player_averages(player_stats)
    
    DT::datatable(
      t(agg_stats),
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        autoWidth = TRUE
      )
    )
  })
  ############### 
  
  # Formation Viewer Tab Logic
  
  # Update year choices
  observe({
    years <- sort(unique(data_all$season))
    updateSelectInput(session, "year",
                      choices = years)
  })
  
  # Update week choices based on year
  observe({
    req(input$year)
    weeks <- data_all %>%
      filter(season == input$year) %>%
      pull(week) %>%
      unique() %>%
      sort()
    updateSelectInput(session, "week",
                      choices = weeks)
  })
  
  # Update game choices based on year and week
  observe({
    req(input$year, input$week)
    games_data <- data_all %>%
      filter(season == input$year,
             week == input$week) %>%
      select(game_id, home_team, away_team) %>%
      distinct() %>%
      mutate(
        game_label = sprintf("%s @ %s", away_team, home_team),
        game_id = as.character(game_id)
      )
    
    updateSelectInput(session, "game",
                      choices = setNames(games_data$game_id, games_data$game_label))
  })
  
  # Update play choices based on game selection
  observe({
    req(input$game)
    plays <- data_all %>%
      filter(game_id == input$game) %>%
      mutate(play_desc = sprintf("Q%d - %d:%02d - %s - %d to go",
                                 qtr,
                                 floor(game_seconds_remaining/60),
                                 game_seconds_remaining %% 60,
                                 offense_formation,
                                 ydstogo))
    
    updateSelectInput(session, "play",
                      choices = setNames(1:nrow(plays), plays$play_desc))
  })
  
  # Get selected play data
  selected_play <- reactive({
    req(input$play, input$game)
    data_all %>%
      filter(game_id == input$game) %>%
      slice(as.numeric(input$play))
  })
  
  # Render formation plot
  output$formation_plot <- renderPlot({
    req(selected_play())
    tryCatch({
      plot_formation(selected_play())
    }, error = function(e) {
      # Print error message for debugging
      print(paste("Error in plot_formation:", e$message))
      # Return a blank plot with error message
      plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "Error creating formation plot\nPlease check data and selections", 
           cex = 1.5, col = "red", adj = 0.5)
    })
  })
  
  # Display play description
  output$play_desc <- renderUI({
    req(selected_play())
    play <- selected_play()
    HTML(sprintf("<div style='background-color: #f8f9fa; padding: 10px; margin: 10px 0;'>
                   <strong>Play Description:</strong> %s
                 </div>", 
                 play$desc))
  })
  
  # Render offense table
  output$offense_table <- DT::renderDataTable({
    req(selected_play())
    DT::datatable(
      create_player_table(selected_play(), "offense"),
      options = list(
        pageLength = 11,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Render defense table
  output$defense_table <- DT::renderDataTable({
    req(selected_play())
    DT::datatable(
      create_player_table(selected_play(), "defense"),
      options = list(
        pageLength = 11,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE
    )
  })
  
  # Display play details
  output$play_details <- renderText({
    req(selected_play())
    play <- selected_play()
    sprintf("Season: %d\nQuarter: %d\nTime: %d:%02d\nYard line: %d\nTo go: %d\nFormation: %s\nDefense: %s",
            play$season,
            play$qtr,
            floor(play$game_seconds_remaining/60),
            play$game_seconds_remaining %% 60,
            play$yardline_100,
            play$ydstogo,
            play$offense_formation,
            play$defense_personnel)
  })
  
  # Fourth Down Analysis Tab Logic
  
  # Select/Deselect All Teams Observer
  observe({
    if (!is.null(input$select_all_teams)) {
      if (input$select_all_teams) {
        updateCheckboxGroupInput(session, "selected_teams",
                                 selected = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                              "CLE", "DAL", "DEN", "DET", "GB", "HOU", "IND", 
                                              "JAX", "KC", "LAC", "LA", "LV", "MIA", "MIN", 
                                              "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", 
                                              "SF", "TB", "TEN", "WAS"))
      } else {
        updateCheckboxGroupInput(session, "selected_teams",
                                 selected = character(0))
      }
    }
  })
  
  # Render Distance Distribution Plot
  # Update the server code for rendering plots
  output$distance_plot <- renderPlot({
    req(input$analysis_year, input$selected_teams, input$analysis_weeks)
    df_filtered <- df_pbp %>%
      filter(posteam %in% input$selected_teams)
    plot_fourth_down_distance(df_filtered, input$analysis_year, input$analysis_weeks)
  })
  
  output$success_plot <- renderPlot({
    req(input$analysis_year, input$selected_teams, input$analysis_weeks)
    df_filtered <- df_pbp %>%
      filter(posteam %in% input$selected_teams)
    plot_fourth_down_success(df_filtered, input$analysis_year, input$analysis_weeks)
  })
  
  # Update the analysis summary output
  output$analysis_summary <- renderText({
    req(input$analysis_year, input$selected_teams, input$analysis_weeks)
    df_filtered <- df_pbp %>%
      filter(
        posteam %in% input$selected_teams,
        season == input$analysis_year,
        season_type == "REG",
        week >= input$analysis_weeks[1],
        week <= input$analysis_weeks[2]
      )
    
    summary_stats <- df_filtered %>%
      summarise(
        total_plays = n(),
        total_attempts = sum(!is.na(fourth_down_converted)),
        successful_attempts = sum(fourth_down_converted == 1, na.rm = TRUE),
        avg_distance = mean(ydstogo, na.rm = TRUE)
      )
    
    sprintf(
      "Analysis Summary:\n\n
    Total Fourth Down Plays: %d\n
    Total Attempts: %d\n
    Successful Attempts: %d (%.1f%%)\n
    Average Distance to Go: %.1f yards",
      summary_stats$total_plays,
      summary_stats$total_attempts,
      summary_stats$successful_attempts,
      100 * summary_stats$successful_attempts / summary_stats$total_attempts,
      summary_stats$avg_distance
    )
  })
  # Fourth Down Decisions Tab Logic
  
  
  # Helper function to get teams by conference/division
  get_filtered_teams <- reactive({
    conf_teams <- switch(input$conference,
                         "AFC" = c("BAL", "BUF", "CIN", "CLE", "DEN", "HOU", "IND", "JAX", "KC", 
                                   "LAC", "LV", "MIA", "NE", "NYJ", "PIT", "TEN"),
                         "NFC" = c("ARI", "ATL", "CAR", "CHI", "DAL", "DET", "GB", "LA", "MIN", 
                                   "NO", "NYG", "PHI", "SEA", "SF", "TB", "WAS"),
                         c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
                           "DET", "GB", "HOU", "IND", "JAX", "KC", "LAC", "LA", "LV", "MIA", 
                           "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", 
                           "TEN", "WAS")
    )
    
    if(input$division != "All") {
      div_teams <- switch(input$division,
                          "North" = c("BAL", "CIN", "CLE", "PIT", "CHI", "DET", "GB", "MIN"),
                          "South" = c("HOU", "IND", "JAX", "TEN", "ATL", "CAR", "NO", "TB"),
                          "East" = c("BUF", "MIA", "NE", "NYJ", "DAL", "NYG", "PHI", "WAS"),
                          "West" = c("DEN", "KC", "LAC", "LV", "ARI", "LA", "SEA", "SF")
      )
      conf_teams <- intersect(conf_teams, div_teams)
    }
    
    conf_teams
  })
  
  # Create a reactive value to track manual changes
  selected_teams_trigger <- reactiveVal(0)
  
  # Update available teams based on conference/division selection
  observeEvent(c(input$conference, input$division), {
    filtered_teams <- get_filtered_teams()
    
    # Only update if not triggered by manual selection
    if (selected_teams_trigger() == 0) {
      current_selected <- input$selected_teams_decisions
      new_selected <- intersect(current_selected, filtered_teams)
      
      updateCheckboxGroupInput(session, "selected_teams_decisions",
                               choices = filtered_teams,
                               selected = new_selected)
    }
  }, ignoreInit = TRUE)
  
  # Handle Select/Deselect All with debouncing
  observeEvent(input$select_all_teams_decisions, {
    filtered_teams <- get_filtered_teams()
    selected_teams_trigger(selected_teams_trigger() + 1)
    
    # Debounce the update
    isolate({
      if (input$select_all_teams_decisions) {
        updateCheckboxGroupInput(session, "selected_teams_decisions",
                                 selected = filtered_teams)
      } else {
        updateCheckboxGroupInput(session, "selected_teams_decisions",
                                 selected = character(0))
      }
    })
    
    # Reset trigger after a short delay
    later::later(function() {
      selected_teams_trigger(0)
    }, 0.1)
  }, ignoreInit = TRUE)
  
  # Track manual checkbox changes
  observeEvent(input$selected_teams_decisions, {
    selected_teams_trigger(selected_teams_trigger() + 1)
    
    # Reset trigger after a short delay
    later::later(function() {
      selected_teams_trigger(0)
    }, 0.1)
  }, ignoreInit = TRUE)
  
  # Filtered data for decision analysis
  filtered_decisions_data <- reactive({
    req(input$decisions_year, input$selected_teams_decisions)
    
    decisions_data %>%
      filter(
        season == input$decisions_year,
        posteam %in% input$selected_teams_decisions,
        season_type == "REG"
      )
  })
  
  
  # Render top plot (Decision Type or Yards)
  output$decisions_plot_top <- renderPlot({
    req(filtered_decisions_data())
    validate(
      need(nrow(filtered_decisions_data()) > 0, "No data available for selected criteria")
    )
    
    if (!input$toggle_top_plot) {
      create_decision_type_plot(filtered_decisions_data(), input$selected_teams_decisions)
    } else {
      create_yards_plot(filtered_decisions_data(), input$selected_teams_decisions)
    }
  })
  
  # Render bottom plot (Attempt Rates)
  output$decisions_plot_bottom <- renderPlot({
    req(filtered_decisions_data())
    validate(
      need(nrow(filtered_decisions_data()) > 0, "No data available for selected criteria")
    )
    
    create_rates_plot(filtered_decisions_data(), 
                      input$selected_teams_decisions, 
                      !input$toggle_bottom_plot)
  })
  
  # Render summary statistics
  output$decisions_summary <- renderText({
    req(filtered_decisions_data())
    validate(
      need(nrow(filtered_decisions_data()) > 0, "No data available for selected criteria")
    )
    
    summary_stats <- filtered_decisions_data() %>%
      summarise(
        total_plays = n(),
        go_for_it = sum(decision == "Go For It"),
        punts = sum(decision == "Punt"),
        field_goals = sum(decision == "Field Goal"),
        success_rate = mean(fourth_down_converted == 1, na.rm = TRUE),
        avg_distance = mean(ydstogo, na.rm = TRUE)
      )
    
    sprintf(
      "Selected Teams Summary (%d):\n\n
      Total Fourth Downs: %d\n
      Go For It: %d (%.1f%%)\n
      Punts: %d (%.1f%%)\n
      Field Goals: %d (%.1f%%)\n
      Success Rate on Attempts: %.1f%%\n
      Average Distance to Go: %.1f yards",
      length(input$selected_teams_decisions),
      summary_stats$total_plays,
      summary_stats$go_for_it,
      100 * summary_stats$go_for_it / summary_stats$total_plays,
      summary_stats$punts,
      100 * summary_stats$punts / summary_stats$total_plays,
      summary_stats$field_goals,
      100 * summary_stats$field_goals / summary_stats$total_plays,
      100 * summary_stats$success_rate,
      summary_stats$avg_distance
    )
  })
  
  # Render team statistics table
  output$decisions_table <- DT::renderDataTable({
    req(filtered_decisions_data())
    validate(
      need(nrow(filtered_decisions_data()) > 0, "No data available for selected criteria")
    )
    
    team_stats <- create_team_stats(filtered_decisions_data())
    
    DT::datatable(
      team_stats,
      options = list(
        pageLength = 32,
        dom = 'tip',
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Go_For_It_Pct',
        background = DT::styleColorBar(
          range(as.numeric(sub("%", "", team_stats$Go_For_It_Pct))),
          '#2ecc71'
        )
      ) %>%
      DT::formatStyle(
        'Success_Rate',
        background = DT::styleColorBar(
          range(as.numeric(sub("%", "", team_stats$Success_Rate))),
          '#3498db'
        )
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
