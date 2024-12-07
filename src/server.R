# server.R


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
  # Update the weekly_stats and aggregated_stats outputs:
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
      select_if(~!all(is.na(.))) %>%
      # Only mask numeric columns except for player_id, year, and week
      mutate(across(where(is.numeric), 
                    ~if(!(cur_column() %in% c("player_id", "year", "week"))) {
                      ifelse(!is.na(.), "###", NA)
                    } else {
                      .
                    }))
    
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
    
    # Calculate averages but replace numeric values with ###
    # Note: player_id, year, and week won't appear in aggregated stats
    # since they're not meant to be averaged
    agg_stats <- calculate_player_averages(player_stats) %>%
      mutate_if(is.numeric, ~ifelse(!is.na(.), "###", NA))
    
    DT::datatable(
      t(agg_stats),
      colnames = rep("", ncol(t(agg_stats))),
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        autoWidth = TRUE
      )
    )
  })
  
  
  # Original weekly_stats output:
  # output$weekly_stats <- DT::renderDataTable({
  #   req(input$player_position, input$player_team, input$player_name)
  #   
  #   player_stats <- position_data() %>%
  #     filter(
  #       player == input$player_name,
  #       year >= input$year_range[1],
  #       year <= input$year_range[2],
  #       week >= input$week_range[1],
  #       week <= input$week_range[2]
  #     ) %>%
  #     select_if(~!all(is.na(.)))
  #   
  #   DT::datatable(
  #     player_stats,
  #     options = list(
  #       scrollX = TRUE,
  #       pageLength = 10,
  #       autoWidth = TRUE
  #     )
  #   )
  # })
  
  # Original aggregated_stats output:
  # output$aggregated_stats <- DT::renderDataTable({
  #   req(input$player_position, input$player_team, input$player_name)
  #   
  #   player_stats <- position_data() %>%
  #     filter(
  #       player == input$player_name,
  #       year >= input$year_range[1],
  #       year <= input$year_range[2],
  #       week >= input$week_range[1],
  #       week <= input$week_range[2]
  #     )
  #   
  #   agg_stats <- calculate_player_averages(player_stats)
  #   
  #   DT::datatable(
  #     t(agg_stats),
  #     colnames = rep("", ncol(t(agg_stats))), # This removes the V1 header
  #     options = list(
  #       scrollX = TRUE,
  #       pageLength = 50,
  #       autoWidth = TRUE
  #     )
  #   )
  # })
  
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