## getwd()
## setwd("~/Desktop/DD Stats")

# Load packages
library(plyr)
library(dplyr)
library(data.table)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(reactable)
library(ggpubr)
library(shiny)
library(shinythemes)
library(paletteer)
library(scales)
library(colorspace)

# Load your data
MLBStatcast2425 <- fread("MLBStatcast2425.csv")
MLBStatcast2425$game_date <- as.Date(MLBStatcast2425$game_date, format = "%m/%d/%Y")

MLBStatcast2425$PitcherTeam <- ifelse(
  MLBStatcast2425$inning_topbot == "Top",
  MLBStatcast2425$home_team,
  MLBStatcast2425$away_team
)

# Combine and clean pitch names
MLBStatcast2425 <- MLBStatcast2425 %>%
  mutate(pitch_name = recode(pitch_name,
                             "4-Seam Fastball" = "Fastball",
                             "Split-Finger"   = "Splitter",
                             "Slurve"         = "Curveball",
                             "Slow Curve"     = "Curveball",
                             "Knuckle Curve"  = "Curveball")) %>%
  filter(!pitch_name %in% c("Eephus", "Other", "Pitch Out", ""))

# Strike zone constants
Left   <- -8.5/12; Right  <- 8.5/12
Bottom <- 18.29/12; Top    <- 44.08/12
Width  <- (Right - Left) / 3
Height <- (Top - Bottom) / 3

# Pitch order and colors
pitch_order <- c("Fastball","Sinker","Cutter","Slider","Sweeper",
                 "Curveball","Screwball","Changeup","Splitter",
                 "Forkball","Knuckleball")
colors <- c(Fastball = "red", Sinker = "orange", Cutter = "brown",
            Slider = "yellow", Sweeper = "gold", Curveball = "skyblue",
            Screwball = "royalblue", Changeup = "darkgreen",
            Splitter = "turquoise", Forkball = "pink",
            Knuckleball = "purple")

# Heat map colors (reversed RdBu palette, 16 shades)
heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::RdBu", n = 9, direction = -1))(16)

# date format
default_dates <- range(MLBStatcast2425$game_date, na.rm = TRUE)

ui <- navbarPage(
  "DD Stats",
  theme = shinytheme("flatly"),
  
  tabPanel("MLB Statcast '24-'25",
           tabsetPanel(
             
             tabPanel("Pitchers",
                      fluidRow(
                        column(3,
                               selectInput("PitcherTeam", "Choose Team",
                                           choices = sort(unique(MLBStatcast2425$PitcherTeam)),
                                           selected = NULL),
                               selectInput("PitcherName", "Choose Pitcher",
                                           choices = sort(unique(MLBStatcast2425$PitcherName)),
                                           selected = NULL),
                               dateRangeInput("PitcherDate", "Choose Date Range",
                                              start = default_dates[1],
                                              end   = default_dates[2],
                                              min   = default_dates[1],
                                              max   = default_dates[2],
                                              format = "yyyy-mm-dd"),
                               checkboxGroupInput("PitcherPitch", "Choose Pitch Type(s)", choices = NULL),
                               
                               # Batter handedness dropdown (Pitchers tab)
                               div(style = "margin-top: 725px;",
                                   selectInput("batter_split_pitchers", "Choose Batter Side:",
                                               choices = c("ALL", "LHH", "RHH"),
                                               selected = "ALL")
                               )
                        ),
                        column(9,
                               DTOutput("PitchSummary"), br(),
                               plotOutput("Velo_Chart", height = "400px"), br(),
                               fluidRow(
                                 column(6, plotOutput("TiltCirclePlot", height = "400px")),
                                 column(6, plotOutput("Movement_Plot", height = "400px"))
                               ), br(),
                               fluidRow(
                                 column(12,
                                        uiOutput("Heat_Maps")
                                 )
                               )
                        )
                      )
             ),
             
             tabPanel("Compare",
                      tags$div(
                        id = "compare-tab",
                        style = "position: relative; min-height: 2000px;",
                        
                        # Input selectors
                        fluidRow(
                          column(6,
                                 selectInput("CompareTeam1", "Choose Team",
                                             choices = sort(unique(MLBStatcast2425$PitcherTeam)),
                                             selected = NULL),
                                 selectInput("ComparePitcher1", "Pitcher 1", choices = NULL),
                                 checkboxGroupInput("ComparePitchTypes1", "Pitch Types", choices = NULL),
                                 dateRangeInput("CompareDates1", "Date Range", start = default_dates[1], end = default_dates[2])
                          ),
                          column(6,
                                 selectInput("CompareTeam2", "Choose Team",
                                             choices = sort(unique(MLBStatcast2425$PitcherTeam)),
                                             selected = NULL),
                                 selectInput("ComparePitcher2", "Pitcher 2", choices = NULL),
                                 checkboxGroupInput("ComparePitchTypes2", "Pitch Types", choices = NULL),
                                 dateRangeInput("CompareDates2", "Date Range", start = default_dates[1], end = default_dates[2])
                          )
                        ),
                        
                        br(),
                        
                        # Batter handedness dropdown (Compare tab)
                        fluidRow(
                          column(12,
                                 selectInput("batter_split_compare", "Choose Batter Side:",
                                             choices = c("ALL", "LHH", "RHH"),
                                             selected = "ALL")
                          )
                        ),
                        
                        fluidRow(
                          column(6, DTOutput("CompareTable1")),
                          column(6, DTOutput("CompareTable2"))
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(6, plotOutput("CompareVelo1", height = "400px")),
                          column(6, plotOutput("CompareVelo2", height = "400px"))
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(6, plotOutput("CompareTiltCircle1", height = "400px")),
                          column(6, plotOutput("CompareTiltCircle2", height = "400px"))
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(6, plotOutput("CompareMove1", height = "400px")),
                          column(6, plotOutput("CompareMove2", height = "400px"))
                        ),
                        
                        br(),
                        
                        fluidRow(
                          column(6, uiOutput("CompareHeat_Maps1")),
                          column(6, uiOutput("CompareHeat_Maps2"))
                        ),
                        
                        tags$div(
                          style = "
              position: absolute;
              top: 0;
              left: 50.65%;
              height: 100%;
              border-left: 2px solid gray;
              z-index: 1000;
              pointer-events: none;
            "
                        )
                      )
             )
             
           )
  )
)


# server
server <- function(input, output, session) {
  
  # Reactives for selected sides
  selected_sides_pitchers <- reactive({
    if (is.null(input$batter_split_pitchers)) return(c("L","R"))
    if (identical(input$batter_split_pitchers, "ALL")) c("L","R")
    else if (identical(input$batter_split_pitchers, "LHH")) "L" else "R"
  })
  selected_sides_compare <- reactive({
    if (is.null(input$batter_split_compare)) return(c("L","R"))
    if (identical(input$batter_split_compare, "ALL")) c("L","R")
    else if (identical(input$batter_split_compare, "LHH")) "L" else "R"
  })
  
  # Update pitcher dropdown when team changes
  observeEvent(input$PitcherTeam, {
    req(input$PitcherTeam)
    
    filtered_pitchers <- sort(unique(
      MLBStatcast2425$PitcherName[MLBStatcast2425$PitcherTeam == input$PitcherTeam]
    ))
    
    updateSelectInput(session, "PitcherName",
                      choices = filtered_pitchers,
                      selected = ifelse(length(filtered_pitchers) > 0, filtered_pitchers[1], NULL))
  })
  
  # Update pitch type checkboxes when pitcher changes
  observeEvent(input$PitcherName, {
    req(input$PitcherName)
    
    avail <- unique(MLBStatcast2425$pitch_name[MLBStatcast2425$PitcherName == input$PitcherName])
    choices <- intersect(pitch_order, avail)
    
    updateCheckboxGroupInput(session, "PitcherPitch",
                             choices = choices,
                             selected = choices)
  })
  
  # Pitch summary table
  output$PitchSummary <- renderDT({
    req(input$PitcherName, input$PitcherPitch, input$PitcherDate)
    df <- MLBStatcast2425 %>%
      filter(PitcherName   == input$PitcherName,
             pitch_name    %in% input$PitcherPitch,
             game_date    >= input$PitcherDate[1],
             game_date    <= input$PitcherDate[2],
             stand        %in% selected_sides_pitchers()) %>%
      filter(!is.na(release_speed), !is.na(release_spin_rate)) %>%
      mutate(Horizontal_Break = Horizontal_Break)
    
    pitch_counts <- df %>% group_by(PitcherName, pitch_name) %>%
      summarise(PitchCount = n(), .groups = 'drop')
    total_counts <- df %>% group_by(PitcherName) %>%
      summarise(TotalPitches = n(), .groups = 'drop')
    usage <- left_join(pitch_counts, total_counts, by = 'PitcherName') %>%
      mutate(UsagePct = round(100 * PitchCount/TotalPitches, 0))
    
    summary_stats <- df %>% group_by(PitcherName, pitch_name) %>%
      summarise(
        Avg_Velo        = round(mean(release_speed,      na.rm=TRUE), 1),
        Max_Velo        = round(max(release_speed,      na.rm=TRUE), 1),
        Avg_SpinRate    = round(mean(release_spin_rate,  na.rm=TRUE), 0),
        Max_SpinRate    = round(max(release_spin_rate,  na.rm=TRUE), 0),
        Avg_IVB         = round(mean(Induced_Vertical_Break, na.rm=TRUE), 1),
        Avg_HB          = round(mean(Horizontal_Break,   na.rm=TRUE), 1),
        Avg_RelH        = round(mean(release_pos_z,      na.rm=TRUE), 1),
        Avg_RelS        = round(mean(release_pos_x,      na.rm=TRUE), 1),
        Avg_Ext         = round(mean(release_extension,   na.rm=TRUE), 1),
        Avg_ArmAngle    = round(mean(arm_angle,          na.rm=TRUE), 1),
        .groups = 'drop'
      )
    
    final <- left_join(summary_stats, usage, by = c('PitcherName','pitch_name')) %>%
      rename(
        `Pitch Type`    = pitch_name,
        `Usage %`       = UsagePct,
        `Avg Velo`      = Avg_Velo,
        `Max Velo`      = Max_Velo,
        `Avg Spin Rate` = Avg_SpinRate,
        `Max Spin Rate` = Max_SpinRate,
        `Avg IVB`       = Avg_IVB,
        `Avg HB`        = Avg_HB,
        `Avg RelH`      = Avg_RelH,
        `Avg RelS`      = Avg_RelS,
        `Avg Ext`       = Avg_Ext,
        `Avg Arm Angle` = Avg_ArmAngle
      ) %>%
      mutate(`Pitch Type` = factor(`Pitch Type`, levels = pitch_order)) %>%
      arrange(`Pitch Type`)
    
    # Use colorspace::lighten instead of scales::lighten
    light_colors <- purrr::map_chr(colors, ~ colorspace::lighten(.x, amount = 0.6))
    names(light_colors) <- names(colors)
    
    pitch_types_present <- as.character(final$`Pitch Type`)
    pitch_types_present <- unique(pitch_types_present)
    light_colors_filtered <- light_colors[pitch_types_present]
    
    datatable(
      final %>% select(
        'Pitch Type', 'Usage %', everything(),
        -PitcherName, -PitchCount, -TotalPitches
      ), 
      rownames = FALSE, 
      options = list(dom ='t')
    ) %>%
      formatStyle(
        'Pitch Type',
        target = 'row',
        backgroundColor = styleEqual(pitch_types_present, light_colors_filtered)
      )
  })
  
  output$Velo_Chart <- renderPlot({
    req(input$PitcherName, input$PitcherPitch, input$PitcherDate)
    
    d <- MLBStatcast2425 %>%
      filter(
        PitcherName == input$PitcherName,
        pitch_name %in% input$PitcherPitch,
        game_date >= input$PitcherDate[1],
        game_date <= input$PitcherDate[2],
        stand %in% selected_sides_pitchers()
      ) %>%
      filter(!is.na(pitch_number), !is.na(release_speed))
    
    if (nrow(d) == 0) {
      plot.new()
      text(0.5, 0.5, 'No data', cex = 1.5)
      return()
    }
    
    d$pitch_name <- factor(d$pitch_name, levels = pitch_order)
    
    ggplot(d, aes(pitch_number, release_speed, colour = pitch_name)) +
      geom_line() + geom_point(size = 3) +
      scale_color_manual(values = colors) +
      scale_x_continuous(
        breaks = function(x) floor(seq(min(x), max(x), by = 1))
      ) +
      labs(title = 'Velocity by Pitch', x = 'Pitch #', y = 'Velocity (mph)') +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm"),
        legend.position = "right",
        legend.justification = "center"
      ) +
      geom_hline(yintercept = seq(70, 100, by = 5), linetype = 'dashed', color = 'gray90') +
      guides(color = guide_legend(
        title = "Pitch Type",
        override.aes = list(size = 4)
      ))
  })
  
  output$TiltCirclePlot <- renderPlot({
    req(input$PitcherName, input$PitcherPitch)
    
    pitch_colors <- c(
      Fastball = "red", Sinker = "orange", Cutter = "brown",
      Slider = "yellow", Sweeper = "gold", Curveball = "skyblue",
      Screwball = "royalblue", Changeup = "darkgreen",
      Splitter = "turquoise", Forkball = "pink",
      Knuckleball = "purple"
    )
    
    df <- MLBStatcast2425 %>%
      filter(
        PitcherName == input$PitcherName,
        pitch_name %in% input$PitcherPitch,
        stand %in% selected_sides_pitchers(),
        !is.na(Tilt),
        !is.na(pitch_name)
      ) %>%
      mutate(
        Tilt_hour = as.numeric(str_extract(Tilt, "^\\d{1,2}")),
        Tilt_min  = as.numeric(str_extract(Tilt, "(?<=:)\\d{1,2}")),
        Tilt_min  = ifelse(is.na(Tilt_min), 0, Tilt_min),
        Tilt_deg  = ((Tilt_hour %% 12) * 30) + (Tilt_min * 0.5),
        pitch_name = factor(pitch_name, levels = names(pitch_colors)),
        spin_axis_rad_pitcher = pi - (((Tilt_deg + 90) %% 360) * pi / 180)
      )
    
    validate(need(nrow(df) > 0, "No spin axis data available."))
    
    bin_size <- 8
    
    df_binned <- df %>%
      mutate(bin = floor(Tilt_deg / bin_size) * bin_size) %>%
      group_by(pitch_name, bin) %>%
      summarise(count = n(), .groups = "drop")
    
    max_count <- max(df_binned$count)
    
    wedge_points <- function(bin, count, pitch_name, inner_r = 1, max_count = max_count, bin_size = bin_size) {
      min_length <- 0.1
      max_length <- 0.8
      outer_r <- inner_r + min_length + (count / max_count) * (max_length - min_length)
      
      angle_start <- bin
      angle_end <- bin + bin_size
      theta_outer <- seq(angle_end, angle_start, length.out = 30) * pi / 180
      theta_inner <- seq(angle_start, angle_end, length.out = 30) * pi / 180
      
      x_outer <- outer_r * cos(pi/2 - theta_outer)
      y_outer <- outer_r * sin(pi/2 - theta_outer)
      x_inner <- inner_r * cos(pi/2 - theta_inner)
      y_inner <- inner_r * sin(pi/2 - theta_inner)
      
      data.frame(
        x = c(x_outer, x_inner),
        y = c(y_outer, y_inner),
        pitch_name = pitch_name,
        bin = bin
      )
    }
    
    wedges_df <- purrr::pmap_dfr(
      list(df_binned$bin, df_binned$count, df_binned$pitch_name),
      wedge_points,
      inner_r = 1,
      max_count = max_count,
      bin_size = bin_size
    )
    
    circle_df <- data.frame(
      x = 1 * cos(seq(0, 2 * pi, length.out = 100)),
      y = 1 * sin(seq(0, 2 * pi, length.out = 100))
    )
    
    hours <- 1:12
    angles <- (hours %% 12) * 30
    label_positions <- data.frame(
      label = ifelse(hours == 12, "12", as.character(hours)),
      x = 1.15 * cos((90 - angles) * pi / 180),
      y = 1.15 * sin((90 - angles) * pi / 180)
    )
    
    ticks_df <- data.frame(
      angle_deg = angles,
      xstart = 0.95 * cos((90 - angles) * pi / 180),
      ystart = 0.95 * sin((90 - angles) * pi / 180),
      xend = 1.0 * cos((90 - angles) * pi / 180),
      yend = 1.0 * sin((90 - angles) * pi / 180)
    )
    
    ggplot() +
      geom_path(data = circle_df, aes(x = x, y = y), color = "black") +
      geom_polygon(data = wedges_df, aes(x = x, y = y, fill = pitch_name, group = interaction(pitch_name, bin)), color = NA) +
      geom_segment(data = ticks_df,
                   aes(x = xstart, y = ystart, xend = xend, yend = yend),
                   color = "black", size = 0.5) +
      geom_text(data = label_positions, aes(x = x * 0.755, y = y * 0.755, label = label),
                size = 8, fontface = "bold") +
      coord_fixed() +
      theme_void() +
      labs(title = "Spin-Based Tilt") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.title = element_blank(),
        legend.position = "none"
      ) +
      scale_fill_manual(values = pitch_colors, drop = FALSE)
  })
  
  output$Movement_Plot <- renderPlot({
    req(input$PitcherName, input$PitcherPitch, input$PitcherDate)
    
    d <- MLBStatcast2425 %>%
      filter(
        PitcherName == input$PitcherName,
        pitch_name %in% input$PitcherPitch,
        game_date >= input$PitcherDate[1],
        game_date <= input$PitcherDate[2],
        stand %in% selected_sides_pitchers(),
        !is.na(Horizontal_Break), !is.na(Induced_Vertical_Break)
      ) %>%
      mutate(Horizontal_Break = Horizontal_Break)
    
    if (nrow(d) == 0) {
      plot.new()
      text(0.5, 0.5, 'No movement data', cex = 1.5)
      return()
    }
    
    d$pitch_name <- factor(d$pitch_name, levels = pitch_order)
    
    means <- d %>%
      group_by(pitch_name) %>%
      summarise(
        Avg_H = mean(Horizontal_Break),
        Avg_V = mean(Induced_Vertical_Break),
        .groups = 'drop'
      )
    
    legend_df <- data.frame(
      pitch_name = factor(input$PitcherPitch, levels = pitch_order)
    )
    
    ggplot() +
      geom_point(data = d, aes(x = Horizontal_Break, y = Induced_Vertical_Break, color = pitch_name),
                 alpha = 0.5, size = 3, stroke = 0.5) +
      geom_point(data = means, aes(x = Avg_H, y = Avg_V, fill = pitch_name),
                 shape = 21, size = 6, color = "black", stroke = 1, show.legend = FALSE) +
      geom_point(data = legend_df, aes(x = 0, y = 0, fill = pitch_name),
                 shape = 21, size = 5, color = NA, stroke = 0, alpha = 1, show.legend = TRUE) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black") +
      scale_x_continuous(
        breaks = seq(-20, 20, 10),
        limits = c(-23, 23),
        expand = expansion(mult = 0)
      ) +
      scale_y_continuous(
        breaks = seq(-20, 20, 10),
        limits = c(-23, 23),
        expand = expansion(mult = 0)
      ) +
      scale_color_manual(values = colors, guide = 'none') +
      scale_fill_manual(values = colors, name = "Pitch Type") +
      labs(
        title = "Movement Profile (Pitcher's Perspective)",
        x = "Horizontal Break (inches)",
        y = "Induced Vertical Break (inches)"
      ) +
      guides(fill = guide_legend(
        override.aes = list(shape = 21, color = NA, stroke = 0, size = 7)
      )) +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = c(-0.415, 0.5),
        legend.justification = c("left", "center"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm")
      ) +
      coord_fixed(ratio = 1)
  })
  
  output$Heat_Maps <- renderUI({
    req(input$PitcherName, input$PitcherPitch, input$PitcherDate, input$batter_split_pitchers)
    
    pitches <- intersect(pitch_order, input$PitcherPitch)
    pitches <- head(pitches, 6)
    
    plot_output_list <- vector("list", length = 6)
    
    for (i in seq_along(pitches)) {
      pitch <- pitches[i]
      safe_pitch <- gsub("[^A-Za-z0-9]", "_", pitch)
      plotname <- paste0("pitcher_heatmap_", safe_pitch)
      
      local({
        my_pitch <- pitch
        my_plotname <- plotname
        
        output[[my_plotname]] <- renderPlot({
          req(input$batter_split_pitchers)
          
          data <- MLBStatcast2425 %>%
            filter(
              PitcherName == input$PitcherName,
              pitch_name == my_pitch,
              game_date >= input$PitcherDate[1],
              game_date <= input$PitcherDate[2],
              stand %in% selected_sides_pitchers(),
              !is.na(plate_x), !is.na(plate_z)
            )
          
          total_pitches <- MLBStatcast2425 %>%
            filter(
              PitcherName == input$PitcherName,
              game_date >= input$PitcherDate[1],
              game_date <= input$PitcherDate[2],
              stand %in% selected_sides_pitchers()
            ) %>%
            nrow()
          
          pitch_count <- nrow(data)
          usage_pct <- ifelse(total_pitches > 0, round((pitch_count / total_pitches) * 100), 0)
          
          if (pitch_count == 0) return(NULL)
          
          ggplot(data, aes(x = -plate_x, y = plate_z)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated) +
            # Strike zone lines
            geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom) +
            geom_segment(x = Left, y = Top, xend = Right, yend = Top) +
            geom_segment(x = Left, y = Bottom, xend = Left, yend = Top) +
            geom_segment(x = Right, y = Bottom, xend = Right, yend = Top) +
            geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height) +
            geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height) +
            geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top) +
            geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top) +
            geom_segment(x = Left, y = 0, xend = Right, yend = 0) +
            geom_segment(x = Left, y = 0, xend = Left, yend = 4.25 / 12) +
            geom_segment(x = Left, y = 4.25 / 12, xend = 0, yend = 8.5 / 12) +
            geom_segment(x = Right, y = 4.25 / 12, xend = Right, yend = 0) +
            geom_segment(x = 0, y = 8.5 / 12, xend = Right, yend = 4.25 / 12) +
            xlim(-2.5, 2.5) + ylim(-0.5, 5) +
            ggtitle(paste0(my_pitch, " - ", pitch_count, " pitches (", usage_pct, "%)")) +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
              panel.grid = element_blank(),
              panel.background = element_blank()
            ) +
            coord_fixed(ratio = 1)
        })
        
        plot_output_list[[i]] <<- plotOutput(my_plotname, height = 400, width = 400)
      })
    }
    
    plot_output_list <- plot_output_list[!sapply(plot_output_list, is.null)]
    
    plot_rows <- split(plot_output_list, ceiling(seq_along(plot_output_list) / 3))
    
    plot_ui <- lapply(plot_rows, function(row_plots) {
      fluidRow(
        lapply(row_plots, function(plot) {
          column(4, plot)
        })
      )
    })
    
    tagList(
      fluidRow(
        column(3, ""),
        column(6, tags$h3("Pitcher Heat Maps", style = "text-align: center; font-weight: bold; margin-top: 20px;")),
        column(3, "")
      ),
      plot_ui
    )
  })
  
  # --- Compare tab: update pitcher list for ComparePitcher1 based on CompareTeam1 ---
  observeEvent(input$CompareTeam1, {
    req(input$CompareTeam1)
    filtered_pitchers <- sort(unique(MLBStatcast2425$PitcherName[MLBStatcast2425$PitcherTeam == input$CompareTeam1]))
    updateSelectInput(session, "ComparePitcher1",
                      choices = filtered_pitchers,
                      selected = ifelse(length(filtered_pitchers) > 0, filtered_pitchers[1], NULL))
  })
  
  # --- Compare tab: update pitcher list for ComparePitcher2 based on CompareTeam2 ---
  observeEvent(input$CompareTeam2, {
    req(input$CompareTeam2)
    filtered_pitchers <- sort(unique(MLBStatcast2425$PitcherName[MLBStatcast2425$PitcherTeam == input$CompareTeam2]))
    updateSelectInput(session, "ComparePitcher2",
                      choices = filtered_pitchers,
                      selected = ifelse(length(filtered_pitchers) > 0, filtered_pitchers[1], NULL))
  })
  
  # --- Compare tab: update pitch types when ComparePitcher1 changes ---
  observeEvent(input$ComparePitcher1, {
    req(input$ComparePitcher1)
    avail <- unique(MLBStatcast2425$pitch_name[MLBStatcast2425$PitcherName == input$ComparePitcher1])
    choices <- intersect(pitch_order, avail)
    updateCheckboxGroupInput(session, "ComparePitchTypes1",
                             choices = choices,
                             selected = choices)
  })
  
  # --- Compare tab: update pitch types when ComparePitcher2 changes ---
  observeEvent(input$ComparePitcher2, {
    req(input$ComparePitcher2)
    avail <- unique(MLBStatcast2425$pitch_name[MLBStatcast2425$PitcherName == input$ComparePitcher2])
    choices <- intersect(pitch_order, avail)
    updateCheckboxGroupInput(session, "ComparePitchTypes2",
                             choices = choices,
                             selected = choices)
  })
  
  # --- Comparison Table Logic (reusable) ---
  make_pitch_summary_table <- function(data, pitcher_name, pitch_types, date_range, sides = c("L","R")) {
    df <- data %>%
      filter(PitcherName == pitcher_name,
             pitch_name %in% pitch_types,
             game_date >= date_range[1],
             game_date <= date_range[2],
             stand %in% sides) %>%
      filter(!is.na(release_speed), !is.na(release_spin_rate)) %>%
      mutate(Horizontal_Break = Horizontal_Break)
    
    pitch_counts <- df %>% group_by(pitch_name) %>%
      summarise(PitchCount = n(), .groups = 'drop')
    total_counts <- summarise(df, TotalPitches = n())
    
    usage <- left_join(pitch_counts, total_counts, by = character()) %>%
      mutate(UsagePct = round(100 * PitchCount / TotalPitches, 0))
    
    summary_stats <- df %>% group_by(pitch_name) %>%
      summarise(
        Avg_Velo        = round(mean(release_speed,      na.rm = TRUE), 1),
        Max_Velo        = round(max(release_speed,      na.rm = TRUE), 1),
        Avg_SpinRate    = round(mean(release_spin_rate,  na.rm = TRUE), 0),
        Max_SpinRate    = round(max(release_spin_rate,  na.rm = TRUE), 0),
        Avg_IVB         = round(mean(Induced_Vertical_Break, na.rm = TRUE), 1),
        Avg_HB          = round(mean(Horizontal_Break,   na.rm = TRUE), 1),
        Avg_RelH        = round(mean(release_pos_z,      na.rm = TRUE), 1),
        Avg_RelS        = round(mean(release_pos_x,      na.rm = TRUE), 1),
        Avg_Ext         = round(mean(release_extension,  na.rm = TRUE), 1),
        Avg_ArmAngle    = round(mean(arm_angle,          na.rm = TRUE), 1),
        .groups = 'drop'
      )
    
    final <- left_join(summary_stats, usage, by = "pitch_name") %>%
      rename(
        `Pitch Type`    = pitch_name,
        `Usage %`       = UsagePct,
        `Avg Velo`      = Avg_Velo,
        `Max Velo`      = Max_Velo,
        `Avg Spin Rate` = Avg_SpinRate,
        `Max Spin Rate` = Max_SpinRate,
        `Avg IVB`       = Avg_IVB,
        `Avg HB`        = Avg_HB,
        `Avg RelH`      = Avg_RelH,
        `Avg RelS`      = Avg_RelS,
        `Avg Ext`       = Avg_Ext,
        `Avg Arm Angle` = Avg_ArmAngle
      ) %>%
      mutate(`Pitch Type` = factor(`Pitch Type`, levels = pitch_order)) %>%
      arrange(`Pitch Type`)
    
    light_colors <- purrr::map_chr(colors, ~ colorspace::lighten(.x, amount = 0.6))
    pitch_types_present <- as.character(final$`Pitch Type`)
    light_colors_filtered <- light_colors[pitch_types_present]
    
    datatable(
      final %>%
        select(`Pitch Type`, `Usage %`, everything(), -PitchCount, -TotalPitches),
      rownames = FALSE,
      options = list(dom = 't')
    ) %>%
      formatStyle(
        'Pitch Type',
        target = 'row',
        backgroundColor = styleEqual(pitch_types_present, light_colors_filtered)
      )
  }
  
  # --- Render Compare Tables ---
  output$CompareTable1 <- renderDT({
    req(input$ComparePitcher1, input$ComparePitchTypes1, input$CompareDates1)
    make_pitch_summary_table(MLBStatcast2425, input$ComparePitcher1, input$ComparePitchTypes1, input$CompareDates1, selected_sides_compare())
  })
  
  output$CompareTable2 <- renderDT({
    req(input$ComparePitcher2, input$ComparePitchTypes2, input$CompareDates2)
    make_pitch_summary_table(MLBStatcast2425, input$ComparePitcher2, input$ComparePitchTypes2, input$CompareDates2, selected_sides_compare())
  })
  
  output$CompareVelo1 <- renderPlot({
    req(input$ComparePitcher1, input$ComparePitchTypes1, input$CompareDates1)
    d <- MLBStatcast2425 %>%
      filter(PitcherName == input$ComparePitcher1,
             pitch_name  %in% input$ComparePitchTypes1,
             game_date   >= input$CompareDates1[1],
             game_date   <= input$CompareDates1[2],
             stand       %in% selected_sides_compare()) %>%
      filter(!is.na(pitch_number), !is.na(release_speed))
    
    if(nrow(d)==0) return({ plot.new(); text(0.5,0.5,'No data',cex=1.5) })
    
    d$pitch_name <- factor(d$pitch_name, levels = pitch_order)
    
    ggplot(d, aes(pitch_number, release_speed, colour = pitch_name)) +
      geom_line() + geom_point(size = 3) +
      scale_color_manual(values = colors) +
      scale_x_continuous(
        breaks = function(x) floor(seq(min(x), max(x), by = 1))
      ) +
      labs(title = paste("Velocity by Pitch:", input$ComparePitcher1),
           x = "Pitch #", y = "Velocity (mph)") +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15))
      ) +
      geom_hline(yintercept = seq(70, 100, by = 5), linetype = 'dashed', color = 'gray90') +
      guides(color = guide_legend(title = "Pitch Type", order = 1))
  })
  
  output$CompareVelo2 <- renderPlot({
    req(input$ComparePitcher2, input$ComparePitchTypes2, input$CompareDates2)
    d <- MLBStatcast2425 %>%
      filter(PitcherName == input$ComparePitcher2,
             pitch_name  %in% input$ComparePitchTypes2,
             game_date   >= input$CompareDates2[1],
             game_date   <= input$CompareDates2[2],
             stand       %in% selected_sides_compare()) %>%
      filter(!is.na(pitch_number), !is.na(release_speed))
    
    if(nrow(d)==0) return({ plot.new(); text(0.5,0.5,'No data',cex=1.5) })
    
    d$pitch_name <- factor(d$pitch_name, levels = pitch_order)
    
    ggplot(d, aes(pitch_number, release_speed, colour = pitch_name)) +
      geom_line() + geom_point(size = 3) +
      scale_color_manual(values = colors) +
      scale_x_continuous(
        breaks = function(x) floor(seq(min(x), max(x), by = 1))
      ) +
      labs(title = paste("Velocity by Pitch:", input$ComparePitcher2),
           x = "Pitch #", y = "Velocity (mph)") +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15))
      ) +
      geom_hline(yintercept = seq(70, 100, by = 5), linetype = 'dashed', color = 'gray90') +
      guides(color = guide_legend(title = "Pitch Type", order = 1))
  })
  
  pitch_colors <- c(
    Fastball = "red", Sinker = "orange", Cutter = "brown",
    Slider = "yellow", Sweeper = "gold", Curveball = "skyblue",
    Screwball = "royalblue", Changeup = "darkgreen",
    Splitter = "turquoise", Forkball = "pink",
    Knuckleball = "purple"
  )
  
  output$CompareTiltCircle1 <- renderPlot({
    req(input$ComparePitcher1, input$ComparePitchTypes1)
    
    df <- MLBStatcast2425 %>%
      filter(
        PitcherName %in% input$ComparePitcher1,
        pitch_name %in% input$ComparePitchTypes1,
        stand %in% selected_sides_compare(),
        !is.na(Tilt), !is.na(pitch_name)
      ) %>%
      mutate(
        Tilt_hour = as.numeric(str_extract(Tilt, "^\\d{1,2}")),
        Tilt_min  = as.numeric(str_extract(Tilt, "(?<=:)\\d{1,2}")),
        Tilt_min  = ifelse(is.na(Tilt_min), 0, Tilt_min),
        Tilt_deg  = ((Tilt_hour %% 12) * 30) + (Tilt_min * 0.5),
        pitch_name = factor(pitch_name, levels = names(pitch_colors)),
        spin_axis_rad_pitcher = pi - (((Tilt_deg + 90) %% 360) * pi / 180)
      )
    
    validate(need(nrow(df) > 0, "No spin axis data available."))
    
    bin_size <- 8
    df_binned <- df %>%
      mutate(bin = floor(Tilt_deg / bin_size) * bin_size) %>%
      group_by(pitch_name, bin) %>%
      summarise(count = n(), .groups = "drop")
    
    max_count <- max(df_binned$count)
    
    wedge_points <- function(bin, count, pitch_name, inner_r = 1, max_count = max_count, bin_size = bin_size) {
      min_length <- 0.1
      max_length <- 0.8
      outer_r <- inner_r + min_length + (count / max_count) * (max_length - min_length)
      
      angle_start <- bin
      angle_end <- bin + bin_size
      theta_outer <- seq(angle_end, angle_start, length.out = 30) * pi / 180
      theta_inner <- seq(angle_start, angle_end, length.out = 30) * pi / 180
      
      x_outer <- outer_r * cos(pi/2 - theta_outer)
      y_outer <- outer_r * sin(pi/2 - theta_outer)
      x_inner <- inner_r * cos(pi/2 - theta_inner)
      y_inner <- inner_r * sin(pi/2 - theta_inner)
      
      data.frame(
        x = c(x_outer, x_inner),
        y = c(y_outer, y_inner),
        pitch_name = pitch_name,
        bin = bin
      )
    }
    
    wedges_df <- purrr::pmap_dfr(
      list(df_binned$bin, df_binned$count, df_binned$pitch_name),
      wedge_points,
      inner_r = 1,
      max_count = max_count,
      bin_size = bin_size
    )
    
    circle_df <- data.frame(
      x = 1 * cos(seq(0, 2 * pi, length.out = 100)),
      y = 1 * sin(seq(0, 2 * pi, length.out = 100))
    )
    
    hours <- 1:12
    angles <- (hours %% 12) * 30
    label_positions <- data.frame(
      label = ifelse(hours == 12, "12", as.character(hours)),
      x = 1.15 * cos((90 - angles) * pi / 180),
      y = 1.15 * sin((90 - angles) * pi / 180)
    )
    
    ticks_df <- data.frame(
      angle_deg = angles,
      xstart = 0.95 * cos((90 - angles) * pi / 180),
      ystart = 0.95 * sin((90 - angles) * pi / 180),
      xend = 1.0 * cos((90 - angles) * pi / 180),
      yend = 1.0 * sin((90 - angles) * pi / 180)
    )
    
    ggplot() +
      geom_path(data = circle_df, aes(x = x, y = y), color = "black") +
      geom_polygon(data = wedges_df, aes(x = x, y = y, fill = pitch_name, group = interaction(pitch_name, bin)), color = NA) +
      geom_segment(data = ticks_df, aes(x = xstart, y = ystart, xend = xend, yend = yend),
                   color = "black", size = 0.5) +
      geom_text(data = label_positions, aes(x = x * 0.755, y = y * 0.755, label = label),
                size = 6, fontface = "bold") +
      coord_fixed() +
      theme_void() +
      labs(title = paste(input$ComparePitcher1, "Spin-Based Tilt")) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.title = element_blank(),
        legend.position = "none"
      ) +
      scale_fill_manual(values = pitch_colors, drop = FALSE)
  })
  
  output$CompareTiltCircle2 <- renderPlot({
    req(input$ComparePitcher2, input$ComparePitchTypes2)
    
    df <- MLBStatcast2425 %>%
      filter(
        PitcherName %in% input$ComparePitcher2,
        pitch_name %in% input$ComparePitchTypes2,
        stand %in% selected_sides_compare(),
        !is.na(Tilt), !is.na(pitch_name)
      ) %>%
      mutate(
        Tilt_hour = as.numeric(str_extract(Tilt, "^\\d{1,2}")),
        Tilt_min  = as.numeric(str_extract(Tilt, "(?<=:)\\d{1,2}")),
        Tilt_min  = ifelse(is.na(Tilt_min), 0, Tilt_min),
        Tilt_deg  = ((Tilt_hour %% 12) * 30) + (Tilt_min * 0.5),
        pitch_name = factor(pitch_name, levels = names(pitch_colors)),
        spin_axis_rad_pitcher = pi - (((Tilt_deg + 90) %% 360) * pi / 180)
      )
    
    validate(need(nrow(df) > 0, "No spin axis data available."))
    
    bin_size <- 8
    df_binned <- df %>%
      mutate(bin = floor(Tilt_deg / bin_size) * bin_size) %>%
      group_by(pitch_name, bin) %>%
      summarise(count = n(), .groups = "drop")
    
    max_count <- max(df_binned$count)
    
    wedge_points <- function(bin, count, pitch_name, inner_r = 1, max_count = max_count, bin_size = bin_size) {
      min_length <- 0.1
      max_length <- 0.8
      outer_r <- inner_r + min_length + (count / max_count) * (max_length - min_length)
      
      angle_start <- bin
      angle_end <- bin + bin_size
      theta_outer <- seq(angle_end, angle_start, length.out = 30) * pi / 180
      theta_inner <- seq(angle_start, angle_end, length.out = 30) * pi / 180
      
      x_outer <- outer_r * cos(pi/2 - theta_outer)
      y_outer <- outer_r * sin(pi/2 - theta_outer)
      x_inner <- inner_r * cos(pi/2 - theta_inner)
      y_inner <- inner_r * sin(pi/2 - theta_inner)
      
      data.frame(
        x = c(x_outer, x_inner),
        y = c(y_outer, y_inner),
        pitch_name = pitch_name,
        bin = bin
      )
    }
    
    wedges_df <- purrr::pmap_dfr(
      list(df_binned$bin, df_binned$count, df_binned$pitch_name),
      wedge_points,
      inner_r = 1,
      max_count = max_count,
      bin_size = bin_size
    )
    
    circle_df <- data.frame(
      x = 1 * cos(seq(0, 2 * pi, length.out = 100)),
      y = 1 * sin(seq(0, 2 * pi, length.out = 100))
    )
    
    hours <- 1:12
    angles <- (hours %% 12) * 30
    label_positions <- data.frame(
      label = ifelse(hours == 12, "12", as.character(hours)),
      x = 1.15 * cos((90 - angles) * pi / 180),
      y = 1.15 * sin((90 - angles) * pi / 180)
    )
    
    ticks_df <- data.frame(
      angle_deg = angles,
      xstart = 0.95 * cos((90 - angles) * pi / 180),
      ystart = 0.95 * sin((90 - angles) * pi / 180),
      xend = 1.0 * cos((90 - angles) * pi / 180),
      yend = 1.0 * sin((90 - angles) * pi / 180)
    )
    
    ggplot() +
      geom_path(data = circle_df, aes(x = x, y = y), color = "black") +
      geom_polygon(data = wedges_df, aes(x = x, y = y, fill = pitch_name, group = interaction(pitch_name, bin)), color = NA) +
      geom_segment(data = ticks_df, aes(x = xstart, y = ystart, xend = xend, yend = yend),
                   color = "black", size = 0.5) +
      geom_text(data = label_positions, aes(x = x * 0.755, y = y * 0.755, label = label),
                size = 6, fontface = "bold") +
      coord_fixed() +
      theme_void() +
      labs(title = paste(input$ComparePitcher2, "Spin-Based Tilt")) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.title = element_blank(),
        legend.position = "none"
      ) +
      scale_fill_manual(values = pitch_colors, drop = FALSE)
  })
  
  # --- Render Compare Movement Plots ---
  output$CompareMove1 <- renderPlot({
    req(input$ComparePitcher1, input$ComparePitchTypes1, input$CompareDates1)
    d <- MLBStatcast2425 %>%
      filter(PitcherName == input$ComparePitcher1,
             pitch_name  %in% input$ComparePitchTypes1,
             game_date   >= input$CompareDates1[1],
             game_date   <= input$CompareDates1[2],
             stand       %in% selected_sides_compare(),
             !is.na(Horizontal_Break), !is.na(Induced_Vertical_Break)) %>%
      mutate(Horizontal_Break = Horizontal_Break)
    
    if(nrow(d) == 0) return({ plot.new(); text(0.5, 0.5, 'No movement data', cex = 1.5) })
    
    d$pitch_name <- factor(d$pitch_name, levels = pitch_order)
    
    means <- d %>%
      group_by(pitch_name) %>%
      summarise(Avg_H = mean(Horizontal_Break),
                Avg_V = mean(Induced_Vertical_Break),
                .groups = 'drop')
    
    ggplot(d, aes(x = Horizontal_Break, y = Induced_Vertical_Break, color = pitch_name)) +
      geom_point(alpha = 0.5, size = 3, stroke = 0.5) +
      geom_point(data = means, aes(x = Avg_H, y = Avg_V, fill = pitch_name),
                 shape = 21, size = 6, color = "black", stroke = 1, show.legend = FALSE) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black") +
      scale_x_continuous(
        breaks = seq(-20, 20, 10),
        limits = c(-23, 23),
        expand = expansion(mult = 0)
      ) +
      scale_y_continuous(
        breaks = seq(-20, 20, 10),
        limits = c(-23, 23),
        expand = expansion(mult = 0)
      ) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      labs(
        title = paste("Movement Profile:", input$ComparePitcher1),
        x = "Horizontal Break (inches)",
        y = "Induced Vertical Break (inches)"
      ) +
      guides(color = guide_legend(title = "Pitch Type", order = 1), fill = "none") +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      ) +
      coord_fixed(ratio = 1)
  })
  
  output$CompareMove2 <- renderPlot({
    req(input$ComparePitcher2, input$ComparePitchTypes2, input$CompareDates2)
    d <- MLBStatcast2425 %>%
      filter(PitcherName == input$ComparePitcher2,
             pitch_name  %in% input$ComparePitchTypes2,
             game_date   >= input$CompareDates2[1],
             game_date   <= input$CompareDates2[2],
             stand       %in% selected_sides_compare(),
             !is.na(Horizontal_Break), !is.na(Induced_Vertical_Break)) %>%
      mutate(Horizontal_Break = Horizontal_Break)
    
    if(nrow(d) == 0) return({ plot.new(); text(0.5, 0.5, 'No movement data', cex = 1.5) })
    
    d$pitch_name <- factor(d$pitch_name, levels = pitch_order)
    
    means <- d %>%
      group_by(pitch_name) %>%
      summarise(Avg_H = mean(Horizontal_Break),
                Avg_V = mean(Induced_Vertical_Break),
                .groups = 'drop')
    
    ggplot(d, aes(x = Horizontal_Break, y = Induced_Vertical_Break, color = pitch_name)) +
      geom_point(alpha = 0.5, size = 3, stroke = 0.5) +
      geom_point(data = means, aes(x = Avg_H, y = Avg_V, fill = pitch_name),
                 shape = 21, size = 6, color = "black", stroke = 1, show.legend = FALSE) +
      geom_hline(yintercept = 0, color = "black") +
      geom_vline(xintercept = 0, color = "black") +
      scale_x_continuous(
        breaks = seq(-20, 20, 10),
        limits = c(-23, 23),
        expand = expansion(mult = 0)
      ) +
      scale_y_continuous(
        breaks = seq(-20, 20, 10),
        limits = c(-23, 23),
        expand = expansion(mult = 0)
      ) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      labs(
        title = paste("Movement Profile:", input$ComparePitcher2),
        x = "Horizontal Break (inches)",
        y = "Induced Vertical Break (inches)"
      ) +
      guides(color = guide_legend(title = "Pitch Type", order = 1), fill = "none") +
      theme_minimal() +
      theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      ) +
      coord_fixed(ratio = 1)
  })
  
  output$CompareHeat_Maps1 <- renderUI({
    req(input$ComparePitcher1, input$ComparePitchTypes1, input$CompareDates1, input$batter_split_compare)
    
    pitches <- intersect(pitch_order, input$ComparePitchTypes1)
    pitches <- head(pitches, 6)
    
    plot_output_list <- vector("list", length = length(pitches))
    
    for (i in seq_along(pitches)) {
      pitch <- pitches[i]
      safe_pitch <- gsub("[^A-Za-z0-9]", "_", pitch)
      plotname <- paste0("compare_heatmap1_", safe_pitch)
      
      local({
        my_pitch <- pitch
        my_plotname <- plotname
        
        output[[my_plotname]] <- renderPlot({
          split <- input$batter_split_compare
          
          data <- MLBStatcast2425 %>%
            filter(
              PitcherName == input$ComparePitcher1,
              pitch_name == my_pitch,
              game_date >= input$CompareDates1[1],
              game_date <= input$CompareDates1[2],
              stand %in% selected_sides_compare(),
              !is.na(plate_x), !is.na(plate_z)
            )
          
          total_pitches <- MLBStatcast2425 %>%
            filter(
              PitcherName == input$ComparePitcher1,
              pitch_name %in% input$ComparePitchTypes1,
              game_date >= input$CompareDates1[1],
              game_date <= input$CompareDates1[2],
              stand %in% selected_sides_compare()
            ) %>%
            nrow()
          
          pitch_count <- nrow(data)
          usage_pct <- ifelse(total_pitches > 0, round((pitch_count / total_pitches) * 100), 0)
          
          if (pitch_count == 0) return(NULL)
          
          ggplot(data, aes(x = -plate_x, y = plate_z)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated) +
            # Strike zone lines
            geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom) +
            geom_segment(x = Left, y = Top, xend = Right, yend = Top) +
            geom_segment(x = Left, y = Bottom, xend = Left, yend = Top) +
            geom_segment(x = Right, y = Bottom, xend = Right, yend = Top) +
            geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height) +
            geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height) +
            geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top) +
            geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top) +
            geom_segment(x = Left, y = 0, xend = Right, yend = 0) +
            geom_segment(x = Left, y = 0, xend = Left, yend = 4.25 / 12) +
            geom_segment(x = Left, y = 4.25 / 12, xend = 0, yend = 8.5 / 12) +
            geom_segment(x = Right, y = 4.25 / 12, xend = Right, yend = 0) +
            geom_segment(x = 0, y = 8.5 / 12, xend = Right, yend = 4.25 / 12) +
            xlim(-2.5, 2.5) + ylim(-0.5, 5) +
            ggtitle(paste0(my_pitch, " - ", pitch_count, " pitches (", usage_pct, "%)")) +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
              panel.grid = element_blank(),
              panel.background = element_blank()
            ) +
            coord_fixed(ratio = 1)
        })
        
        plot_output_list[[i]] <<- plotOutput(my_plotname, height = 400, width = 400)
      })
    }
    
    if(length(pitches) < 6){
      for(j in (length(pitches)+1):6){
        plot_output_list[[j]] <- div()
      }
    }
    
    n_pitches <- length(pitches)
    n_rows <- ceiling(n_pitches / 2)
    
    rows <- vector("list", length = n_rows)
    for (r in seq_len(n_rows)) {
      left_idx <- (r - 1) * 2 + 1
      right_idx <- left_idx + 1
      
      rows[[r]] <- fluidRow(
        column(6, plot_output_list[[left_idx]]),
        column(6, if(right_idx <= length(plot_output_list)) plot_output_list[[right_idx]] else div())
      )
    }
    
    tagList(
      tags$h3("Pitcher 1 Heat Maps", style = "text-align: center; font-weight: bold; margin-top: 20px;"),
      rows
    )
  })
  
  output$CompareHeat_Maps2 <- renderUI({
    req(input$ComparePitcher2, input$ComparePitchTypes2, input$CompareDates2, input$batter_split_compare)
    
    pitches <- intersect(pitch_order, input$ComparePitchTypes2)
    pitches <- head(pitches, 6)
    
    plot_output_list <- vector("list", length = max(6, length(pitches)))
    
    for (i in seq_along(pitches)) {
      pitch <- pitches[i]
      safe_pitch <- gsub("[^A-Za-z0-9]", "_", pitch)
      plotname <- paste0("compare_heatmap2_", safe_pitch)
      
      local({
        my_pitch <- pitch
        my_plotname <- plotname
        
        output[[my_plotname]] <- renderPlot({
          req(input$ComparePitcher2, input$ComparePitchTypes2, input$CompareDates2, input$batter_split_compare)
          
          data <- MLBStatcast2425 %>%
            filter(
              PitcherName == input$ComparePitcher2,
              pitch_name == my_pitch,
              game_date >= input$CompareDates2[1],
              game_date <= input$CompareDates2[2],
              stand %in% selected_sides_compare(),
              !is.na(plate_x), !is.na(plate_z)
            )
          
          total_pitches <- MLBStatcast2425 %>%
            filter(
              PitcherName == input$ComparePitcher2,
              pitch_name %in% input$ComparePitchTypes2,
              game_date >= input$CompareDates2[1],
              game_date <= input$CompareDates2[2],
              stand %in% selected_sides_compare()
            ) %>%
            nrow()
          
          pitch_count <- nrow(data)
          usage_pct <- ifelse(total_pitches > 0, round((pitch_count / total_pitches) * 100), 0)
          
          req(pitch_count > 0)
          
          ggplot(data, aes(x = -plate_x, y = plate_z)) +
            stat_density2d_filled() +
            scale_fill_manual(values = heat_colors_interpolated) +
            geom_segment(x = Left, y = Bottom, xend = Right, yend = Bottom) +
            geom_segment(x = Left, y = Top, xend = Right, yend = Top) +
            geom_segment(x = Left, y = Bottom, xend = Left, yend = Top) +
            geom_segment(x = Right, y = Bottom, xend = Right, yend = Top) +
            geom_segment(x = Left, y = Bottom + Height, xend = Right, yend = Bottom + Height) +
            geom_segment(x = Left, y = Top - Height, xend = Right, yend = Top - Height) +
            geom_segment(x = Left + Width, y = Bottom, xend = Left + Width, yend = Top) +
            geom_segment(x = Right - Width, y = Bottom, xend = Right - Width, yend = Top) +
            geom_segment(x = Left, y = 0, xend = Right, yend = 0) +
            geom_segment(x = Left, y = 0, xend = Left, yend = 4.25 / 12) +
            geom_segment(x = Left, y = 4.25 / 12, xend = 0, yend = 8.5 / 12) +
            geom_segment(x = Right, y = 4.25 / 12, xend = Right, yend = 0) +
            geom_segment(x = 0, y = 8.5 / 12, xend = Right, yend = 4.25 / 12) +
            xlim(-2.5, 2.5) + ylim(-0.5, 5) +
            ggtitle(paste0(my_pitch, " - ", pitch_count, " pitches (", usage_pct, "%)")) +
            theme(
              legend.position = "none",
              plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
              panel.grid = element_blank(),
              panel.background = element_blank()
            ) +
            coord_fixed(ratio = 1)
        })
        
        plot_output_list[[i]] <<- plotOutput(my_plotname, height = 400, width = 400)
      })
    }
    
    if (length(pitches) < 6) {
      for (j in (length(pitches) + 1):6) {
        plot_output_list[[j]] <- div()
      }
    }
    
    n_pitches <- length(pitches)
    n_rows <- ceiling(n_pitches / 2)
    
    rows <- vector("list", length = n_rows)
    for (r in seq_len(n_rows)) {
      left_idx <- (r - 1) * 2 + 1
      right_idx <- left_idx + 1
      
      rows[[r]] <- fluidRow(
        column(6, plot_output_list[[left_idx]]),
        column(6, if (right_idx <= length(plot_output_list)) plot_output_list[[right_idx]] else div())
      )
    }
    
    tagList(
      tags$h3("Pitcher 2 Heat Maps", style = "text-align: center; font-weight: bold; margin-top: 20px;"),
      rows
    )
  })
}

# Run the app
shinyApp(ui, server)

