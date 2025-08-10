# ==============================================================================
# 0. SETUP: LOAD LIBRARIES & SET CONSTANTS
# ==============================================================================
# Run install.packages(c("shiny", "shinycssloaders")) if you don't have them
library(shiny)
library(shinycssloaders) # For loading spinners
library(suncalc)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(maps)
library(metR)
library(tidyr)
library(purrr)
library(ggrepel)
library(lutz)
library(tools)
library(furrr)

# ---- ACTIVATE PARALLEL PROCESSING ----
# Using all cores minus one. Can be adjusted.
plan(multisession, workers = availableCores() - 1)

# ---- GLOBAL CONSTANTS ----
ANALYSIS_YEAR <- 2025
PLOT_CAPTION <- "Source: suncalc R Package | Author: Rodney Cuevas"

# Create a 'cache' directory if it doesn't exist
if (!dir.exists("cache")) {
  dir.create("cache")
}

# ==============================================================================
# 1. HELPER FUNCTIONS (WRAPPING YOUR SCRIPT'S LOGIC)
# ==============================================================================

# This massive function now accepts the year as an argument
get_sun_data <- function(state_name, analysis_year) {
  
  # --- Caching Logic ---
  cache_version <- "v2.2" # Incremented version for new year logic
  cache_file <- file.path("cache", paste0("daylight_data_cache_", cache_version, "_", state_name, "_", analysis_year, ".rds"))
  
  if (file.exists(cache_file)) {
    shiny::showNotification(paste("Loading cached data for", state_name, " (", analysis_year, ")"), type = "message")
    cached_data <- readRDS(cache_file)
    return(cached_data)
  }
  
  # --- If no cache, run all calculations ---
  shiny::showNotification("No cache found. Running calculations... (This may take a minute)", id = "calc_notify", duration = NULL)
  
  # --- Geographic & Point of Interest Calculations ---
  state_sf_polygon <- st_as_sf(map_data("state", region = state_name), coords = c("long", "lat"), crs = 4326) %>%
    group_by(group) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  bbox <- st_bbox(state_sf_polygon)
  grid_points <- expand.grid(lon = seq(bbox["xmin"], bbox["xmax"], length.out = 100), lat = seq(bbox["ymin"], bbox["ymax"], length.out = 100))
  grid_sf <- st_as_sf(grid_points, coords = c("lon", "lat"), crs = 4326)
  state_grid_sf <- st_filter(grid_sf, state_sf_polygon)
  
  if (nrow(state_grid_sf) == 0) {
    shiny::showNotification(paste("Error: Found 0 grid points inside", toTitleCase(state_name)), type = "error")
    return(NULL)
  }
  
  state_grid_points <- as.data.frame(st_coordinates(state_grid_sf))
  colnames(state_grid_points) <- c("lon", "lat")
  
  points_of_interest <- state_grid_points %>%
    arrange(lat) %>%
    mutate(lat_rank = ntile(lat, 100)) %>%
    group_by(lat_rank) %>%
    summarise(lat = mean(lat), lon = mean(lon), .groups = "drop") %>%
    filter(lat_rank == 1 | lat_rank == 50 | lat_rank == 100) %>%
    mutate(location = case_when(lat_rank == 1 ~ "south", lat_rank == 50 ~ "center", lat_rank == 100 ~ "north")) %>%
    select(location, lat, lon)
  
  center_coords <- points_of_interest %>% filter(location == "center")
  analysis_timezone <- tz_lookup_coords(center_coords$lat, center_coords$lon, method = "accurate")
  
  # --- Heavy Data Calculation (Parallel) ---
  calculate_monthly_daylight_change <- function(lat, lon, year, month) {
    start_date <- ymd(paste(year, month, "01", sep = "-"))
    end_date <- ceiling_date(start_date, "month") - days(1)
    sun_times_start <- getSunlightTimes(date = start_date, lat = lat, lon = lon)
    sun_times_end <- getSunlightTimes(date = end_date, lat = lat, lon = lon)
    daylight_start_min <- as.numeric(difftime(sun_times_start$sunset, sun_times_start$sunrise, units = "mins"))
    daylight_end_min <- as.numeric(difftime(sun_times_end$sunset, sun_times_end$sunrise, units = "mins"))
    return(daylight_end_min - daylight_start_min)
  }
  
  final_data_grid <- tidyr::crossing(month = 1:12, state_grid_points)
  daylight_changes <- future_pmap_dbl(
    .l = list(lat = final_data_grid$lat, lon = final_data_grid$lon, year = analysis_year, month = final_data_grid$month),
    .f = ~calculate_monthly_daylight_change(..1, ..2, ..3, ..4),
    .progress = TRUE
  )
  final_data <- final_data_grid %>%
    mutate(daylight_change_min = daylight_changes, month_name = factor(month.name[month], levels = month.name))
  
  daily_data <- expand.grid(
    date = seq(ymd(paste0(analysis_year, "-01-01")), ymd(paste0(analysis_year, "-12-31")), by = "day"),
    location = points_of_interest$location
  ) %>%
    left_join(points_of_interest, by = "location") %>%
    mutate({
      sun_times <- getSunlightTimes(data = .);
      daylight_hours <- as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours"));
      data.frame(daylight_hours = daylight_hours)
    })
  
  # --- Data Prep for Plot Labels ---
  calculate_end_of_month_daylight <- function(lat, lon, year, month) {
    end_date <- ceiling_date(ymd(paste(year, month, "01", sep = "-")), "month") - days(1)
    sun_times <- getSunlightTimes(date = end_date, lat = lat, lon = lon)
    return(as.numeric(difftime(sun_times$sunset, sun_times$sunrise, units = "hours")))
  }
  daylight_summary <- tidyr::crossing(month = 1:12, points_of_interest) %>%
    rowwise() %>%
    mutate(daylight_hours = calculate_end_of_month_daylight(lat, lon, analysis_year, month)) %>%
    ungroup() %>%
    select(month, location, daylight_hours) %>%
    pivot_wider(names_from = location, values_from = daylight_hours) %>%
    arrange(month) %>%
    mutate(
      month_name = month.name[month],
      facet_label_chr = sprintf("%s\nN: %.1fh | C: %.1fh | S: %.1fh", month_name, north, center, south),
      facet_label = factor(facet_label_chr, levels = facet_label_chr)
    )
  final_data <- final_data %>%
    select(-any_of("facet_label")) %>%
    left_join(daylight_summary %>% select(month, facet_label), by = "month")
  
  # --- Final list of all data objects ---
  all_data <- list(
    state_sf_polygon = state_sf_polygon,
    final_data = final_data,
    daily_data = daily_data,
    points_of_interest = points_of_interest,
    analysis_timezone = analysis_timezone
  )
  
  # --- Save to cache and return ---
  saveRDS(all_data, file = cache_file)
  shiny::removeNotification("calc_notify")
  shiny::showNotification("Calculations complete!", type = "message")
  
  return(all_data)
}


# ==============================================================================
# 2. UI: THE USER INTERFACE
# ==============================================================================
ui <- fluidPage(
  titlePanel("R Sun Almanac: A State Daylight Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions"),
      p("Select a U.S. state and a year, then click the button to generate a full suite of daylight and solar analysis plots."),
      
      selectInput("state", "Choose a State:",
                  choices = toTitleCase(state.name),
                  selected = "Mississippi"),
      
      # ADDED YEAR INPUT            
      numericInput("year", "Choose a Year:",
                   value = 2025, min = 1950, max = 2050, step = 1),
      
      actionButton("go", "Generate Plots", icon = icon("chart-line"), class = "btn-primary btn-lg"),
      
      hr(),
      p(HTML("<strong>Author:</strong> Rodney Cuevas")),
      p(HTML("<strong>Source:</strong> <code>suncalc</code> R Package"))
    ),
    
    mainPanel(
      h3(textOutput("plot_header")),
      # Use a tabset panel to neatly organize the 10 plots
      tabsetPanel(id = "plots",
                  tabPanel("Monthly Daylight Change", withSpinner(plotOutput("p1_contour", height = "800px"))),
                  tabPanel("Annual Daylight Curve", withSpinner(plotOutput("p2_annual_curve", height = "800px"))),
                  tabPanel("Average Monthly Change", withSpinner(plotOutput("p3_monthly_avg", height = "800px"))),
                  tabPanel("Daylight Symmetry", withSpinner(plotOutput("p4_symmetry", height = "800px"))),
                  tabPanel("Sunrise & Sunset Times", withSpinner(plotOutput("p5_sunrise_sunset", height = "800px"))),
                  tabPanel("Solar Noon Variation", withSpinner(plotOutput("p6_solar_noon", height = "800px"))),
                  tabPanel("Twilight Duration", withSpinner(plotOutput("p7_twilight", height = "800px"))),
                  tabPanel("Extreme Events Calendar", withSpinner(plotOutput("p8_extremes", height = "800px"))),
                  tabPanel("Sun Angle Comparison", withSpinner(plotOutput("p9_sun_angle", height = "800px"))),
                  tabPanel("Seasonal Sun Angle", withSpinner(plotOutput("p10_seasonal_angle", height = "800px")))
      )
    )
  )
)

# ==============================================================================
# 3. SERVER: THE APP'S BRAIN
# ==============================================================================
server <- function(input, output, session) {
  
  # A reactiveVal to store the results of the big calculation.
  # By REMOVING `ignoreNULL = FALSE`, this will now wait for the first button click.
  sun_data <- eventReactive(input$go, {
    get_sun_data(tolower(input$state), input$year)
  })
  
  output$plot_header <- renderText({
    # Only show the header after the data is calculated
    req(sun_data())
    paste("Displaying Plots for:", toTitleCase(input$state), " (", input$year, ")")
  })
  
  # --- RENDER ALL 10 PLOTS ---
  # Each plot function is wrapped in renderPlot() and accesses the reactive sun_data()
  
  output$p1_contour <- renderPlot({
    df <- sun_data()
    req(df) # Ensure data is loaded
    
    state_border <- geom_sf(data = df$state_sf_polygon, inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.5)
    contour_breaks <- seq(-60, 70, by = 10)
    
    ggplot(df$final_data, aes(x = lon, y = lat)) +
      geom_raster(aes(fill = daylight_change_min), interpolate = TRUE) +
      geom_contour(aes(z = daylight_change_min), color = "white", alpha = 0.7, linewidth = 0.4, breaks = contour_breaks) +
      geom_text_contour(aes(z = daylight_change_min), stroke = 0.2, check_overlap = TRUE, size = 2.5, breaks = contour_breaks) +
      state_border + facet_wrap(~ facet_label, ncol = 4) +
      scale_fill_gradient2(name = "Change (min)", low = "darkblue", mid = "white", high = "darkred", midpoint = 0, na.value = "transparent") +
      labs(title = paste("Monthly Change in Daylight Hours Across", toTitleCase(input$state), input$year),
           subtitle = "Change from the first day to the last day of each month (100x100 grid resolution)",
           x = "Longitude", y = "Latitude", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 10) +
      theme(strip.text = element_text(face = "bold", size = 9, lineheight = 1.2),
            legend.position = "bottom", legend.key.width = unit(1.5, "cm"),
            panel.spacing.y = unit(1.5, "lines"),
            plot.caption = element_text(hjust = 0, size = 8, color = "grey50")) +
      coord_sf(crs = 4326, expand = FALSE)
  })
  
  output$p2_annual_curve <- renderPlot({
    df <- sun_data()
    req(df)
    
    ggplot(df$daily_data, aes(x = date, y = daylight_hours, color = location)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(
        name = paste("Location in", toTitleCase(input$state)), 
        values = c("north" = "darkred", "center" = "darkgreen", "south" = "darkblue"), 
        # FIX: Use a named vector for labels to ensure they match the values correctly
        labels = c("north" = "North", "center" = "Center", "south" = "South")
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = paste("Annual Daylight Curve for", toTitleCase(input$state), "(", input$year, ")"),
           subtitle = "Total hours of daylight per day shows longer summer days in the north",
           x = "Month", y = "Hours of Daylight", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p3_monthly_avg <- renderPlot({
    df <- sun_data()
    req(df)
    monthly_summary <- df$final_data %>% filter(!is.na(daylight_change_min)) %>% group_by(month_name) %>% summarise(average_change = mean(daylight_change_min))
    
    ggplot(monthly_summary, aes(x = month_name, y = average_change, fill = average_change)) +
      geom_col() +
      geom_text(aes(label = round(average_change, 1)), vjust = ifelse(monthly_summary$average_change > 0, -0.5, 1.5), color = "black") +
      scale_fill_gradient2(name = "Change (min)", low = "#0000FF", mid = "white", high = "#FF0000", midpoint = 0) +
      labs(title = paste("Average Monthly Change in Daylight Across", toTitleCase(input$state)),
           subtitle = "Shows the speed of seasonal change. Fastest changes occur near the equinoxes.",
           x = "Month", y = "Average Change in Daylight (minutes)", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
            plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p4_symmetry <- renderPlot({
    df <- sun_data()
    req(df)
    center_daily_data <- df$daily_data %>% filter(location == "center")
    solstice_date <- center_daily_data$date[which.max(center_daily_data$daylight_hours)]
    growing_days <- center_daily_data %>% filter(date < solstice_date)
    shrinking_days <- center_daily_data %>% filter(date > solstice_date)
    daylight_matches_list <- lapply(1:nrow(growing_days), function(i) {
      target_daylight <- growing_days$daylight_hours[i]
      match_index <- which.min(abs(shrinking_days$daylight_hours - target_daylight))
      if (length(match_index) > 0) {
        data.frame(growing_date = growing_days$date[i], matching_date = shrinking_days$date[match_index], daylight = target_daylight)
      }
    })
    daylight_matches <- bind_rows(daylight_matches_list)
    sample_matches <- daylight_matches %>% filter(day(growing_date) == 1 & month(growing_date) %in% c(1:6))
    
    ggplot(center_daily_data, aes(x = date, y = daylight_hours)) +
      geom_line(color = "black", linewidth = 1.5, alpha = 0.8) +
      geom_vline(xintercept = solstice_date, linetype = "dashed", color = "darkorange") +
      geom_segment(data = sample_matches, aes(x = growing_date, xend = matching_date, y = daylight, yend = daylight), color = "steelblue", linetype = "dotted", linewidth = 1) +
      geom_point(data = sample_matches, aes(x = growing_date, y = daylight), color = "steelblue", size = 3) +
      geom_point(data = sample_matches, aes(x = matching_date, y = daylight), color = "steelblue", size = 3) +
      geom_text(data = sample_matches, aes(x = growing_date, y = daylight, label = format(growing_date, "%b %d")), nudge_y = 0.2, size = 3.5, hjust = 1) +
      geom_text(data = sample_matches, aes(x = matching_date, y = daylight, label = format(matching_date, "%b %d")), nudge_y = 0.2, size = 3.5, hjust = 0) +
      annotate("text", x = solstice_date, y = 10, label = "Summer Solstice", angle = 90, vjust = -1, color = "darkorange") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = paste("The Symmetry of Daylight in Central", toTitleCase(input$state)),
           subtitle = "For every day in spring, a day in autumn has nearly the same amount of daylight.",
           x = "Date", y = "Hours of Daylight", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) + theme(plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p5_sunrise_sunset <- renderPlot({
    df <- sun_data()
    req(df)
    
    # Dynamically find DST dates for the selected year
    all_dates <- seq(ymd(paste0(input$year, "-01-01")), ymd(paste0(input$year, "-12-31")), by = "day")
    tz_abbr <- format(with_tz(all_dates, df$analysis_timezone), "%Z")
    dst_changes <- all_dates[which(tz_abbr != lag(tz_abbr))]
    spring_forward_date <- dst_changes[1]
    fall_back_date <- dst_changes[2]
    
    center_location <- df$points_of_interest %>% filter(location == "center")
    sunrise_sunset_data <- data.frame(date = all_dates)
    sun_times_df <- getSunlightTimes(date = sunrise_sunset_data$date, lat = center_location$lat, lon = center_location$lon)
    dummy_date_for_y_axis <- as.Date("1970-01-01")
    sunrise_sunset_data <- sunrise_sunset_data %>%
      cbind(sun_times_df %>% select(sunrise, sunset)) %>%
      mutate(sunrise_local = with_tz(sunrise, df$analysis_timezone),
             sunset_local = with_tz(sunset, df$analysis_timezone),
             sunrise_time_for_plot = ymd_hms(paste(dummy_date_for_y_axis, format(sunrise_local, "%H:%M:%S"))),
             sunset_time_for_plot = ymd_hms(paste(dummy_date_for_y_axis, format(sunset_local, "%H:%M:%S")))) %>%
      pivot_longer(cols = c(sunrise_time_for_plot, sunset_time_for_plot), names_to = "event", values_to = "time_for_plot")
    
    p <- ggplot(sunrise_sunset_data, aes(x = date, y = time_for_plot, color = event)) +
      geom_line(linewidth = 1.2) +
      scale_y_datetime(date_labels = "%I:%M %p", date_breaks = "1 hour") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0.01, 0.01)) +
      scale_color_manual(name = "Event", values = c("sunrise_time_for_plot" = "darkorange", "sunset_time_for_plot" = "darkblue"), labels = c("Sunrise", "Sunset")) +
      labs(title = paste("Sunrise and Sunset Times Throughout", input$year, "in Central", toTitleCase(input$state)),
           subtitle = paste0("Times shown in local time (", format(with_tz(ymd(paste0(input$year, "-01-01")), df$analysis_timezone), "%Z"),"/",format(with_tz(ymd(paste0(input$year, "-07-01")), df$analysis_timezone), "%Z"),") - note the jumps at daylight saving transitions"),
           x = "Month", y = "Time of Day", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
    
    # Add DST lines if they exist for that year
    if(!is.na(spring_forward_date)) {
      p <- p + geom_vline(xintercept = spring_forward_date, linetype = "dashed", alpha = 0.5) +
        annotate("text", x = spring_forward_date, y = as.POSIXct("1970-01-01 08:00:00"), label = "Spring Forward", angle = 90, vjust = -0.5, size = 3)
    }
    if(!is.na(fall_back_date)) {
      p <- p + geom_vline(xintercept = fall_back_date, linetype = "dashed", alpha = 0.5) +
        annotate("text", x = fall_back_date, y = as.POSIXct("1970-01-01 08:00:00"), label = "Fall Back", angle = 90, vjust = -0.5, size = 3)
    }
    print(p)
  })
  
  output$p6_solar_noon <- renderPlot({
    df <- sun_data()
    req(df)
    center_location <- df$points_of_interest %>% filter(location == "center")
    solar_noon_data <- data.frame(date = seq(ymd(paste0(input$year, "-01-01")), ymd(paste0(input$year, "-12-31")), by = "day"))
    sun_times_noon <- getSunlightTimes(date = solar_noon_data$date, lat = center_location$lat, lon = center_location$lon)
    solar_noon_data <- solar_noon_data %>%
      mutate(solarNoon = sun_times_noon$solarNoon,
             solar_noon_time = as.POSIXct(format(solarNoon, "%H:%M:%S"), format = "%H:%M:%S"),
             mean_solar_noon = mean(solar_noon_time),
             deviation_minutes = as.numeric(difftime(solar_noon_time, mean_solar_noon, units = "mins")))
    
    ggplot(solar_noon_data, aes(x = date, y = deviation_minutes)) +
      geom_line(color = "darkgreen", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = paste("Solar Noon Variation Throughout", input$year, "in Central", toTitleCase(input$state)),
           subtitle = "Shows the equation of time effect - when sundials run fast or slow compared to clocks",
           x = "Month", y = "Deviation from Mean Solar Noon (minutes)", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) + theme(plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p7_twilight <- renderPlot({
    df <- sun_data()
    req(df)
    twilight_data <- expand.grid(date = seq(ymd(paste0(input$year, "-01-01")), ymd(paste0(input$year, "-12-31")), by = "7 days"), location = df$points_of_interest$location) %>% 
      left_join(df$points_of_interest, by = "location")
    sun_times_twilight <- getSunlightTimes(data = twilight_data)
    twilight_data <- twilight_data %>%
      mutate(civil_twilight_morning = as.numeric(difftime(sun_times_twilight$sunrise, sun_times_twilight$dawn, units = "mins")),
             civil_twilight_evening = as.numeric(difftime(sun_times_twilight$dusk, sun_times_twilight$sunset, units = "mins")),
             total_civil_twilight = civil_twilight_morning + civil_twilight_evening)
    
    ggplot(twilight_data, aes(x = date, y = total_civil_twilight, color = location)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2, alpha = 0.6) +
      scale_color_manual(
        name = paste("Location in", toTitleCase(input$state)), 
        values = c("north" = "darkred", "center" = "darkgreen", "south" = "darkblue"),
        # FIX: Use a named vector for labels to ensure they match the values correctly
        labels = c("north" = "North", "center" = "Center", "south" = "South")
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      labs(title = paste("Total Civil Twilight Duration Throughout", input$year),
           subtitle = "Combined morning and evening twilight varies by latitude and season",
           x = "Month", y = "Total Civil Twilight (minutes)", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) + theme(plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p8_extremes <- renderPlot({
    df <- sun_data()
    req(df)
    extreme_times_data <- df$daily_data %>% select(date, location, lat, lon)
    sun_times_extreme <- getSunlightTimes(data = extreme_times_data)
    extreme_times <- extreme_times_data %>%
      mutate(sunrise_ts = sun_times_extreme$sunrise, sunset_ts = sun_times_extreme$sunset,
             daylight_hours = as.numeric(difftime(sunset_ts, sunrise_ts, units = "hours")),
             sunrise_local = with_tz(sunrise_ts, df$analysis_timezone),
             sunset_local = with_tz(sunset_ts, df$analysis_timezone),
             sunrise_numeric = hour(sunrise_local) * 3600 + minute(sunrise_local) * 60 + second(sunrise_local),
             sunset_numeric = hour(sunset_local) * 3600 + minute(sunset_local) * 60 + second(sunset_local),
             sunrise_time_str = format(sunrise_local, "%I:%M %p"),
             sunset_time_str = format(sunset_local, "%I:%M %p"),
             sunrise_tz = format(sunrise_local, "%Z"),
             sunset_tz = format(sunset_local, "%Z")) %>%
      group_by(location) %>%
      summarise(earliest_sunrise_date = date[which.min(sunrise_numeric)],
                earliest_sunrise_time = paste(sunrise_time_str[which.min(sunrise_numeric)], sunrise_tz[which.min(sunrise_numeric)]),
                latest_sunrise_date = date[which.max(sunrise_numeric)],
                latest_sunrise_time = paste(sunrise_time_str[which.max(sunrise_numeric)], sunrise_tz[which.max(sunrise_numeric)]),
                earliest_sunset_date = date[which.min(sunset_numeric)],
                earliest_sunset_time = paste(sunset_time_str[which.min(sunset_numeric)], sunset_tz[which.min(sunset_numeric)]),
                latest_sunset_date = date[which.max(sunset_numeric)],
                latest_sunset_time = paste(sunset_time_str[which.max(sunset_numeric)], sunset_tz[which.max(sunset_numeric)]),
                longest_day_date = date[which.max(daylight_hours)],
                longest_day_hours = round(max(daylight_hours), 2),
                shortest_day_date = date[which.min(daylight_hours)],
                shortest_day_hours = round(min(daylight_hours), 2)) %>%
      pivot_longer(cols = -location, names_to = c("type", ".value"), names_pattern = "(.+)_(date|time|hours)") %>%
      mutate(label = case_when(grepl("earliest_sunrise", type) ~ "Earliest\nSunrise", grepl("latest_sunrise", type) ~ "Latest\nSunrise", grepl("earliest_sunset", type) ~ "Earliest\nSunset", grepl("latest_sunset", type) ~ "Latest\nSunset", grepl("longest_day", type) ~ "Longest\nDay", grepl("shortest_day", type) ~ "Shortest\nDay"),
             date_time_label = case_when(!is.na(time) ~ paste(format(date, "%b %d"), "\n", time), !is.na(hours) ~ paste(format(date, "%b %d"), "\n", hours, "hrs"), TRUE ~ format(date, "%b %d")))
    extremes_center <- extreme_times %>% filter(location == "center", !is.na(date)) %>% mutate(date = as.Date(date))
    
    ggplot(extremes_center, aes(x = date, y = 1)) +
      geom_point(size = 6, color = "darkred") +
      geom_text_repel(aes(label = label), box.padding = 0.6, min.segment.length = 0, nudge_y = 0.25, direction = "y", segment.color = "grey50", size = 3.5) +
      geom_text_repel(aes(label = date_time_label), nudge_y = -0.15, direction = "x", segment.color = "grey50", box.padding = 0.1, point.padding = 0.2, min.segment.length = 0, max.overlaps = Inf, size = 2.8, fontface = "bold", lineheight = 0.9) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b", limits = c(ymd(paste0(input$year, "-01-01")), ymd(paste0(input$year, "-12-31")))) +
      ylim(0.4, 1.6) +
      labs(title = paste("Calendar of Extreme Sunrise/Sunset Events in Central", toTitleCase(input$state), "for", input$year),
           subtitle = "Note: Earliest sunrise and latest sunset occur on different days than the solstice",
           x = "Date", y = "", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
            plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p9_sun_angle <- renderPlot({
    df <- sun_data()
    req(df)
    calculate_solar_noon_altitude <- function(lat, day_of_year) {
      B <- (360 / 365) * (day_of_year - 81) * pi / 180
      declination <- 23.45 * sin(B) * pi / 180
      lat_rad <- lat * pi / 180
      altitude_rad <- pi/2 - abs(lat_rad - declination)
      altitude_rad * 180 / pi
    }
    sun_angle_data <- expand.grid(date = seq(ymd(paste0(input$year, "-01-01")), ymd(paste0(input$year, "-12-31")), by = "day"), location = df$points_of_interest$location) %>%
      left_join(df$points_of_interest, by = "location") %>%
      mutate(day_of_year = yday(date), altitude_degrees = calculate_solar_noon_altitude(lat, day_of_year))
    
    ggplot(sun_angle_data, aes(x = date, y = altitude_degrees, color = location)) +
      geom_line(linewidth = 1.2) + geom_hline(yintercept = 90, linetype = "dotted", alpha = 0.5) + geom_hline(yintercept = 0, linetype = "solid", alpha = 0.3) +
      annotate("text", x = ymd(paste0(input$year, "-06-21")), y = 85, label = "Summer Solstice", size = 3, angle = 0, vjust = -0.5) +
      annotate("text", x = ymd(paste0(input$year, "-12-21")), y = 40, label = "Winter Solstice", size = 3, angle = 0, vjust = -0.5) +
      scale_color_manual(
        name = paste("Location in", toTitleCase(input$state)), 
        values = c("north" = "darkred", "center" = "darkgreen", "south" = "darkblue"),
        # FIX: Use a named vector for labels to ensure they match the values correctly
        labels = c("north" = "North", "center" = "Center", "south" = "South")
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") + scale_y_continuous(breaks = seq(0, 90, by = 15), labels = function(x) paste0(x, "°")) +
      labs(title = paste("Sun Angle at Solar Noon Throughout", input$year, "in", toTitleCase(input$state)),
           subtitle = "Maximum elevation angle of the sun above the horizon each day",
           x = "Month", y = "Sun Altitude Angle", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) + theme(legend.position = "top", plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
  
  output$p10_seasonal_angle <- renderPlot({
    df <- sun_data()
    req(df)
    calculate_solar_noon_altitude <- function(lat, day_of_year) {
      B <- (360 / 365) * (day_of_year - 81) * pi / 180
      declination <- 23.45 * sin(B) * pi / 180
      lat_rad <- lat * pi / 180
      altitude_rad <- pi/2 - abs(lat_rad - declination)
      altitude_rad * 180 / pi
    }
    sun_angle_data <- expand.grid(date = seq(ymd(paste0(input$year, "-01-01")), ymd(paste0(input$year, "-12-31")), by = "day"), location = df$points_of_interest$location) %>%
      left_join(df$points_of_interest, by = "location") %>%
      mutate(day_of_year = yday(date), altitude_degrees = calculate_solar_noon_altitude(lat, day_of_year))
    center_sun_data <- sun_angle_data %>% filter(location == "center")
    key_dates <- center_sun_data %>%
      filter(date %in% c(ymd(paste0(input$year, "-03-20")), ymd(paste0(input$year, "-06-21")), ymd(paste0(input$year, "-09-23")), ymd(paste0(input$year, "-12-21")))) %>%
      mutate(label = c("Spring\nEquinox", "Summer\nSolstice", "Fall\nEquinox", "Winter\nSolstice"))
    min_angle <- floor(min(center_sun_data$altitude_degrees) / 5) * 5
    max_angle <- ceiling(max(center_sun_data$altitude_degrees) / 5) * 5
    
    ggplot(center_sun_data, aes(x = date, y = altitude_degrees)) +
      geom_line(color = "darkgoldenrod", linewidth = 1.5) + geom_point(data = key_dates, size = 4, color = "darkred") +
      geom_text(data = key_dates, aes(label = label), vjust = -1.5, size = 3.5) +
      geom_text(data = key_dates, aes(label = paste0(round(altitude_degrees, 1), "°")), vjust = 1.5, size = 3, fontface = "bold") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_y_continuous(breaks = seq(min_angle, max_angle, by = 5), labels = function(x) paste0(x, "°"), limits = c(min_angle, max_angle)) +
      labs(title = paste("Solar Noon Sun Angle in Central", toTitleCase(input$state), "for", input$year),
           subtitle = "The sun's maximum daily elevation varies by ~47° throughout the year",
           x = "Month", y = "Sun Altitude Angle at Solar Noon", caption = PLOT_CAPTION) +
      theme_minimal(base_size = 14) + theme(panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0, size = 8, color = "grey50"))
  })
}

# ==============================================================================
# 4. RUN THE APP
# ==============================================================================
shinyApp(ui = ui, server = server)
