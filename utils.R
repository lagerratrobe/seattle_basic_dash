library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(arrow, warn.conflicts = FALSE)

# Read last ~48 hours of weather data from GCS parquet files
getData <- function() {
  base_url <- "https://storage.googleapis.com/weatherdata-parquet-randre/weather_"

  dates <- Sys.Date() - 0:2
  urls <- paste0(base_url, format(dates, "%Y-%m-%d"), ".parquet")

  frames <- list()
  for (url in urls) {
    tryCatch({
      frames[[length(frames) + 1]] <- arrow::read_parquet(url)
    }, error = function(e) {
      message("Skipping missing file: ", url)
    })
  }

  df <- bind_rows(frames) |>
    transmute(
      Time = with_tz(as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), "America/Los_Angeles"),
      Temperature = tempf,
      Precip = dailyrainin,
      SolarWatts = solarradiation,
      Humidity = humidity,
      Pressure = baromrelin
    ) |>
    arrange(desc(Time)) |>
    head(n = 48)

  return(df)
}



getPlot <- function(weather_data, weather_variable) {
    if (weather_variable == "Temperature") {
    # Useful vars to use in plotting
    max_temp = max(weather_data[[weather_variable]])
    min_temp = min(weather_data[[weather_variable]])
    #max_temp_time = weather_data$Time[which(weather_data[[weather_variable]] == max_temp)]
    max_temp_time = weather_data$Time[24]
    min_temp_time = weather_data$Time[44] # hard-code to lower left
    time_now = weather_data$Time[2] # Back up one to move label left
    current_temp = weather_data[[weather_variable]][1]
    midnight = getMidnight(weather_data)
    
    plot = ggplot(weather_data, mapping = aes(x=Time, y=.data[[weather_variable]])) + 
      geom_line() +
      # Max temp line
      geom_hline(yintercept=max(max_temp),color="red") +
      annotate("text",
               x=max_temp_time ,
               y=max_temp + .5,
               label=sprintf("High = %s deg F", max_temp),
               color="red") +
      # Min temp line
      geom_hline(yintercept=max(min_temp),color="blue") +
      annotate("text",
               x=min_temp_time,
               y=min_temp + .5,
               label=sprintf("Low = %s deg F", min_temp),
               color="blue") +
      # Current Temp 
      annotate("text",
               x=time_now,
               y=current_temp + .5,
               label=sprintf("Now = %.0f deg", current_temp),
               color="darkgreen") +
      # Midnight times
      geom_vline(xintercept=midnight,color="grey35") +
      annotate("text",
               x=midnight ,
               y=max_temp * .98,
               label="Midnight",
               color="grey35") +
      # Plot title and axis labels
      ggtitle("Last 48 Hours of Temperature") +
      theme(plot.title = element_text(hjust = 0.5, size = 28)) +
      labs(x = "Time",
           y = "Temperature")
  }
  if (weather_variable == "Precip") {
    total_precip = getTotalPrecip(weather_data)[[1]]
    time_midpoint = weather_data$Time[24]
    midnight = getMidnight(weather_data)
    plot = ggplot(weather_data, mapping = aes(x=Time, y=.data[[weather_variable]])) +
      geom_line() +
      # Plot Title
      ggtitle("Last 48 Hours of Precip") +
      theme(plot.title = element_text(hjust = 0.5, size = 28)) +
      # Total Precip label
      annotate("text",
               x=time_midpoint,
               y=total_precip * .9,
               label= sprintf("%.2f\" precip total", total_precip),
               color="blue") +
      # Midnight labels
      geom_vline(xintercept=midnight,color="grey35") +
      annotate("text",
               x=midnight ,
               y=total_precip,
               label="Midnight",
               color="grey35")
  }
  
  return(plot)
}

getTotalPrecip <- function(df) {
  df |> 
    group_by(day(Time)) |> 
    summarise(`max_precip` = max(Precip)) |> 
    summarise(total_precip = sum(max_precip)) -> total_precip
  
  return(total_precip)
}

getMidnight <- function(weather_data) {
  midnights = weather_data$Time[hour(weather_data$Time) == 0]
  return(midnights)
}
