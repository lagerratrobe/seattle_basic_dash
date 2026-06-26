library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)

# Read in the entire data and convert obsTimeLocal to POSIXct data type
getData <- function() {
  df <- readRDS(
    url("https://github.com/lagerratrobe/weather_station/raw/main/Data/station_obs.RDS")) |> 
    filter(stationID == "KWASEQUI431") |>
    mutate(stationID,
           "Time" = lubridate::parse_date_time(
             obsTimeLocal,
             "Ymd HMS",
             tz = "UTC",
             truncated = 0,
             quiet = FALSE,
             exact = FALSE) ,
           "Temperature" = imperial.temp,
           "Precip" = imperial.precipTotal,
           "SolarWatts" = solarRadiation,
           "Humidity" = humidity,
           "Pressure" = imperial.pressure,
           .keep = "none") |>
    arrange(desc(Time)) %>% 
    head(n=48)
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
               label=sprintf("Now = %d deg", current_temp),
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
