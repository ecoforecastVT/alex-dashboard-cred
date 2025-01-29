wind_dir_obs <- read_csv('data_raw/wind_direction_obs.csv', skip=1) |> 
  rename(datetime = `Timestamp (UTC+09:30)`, value = `Value (deg)`, code = `Grade Code`) |> 
  mutate(date = as.Date(datetime),
         hour = lubridate::hour(datetime)) |> 
  group_by(date, hour) |> 
  summarise(wind_dir = mean(value, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(time = paste0(hour,':00:00'),
         datetime = as.POSIXct(paste(date,time), format="%Y-%m-%d %H:%M:%S")) |> 
  select(datetime, wind_dir_obs)

wind_velocity_obs <- read_csv('data_raw/wind_velocity_obs.csv',skip=1) |> 
  rename(datetime = `Timestamp (UTC+09:30)`, value = `Value (m/s)`, code = `Grade Code`) |> 
  mutate(date = as.Date(datetime),
         hour = lubridate::hour(datetime)) |> 
  group_by(date, hour) |> 
  summarise(wind_speed = mean(value, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(time = paste0(hour,':00:00'),
         datetime = as.POSIXct(paste(date,time), format="%Y-%m-%d %H:%M:%S")) |> 
  select(datetime, wind_speed_obs)


wind_obs_df <- wind_dir_obs |> 
  right_join(wind_velocity_obs, by = c('datetime'))

