# # In-situ lake level, water temp, salinity (EC)
# water level data is height above sea level
# height of bottom of ALEX (per glm.nml) = -5.3 m
# In situ WQ data
lake_directory <- getwd()
options(timeout=300)
#setwd('data_raw')
download.file(url = paste0("https://water.data.sa.gov.au/Export/BulkExport?DateRange=Custom&StartTime=2020-01-01%2000%3A00&EndTime=", Sys.Date(), "%2000%3A00&TimeZone=0&Calendar=CALENDARYEAR&Interval=PointsAsRecorded&Step=1&ExportFormat=csv&TimeAligned=True&RoundData=True&IncludeGradeCodes=False&IncludeApprovalLevels=False&IncludeQualifiers=False&IncludeInterpolationTypes=False&Datasets[0].DatasetName=Lake%20Level.Best%20Available--Continuous%40A4261133&Datasets[0].Calculation=Instantaneous&Datasets[0].UnitId=82&Datasets[1].DatasetName=EC%20Corr.Best%20Available%40A4261133&Datasets[1].Calculation=Instantaneous&Datasets[1].UnitId=305&Datasets[2].DatasetName=Water%20Temp.Best%20Available--Continuous%40A4261133&Datasets[2].Calculation=Instantaneous&Datasets[2].UnitId=169&_=1711554907800"),
              destfile = file.path(lake_directory, "data_raw", "current_insitu.csv"))

if (!dir.exists(file.path(lake_directory,'targets'))){
  dir.create(file.path(lake_directory,'targets'))
}

cleaned_insitu_file <- file.path(lake_directory,'targets/',paste0("ALEX-targets-insitu.csv"))

readr::read_csv(file.path(lake_directory, "data_raw/current_insitu.csv"),
                skip = 5, show_col_types = FALSE,
                col_names = c('time','Value_level', 'Value_EC', 'Value_temperature')) |>
  # simple conversion to salt
  mutate(Value_salt = oce::swSCTp(conductivity = Value_EC/1000,
                                  temperature = Value_temperature,
                                  conductivityUnit = 'mS/cm'),
         Value_depth = 5.3 + Value_level) |> # 5.3 is the height
  select(-Value_EC, -Value_level) |>
  pivot_longer(names_to = 'variable', names_prefix = 'Value_',
               cols = starts_with('Value'),
               values_to = 'observed') |>
  mutate(time = lubridate::force_tz(time, tzone = "Etc/GMT+9"),
         time = time - lubridate::minutes(30),
         time = lubridate::with_tz(time, tzone = "UTC"),
         date = lubridate::as_date(time),
         hour = lubridate::hour(time)) |>
  group_by(date, hour, variable) |>
  summarize(observation = mean(observed, na.rm = TRUE), .groups = "drop") |>
  mutate(depth = ifelse(variable %in% c('salt', 'temperature'), 0.5, NA),
         site_id = 'ALEX',
         datetime = lubridate::as_datetime(date) + lubridate::hours(hour)) |>
  filter(hour == 0) |>
  select(site_id, datetime, depth, variable, observation) |>
  readr::write_csv(cleaned_insitu_file)
