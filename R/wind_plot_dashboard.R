## function code 

wind_plot_dashboard <- function(data, tzone = "America/New_York", ylims = c(-5,35), forecast_column_name, obs_column_name, historical_horizon, forecast_start_day, var_unit){
  
  if(forecast_column_name == 'wind_speed'){
    var_title = 'Wind Speed'
    var_unit = 'Wind Speed (km/hr)'
    index <- 1
  } else if(forecast_column_name == 'wind_dir'){
    var_title = 'Wind Direction'
    var_unit = 'Wind Direction (deg)'
    label_height_adjust <- 0.01
  } else if(forecast_column_name == 'air_temp'){
    var_title = 'Air Temperature'
    var_unit = 'Temperature (°C)' 
    label_height_adjust <- 0.5
  }else{
    message('Please use correct variable...')
    return(NULL)
  }
  
  p <- ggplot2::ggplot(full_met_data_df, ggplot2::aes(x = datetime)) +
    ggplot2::ylim(ylims) +
    ggplot2::xlim(forecast_start_day - lubridate::days(historical_horizon), (max(data$datetime)) + lubridate::days(5)) +
    ggplot2::geom_line(ggplot2::aes(y = wind_speed), color = 'black') +
    ggplot2::geom_point(ggplot2::aes(x = datetime, y = noquote(obs_column_name)), color = 'red') +
    ggplot2::geom_vline(aes(xintercept = forecast_start_day),
                        alpha = 1, linetype = "dashed") +
    ggplot2::annotate(x = (forecast_start_day - lubridate::days(3)), y = max(ylims) - 1, label = 'Past', geom = 'text') +
    ggplot2::annotate(x = (forecast_start_day + lubridate::days(3)), y = max(ylims) - 1, label = 'Future', geom = 'text') +
    ggplot2::theme_light() +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('solid'),
                                   labels = c('Forecast Date')) +
    ggplot2::scale_y_continuous(name = var_unit,
                                limits = ylims) +
    ggplot2::labs(x = "Date",
                  y = var_unit,
                  title = paste0(var_title," Forecast", lubridate::date(data$forecast_start_day))) +#,
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                   plot.title = element_text(hjust = 0.5))
  
  
  return(p)
}
