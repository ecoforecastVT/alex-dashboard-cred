historic_trend_calc <- function(day_of_interest, interest_var, days_historic, interest_site){
  
  s3_score <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/scores/parquet", 
                               endpoint_override = "amnh1.osn.mghpcc.org", 
                               anonymous = TRUE)
  
  if(interest_var == 'depth'){
    score_df <- arrow::open_dataset(s3_score) |>
      filter(variable == interest_var,
             site_id %in% interest_site,
             horizon == 0,
             model_id == 'glm_flare_v3') |>
      dplyr::collect() |> 
      filter(datetime >= (day_of_interest - lubridate::days(days_historic))) |> 
      mutate(rownum = row_number())
    
    past_reference_datetime <- day_of_interest - lubridate::days(days_historic + days_historic) ## set it to be two periods ago (use period before trend calculation)
    trend_calculation_date_start <- day_of_interest - lubridate::days(days_historic)
    past_variance <- arrow::open_dataset(s3_score) |> 
      filter(variable == interest_var,
             site_id %in% interest_site,
             #horizon <= days_historic,
             horizon == 0,
             datetime >= past_reference_datetime,
             datetime <= trend_calculation_date_start,
             model_id == 'glm_flare_v3') |> 
      dplyr::collect() |> 
      summarise(mean_var = var(mean)) |> 
      pull(mean_var)
    
  } else {
    score_df <- arrow::open_dataset(s3_score) |>
      filter(variable == interest_var,
             site_id %in% interest_site,
             horizon == 0,
             depth %in% c(0.5),
             model_id == 'glm_flare_v3') |>
      dplyr::collect() |> 
      filter(datetime >= (day_of_interest - lubridate::days(days_historic))) |> 
      mutate(rownum = row_number())
    
    past_reference_datetime <- day_of_interest - lubridate::days(days_historic + days_historic) ## set it to be two periods ago (use period before trend calculation)
    trend_calculation_date_start <- day_of_interest - lubridate::days(days_historic)
    past_variance <- arrow::open_dataset(s3_score) |> 
      filter(variable == interest_var,
             site_id %in% interest_site,
             depth %in% c(0.5),
             #horizon <= days_historic,
             horizon == 0,
             datetime >= past_reference_datetime,
             datetime <= trend_calculation_date_start,
             model_id == 'glm_flare_v3') |> 
      dplyr::collect() |> 
      summarise(mean_var = var(mean)) |> 
      pull(mean_var)
  }
  ## FIGURE OUT THE CRITERIA WE WANT TO USE FOR THE TREND SIGNIFICANCE
  
  ## USING SLOPE FROM LM() FUNCTION
  ## INDICATE CHANGE IF SLOPE INCREASES OR DECREASES BY 5% OF TIMESERIES AVERAGE
  trend_model <- lm(score_df$mean ~ score_df$rownum)
  var_trend <- trend_model$coefficients[[2]]
  
  var_threshold <- past_variance ## the mean variance of 0-horizon predictions from 2X the historic days prior until the historic trend period (1X historic days prior)
  
  if (var_trend <= var_threshold){
    arrow_icon <- 'arrow-bar-right'
    trend_text <- 'No Change'
  } else if (abs(var_trend) > var_threshold & sign(var_trend) == 1){
    arrow_icon <- 'graph-up-arrow'
    trend_text <- 'Increasing'
  } else if (abs(var_trend) > var_threshold & sign(var_trend) == -1){
    arrow_icon <- 'graph-down-arrow'
    trend_text <- 'Decreasing'
  }
  
  return(list(round(var_trend,1), arrow_icon, trend_text))
}
