future_trend_calc <- function(day_of_interest, interest_var, days_ahead, interest_site){
  
  s3_score <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/scores/parquet", 
                               endpoint_override = "amnh1.osn.mghpcc.org", 
                               anonymous = TRUE)
  
  if (interest_var == 'depth'){
  score_df <- arrow::open_dataset(s3_score) |>
    filter(variable == interest_var,
           site_id %in% interest_site,
           horizon >= 0,
           horizon <= days_ahead,
           reference_datetime == (day_of_interest),
           model_id == 'glm_flare_v3') |>
    dplyr::collect() |> 
    mutate(rownum = row_number())
  
  past_reference_datetime <- day_of_interest - lubridate::days(days_ahead)
  past_variance <- arrow::open_dataset(s3_score) |> 
    filter(variable == interest_var,
           site_id %in% interest_site,
           horizon == 0,
           datetime >= past_reference_datetime,
           datetime <= day_of_interest,
           model_id == 'glm_flare_v3') |> 
    collect() |> 
    summarise(mean_var = var(mean, na.rm = TRUE)) |> 
    pull(mean_var)
  
  } else {
    score_df <- arrow::open_dataset(s3_score) |>
      filter(variable == interest_var,
             site_id %in% interest_site,
             depth %in% c(0.5),
             horizon >= 0,
             horizon <= days_ahead,
             reference_datetime == (day_of_interest),
             model_id == 'glm_flare_v3') |>
      dplyr::collect() |> 
      mutate(rownum = row_number())
    
    past_reference_datetime <- day_of_interest - lubridate::days(days_ahead)
    past_variance <- arrow::open_dataset(s3_score) |> 
      filter(variable == interest_var,
             site_id %in% interest_site,
             depth %in% c(0.5),
             horizon == 0,
             datetime >= past_reference_datetime,
             datetime <= day_of_interest,
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
  
  var_threshold <- past_variance ## the mean variance from the past historical days `days ahead`
  
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
