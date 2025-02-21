historic_trend_calc <- function(day_of_interest, interest_var, days_historic, interest_site){
  
  s3_score <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/scores/parquet", 
                               endpoint_override = "renc.osn.xsede.org", 
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
  }
  ## FIGURE OUT THE CRITERIA WE WANT TO USE FOR THE TREND SIGNIFICANCE
  
  ## USING SLOPE FROM LM() FUNCTION
  ## INDICATE CHANGE IF SLOPE INCREASES OR DECREASES BY 5% OF TIMESERIES AVERAGE
  trend_model <- lm(score_df$mean ~ score_df$rownum)
  var_trend <- trend_model$coefficients[[2]]
  
  var_threshold <- mean(score_df$mean)*0.05
  
  if ((var_trend <= var_threshold) & (var_trend >= var_threshold)){
    arrow_icon <- 'arrow-bar-right'
  } else if (var_trend > var_threshold){
    arrow_icon <- 'arrow-up-square'
  } else if (var_trend < var_threshold){
    arrow_icon <- 'arrow-down-square'
  }
  
  return(list(round(var_trend,1), arrow_icon))
 
  
  return(list(round(var_trend,1), arrow_icon))
}
