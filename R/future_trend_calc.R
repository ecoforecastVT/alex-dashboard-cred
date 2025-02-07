future_trend_calc <- function(day_of_interest, interest_var, days_ahead, interest_site){
  
  s3_score <- arrow::s3_bucket(bucket = "bio230121-bucket01/flare/scores/parquet", 
                               endpoint_override = "renc.osn.xsede.org", 
                               anonymous = TRUE)
  
  score_df <- arrow::open_dataset(s3_score) |>
    filter(variable == interest_var,
           depth %in% c(0.5),
           site_id %in% interest_site,
           horizon >= 0,
           horizon <= days_ahead,
           reference_datetime == (day_of_interest),
           model_id == 'glm_flare_v3') |>
    dplyr::collect()
  
  ## FIGURE OUT THE CRITERIA WE WANT TO USE FOR THE TREND SIGNIFICANCE
  
}