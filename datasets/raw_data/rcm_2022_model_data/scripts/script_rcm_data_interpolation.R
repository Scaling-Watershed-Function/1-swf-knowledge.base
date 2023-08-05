###############################################################################
# RCM data interpolation for missing values
###############################################################################

gc()

librarian::shelf(tidyverse,
                 utils,
                 nhdplusTools,
                 sp,
                 sf,
                 leaflet,
                 stringr)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"

rcm_gap_filled_dat <- read_csv(rcm_gap_filled_dat,paste(local_data,"RF_filled_rcm_2022_model_data.csv", sep ='/'),
          show_col_types =  = FALSE)

# Main function to interpolate missing values
interpolate_missing_values <- function(data, column, regression = TRUE) {
  
  column <- rlang::sym(column)
  
  data <- data %>%
    mutate(!!column := ifelse(!!column < 0 | is.na(!!column), NA, !!column))
  
  for (i in seq_len(nrow(data))) {
    
    # Check if the column value is missing (represented by NA)
    if (is.na(data[[rlang::as_string(column)]][i])) {
      immediate_median <- get_immediate_neighbors_median(data, rlang::as_string(column), data$comid[i], data$tocomid[i])
      
      # If there are no immediate neighbors, replace with the median value from the defined subsample
      if (is.na(immediate_median)) {
        # Calculate the subsample based on 'stream_order', 'mean_ann_pcpt_mm', 'wshd_area_km2', and 'basin'
        subsample <- calculate_subsample(data, rlang::as_string(column), i)
        immediate_median <- median(subsample, na.rm = TRUE)
      }
      
      # If the value is still NA and regression is TRUE, replace with the predicted value from the log-linear model
      if (is.na(immediate_median) & regression) {
        # Make sure we only use rows with non-NA and positive values for 'mean_ann_pcpt_mm' and 'wshd_area_km2' 
        # to fit the model
        valid_rows <- !is.na(data[[rlang::as_string(column)]]) & data$mean_ann_pcpt_mm > 0 & data$wshd_area_km2 > 0
        # Create the formula dynamically
        formula <- as.formula(paste(rlang::as_string(column), "~ log(stream_order) + log(mean_ann_pcpt_mm) + log(wshd_area_km2)"))
        model <- lm(formula, data = data[valid_rows, ])
        # Predict the value for the current row
        immediate_median <- as.numeric(predict(model, newdata = data[i, ])[1])
        
        # If the predicted value is less than 0, replace with the minimum positive value in the column
        if (immediate_median < 0) {
          immediate_median <- min(data[[rlang::as_string(column)]][data[[rlang::as_string(column)]] > 0], na.rm = TRUE)
        }
      }
      
      # Assign the calculated value to the missing value
      data[[rlang::as_string(column)]][i] <- immediate_median
    }
  }
  
  return(data)
}

# Interpolating values:

# Roughness (29 NAs)
roughness_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                              select(comid,
                                                     tocomid,
                                                     basin,
                                                     stream_order,
                                                     mean_ann_pcpt_mm,
                                                     wshd_area_km2,
                                                     roughness),
                                            column = "roughness",
                                            regression = TRUE)