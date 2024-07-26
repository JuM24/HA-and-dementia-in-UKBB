# FO skin and subcutaneous tissues disorders 131696-131838; just even ones
FO_skin_fields <- sapply(seq(131696, 131838, 2), as.character)
FO_skin_fields <- sapply(FO_skin_fields, function(x) paste0('X', x, '.'))
# FO infectious and parasitic disorders; 130000-130344 just even ones
FO_infect_fields <- sapply(seq(130000, 130344, 2), as.character)
FO_infect_fields <- sapply(FO_infect_fields, function(x) paste0('X', x, '.'))



# outlier-removal function
outliers <- function(x, var_metric, method) {
  if (method == 'SD'){
    maximum <- (mean(x, na.rm=T)) + (var_metric * sd(x, na.rm=T))
    minimum <- (mean(x, na.rm=T)) - (var_metric * sd(x, na.rm=T))
  }
  else if (method == 'IQR'){
    maximum <- (quantile(x, 0.75, na.rm=T)) + (var_metric * IQR(x, na.rm=T))
    minimum <- (quantile(x, 0.25, na.rm=T)) - (var_metric * IQR(x, na.rm=T))
  }
  x[(x > maximum) | (x < minimum)] <- NA
  return(x)
}




# finds the closest non-missing variable measurement for the variable of interest;
# the variable name must be of the form `var_prefix_x`, where x indicates the 
# assessment number (0-3) and `var_prefix` the name of the variable, e.g., 'education'
find_closest_non_missing <- function(df, var_prefix, date_hear_loss_any) {
  # absolute date difference between date of hearing loss and assessment dates
  for (i in 0:3) {
    df[[paste0('date_diff_loss_', i)]] <- ifelse(
      is.na(df[[paste0(var_prefix, '_', i)]]), 
      Inf, # assessment dates with NAs given infinite date distance to prioritise non-NA dates
      abs(difftime(df[[paste0('date_', i)]], df[[date_hear_loss_any]], units = 'days'))
    )
  }
  
  # determine the smallest difference between HL start and assessment dates
  df$use_ass <- NA
  # compute minimum of the non-missing data
  min_diff <- do.call(pmin, c(df[paste0('date_diff_loss_', 0:3)], na.rm = TRUE)) 
  for (i in 0:3) {
    # identify assessments with non-missing data
    non_missing <- !is.na(df[[paste0(var_prefix, '_', i)]])
    # logical vector: does the current assessment have the minimum difference?
    closest <- df[[paste0('date_diff_loss_', i)]] == min_diff & non_missing 
    df$use_ass[closest & is.na(df$use_ass)] <- i # determine use_ass based on above
  }
  
  # select the date that minimises the time difference between HL start and assessment date
  for (i in 0:3) {
    valid_indices <- !is.na(df$use_ass) & df$use_ass == i
    df[[paste0(var_prefix, '_USE')]][valid_indices] <- 
      df[[paste0(var_prefix, '_', i)]][valid_indices]
  }
  
  # create a column indicating the duration of this minimum difference
  df[[paste0('min_diff_', var_prefix)]] <- min_diff
  
  # Remove intermediate columns
  df <- df[ , !(names(df) %in% paste0('date_diff_loss_', 0:3))]
  
  return(df)
}


## this is a variation of the original function; 
## here, an assessment is chosen only if it occurs before time 0

# finds the closest non-missing variable measurement for the variable of interest;
# the variable name must be of the form `var_prefix_x`, where x indicates the assessment number (0-3)
# and `var_prefix` the name of the variable, e.g., 'education'
find_closest_non_missing_before_0 <- function(df, var_prefix, date_hear_loss_any) {
  # absolute date difference between date of hearing loss and assessment dates
  for (i in 0:3) {
    df[[paste0('date_diff_loss_', i)]] <- ifelse(
      is.na(df[[paste0(var_prefix, '_', i)]]), 
      Inf, # assessment dates with NAs given infinite date distance to prioritise non-NA dates
      difftime(df[[date_hear_loss_any]], df[[paste0('date_', i)]], units = 'days')
    )
  }
  
  # determine the smallest difference between HL start and assessment dates
  df$use_ass <- NA
  # compute minimum of the non-missing data
  min_diff <- do.call(pmin, c(df[paste0('date_diff_loss_', 0:3)], na.rm = TRUE))
  for (i in 0:3) {
    # identify assessments with non-missing data
    non_missing <- !is.na(df[[paste0(var_prefix, '_', i)]])
    # logical vector: does the current assessment have the minimum difference?
    closest <- df[[paste0('date_diff_loss_', i)]] == min_diff & non_missing
    df$use_ass[closest & is.na(df$use_ass)] <- i # determine use_ass based on above
  }
  
  # select the date that minimises the time difference between HL start and assessment date
  for (i in 0:3) {
    valid_indices <- !is.na(df$use_ass) & df$use_ass == i
    df[[paste0(var_prefix, '_USE')]][valid_indices] <- 
      df[[paste0(var_prefix, '_', i)]][valid_indices]
  }
  
  # create a column indicating the duration of this minimum difference
  df[[paste0('min_diff_', var_prefix)]] <- min_diff
  
  # Remove intermediate columns
  df <- df[ , !(names(df) %in% paste0('date_diff_loss_', 0:3))]
  
  # remove positive values
  df[df[[paste0('min_diff_', var_prefix)]] > 0, paste0(var_prefix, '_USE')] <- NA
  
  # change negative to absolute so the other parts of the code work as expected
  df[df[[paste0('min_diff_', var_prefix)]] < 0, paste0('min_diff_', var_prefix)] <- 
    abs(df[df[[paste0('min_diff_', var_prefix)]] < 0, paste0('min_diff_', var_prefix)])
  
  return(df)
}


# classification of education into three levels in UKBB
education_classify <- function(x){
  if (any((x == 1 | x == 6) & !is.na(x), na.rm = TRUE)){
    return(3)
  } else if (any((x == 2 | x == 3 | x == 4 | x == 5) & !is.na(x), na.rm = TRUE)){
    return(2)
  } else if (any (x == -7  & !is.na(x), na.rm = TRUE)){
    return(1)
  } else {
    return(NA)
  }
}


# survival plots for imputed data
analyse_imputed_data <- function(dataset) {
  dataset$follow_up <- as.numeric(dataset$follow_up)
  surv_obj <- Surv(time = dataset$follow_up, event = dataset$dementia)
  surv_weighted <- survfit(surv_obj ~ hear_aid_any, data = dataset, weights = dataset$weights)
  df_surv <- broom::tidy(surv_weighted)
  df_surv <- df_surv %>%
    mutate(cloglog = log(-log(1-estimate)))
  
  return(df_surv)
}