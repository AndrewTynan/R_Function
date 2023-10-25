
five_num_sum_by_multi_groups <- function(data, by, var, decimals=2) {
  #' Enhanced five number summary of variable by group(s) 
  #' 
  #' @description 
  #' `five_num_sum_by` returns the Min, 1st Quartile,  Mean, Median, 3rd Quartile, Max, and Inter-quartile range, and low and high outlier thresholds
  #'  of the `var` arguemnt for all variables present in the `by` argument for the data.frame in the `data` argument  
  #'  
  #' it is similar to the the base R summary() function, but provides the ability to group_by and a more comprehensive list of summary stats 
  #'
  #' @details 
  #' using across() enables multiple grouping variables to be provided 
  #' NOTE: need to refine the _outlier_threshold metrics a bit 
  #'
  #' @param data the data.frame 
  #' @param by the grouping variable 
  #' @param var the variable the mean is calculated for 
  #' @param var_name a string of the variable provided to the `var` argument... NOTE: can prob remove this and just enquote() (or similar) the `var` argument to create metric_name
  #' 
  #' @return A data.frame 

    data %>%
        group_by(across({{ by }})) %>% 
        summarize(count = n(),          # could add n_distinct()
                    Min = min({{ var }}),
                    Q1 = quantile({{ var }}, .25),
                    mean = mean({{ var }}), 
                    median = median({{ var }}), 
                    Q3 = quantile({{ var }}, .75),
                    Max = max({{ var }}),
                    stddev = sd({{ var }}), .groups = 'drop') %>%
        mutate(IQR = Q3 - Q1,
            IQR_lower_outlier_threshold = Min - (1.5 * IQR),
            IQR_upper_outlier_threshold = Min + (1.5 * IQR),
            sd3_lower_outlier_threshold = mean - (3 * stddev),
            sd3_upper_outlier_threshold = mean + (3 * stddev), 
            metric_name = as_label(enquo(var))) %>%       
        relocate(metric_name, .before = count) %>%
        mutate(across(where(is.numeric), round, decimals))
} 


# examples 
mtcars %>% five_num_sum_by_multi_groups(., c(cyl,am), wt, 3) 

mtcars %>% five_num_sum_by_multi_groups(., c(cyl,am,vs), wt, 3) 

