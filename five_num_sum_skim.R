

five_num_sum_skim <- function(data, var, by = NA, reporting_output = NA, kbl_settings_group_col_num = NA, kbl_settings_note = NA) {

  #' Enhanced five number summary of variable by group(s) 
  #' 
  #' @description 
  #' `five_num_sum_skim` returns the Min, 1st Quartile,  Mean, Median, 3rd Quartile, Max, and histogram 
  #'  of the `var` arguemnt for all variable in the`by` argument for the data.frame in the `data` argument  
  #'  
  #' it is similar to the the base R summary() function, but provides the ability to group_by and includes a histogram 
  #'
  #' @details 
  #' it depends on clean_metric_names(), clean_col_names(), and kbl_settings() 
  #'
  #' @param data the data.frame 
  #' @param by the grouping variable 
  #' @param var the variable the mean is calculated for 
  #' @param var_name a string of the variable provided to the `var` argument... NOTE: can prob remove this and just enquote() (or similar) the `var` argument to create metric_name
  #' @param reporting_output enables nice reporting using clean_metric_names(), clean_col_names(), kbl_settings()
  #' @param kbl_settings_group_col_num integer input for kbl_settings::group_col_num
  #' @param kbl_settings_note text input for kbl_settings::kbl_settings_note
  #' 
  #' @return A data.frame or returns a kable object if kbl_settings() is used via the reporting_output = 'yes'

    suppressPackageStartupMessages(library(skimr))
    suppressPackageStartupMessages(library(glue))    

    by_string                   <- as_label(enquo(by)) 
    lhs_name                    <- x_expression(data) 
    metric_names_to_clean       <- if(by_string != 'NA') data %>% select({{ by }}) 
    metric_names_to_clean       <- c(colnames(metric_names_to_clean), 'Metric') 
    kbl_settings_note_          <- kbl_settings_note 
    kbl_settings_group_col_num_ <- { if(by_string != 'NA' && is.na(kbl_settings_group_col_num)) {
                                      2
                                    } else if(by_string == 'NA' && is.na(kbl_settings_group_col_num)) { 
                                      1 
                                    } else if( !is.na(kbl_settings_group_col_num)) {
                                      kbl_settings_group_col_num
                                    }
                                   }                              

    data %>% 
    { if ( by_string == 'NA' ) { 
        (.) %>% 
        select({{ var }}) 
       } else { 
         (.) %>% 
         select({{ by }}, {{ var }}) %>% 
         group_by({{ by }})
      }
    } %>% 
    skim() %>% 
    tibble::as_tibble() %>% 
    select(-skim_type) %>% 
    rename(Metric  = 1,
            mean   = numeric.mean,
            stddev = numeric.sd,
            min    = numeric.p0,
            Q1     = numeric.p25,
            median = numeric.p50,
            Q3     = numeric.p75,
            max    = numeric.p100,
            hist   = numeric.hist) %>% 
    { if( by_string == 'NA' ) { 
            (.) 
       } else { 
         (.) %>% 
         relocate({{ by }}, .before = Metric) 
      } 
    } %>% 
    select(-c(n_missing, complete_rate)) %>% 
    mutate(across(mean:max, ~ round(.x,2))) %>% 
    { if (reporting_output == 'yes') { # add formatting 
            (.) %>% 
            clean_metric_names(metric_names_to_clean) %>% 
            clean_col_names() %>% 
            kbl_settings(., group_col_num = kbl_settings_group_col_num_, kbl_settings_note = kbl_settings_note_)
       } else { # return new df with custom name based on function args   
          by_string  <- as_label(enquo(by)) 
          var_string <- as_label(enquo(var)) 
          name       <- glue('{lhs_name}_{by_string}_{var_string}') 
          assign(name, (.), envir = .GlobalEnv) 
          print(glue('Created a new data.frame called: 
                      {name}')) 
          (.) # return the new df 
       }
     } 
} 



cat(cyan("To get summary stats for a variable use the var = variable_name.
To format the output use reporting_output = 'yes'."))
writeLines('\n')
cat(cyan("diamonds %>%
            five_num_sum_skim(., 
                                var = depth,
                                reporting_output = 'yes')"))
diamonds %>% 
    five_num_sum_skim(., 
                        var = depth,
                        reporting_output = 'yes') 

writeLines('\n')
cat(cyan("To get summary stats for multiple variables put the variable names in a vector using c(variable_1, variable_1, etc)."))
writeLines('\n')
cat(cyan("diamonds %>%
            five_num_sum_skim(., 
                                var = c(depth, price),
                                reporting_output = 'yes')"))
diamonds %>% 
    five_num_sum_skim(., 
                        var = c(depth, price),
                        reporting_output = 'yes')         

writeLines('\n')
cat(cyan("To adjust the number of coulmns included in the 'Groups' header use kbl_settings_group_col_num. It can also be set to 0 in order to remove that label, as shown here.")) 
writeLines('\n')
cat(cyan("diamonds %>% 
        five_num_sum_skim(., 
                            var = c(depth, price),
                            kbl_settings_group_col_num = 0, 
                            reporting_output = 'yes')   "))
diamonds %>% 
    five_num_sum_skim(., 
                        var = c(depth, price),
                        kbl_settings_group_col_num = 0, 
                        reporting_output = 'yes')                                              


cat(cyan("To add a grouping variable use by = variable_name.
To add a footnote use kbl_settings_note 'add text here'."))
writeLines('\n')
cat(cyan("diamonds %>%
            five_num_sum_skim(., 
                                by = clarity, 
                                var = depth, 
                                reporting_output = 'yes', 
                                kbl_settings_note = c('The histrogram is a nice way to check the data; here we can see that Vs2 and Vs1 seem like they could be normally distributed.
                                                    Plus across the different levels of Clarity it seems like the overall spread is pretty normal.'))  "))
writeLines('\n')
diamonds %>% 
    five_num_sum_skim(., 
                        by = clarity, 
                        var = depth, 
                        reporting_output = 'yes', 
                        kbl_settings_group_col_num = 1, 
                        kbl_settings_note = c('The histrogram is a nice way to check the data; here we can see that Vs2 and Vs1 seem like they could be normally distributed.
                                              Plus across the different levels of Clarity it seems like the overall spread is pretty normal.'))    


cat(cyan("Some use cases involve outputting the summary data.frame not a reporting table.
         Settings reporting_output = 'no' returns the new data.frame (named based on input arguemnts).
         And also prints both the new data.frames name as well as the data.frame itself."))
writeLines('\n')         
cat(cyan("diamonds %>%
            five_num_sum_skim(., 
                                by = clarity, 
                                var = depth, 
                                reporting_output = 'no', 
                                output_df_name = 'diamonds')"))
writeLines('\n')
diamonds %>%
    five_num_sum_skim(., 
                        by = clarity, 
                        var = depth, 
                        reporting_output = 'no') 
