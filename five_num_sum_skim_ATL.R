

# NOTE: added some new pipe_out conditions and broke things a bit, need to reconcile these and do some testing


 five_num_sum_skim <- function(data, var, by = NA, reporting_output = NA, pipe_output = NA, kbl_settings_group_col_num = NA, kbl_settings_note = NA) {

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
    mad_skim                    <- skim_with(numeric = sfl(mad = mad),
                                             base    = sfl(length = length))                                                         

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
    mad_skim() %>% 
    tibble::as_tibble() %>% 
    select(-skim_type) %>% 
    rename(Metric  = 1,
            count  = length, 
            mean   = numeric.mean,
            stddev = numeric.sd,
            min    = numeric.p0,
            Q1     = numeric.p25,
            median = numeric.p50,
            Q3     = numeric.p75,
            max    = numeric.p100,
            hist   = numeric.hist,
            mad   = numeric.mad) %>% 
    relocate(mad, .before = hist) %>% 
    { if( by_string == 'NA' ) { 
            (.) 
       } else { 
         (.) %>% 
         relocate({{ by }}, .before = Metric) 
      } 
    } %>% 
    # select(-c(n_missing, complete_rate)) %>% 
    mutate(across(mean:mad, ~ round(.x,2))) %>% 
    { if (reporting_output == 'yes') { # add formatting 
            (.) %>% 
            clean_metric_names(metric_names_to_clean) %>% 
            clean_col_names() %>% 
            kbl_settings(., group_col_num = kbl_settings_group_col_num_, kbl_settings_note = kbl_settings_note_)
       } else if(pipe_output == 'yes') { 
          (.) # return the new df 
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




  five_num_sum_skim <- function(data, var, by = NA, reporting_output = NA, pipe_output = NA, kbl_settings_group_col_num = NA, kbl_settings_note = NA) {

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
    # reporting_output_           <- if( !is.na(reporting_output) ) reporting_output                           
    mad_skim                    <- skim_with(numeric = sfl(mad = mad),
                                             base    = sfl(length = length))                                                         

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
    mad_skim() %>% 
    tibble::as_tibble() %>% 
    select(-skim_type) %>% 
    rename(Metric  = 1,
            count  = length, 
            mean   = numeric.mean,
            stddev = numeric.sd,
            min    = numeric.p0,
            Q1     = numeric.p25,
            median = numeric.p50,
            Q3     = numeric.p75,
            max    = numeric.p100,
            hist   = numeric.hist,
            mad   = numeric.mad) %>% 
    relocate(mad, .before = hist) %>% 
    { if( by_string == 'NA' ) { 
            (.) 
       } else { 
         (.) %>% 
         relocate({{ by }}, .before = Metric) 
      } 
    } %>% 
    # select(-c(n_missing, complete_rate)) %>% 
    mutate(across(mean:mad, ~ round(.x,2))) %>% 

    # print(reporting_output_)
    { if( !is.na(pipe_output) & pipe_output == 'Yes') {  
          (.) # return the new df 

       } else if( !is.na(reporting_output) & reporting_output == 'Yes' ) {  # add formatting 
            (.) %>% 
            clean_metric_names(metric_names_to_clean) %>% 
            clean_col_names() %>% 
            kbl_settings(., group_col_num = kbl_settings_group_col_num_, kbl_settings_note = kbl_settings_note_)

        } else if( !is.na(reporting_output) & reporting_output == "No") { # return new df with custom name based on function args   
                by_string  <- as_label(enquo(by)) 
                var_string <- as_label(enquo(var)) 
                name       <- glue('{lhs_name}_{by_string}_{var_string}') 
                assign(name, (.), envir = .GlobalEnv) 
                print(glue('Created a new data.frame called: 
                            {name}')) 
                (.) # return the new df 
        } else {  
        (.) 
        }
    } 
} #  kbl_settings_group_col_num = NA, kbl_settings_note = NA) {


