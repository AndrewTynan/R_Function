
sem <- function(x, na.rm = FALSE) {
    
    sd(x, na.rm = na.rm) / sqrt(length(x)) # standard error of mean 

}

# example
mtcars %>%
    group_by(cyl) %>% 
    summarise(sem = sem(wt)) 


conf_int_low <- function(x, conf_level = 0.95, na.rm = FALSE) { 
    
    t.test(x, conf.level = conf_level)$conf.int[1] 

} 

conf_int_high <- function(x, conf_level = 0.95, na.rm = FALSE) { 
    
    t.test(x, conf.level = conf_level)$conf.int[2] 

} 


# examples 
mtcars %>%
    group_by(cyl) %>% 
    summarise(ci_low  = conf_int_low(wt),
              ci_high = conf_int_high(wt)) 

mtcars %>% 
    group_by(cyl) %>% 
    summarise(ci_low  = conf_int_low(wt, 0.8),
              ci_high = conf_int_high(wt, 0.8))               


mean_w_ci_sem <- function(data, var, by=NA, decimals=2, conf_level=0.95) {

  # NOTE: depends on conf_int_low() & conf_int_high() 
  library(scales) 
  conf_level_perc <- scales::label_percent()(conf_level)
  by_string       <- as_label(enquo(by))  

  data %>% 
    {if(by_string != 'NA') { 
        (.) %>% 
        group_by({{ by }}) 
     } else { 
        (.) 
     }} %>% 
    summarise("avg_{{ var }}"     := round(mean({{ var }}, na.rm = TRUE),decimals),
              "stddev_{{ var }}"  := round(sd({{ var }}, na.rm = TRUE),decimals),
              "sem_{{ var }}"     := round(sem({{ var }}),decimals),
              "{{conf_level_perc}}_ci_low_{{ var }}"  := round(conf_int_low({{ var }}, conf_level, na.rm = TRUE),decimals), 
              "{{conf_level_perc}}_ci_high_{{ var }}" := round(conf_int_high({{ var }}, conf_level, na.rm = TRUE),decimals)
              ) %>% 
    rename_with(., ~ gsub('["]+', "",  .x)) # remove quotes from ci col names
              
} 

# examples 
mtcars %>% mean_w_ci_sem(., wt, conf_level=0.95) 

mtcars %>% mean_w_ci_sem(., wt, cyl, conf_level=0.95) 



overall_CI <- function(data, var) {      

    lm_fit <- lm(data[, c(var)] ~ 1, mtcars) 
    lm_ci  <- confint(lm_fit, level = 0.95) 
    output <- as.data.frame(lm_ci) %>%
              mutate(period = var) %>%
              relocate(period)
    rownames(output) <- NULL
    output

    # data %>% 
    # group_by( {{ period }} ) %>%
    # mutate(#"{{ period }}" := {{ period }},
    #         "confint_95_perc_{{ var }}" := lm(  year  ~ 1, .  )) # %>% dplyr::filter(year == {{ period }}

}

#example 
mtcars %>% segment_CI(., 'wt') 



mean_by <- function(data, by, var) {
  #' Mean of variable by group(s) 
  #' 
  #' @description 
  #' `mean_by` returns the mean of the `var` arguemnt for all variables present in the `by` argument for the data.frame in the `data` argument  
  #'
  #' @details 
  #' using across() enables multiple grouping variables to be provided 
  #'
  #' @param data the data.frame 
  #' @param by the grouping variable 
  #' @param var the variable the mean is calculated for 
  #'       
  #' @return a data.frame 
  data %>%
    group_by(across({{ by }})) %>% 
    summarise(avg = mean({{ var }}, na.rm = TRUE))
}

mtcars %>% mean_by(., cyl, wt)



median_by_named <- function(data, by, var, decimals=2) {
  data %>% 
    group_by({{ by }}) %>% 
    summarise("median_{{ var }}" := round(median({{ var }}, na.rm = TRUE),2))
}

mtcars %>% median_by_named(., cyl, wt) 



mean_boot <- function(data, var) { 

    library(boot)

    # define a mean function to bootstrap
    meanfun <- function(data, i){
    d <- data[i, ]
    return(mean(d))   
    }    

    data %>% 
        select({{var}}) %>%
        boot(., statistic = meanfun, R = 1000) %$% 
        print(.) %>%
        boot.ci(., conf = 0.95, type = "bca")

} 

mtcars %>% mean_boot(., wt)


cumsum_group_by <- function(data, by, var, cumsum_descending = 'no', var_2 = NA) {

  #' Cumulative Sum of a metric by another metric, with the 
  #' 
  #' @description 
  #' cumulative sums a one metric variable based on the ordering of the by varible. 
  #'
  #' @param data the data.frame 
  #' @param by the grouping variable 
  #' @param var the variable the cumulative sum is calculated for 
  #' @param var_name a string that determine the order of the cumularive sum, default is ascending 
  #' @param var_2 another metric variable that  
  #' 
  #' @return An html summary table

    suppressPackageStartupMessages(library(glue))
    suppressPackageStartupMessages(library(tidyr))  

    name_var_2 <- deparse(substitute(var_2))

    if(name_var_2 != 'NA') {
    df_var_2 <- data %>% 
            group_by( {{ by }} ) %>%    
            summarise("Avg_{{ var_2 }}"    := round(mean( {{var_2}} ),2), 
                      "Median_{{ var_2 }}" := round(median( {{var_2}} ),2), 
                      "Max_{{ var_2 }}"    := round(max(  {{var_2}} ),2),
                      "Min_{{ var_2 }}"    := round(min(  {{var_2}} ),2))   
    }

    df1 <- data %>% 
           group_by( {{ by }} ) %>%                                                                            
           summarise("{{ var }}_count" := n(), .groups = 'drop') 

    df1 %>%
    bind_cols(., df1 %>% summarise("{{ var }}_total" := sum(.[[2]]))) %>%
    mutate("{{ var }}_percent" := .[[2]]  / .[[3]]) %>%  
    select(-3) %>%  
    {if(cumsum_descending == 'yes') { 
       (.) %>% 
       arrange(desc( {{ by }} )) %>%
       mutate("{{ var }}_cumulative_percent_descending" := scales::percent( cumsum( .[[3]] )))

     } else if( cumsum_descending == 'no' | is.na(cumsum_descending)) {
       (.) %>%             
       arrange( ({{ by }} )) %>%
       mutate("{{ var }}_cumulative_percent" := scales::percent(cumsum( .[[3]] )))
     }
    } %>%          
    arrange({{ by }}) %>%    
    mutate("{{ var }}_percent" := scales::percent( .[[3]] )) %>% 
    {if(name_var_2 != 'NA') { 
     suppressMessages(left_join(., df_var_2))
     } else {
      (.) 
     }
    } 
} 


# tweak mtcars to make an example df 
data(mtcars)
rownames(mtcars) <- make.names(mtcars[,1], unique = TRUE)
rownames(mtcars) <- NULL
mtcars <- mtcars %>% 
          mutate(qsec = round(qsec)) %>% 
          rename(car_model = 1,
                 quarter_mile_time = qsec)



cat(red("NOTE: Quarter Mile Time is just an example, it does not really make sense to sum. Just needed an example.
A more realistic use case is summing an aggregate measure like 'days active' and counting employees not car models.")) 
writeLines('\n')
cat(cyan("mtcars %>% 
cumsum_group_by(., quarter_mile_time, car_model, 'no') %>%
clean_col_names() %>%
kbl_settings(., 
              group_col_num = 1, 
              desc_stats_label_flag = 'yes')"))
writeLines('\n')
mtcars %>%
cumsum_group_by(., quarter_mile_time, car_model, 'no')  %>%
clean_col_names() %>%
kbl_settings(., 
              group_col_num = 1, 
              desc_stats_label_flag = 'yes',      
              kbl_settings_note = "quarter_mile_time is the car model's fastest time to travel 1/4 mile from standstill (in seconds).")        

writeLines('\n')
cat(cyan("Change the cumsum_descending argument to 'yes' to get descedning"))
cat(cyan("mtcars %>% 
cumsum_group_by(., quarter_mile_time, car_model, 'yes') %>%
clean_col_names() %>%
kbl_settings(., 
              group_col_num = 1, 
              desc_stats_label_flag = 'yes')"))
writeLines('\n')

mtcars %>% 
    cumsum_group_by(., quarter_mile_time, car_model, 'yes') %>%
    clean_col_names() %>%
    kbl_settings(., 
                group_col_num = 1, 
                desc_stats_label_flag = 'yes',      
                kbl_settings_note = "quarter_mile_time is the car model's fastest time to travel 1/4 mile from standstill (in seconds).")        

writeLines('\n')
cat(cyan("Use var_2 to add some summary stats for a metric."))
cat(cyan("mtcars %>%  
    cumsum_group_by(., quarter_mile_time, car_model, 'no', hp)  %>%
    clean_col_names() %>%
    kbl_settings(., 
                group_col_num = 1, 
                desc_stats_label_flag = 'no',      
                kbl_settings_note = 'quarter_mile_time is the car model's fastest time to travel 1/4 mile from standstill (in seconds).'"))
mtcars %>%  
    cumsum_group_by(., quarter_mile_time, car_model, 'no', hp)  %>%
    clean_col_names() %>%
    kbl_settings(., 
                group_col_num = 1, 
                desc_stats_label_flag = 'no',      
                kbl_settings_note = "quarter_mile_time is the car model's fastest time to travel 1/4 mile from standstill (in seconds).")                                                  




var_total <- function(data, var, num) {

    # var is the metric of interst
    # num is the count which is used as a filter on var
    data %>% 
    summarise("total_{{ var }}"                  := scales::comma(sum(    {{ var }} )),
              "More_than_{{num}}_{{ var }}"      := scales::comma(length( {{ var }} [ {{ var }} > {{ num }} ])), # might need to tweake this, might want to add _count to the end, but the namning could become odd
              "More_than_{{num}}_{{ var }}_sum"  := scales::comma(sum(    {{ var }} [ {{ var }} > {{ num }} ])),
              "More_than_{{num}}_{{ var }}_perc" := scales::percent(sum( {{ var }} [ {{ var }} > {{ num }} ]) / sum( {{ var }} ), 0.1))
}

diamonds %>% 
    var_total(., price, 15000) %>%
    head()
    # print_col_vals(., price)


var_perc <- function(data, count_var, sum_var, num=NA, name_var) {

    data %>% 
    mutate(total   = n_distinct( {{ count_var }} ),
           total_2 = sum( {{ sum_var }} )) %>% 
    filter( {{sum_var}} > {{ num }} ) %>% 
    summarise("{{ name_var }}"            := scales::comma(n_distinct( {{ count_var }} )),
              "{{ sum_var }}_count"       := scales::comma(sum( {{ sum_var }} )),
              "perc_of_{{ count_var }}"   := scales::percent(n_distinct( {{ count_var }} ) / max(total), 0.1),
              "perc_of_{{ sum_var }}"     := scales::percent( sum( {{ sum_var }} ) / max(total_2), 0.1))
}     

diamonds %>% 
    var_perc(., clarity, price, 15000, name_var='clarity') %>%
    head()


lag_ratio <- function(data, by, var, wrap = NA) { 

    # Note: does not wrap the last value in the var column, so there is no ratio for the first row. 
    # Note: lag depends on ungrouped data
    
    last_val_ <- data %>% ungroup() %>% summarise(last_val = last({{var}})) %>% pull() 

    data %>% 
    ungroup() %>% # the dplyr::lag() function requires the df to be ungrouped
    {if(is.na(wrap)) {
        (.) %>%
         mutate("{{ var }}_lag_ratio" :=  round( {{ var }} / lag( {{ var }}, n = 1, order_by = {{ by }}), 2)) 

    } else if( !is.na(wrap) && wrap == 'yes') { 
        (.) %>% # Note: this uses the last value in the var column as the prvious value to get the delta for the first value in the var column.
        mutate(last_val = last_val_,
               lag_ratio  = round( {{ var }} / lag( {{ var }}, n = 1, order_by = {{ by }}), 2),
               lag_ratio2 = round( {{ var }} / last_val, 2)) %>%
        mutate(lag_ratio = coalesce(lag_ratio, lag_ratio2)) %>% 
        dplyr::select(-c(last_val, lag_ratio2)) %>% 
        rename("{{ var }}_lag_ratio" := lag_ratio) 
     }
   } 
} 

lag_delta <- function(data, by, var, wrap = NA) { 

    last_val_ <- data %>% ungroup() %>% summarise(last_val = last({{var}})) %>% pull()

    data %>%
    ungroup() %>% # the dplyr::lag() function requires the df to be ungrouped 
    {if( is.na(wrap)) { 
        (.) %>% mutate("{{ var }}_lag_delta" :=  round( ( {{ var }} - lag( {{ var }},  n = 1, order_by = {{ by }}) ) / lag( {{ var }},  n = 1, order_by = {{ by }}) , 2)) 

    } else if( !is.na(wrap) & wrap == 'yes') { 
        (.) %>%  # Note: this uses the last value in the var column as the prvious value to get the delta for the first value in the var column.
        mutate(last_val = last_val_,
               lag_delta  =  round( ( {{ var }} - lag( {{ var }},  n = 1, order_by = {{ by }}) ) / lag( {{ var }},  n = 1, order_by = {{ by }}) , 2), 
               lag_delta2 =  round( abs(({{ var }} - last_val)) / last_val,2)) %>%
        mutate(lag_delta = coalesce(lag_delta, lag_delta2)) %>% 
        dplyr::select(-c(last_val, lag_delta2)) %>% 
        rename("{{ var }}_lag_delta" := lag_delta) 
     } 
   } 
} 


mtcarscyl_counts <- 
    mtcars %>% 
    group_by(cyl) %>% 
    count() %>%
    rename(counts = 2) 


mtcarscyl_counts %>% 
    lag_ratio(.,cyl, counts)    


mtcarscyl_counts %>% 
    lag_ratio(.,cyl, counts,  'yes')  # wrapping pulls the last value to make the delta for the first value     


mtcarscyl_counts %>% 
    lag_delta(.,cyl, counts) 


 mtcarscyl_counts %>% 
    lag_delta(.,cyl, counts,  'yes') 


group_deltas <- function(data, by, var, factor_levels, title, decimals=2) {

#     if(is.na(factor_levels)) print('Please provide values for the factor_levels!')

    d <- data %>% # NOTE: should do more checks to make sure differnet inputs work 
            group_by({{ by }}) %>% 
            summarise(mean := mean({{ var }}, na.rm = TRUE)) %>% 
            rename(period = {{by}}) 

    tidyr::crossing(d, d %>% rename('one' = 1, 'two' = 2))  %>% 
    rename('mean1' = 2, 'mean2' = 4) %>% 
    unite(., periods, c(period, one), sep='-') %>% 
    tidyr::separate(., col = periods, into = c('period1', 'period2'), sep = "-", remove = FALSE) %>%
    mutate(period1      = factor(period1, levels = factor_levels, ordered = TRUE),
           period2      = factor(period2, levels = factor_levels, ordered = TRUE), 
           period_check = if_else(period1 > period2, 'yes', 'no')) %>% 
    ungroup() %>% 
    filter(period_check == 'yes') %>%
    dplyr::select(c(periods,mean1,mean2)) %>% 
    mutate(difference = .[[2]] - .[[3]],
           delta      = (.[[2]] - .[[3]]) / .[[3]],
           log_diff   = log( .[[2]]) - log(.[[3]]) ) %>% 
    mutate(across(c(2:6), ~ round(.x,decimals))) %>% 
    clean_col_names() %>%                           # could add an if here using a new arg to choose reporting or not 
    kbl_settings(., 
                    group_col_num = 1, 
                    group_custom_header = 'Group Pairs',
                    custom_header = glue('{title}: Mean Diff. & Deltas'),
                    kbl_settings_note = glue('The factor level ordering determines which deltas are culculated (since delta is not a symmetrical metric).
                                              The higher levels are used as the Xt and the lower levels as the Xt-1.
                                              Log Diff is provided because it is a symmetrical metric, so it is provided as a complement to the delta.')) 

} 



mtcars %>% 
    group_by(cyl) %>% 
    count() %>% 
    rename(counts = 2) %>% 
    # mutate(cyl = factor(cyl, levels = c(4,6,8))) %>% 
    group_deltas(., cyl, counts, factor_levels = c(4,6,8), title = 'mtcars counts by cycle', decimals=3) 



t_test_many_metrics <- function(df, metric_cols, grouping_var) {

    #' @description 
    #' multiple t-tests 
    #'
    #' @details 
    #' it depends on clean_metric_names(), clean_col_names(), kableExtra::kbl(), kableExtra::kable_classic() and kableExtra::add_header_above 
    #'
    #' @param df the data.frame 
    #' @param metric_cols a character vector with names of continuous varables (which need to be part of the df data.frame)
    #' @param grouping_var a two level grouping variable used as the two test groups 
    #' 
    #' @return An html table 

     suppressPackageStartupMessages(library(glue))
     suppressPackageStartupMessages(library(broom))
    
    options(scipen=999) # to get more read-able p.value

    grouping_var_vector <- df %>% pull( {{grouping_var}} )   

    first_group <- grouping_var_vector[1]
    second_group <- unique(grouping_var_vector)
    second_group <- second_group[ !second_group == first_group ]

    df <- df %>% select(all_of( {{metric_cols}} )) 

    df <- df %>% 
            select_if(is.numeric) %>%   
            map_df(~ broom::tidy(t.test(. ~ grouping_var_vector)), .id = 'var') %>%
            mutate(p.value                  = round(p.value, 5), 
                    statistical_signifiance = if_else(p.value <= 0.05, 'yes', 'no')) %>% 
            relocate(statistical_signifiance, .after = p.value) %>%       
            relocate(statistic, .after = statistical_signifiance) %>%    
            mutate(across(c(estimate1, estimate2, statistic, conf.low, conf.high), ~ round(.x,2))) %>% 
            rename(Metric                                       = var,
                    "{{ grouping_var }}_{{first_group}}_Mean"  := estimate1, 
                    "{{ grouping_var }}_{{second_group}}_Mean" := estimate2, 
                    t.value                                     = statistic,
                    Degrees_of_Freedom                          = parameter) %>%
            select(-estimate) %>%
            arrange(p.value) %>%
            clean_col_names() %>% 
            clean_metric_names() %>%
            kableExtra::kbl() %>% 
            kableExtra::kable_classic() %>% 
            kableExtra::add_header_above(c(" " = 1, "Statistics" = 10)) 

    IRdisplay::display_html(as.character(df)) 

}  


# prepre data for t_test_many_metrics
diamonds_cut_ideal_or_premium <- diamonds %>% 
                                    filter(cut %in% c('Ideal', 'Premium')) %>%
                                    mutate(cut = as.character(cut))

metrics_for_t_test <- c('depth', 'table', 'price', 'x', 'y', 'z')


cat(cyan("t_test_many_metrics(diamonds_cut_ideal_or_premium,
                                metrics_for_t_test, 
                                cut) "))
t_test_many_metrics(diamonds_cut_ideal_or_premium,
                    metrics_for_t_test, 
                    cut) 



