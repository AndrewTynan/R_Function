

library(tidyverse)
library(tidymodels)
library(cowplot)
library(broom)
library(glue)
# library(rlang)

library(kableExtra)
library(janitor)

********************
********************
Example Comparison 

    create some an exmaple dataset with a grouping variable
    get the bootstrap mean for each group and compare
********************
********************

# make an example dataset with a binary Pre/Post variable, using the built in mtcars dataset 
new_mtcars <- mtcars %>% 
                dplyr::filter(cyl != 8) %>%
                mutate(period = if_else(am == 1, 'Pre','Post'))                

 new_mtcars %>% 
    dplyr::group_by(am, period) %>% 
    count() 

mtcars %>% 
    dplyr::group_by(cyl) %>% 
    count() 


# run bootstraps 
suppressMessages({  # remove warnings about: `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

bootr(new_mtcars)

}) 



# functions to run in the next cell 

meanfun <- function(split, var){

    # mean with std.err for bootstrapping (and getting 95% Confidnece Interval) 
    dat <- analysis(split) %>% pull(var)

    return(tibble(term = "mean",
                  estimate = mean(dat),
                  std.err  = sd(dat) / sqrt(length(dat)),
                  std.dev  = sd(dat)
                  ))

}


the_theme <- theme(title            = element_text(size = 20),
                    axis.text.x     = element_text(size = 20),
                    axis.text.y     = element_text(size = 20), 
                    legend.text     = element_text(size = 16), 
                    legend.position = "right")   


bootplot <- function(data, int_pctl_data, period_string) {

    # make histogram plot
    histplot <<- data %>%
                 select(-1) %>% 
                 unnest(mean) %>% 
                 ggplot(., aes(estimate)) +
                 geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))) +
                 the_theme + 
                 geom_vline(aes(xintercept = .lower),    data = int_pctl_data, col = "blue") +
                 geom_vline(aes(xintercept = .estimate), data = int_pctl_data, col = "green") +    
                 geom_vline(aes(xintercept = .upper),    data = int_pctl_data, col = "blue") + 
                 geom_text(aes(x = .lower,    label = round(.lower, 2), y = 150),    data = int_pctl_data, colour = "blue", angle = 0, size = 8) +                 
                 geom_text(aes(x = .estimate, label = round(.estimate, 2), y = 150), data = int_pctl_data, colour = "green", angle = 0, size = 8) +
                 geom_text(aes(x = .upper,    label = round(.upper, 2), y = 150),    data = int_pctl_data, colour = "blue", angle = 0, size = 8) +       
                 labs(title    = glue('Histogram & Density Plot for {period_string}'),
                      subtitle = 'With Lower & Upper 95% Confidence Intervals',
                       x        = 'Bootstrapped Mean',
                       y        = 'Count',
                       fill     = 'Period')             

} 


bootplot_both_periods <- function(data) {

    # make histogram plot for both segments 
    histplot2 <<- data %>%
                  ggplot(., aes(estimate, fill = period)) +
                  geom_histogram(alpha = 0.7, position="identity") +
                  the_theme +                   
                  labs(title    = 'Histogram & Density Plot for Pre & Post Periods',
                       subtitle = 'Lower & Upper 95% Confidence Intervals (blue) and Mean (green)',
                       x        = 'Bootstrapped Mean',
                       y        = 'Count',
                       fill     = 'Period') 

} 


bootstraps_impl <- function(data, period_string, var_string) { 

    # get bootstrap, int_pctl, and histplot; also assign them to global env to be used by other functions 
    # depends on the data.frame having a column named period, with values of 'Pre' & 'Post'
    boots <- bootstraps(data %>% filter(period == period_string), times = 2000, apparent = TRUE) %>%  
                            mutate(mean   = map(splits, meanfun, var_string), 
                                   period = period_string) %>%
                            relocate(period, .after = id)
    name       <- glue('{period_string}_boots')
    assign(name, boots, envir = .GlobalEnv)   

    int_pctl_ <- int_pctl(boots, mean) %>% bind_cols(., tibble(period = period_string))   
    name      <- glue('{period_string}_int_pctl')
    assign(name, int_pctl_, envir = .GlobalEnv)       

    histplot <- bootplot(boots, int_pctl_, period_string) 
    name     <- glue('{period_string}_plot')    
    assign(name, histplot, envir = .GlobalEnv)   

} 


# need to check if this is differt, had it commented out somewhere random, hmm..
bootstraps_impl_2 <- function(data, period_string, var_string) { 

    boots <- bootstraps(data %>% filter(period == period_string), times = 2000, apparent = TRUE) %>%  
                            mutate(mean   = map(splits, meanfun, var_string), 
                                   period = period_string) %>%
                            relocate(period, .after = id)
    name       <- glue('{period_string}_boots')
    assign(name, boots, envir = .GlobalEnv)   

    int_pctl_ <- int_pctl(boots, mean) %>% bind_cols(., tibble(period = period_string))   
    name      <- glue('{period_string}_int_pctl')
    assign(name, int_pctl_, envir = .GlobalEnv)       

    histplot <- bootplot(boots, Post_int_pctl, period_string) 
    name     <- glue('{period_string}_plot')    
    assign(name, histplot, envir = .GlobalEnv)   

} 


bootr <- function(data, var_string) {

    # run bootstraps, do t.test, make plots and tables 

    for(period in c('Pre', 'Post')) {

        bootstraps_impl(data, period, var_string)

    }

    #combined dataset 
    whole_df <- bind_rows(Pre_boots %>%
                          select(-1) %>% 
                          unnest(mean),
                          Post_boots %>%
                          select(-1) %>% 
                          unnest(mean))                    

    options(scipen           = 999,
            repr.plot.width  = 14,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)

    # outputs  
    print(bootplot_both_periods(whole_df))

    print(t.test(estimate ~ period, var.equal = TRUE, data = whole_df) %>% 
            tidy(.) %>% 
            select(-c(estimate, parameter, method, alternative)) %>%
            rename(Pre_Mean  = estimate1, 
                   Post_Mean = estimate2,
                   t.value   = statistic))

    print(cowplot::plot_grid(Pre_plot, Post_plot,
                                     align = "h", 
                                     axis = "b", 
                                     nrow = 1)) 

    bind_rows(Pre_int_pctl, Post_int_pctl)  %>% relocate(period, .before = term) %>% select(-.method) # add std.err to output                            

}


suppressMessages({  # remove warnings about: `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
bootr(new_mtcars, 'qsec')
}) 



*******************
*******************
THis section makes a more general bootstrap and does not do t.tests 
*******************
*******************


# made another version 
one_off_bootr <- function(data, group_col, metric_col) {

    options(scipen           = 999,
            repr.plot.width  = 14,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)    

    group_vars <- data %>% dplyr::select({{ group_col }}) %>% pull() %>% unique()
    # group_vars  <- sort(group_vars)

    suppressWarnings(rm(boot_output_ci, boot_output_plots ))
    boot_output_ci    <- as.list(1:length(group_vars)) 
    boot_output_plots <- as.list(1:length(group_vars))     
    n <- 0

    for(i in group_vars) {
        n <- n + 1 
        print(n)

        data_ <- data %>% dplyr::filter( {{group_col}} == i)

        boots <- bootstraps(data_, times = 2000, apparent = TRUE) %>%  
                     mutate(mean   = map(splits, meanfun, metric_col), 
                            period = i) %>%
                   relocate(period, .after = id)
        
        # int_pctl_ <- int_pctl(boots, mean) %>% bind_cols(., tibble(period = i))   

        boot_output_ci[[n]]    <- as.data.frame(int_pctl(boots, mean) %>% bind_cols(., tibble(period = i))) 

        boot_output_plots[[n]] <- bootplot(boots, 
                                            int_pctl(boots, mean) %>% bind_cols(., tibble(period = i)), 
                                            i)

    } 
    
    bootoutput <<- bind_rows(boot_output_ci)
    boot_output_plots <<- boot_output_plots
    print(bootoutput) 
    print(boot_output_plots)

    # print(cowplot::plot_grid(boot_plot,
    #                         align = "h", 
    #                         axis = "b", 
    #                         nrow = 1)) 

    # boot_int_pctl  %>% relocate(period, .before = term) %>% select(-.method) # add std.err to output                            

} 


# suppressMessages({  # remove warnings about: `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
one_off_bootr(mtcars2, carb, 'qsec')
# }) 



bootoutput %>%
    ggplot() + 
    aes(x = period, y = .estimate, color = period) + 
    geom_point() + 
    geom_text(aes(label = round(.estimate,1)), vjust = -3, size = 6) +
    geom_errorbar(aes(ymin = .lower, ymax = .upper), 
                 width=.2,
                 position=position_dodge(.9)) + 
    scale_y_continuous(limits = c(0, 50))




# print(mtcars %>% pull(carb) %>% unique())
mtcars2 <- mtcars %>%
            mutate(carb = if_else(carb %in% c(3,6,8), 3, carb)) 
            
mtcars2 <- bind_rows(mtcars2, mtcars2, mtcars2)

mtcars2 %>%
    group_by(carb) %>%
    count()




*******************
*******************
Random ideas about adding other tests..
*******************
*******************

mtcars2_boots <- bootstraps(mtcars2 %>% mutate(cyl = factor(cyl)), times = 2000, apparent = TRUE) %>%  
                    mutate(period = 3,
                        period = factor(period)) %>%        
                    relocate(period)   


# first_resample <- mtcars2_boots$splits[[1]]
# first_resample# $data 

# print(mtcars2_boots)

#     mutate(aov = map(data, ~aov(value ~ content * process, data = .x)))

 suppressWarnings({ 

# test <- 
mtcars2_boots %>% 
        dplyr::select(splits) %>% 
        mutate(mean = map(splits, meanfun,  'qsec'),
                model = map(splits, ~aov(qsec ~ cyl, data = .x)),
                model_summary = map(model, broom::tidy),
                tukeyHSD = map(model, TukeyHSD, conf.level=.95), 
                 tukeyHSD = map(tukeyHSD, tidy)) %>%
        dplyr::select(model_summary) %>%
        unnest(., model_summary)  %>%
        head 
#         mutate(cyl = factor(cyl)) %>%
        # group_by(carb) %>%   
        # nest() %>% 
        #         # model = map(data, ~aov(qsec ~ cyl, .)),                  
        #         # model_summary = map(model, broom::tidy), 
        #         # tukeyHSD = map(model, TukeyHSD, conf.level=.95),
        #         # tukeyHSD = map(tukeyHSD, tidy) 
                # ) %>% head(2) # dim %>% print
        #   select(Class, tidy) %>% 
        #   unnest(tidy)

# test
# for(i in test) { 

# print(test)
# print(test %>% unnest(cols = model_summary))
# print(test %>% unnest(cols = tukeyHSD))
# test %>% 
# unnest(cols = tukeyHSD) %>% 
# ggplot() + 
# aes(x = contrast, y = estimate) + 
# geom_point() + 
# geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
#                 width=.2,
#                 position=position_dodge(.9)) + 
# facet_wrap(~ carb, nrow = 1)

# }
 }) 




*******************
*******************
The more polished version. 
*******************
*******************



# made another version 
one_off_bootr2 <- function(data, group_col, metric_col) {

    options(scipen           = 999,
            repr.plot.width  = 14,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)    

    group_vars    <- data %>% dplyr::select({{ group_col }}) %>% pull() %>% unique()
    group_col_str <- deparse(substitute(group_col)) # make string 

    suppressWarnings(rm(boot_output_ci, boot_output_plots))
    boot_output <- as.list(1:length(group_vars)) 
    boot_raw    <- as.list(1:length(group_vars)) 
    n <- 0

    for(i in group_vars) {
        n <- n + 1 

        data_ <- data %>% dplyr::filter( {{group_col}} == i)

        boots <- bootstraps(data_, times = 2000, apparent = TRUE) %>%  
                    mutate(period = i,
                           period = factor(period)) %>%        
                    relocate(period)         

        boots_sum <- boots %>% 
                     mutate(mean = map(splits, meanfun,  metric_col)) %>% 
                     select(id, mean) %>%
                     unnest(., mean)  %>%
                        #  summarise(avg     = mean(estimate),
                        #            std.dev = sd(estimate)) %>% 
                        five_num_sum_skim(., 
                                            var = estimate, 
                                            reporting_output = 'no',
                                            pipe_output = 'yes') %>%        
                        mutate(period = i,
                               period = factor(period)) %>%        
                        relocate(period) 
        # print(head(boots_sum))

        boot_raw[[n]]    <- boots 
        boot_output[[n]] <- boots_sum 

    } 

    bootraw          <- bind_rows(boot_raw) 
    bootoutput       <- bind_rows(boot_output)
    boot_output_plot <- bootoutput %>%
                            ggplot() + 
                                aes(x = period, y = mean, color = period) + 
                                geom_point() + 
                                geom_text(aes(label = round(mean, 1)), vjust = -3, size = 6) +
                                geom_errorbar(aes(ymin = mean - stddev, ymax = mean + stddev), 
                                            width=.2,
                                            position=position_dodge(.9)) + 
                                labs(title = glue('Average {metric_col} by {group_col_str}')) 

    bootoutput <- bootoutput %>%
                    kbl_settings(., 
                                group_col_num = 1,
                                group_custom_header = 'Segment',
                                custom_header = 'Bootstrap Metrics')                                
    print(bootoutput) 
    print(boot_output_plot)          

    # boots_sum <- bootraw %>% 
    #              mutate(mean = map(splits, meanfun,  metric_col)) %>% 

    # print(head(bootraw,2))
    # anova <- aov(period ~ qsec, bootraw)
    # summary(anova)                    

} 

suppressWarnings({ 
one_off_bootr2(mtcars2, carb, 'qsec')
}) 




*******************
*******************
Random early stuff 
*******************
*******************



mtcars2 %>%
group_by(carb) %>%
summarise(mean_qsec = mean(qsec))



# lm(hp ~ factor(cyl), mtcars) %>% tidy()

boots <- bootstraps(mtcars2 %>% mutate(cyl = factor(cyl)), times = 2000, apparent = TRUE)
# boots

# fit_ls_on_bootstrap <- function(split) {
#     ls(mpg ~ wt,  analysis(split)) #, start = list(mpg = 1, bwt= 0))
# }

boot_models <-
  boots %>% 
    mutate(
        # model   = map(splits, fit_ls_on_bootstrap, .x),
        model     = map(splits, ~ lm(qsec ~ cyl, data = .)), 
        coef_info = map(model, tidy)
  )

# boot_models

boot_coefs <- 
  boot_models %>% 
  unnest(coef_info)

percentile_intervals <- int_pctl(boot_models, coef_info)
percentile_intervals  

ggplot(boot_coefs, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = "free") +
  geom_vline(aes(xintercept = .lower), data = percentile_intervals, col = "blue") +
  geom_vline(aes(xintercept = .upper), data = percentile_intervals, col = "blue")


# fit_nls_on_bootstrap <- function(split) {
#     nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
# }

# boot_models <-
#   boots %>% 
#   mutate(model = map(splits, fit_nls_on_bootstrap),
#          coef_info = map(model, tidy))

# boot_models
