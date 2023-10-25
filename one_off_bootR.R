

library(tidyverse) 
library(kableExtra) 
library(janitor) 
library(lubridate) 
library(broom) 
library(stringr) 
library(tidyr)
# library(emmeans)
# library(lme4)



library(rstatix)


the_theme <- theme(title            = element_text(size = 20),
                    axis.text.x     = element_text(size = 20),
                    axis.text.y     = element_text(size = 20), 
                    legend.text     = element_text(size = 16), 
                    legend.position = "right")   

options(scipen           = 999,
        repr.plot.width  = 14,   # in inches (default = 7)
        repr.plot.height = 10)   # in inches (default = 7)        




# bootstrap mean and plot 

meanfun <- function(split, var){

    # mean with std.err for bootstrapping (and getting 95% Confidnece Interval) 
    dat <- analysis(split) %>% pull(var)

    return(tibble(term = "mean",
                  estimate = mean(dat),
                  std.err  = sd(dat) / sqrt(length(dat))))

}


one_off_bootr2 <- function(data, group_col, metric_col, group_col_levels, title_string, percent_scale_flag = NA,  metric_col_sym, center_scaled = NA, manually_scaled = NA) {
    
    library(tidymodels)
    options(scipen           = 999,
            repr.plot.width  = 14,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)    

    the_theme <- theme(title            = element_text(size = 20),
                        axis.text.x     = element_text(size = 20),
                        axis.text.y     = element_text(size = 20), 
                        legend.text     = element_text(size = 16), 
                        legend.position = "right")               

    group_vars    <- data %>% dplyr::select({{ group_col }}) %>% pull() %>% unique()
    group_col_str <- deparse(substitute(group_col)) # make string 

    min_ <- data %>% summarise(min_ = min( {{metric_col_sym}} , na.rm = TRUE)) %>% pull(min_) 
    max_ <- data %>% summarise(max_ = max( {{metric_col_sym}} , na.rm = TRUE)) %>% pull(max_)
    mean_ <- data %>% summarise(mean_ = mean( {{metric_col_sym}} , na.rm = TRUE)) %>% pull(mean_)
    sd_ <- data %>% summarise(sd_ = sd( {{metric_col_sym}} , na.rm = TRUE)) %>% pull(sd_) 
    range_ <- mean_ + (3 * sd_)
    # print(min_)
    # print(max_)         

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
                        five_num_sum_skim(., 
                                            var = estimate, 
                                            reporting_output = 'no',
                                            pipe_output = 'yes') %>%        
                        mutate(period = i,
                               period = factor(period)) %>%        
                        relocate(period) %>%
                        mutate(period = factor(period, levels = group_col_levels, ordered = TRUE)) %>%
                        dplyr::select(-c(median, mad))

        boot_raw[[n]]    <- boots 
        boot_output[[n]] <- boots_sum 

    } 

    bootraw          <- bind_rows(boot_raw) 
    bootoutput       <- bind_rows(boot_output)
    boot_output_plot <- bootoutput %>%
                            ggplot() + 
                                aes(x = period, y = mean, color = period) + 
                                geom_point() + 
                                geom_text(aes(label = round(mean, 2)), vjust = -3, size = 6) +
                                scale_y_continuous(limits = c(min_, max_)) + 
                                { if( !is.na(percent_scale_flag) & percent_scale_flag ) scale_y_continuous(labels = scales::percent, limits = c(0, 1)) } +
                                { if( !is.na(center_scaled) & center_scaled ) scale_y_continuous(limits = c(mean_ - sd_, mean_ + sd_ )) } +
                                { if( !is.na(manually_scaled) ) scale_y_continuous(limits = c(manually_scaled[1], manually_scaled[2] )) } +
                                the_theme + 
                                scale_color_brewer(palette="Set1") + 
                                geom_errorbar(aes(ymin = mean - stddev, ymax = mean + stddev), 
                                            width=.2,
                                            position=position_dodge(.9)) + 
                                labs(title = glue('Average {title_string} by {group_col_str}'),
                                    subtitle = 'Bootstrap Standard Deviation Error Bars') 

    bootoutput <- bootoutput %>%
                    dplyr::select(-Metric) %>%
                    arrange(period) %>%
                    # lag_delta(., period, mean) %>%
                    # mutate(delta = coalesce(delta, delta2)) %>%
                    # dplyr::select(-delta2) %>%
                    kbl_settings(., 
                                group_col_num = 1,
                                group_custom_header = 'Segment',
                                custom_header = glue('{title_string} Bootstrap Metrics'))                                
    print(bootoutput) 
    print(boot_output_plot)          

    # boots_sum <- bootraw %>% 
    #              mutate(mean = map(splits, meanfun,  metric_col)) %>% 

    # print(head(bootraw,2))
    # anova <- aov(period ~ qsec, bootraw)
    # summary(anova)                    

} 


fte_pp_percent <- fte_pp_percent %>%
                    mutate(ds   = as.Date(ds),
                           year = lubridate::year(ds)) %>% 
                    relocate(year, .after = ds) 

fte_pp_percent <- fte_pp_percent %>%  
                    mutate(period = factor(period, levels = c('Pre Period 2022', 'Pre Period 2023', 'Post Period 2023'), ordered = TRUE))  


# spot check for normal distribution of fte_pp_percent, it does not look normal 
fte_pp_percent %>% 
ggplot(aes(x = period, y = fte_pp_percent)) +
geom_point(position = position_jitter(width = .2), 
                alpha    = 0.8, 
                color    = "black",
                size     = 0.5)


group_delta(fte_pp_percent, period, fte_pp_percent, 'Perc. of FTE Visiting People Portal') 



fte_pp_percent %>% 
    five_num_sum_skim(., 
                        by = period, 
                        var = fte_pp_percent, 
                        pipe_output = "Yes", 
                        reporting_output = 'No' 
                        ) %>% 
    lag_delta(., period, mean)  



suppressWarnings({ 
one_off_bootr2(data              = fte_pp_percent, 
                group_col        = period, 
                metric_col       = 'fte_pp_percent', 
                group_col_levels = c('Pre Period 2022', 'Pre Period 2023', 'Post Period 2023'), 
                title_string     = 'Percent of FTE Visiting People Portal',
                percent_scale_flag = TRUE,
                metric_col_sym = fte_pp_percent 
                ) 
}) 


fte_pp_percent_period_fte_pp_percent %>% 
    mean_and_stddev_plotter(., mean, stddev, 'Daily Percent of FTE Visiting People Portal') + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) 



fte_pp_percent %>%  
    mean_and_CI_plotter(., fte_pp_percent, period, 'Percent of FTE Visiting People Portal') 



        










