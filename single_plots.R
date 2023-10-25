

plot_mean_and_CI <- function(data, by, metric, title_string, ...) { 

    # suppressPackageStartupMessages(library(tidyverse))
    # suppressPackageStartupMessages(library(broom)) 

    gg_the_theme()    

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height)

    metric_name  <- as_label(enquo(metric))
    model_str <- glue('{metric_name}  ~ 1')

    data %>% 
        group_by( {{by}} ) %>% 
        group_modify(~  broom::tidy( lm( model_str, data = .), conf.int = TRUE)) %>%
        mutate(period = as.factor( {{by}} )) %>% # NOTE: check this! 
        rename(mean = estimate) %>% 
        ggplot() +
        aes(y = mean, x = {{by}} , color = {{by}} ) + 
        geom_point(stat = "identity") + 
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2,  position = position_dodge(.9)) +
        scale_y_continuous(limits = c(0, data %>% summarise(max_ = max( {{metric}} )) %>% pull(max_))) +  # could update this 
        the_theme + 
        scale_color_brewer(palette="Set1") + 
        labs(title    = glue('{title_string}'),
             subtitle = 'Mean with 95% Confidence Intervals') 

} 

mtcars %>%  
    mutate(cyl = as.factor(cyl)) %>% 
    plot_mean_and_CI(., cyl, wt, 'Avg Weight by Number of Cylinders', width = 10, height = 6)



plot_mean_and_CI_ss <- function(data, x_var, y_var, ...) { 

    # atlernate way to create a mean by group with CI error bars 
    # more idiomatic way to get the confidence intervals using stat_summary & mean_cl_normal
    gg_the_theme() 

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height) 

    x_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(x_var))))
    y_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(y_var))))

    data %>% 
    mutate({{x_var}} := as.factor( {{x_var}} )) %>% 
    ggplot(aes(x = factor({{x_var}}), y = {{y_var}}, color = {{x_var}})) +
    stat_summary(fun.data = "mean_cl_normal",
                 geom     = "errorbar",
                 width    = .4) + 
    stat_summary(fun = "mean", geom = "point") + 
    the_theme + 
    scale_color_brewer(palette="Set1") + 
    labs(title    = glue('Avg {y_var_str} by {x_var_str}'),
        subtitle = 'Mean with 95% Confidence Intervals',
        x = x_var_str, 
        y = y_var_str) 

}

plot_mean_and_CI_ss(mtcars, cyl, wt, width = 10, height = 6)



plot_mean_and_stddev <- function(data, by, var, ...) { 

    # add condition to see if library is already loaded 
    # suppressPackageStartupMessages(library(tidyverse))

    gg_the_theme() 

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height)     

    y_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(var))))
    x_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(by))))    

    data %>%  
    group_by({{by}}) %>% 
    summarise(mean = mean({{var}}, na.rm = TRUE),
             stddev = sd({{var}}, na.rm = TRUE)) %>%
    rename(group = 1) %>%
    mutate(group = as.factor(group)) %>% # plot needs discrete non-continuous variable 
    ggplot() + 
    aes(y = mean, x = group, color = group) + 
    geom_point(stat = "identity") + 
    geom_errorbar(aes(ymin = mean - stddev, ymax = mean + stddev), width = .2,  position = position_dodge(.9)) + 
    the_theme  + 
    scale_color_brewer(palette="Set1") + 
    labs(title    = glue('{y_var_str} Mean and Standard Deviation'),
         subtitle = glue('By {x_var_str}'),
         x        = glue('{x_var_str}'),
         y        = glue('Mean {y_var_str}'),
         color    = glue('{x_var_str}'))

}  

mtcars %>% plot_mean_and_stddev(., cyl, wt, width = 10, height = 6)  # + scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +                    




# make some time series data 
dates <- as.data.frame( seq(as.Date('2023-01-01'),as.Date('2023-01-30'),by = 1)) %>% rename('date' = 1)
groups <- as.data.frame(rep(LETTERS[1 : 3], each = 10)) %>% rename('group' = 1)
nums1 <- as.data.frame(sample.int(100, 30))
nums2 <- as.data.frame(sample.int(100, 30))
nums3 <- as.data.frame(sample.int(100, 30))
numbers <- bind_rows(nums1, nums2, nums3) %>% rename(counts = 1)
ts_data <- bind_cols(crossing(dates, groups), numbers) 


plot_ts <- function(data, ts, metric, by, ...) { 

    gg_the_theme() 

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height)        

    y_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(metric))))
    x_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(by))))   
    ts_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(ts))))   

    data %>%   
        ggplot() + 
        geom_point(aes(x = {{ts}}, y = {{metric}}, color = {{by}})) + 
        geom_line(aes(x = {{ts}}, y = {{metric}}, color = {{by}})) +      
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
        facet_wrap(vars( {{by}} ), scales = "free_x") +     # NOTE: using vars() instead of ~ to handle {{}} 
        the_theme + 
        scale_color_brewer(palette="Set1") +   
        labs(title = glue('{y_var_str} by {ts_var_str} and {x_var_str}'))  

} 

ts_data %>% 
    plot_ts(., date, counts, group, width = 16, height = 8)


plot_col_old <- function(data, by, var, y_format=NA) { 

    # add condition to see if library is already loaded 
    # suppressPackageStartupMessages(library(tidyverse))

    single_plot_settings()     

    name = deparse(substitute(var)) 
    max_ = max(data[name]) + (max(data[name]) * 0.05)

    metric_name  <- as_label(enquo(var))
    by_name <- as_label(enquo(by))    
    by_name <- stringr::str_to_title(gsub('_',' ',by_name))    

    plot <- data %>% 
            mutate('{{by}}' := as.factor({{by}})) %>%
            ggplot(., 
            aes(x = reorder({{by}}, -{{var}}), 
                y = {{var}}, 
                fill = {{by}})) + 
            geom_col() + 
            theme(strip.text.x = element_text(size = 18),
                    legend.position="none",  
                    axis.text.x = element_text(angle = 90, size = 15)) +
            the_theme 

            # percent formatting    
            { if( !is.na(y_format)) {
                plot + 
                scale_y_continuous(labels = scales::percent, limits = c(0, max_)) +
                geom_text(aes(label = paste0(round(100 * {{var}}, 1), "%")), vjust = -0.25, size = 8) 
            # count formatting 
            } else if( is.na(y_format)) {
                plot +         
                scale_y_continuous(labels = scales::comma, limits = c(0, max_)) + 
                geom_text(aes(label = paste0(format({{var}}, nsmall = 1, big.mark = ","))), vjust = -0.25, size = 8) 
            }} + 
            scale_color_brewer(palette="Set1") + 
            labs(title    = {if( !is.na(y_format)) glue('Percents by {by_name}') else glue('Counts by {by_name}')}, 
                x        = glue('{by_name}'),
                y        = glue('Count'),
                color    = glue('{by_name}')) 

} 



plot_col <- function(data, by, var, y_format=NA, ...) { 

    # add condition to see if library is already loaded 
    # suppressPackageStartupMessages(library(tidyverse))

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height)       
  
    x_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(by))))
    max_      <- var_max_value_padded(data, deparse(substitute(var)), 0.05) 

    (data %>% 
        mutate('{{by}}' := as.factor({{by}})) %>%
        ggplot(., 
        aes(x = reorder({{by}}, -{{var}}), 
            y = {{var}}, 
            fill = {{by}})) + 
        geom_col()) %>% 
        gg_theme_default() %>%
        ggplot_format_scale_y_continuous(., {{var}}, 0, max_, y_format) + 
        scale_color_brewer(palette="Set1") + 
        labs(title   = {if( !is.na(y_format)) glue('Percents by {x_var_str}') else glue('Counts by {x_var_str}')}, 
            x       = glue('{x_var_str}'),
            y       = glue('Count'),
            color   = glue('{x_var_str}')) 

} 

diamonds %>%
    group_by(cut) %>% 
    count %>% 
    plot_col(., cut, n, width = 10, height = 6)


mtcars %>% 
    group_by(cyl) %>%
    count %>% 
    ungroup() %>% 
    mutate(percent = round(n / sum(n), 2)) %>% 
    plot_col(., cyl, percent, 'yes', width = 10, height = 6) 



plot_mirror_count_and_percent <- function(data, x_var, perc_var, count_var, by_str = NA, caption_str = NA, ...) { 

     suppressPackageStartupMessages(library(tidyverse))
     suppressPackageStartupMessages(library(glue))
     suppressPackageStartupMessages(library(cowplot))
     suppressPackageStartupMessages(library(scales))

    gg_the_theme() 
    the_theme <- theme_void() + 
                 the_theme + 
                 theme(legend.position = "none") 

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width, height = plot_dims_args$height)  

    x_var_str      <- stringr::str_to_title(gsub('_',' ', deparse(substitute(x_var))))
    by_str         <- if( is.na(by_str)) 'Percent' else glue('{by_str}')    # Note: for use when plotting sub-groups; as shown in multi plot section 
    caption_str    <- if( !is.na(caption_str))  glue('{caption_str}  ') 
    perc_var_max_  <- var_max_value_20_perc_padded(data, deparse(substitute(perc_var))) 
    count_var_max_ <- var_max_value_20_perc_padded(data, deparse(substitute(count_var)))  

    p1 <- (data %>%   
            ggplot(., 
            aes(x    = reorder({{x_var}}, -{{perc_var}}), 
                y    = {{perc_var}}, 
                fill = {{perc_var}})) + 
            geom_col() + 
            the_theme +         
            scale_x_discrete(position = "top", labels = scales::label_wrap(10))) %>%  
            # gg_theme_default() %>%
            ggplot_format_scale_y_continuous(., {{perc_var}}, 0, perc_var_max_, y_format='yes') + 
            labs(title = glue('{x_var_str} {by_str}'),
                subtitle = 'Counts shown below in second plot for context on volume \n', 
                caption = caption_str)                                            

    p2 <- data %>%   
            ggplot(., 
            aes(x    = reorder({{x_var}}, -{{perc_var}}), 
                y    = {{count_var}}, 
                fill = {{count_var}})) + 
            geom_col() + 
            the_theme + 
            theme(axis.text.x=element_blank()) +  
            scale_y_reverse(labels = scales::comma, limits = c(count_var_max_, 0)) + # NOTE: REVERSE scale_y_reverse
            geom_text(aes(label = paste0(format({{count_var}}, nsmall = 1, big.mark = ","))), vjust = 1.5, size = 8) 

    cowplot::plot_grid(p1, p2,
                        align = "v", 
                        axis = "b", 
                        ncol = 1)       

} 


diamonds_cut <- 
    diamonds %>% 
    group_by(cut) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(perc = round(n/ sum(n),2)) 

plot_mirror_count_and_percent(diamonds_cut, cut, perc, n, width = 16, height = 8)  



plot_histogram <- function(data, var, bins_num=15, ...) { 

    # add condition to see if library is already loaded 
    # suppressPackageStartupMessages(library(tidyverse))

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height)  

    metric_name <- stringr::str_to_title(gsub('_',' ', deparse(substitute(var))))  

    (data %>%
    ggplot(., aes(x = {{var}}, fill = cut({{var}}, 20))) + 
    geom_histogram(bins = bins_num) + 
    scale_y_continuous(labels = scales::comma)) %>%
    # scale_fill_discrete(h = c(240, 10), c = 120, l = 70) + 
    # the_theme + 
    gg_theme_default() + 
    theme(legend.position = "none") + 
    labs(title = glue('{metric_name} Histogram'),
         x     = glue('{metric_name}'))        

} 

starwars %>%  
    rename(character_height = height) %>%
    plot_histogram(., character_height, bins_num = 25, width = 10, height = 6)




plot_decile <- function(data, i, opacity = 0.2, lower_percentile_filter = 0, upper_percentile_filter = 100) {

    #' @description 
    #' for iterative plotting
    #' this approach uses stat_summary(); another way to do this would be dplyr::group_by() & dplyr::summarise()
    #' stat_summary() seems to handle y axis limits better 
    #'
    #' @details 
    #' The use case is to compare an average value across deciles.
    #'
    #' @param data the data.frame 
    #' @param var  the metric variable    
    #' @param opacity for the jitter points
    #' @param percentile_filter for removing outliers     

    suppressPackageStartupMessages(library(glue))    

    name <- str_to_title(gsub("_", " ", i))
    name <- glue('Avg {i}')        

    data %>% 
        mutate(decile     = as.factor(ntile(.data[[i]], 10)), 
               percentile = ntile(.data[[i]], 100)) %>% 
        dplyr::filter(percentile <= {{upper_percentile_filter}},
                      percentile >= {{lower_percentile_filter}}) %>%  
        ggplot(., aes(x = decile, y = .data[[i]])) + 
        geom_point(position = position_jitter(width = .2), 
                   alpha    = {{opacity}}, 
                   color    = "black",
                   size     = 0.5) + # 1.5 is default
        stat_summary(fun   = mean, 
                    na.rm = TRUE, 
                    geom   = "point", 
                    color  = "dodgerblue", 
                    size   = 4, 
                    shape  = "diamond") +
        stat_summary(fun.data = mean_sdl, 
                    na.rm     = TRUE, 
                    geom      = "errorbar", 
                    width     = .3,
                    size      = 1.5,
                    color     = "dodgerblue",
                    fun.args  = list(mult = 1)) + # set to 1 standard deviation, the default is 2 
        stat_summary(aes(label = round(..y.., 1)), 
                     fun       = mean, 
                     geom      = "text", 
                     size      = 7,
                     vjust     = -4,
                     color     = "dodgerblue") + 
        theme(legend.position = "none") + 
        labs(title   = name,
            subtitle = 'By Decile',
            caption  = glue('Deciles are 10 equally sized segments ranked by the distribution of a metric,
                             in this case {name}. The error bars are 1 standard deviation.'),                
            x        = 'Decile',
            y        = name) + 
        theme(title         = element_text(size = 20),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))     

} 

plot_decile(diamonds_tweaked, i = 'price')   




plot_density <- function(data, i, by) { 

    suppressPackageStartupMessages(library(glue))

    data %>%  
        ggplot(aes(x = .data[[i]], fill = .data[[by]])) +   

        geom_density(alpha = 0.3) + 
        theme(title           = element_text(size = 20),
              axis.text.x     = element_text(size = 20),
              axis.text.y     = element_text(size = 20),   
              legend.text     = element_text(size = 20),
              legend.position = "bottom") + 
        labs(title = glue('{i} Density Plot by {by}')) 

} 

plot_density(diamonds_tweaked, 'price', 'cut') 




plot_ecdf_histogram <- function(data, i, shapiro_test_sample_size = 1000, lower_percentile_filter = 0, upper_percentile_filter = 100) {

    #' @description 
    #' for iterative plotting 
    #'
    #' @details 
    #' The use case is to check the distribution 
    #'
    #' @param data the data.frame 
    #' @param var  the metric variable    
    #' @param lower_percentile_filter for removing low outliers 
    #' @param upper_percentile_filter for removing high outliers       

     suppressPackageStartupMessages(library(glue))
     suppressPackageStartupMessages(library(cowplot))
     suppressPackageStartupMessages(library(scales))
     suppressPackageStartupMessages(library(broom))
     suppressPackageStartupMessages(library(gridExtra))

    options(scipen           = 999,
            repr.plot.width  = 24,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)

    the_theme <- theme(title           = element_text(size = 20),
                       axis.text.x     = element_text(size = 20),
                       axis.text.y     = element_text(size = 20),   
                       legend.position = "none")    

    d         <- sample(data[i] %>% pull(.), shapiro_test_sample_size)
    norm_test <- shapiro.test(d) %>% glance(.)

    data <- data %>% 
            dplyr::mutate(percentile = ntile( .data[[i]], 100)) %>% 
            dplyr::filter(percentile <= {{upper_percentile_filter}},
                          percentile >= {{lower_percentile_filter}})     

    p1 <- data %>% 
            ggplot(aes(x = .data[[i]] )) + 
            stat_ecdf(geom = "point",  color = "darkgreen") + 
            scale_y_continuous(labels = scales::percent) + 
            the_theme + 
            labs(title = glue('Cumulative Distribution of {i}'),
                    y  = glue('Cumulative Sum of {i}')) 
                  
    bin_width <- data[i] %>% 
                  rename(x = 1) %>% 
                  summarise(bin_width = 2 * IQR( x ) / (length( x )^(1/3))) %>%
                  pull(.)

    p2 <- data %>%
            ggplot() + 
            aes(x = .data[[i]] ) +        
            geom_histogram(#aes(y = ..density..),
                            colour = 1, 
                            fill = "aquamarine4",
                             binwidth = bin_width, 
                            ) +      
            the_theme + 
            labs(title = glue('Histogram of {i}'),
                 x     = glue('{i}'),
                 y     = 'Counts') 

    p3 <- data %>% 
            ggplot() + 
            aes(sample = .data[[i]]) +      
            stat_qq(distribution = stats::qnorm, color="darkgreen") +
            stat_qq_line(distribution = stats::qnorm, color="black") + 
            the_theme + 
            labs(title = glue('QQ Plot of {i}'))  

    p4 <- data.frame(x = 1:10, y = 1:10) %>%
            ggplot(., aes(x, y)) +
             geom_blank() +
             theme_void() + 
             annotation_custom(tableGrob(norm_test), xmin = 6, xmax = 5)

    base_plots <- cowplot::plot_grid(p1, p2, p3, 
                                    align = "h", 
                                    axis = "b", 
                                    nrow = 1)     
    
    cowplot::plot_grid(base_plots, p4,
                        align = "v", 
                        axis = "b", 
                        nrow = 2,
                        rel_heights = c(6,1),
                        rel_widths = c(1,3))                        

}

plot_ecdf_histogram(diamonds_tweaked, 'price')




