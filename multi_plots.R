

### NOTE many of the single plot functions change changed from things like density_plotter() to plot_density()
### so, now need to update multiple_plots() to check that it can hendle them.
### and, need to change the examples to use the new function names!  

# prepare data set 
diamonds_tweaked <- diamonds %>%  
                        dplyr::filter(clarity %in% c('VVS1', 'SI2')) %>% 
                        mutate(cut = as.character(cut)) 



multiple_plots <- function(data, plot_fun, metric, by_group = NA, ...) { 

    #' @description 
    #' for iterative plotting
    #'
    #' @param plot_fun takes either decile_plotter(), density_plotter(), or ecdf_histogram_plotter() 
    #' @param metric   the metric variable to be plotted 
    #' @param by_group the grouping variable 

    suppressPackageStartupMessages(library(glue))
    options(scipen           = 999,
            repr.plot.width  = 24,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)

    dot_values <- list(...) 
    dot_names <- names(dot_values)

    if( length(dot_values) >= 1) {
        
        if(names(dot_values)[1] == 'lower_percentile_filter') lower_percentile_filter_ <- dot_values$lower_percentile_filter
        if(names(dot_values)[1] == 'upper_percentile_filter') {
                upper_percentile_filter_ <- dot_values$upper_percentile_filter
        } else {
                upper_percentile_filter_ <- dot_values$upper_percentile_filter  
        }
      } 

    by_group_string  <- as_label(enquo(by_group))        

    for(i in metric) { 

        if(by_group_string == 'NA' && length(dot_values) < 1) { 
            p <- plot_fun(data, i) 

        } else if(by_group_string != 'NA' && length(dot_values) < 1){
            p <- plot_fun(data, i, by_group) 

        } else if(by_group_string == 'NA' && length(dot_values) == 2 && exists("lower_percentile_filter_") && exists("upper_percentile_filter_") && !is.null(lower_percentile_filter_) && !is.null(upper_percentile_filter_)) { 
          p <- plot_fun(data, i, lower_percentile_filter = lower_percentile_filter_, upper_percentile_filter = upper_percentile_filter_) 

        } else if(by_group_string == 'NA' && length(dot_values) == 1 && !exists("lower_percentile_filter_") && exists("upper_percentile_filter_") && !is.null(upper_percentile_filter_)) {       
          p <- plot_fun(data, i, upper_percentile_filter = upper_percentile_filter_) 

        } else if(by_group_string == 'NA' && length(dot_values) == 1 && exists("lower_percentile_filter_") && !is.null(lower_percentile_filter_)) { #  && !exists("upper_percentile_filter_") &&          
          p <- plot_fun(data, i, lower_percentile_filter = lower_percentile_filter_) 

        } 
        print(p) 
    } 
} 


cat(cyan("This example makes 3 tyeps of plots for 2 metrics
Just update the 3rd argument forthe 'metric to include any more plots for those metrics.
Add however many variables yoou'd like to plot to the 'metric' arguemnt. The example uses two: c('price', 'depth')"))
writeLines('\n')

cat(cyan("multiple_plots(diamonds_tweaked, density_plotter, c('price', 'depth'), 'cut') 
multiple_plots(diamonds_tweaked, decile_plotter, c('price', 'depth'))
multiple_plots(diamonds_tweaked, ecdf_histogram_plotter, c('price', 'depth'))"))
writeLines('\n')

multiple_plots(diamonds_tweaked, density_plotter, c('price', 'depth'), 'cut') 
multiple_plots(diamonds_tweaked, decile_plotter, c('price', 'depth'))
multiple_plots(diamonds_tweaked, ecdf_histogram_plotter, c('price', 'depth'))


cat(cyan("based on the ecdf plot above, it might be helpful to explore 'price' after trimming it long tails. 
         To filter the top and / or bottom percentiles of the distribution use lower_percentile_filter and upper_percentile_filter"))
cat(cyan("multiple_plots(diamonds_tweaked, ecdf_histogram_plotter, 'depth', lower_percentile_filter = 2, upper_percentile_filter = 99)"))         
writeLines('\n')

multiple_plots(diamonds_tweaked, ecdf_histogram_plotter, 'depth', lower_percentile_filter = 2, upper_percentile_filter = 99)


cat(cyan("The 'y' metric has one very high outlier in the top decile."))
cat(cyan("multiple_plots(diamonds_tweaked, decile_plotter, 'y')"))      
multiple_plots(diamonds_tweaked, decile_plotter, 'y')

cat(cyan("decile_plotter() also offer the same percentile filter options.
Adding the percentile_filter = 99 removes this and provides a better sense of the distribution by decile."))
cat(cyan("multiple_plots(diamonds_tweaked, decile_plotter, 'y',  upper_percentile_filter = 98)"))   
writeLines('\n')

multiple_plots(diamonds_tweaked, decile_plotter, 'y',  upper_percentile_filter = 98)


cat(cyan("Just change the 4th functon argument (eg by_group) to explore the metric using another grouping variable."))
cat(cyan("multiple_plots(diamonds_tweaked, density_plotter, c('price', 'depth'), 'color')"))
writeLines('\n')

multiple_plots(diamonds_tweaked, density_plotter, c('price', 'depth'), 'color') 


diamonds_cut_by_cut_color <- 
    diamonds %>%   
    group_by(cut, color) %>% 
    count() %>%
    ungroup() %>%  
    group_by(cut) %>% 
    mutate(sub_total = sum(n)) %>% 
    ungroup() %>% 
    mutate(color_percent  = round(n / sub_total, 3)) %>% 
    arrange(cut, color, desc(n)) 

# diamond_cut <- diamonds_cut_by_cut_color %>% print_col_vals(cut) 

for (i in diamond_cut) { 

    p <- mirror_plotter_count_and_percent(diamonds_cut_by_cut_color %>% filter(cut == i),
                                        color, 
                                        color_percent, 
                                        n, 
                                        glue('for {i} Cut'), 
                                        caption_str = 'Note: dataset is unfiltered.'
                                        ) 
    print(p) 
    writeLines('\n') 
} 

