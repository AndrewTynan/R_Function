
<span style="color: lightblue;">
</span> 


clean_col_names <- function(df) {
    df %>%
       rename_with(., ~ gsub("_", " ",  .x)) %>%
       rename_with(., ~ str_to_title(.x)) 
}  

str_to_title_gsub <- function(var) stringr::str_to_title(gsub('_',' ', deparse(substitute(var))))    


var_max_value_padded <- function(data, var, padding=0.2) max(data[var]) + (max(data[var]) * padding)  


ggplot_format_scale_y_continuous <- function(ggplot_obj, var, y_mmin=0, y_max, y_format=NA) { 
 
    { if( !is.na(y_format)) { # percent formatting 
        ggplot_obj <- ggplot_obj + 
                      scale_y_continuous(labels = scales::percent, limits = c(y_mmin, y_max)) +
                      geom_text(aes(label = paste0(round(100 * {{var}}, 1), "%")), vjust = -0.25, size = 8) 
    
    } else if( is.na(y_format)) { # count formatting  
        ggplot_obj <- ggplot_obj + 
                      scale_y_continuous(labels = scales::comma, limits = c(y_mmin, y_max)) + 
                      geom_text(aes(label = paste0(format({{var}}, nsmall = 1, big.mark = ","))), vjust = -0.25, size = 8) 
    }
   } 
    return(ggplot_obj)
} 



kbl_settings <- function(df, group_col_num) {

  #' @param group_col_num the number of grouping columns before the metrics (usually 1 or 2), which is used to set the 'Groups' header location 

    # the last column number, which is used to set the 'Metrics' header location
    metrics_col_num <- dim(df)[2] - group_col_num

    new_df <- df %>%      
                kbl() %>%  
                kable_classic() %>%
             { 
                if (group_col_num == 0) {
                (.) %>% add_header_above(c("Metrics" = metrics_col_num)) 

                } else {
                (.) %>% add_header_above(c("Groups" = group_col_num, "Metrics" = metrics_col_num)) 
                }
             } 

    # display_html does not accept piped data
    IRdisplay::display_html(as.character(new_df))

} 


kbl_settings <- function(df, group_col_num, desc_stats_label_flag = 'no', kbl_settings_note = 'no') {

    #' @param group_col_num the number of grouping columns before the metrics (usually 0, 1, 2), which is used to set the 'Groups' header location 

    # subtract the number of grouping columns from the columns in the df, this is used to set the 'Metrics' header locations
    metrics_col_num <- dim(df)[2] - group_col_num

    new_df <- df %>%      
                kableExtra::kbl() %>%  
                kableExtra::kable_classic() %>%
             { 
                if (group_col_num == 0) {
                    (.) %>% add_header_above(c("Metrics" = metrics_col_num)) 

                } else if(desc_stats_label_flag == 'yes') {
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Descriptive Statistics" = metrics_col_num)) 

                } else { 
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Metric Statistics" = metrics_col_num)) 

                }
             } %>%
            { 
               if (kbl_settings_note != 'no') { 
                     (.) %>% footnote(general = kbl_settings_note) 

                      } else { 
                     (.) 
                } 
            }      

    # display_html does not accept piped data
    IRdisplay::display_html(as.character(new_df))

} 


kbl_settings <- function(df, group_col_num, group_custom_header = NA, desc_stats_label_flag = NA, custom_header = NA, kbl_settings_note = NA) {

     # QUESTION does it make sense to have group_col_num possibly be NA ? 

    #' @param group_col_num the number of grouping columns before the metrics (usually 0, 1, 2), which is used to set the 'Groups' header location 

    metrics_col_num <- dim(df)[2] - group_col_num # subtract the number of grouping columns from the columns in the df, this is used to set the 'Metrics' header locations
    custom_header_        <- setNames(metrics_col_num, custom_header) # create names character vecrtor 
    group_custom_header_  <- setNames(group_col_num, group_custom_header) # create names character vecrtor 

    new_df <- df %>%  

              # insert group_custom_header into df 
             {    
               if( !is.na(group_custom_header)) {
                  (.) %>% 
               #    rename( {{ group_custom_header }} := 1) 
                   rename( ' ' := 1) 
                 } 
             } %>%

               # add kable
                kableExtra::kbl() %>%  
                kableExtra::kable_classic() %>%
                
               # now add custom headers for metrics and / or columns 
             { 
                if (group_col_num == 0) {
                    (.) %>% add_header_above(c("Metrics" = metrics_col_num)) 

                } else if( !is.na(desc_stats_label_flag)) {
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Descriptive Statistics" = metrics_col_num)) 

                } else if( !is.na(group_custom_header) & !is.na(custom_header)) {
                     (.) %>% add_header_above(c(group_custom_header_, custom_header_))                      

                } else if( !is.na(group_custom_header) & is.na(custom_header)) {
                     (.) %>% add_header_above(c(" " = group_col_num, "Metric Statistics" = metrics_col_num))  # already inserted group_custom_header using rename so that it appears 



                } else if( !is.na(custom_header) ) {
                     (.) %>% add_header_above(c("Groups" = group_col_num, custom_header_)) 

                } else { 
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Metric Statistics" = metrics_col_num)) 

                }
             } %>%

             # add footnote 
             { 
               if ( !is.na(kbl_settings_note) ) {
                     (.) %>% footnote(general = kbl_settings_note) 

                      } else { 
                     (.) 
              } 
            }      

    # display_html does not accept piped data
    IRdisplay::display_html(as.character(new_df)) 

} 


five_num_sum_skim <- function(data, by, var, reporting_output = 'no', kbl_settings_colnum = 1, kbl_settings_note = 'no') {

    kbl_settings_note_ <- kbl_settings_note

    metric_names_to_clean <- data %>% select({{ by }}) 
    metric_names_to_clean <- c(colnames(metric_names_to_clean), 'Metric')

    data %>% 
    select({{ by }}, {{ var }}) %>% 
    group_by({{ by }}) %>% 
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
    relocate({{ by }}, .before = Metric) %>% 
    select(-c(n_missing, complete_rate)) %>% 
    mutate(across(mean:max, ~ round(.x,2))) %>% 
    clean_metric_names(metric_names_to_clean) %>% 
    clean_col_names() %>% 
    { 
       if (reporting_output == 'yes') { # add formatting 
            (.) %>% 
            clean_col_names() %>% 
            kbl_settings(., kbl_settings_colnum, kbl_settings_note = kbl_settings_note_)

       } else { # return exisitng df 
           (.)
       } 
    } 
}  


kbl_settings <- function(df, group_col_num, desc_stats_label_flag = 'no', kbl_settings_note = 'no') {

    #' @param group_col_num the number of grouping columns before the metrics (usually 0, 1, 2), which is used to set the 'Groups' header location 

    # subtract the number of grouping columns from the columns in the df, this is used to set the 'Metrics' header locations
    metrics_col_num <- dim(df)[2] - group_col_num

    new_df <- df %>%      
                kableExtra::kbl() %>%  
                kableExtra::kable_classic() %>%
             { 
                if (group_col_num == 0) {
                  (.) %>% add_header_above(c("Metrics" = metrics_col_num)) 

                } else if(desc_stats_label_flag == 'yes') {
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Descriptive Statistics" = metrics_col_num)) 

                } else { 
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Metric Statistics" = metrics_col_num)) 

                }
             } %>%
             { 
               if (kbl_settings_note != 'no') { 
                     (.) %>% footnote(general = kbl_settings_note) 

                      } else { 
                     (.) 
              } 
            }      

    # display_html does not accept piped data
    IRdisplay::display_html(as.character(new_df))

}     


kbl_settings <- function(df, group_col_num, group_custom_header = NA, desc_stats_label_flag = NA, custom_header = NA, kbl_settings_note = NA) {

    # NOTE group_custom_header is really only a yes / noe flag now.. need to chcek 

     # QUESTION does it make sense to have group_col_num possibly be NA ? 

    #' @param group_col_num the number of grouping columns before the metrics (usually 0, 1, 2), which is used to set the 'Groups' header location 

    metrics_col_num <- dim(df)[2] - group_col_num # subtract the number of grouping columns from the columns in the df, this is used to set the 'Metrics' header locations

    custom_header_        <- setNames(metrics_col_num, custom_header) # create names character vecrtor 
    group_custom_header_  <- setNames(group_col_num, group_custom_header) # create names character vecrtor 

    new_df <- df %>%  

              # insert group_custom_header into df 
             {    
               if( !is.na(group_custom_header)) {
                  (.) %>% 
               #    rename( {{ group_custom_header }} := 1) 
                   rename( ' ' := 1) 
                } else { 
                (.) 
                } 

             } %>%

               # add kable
                kableExtra::kbl() %>%  
                kableExtra::kable_classic() %>%
                
               # now add custom headers for metrics and / or columns 
             { 
                if (group_col_num == 0) {
                    (.) %>% add_header_above(c("Metrics" = metrics_col_num)) 

                } else if( !is.na(desc_stats_label_flag)) {
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Descriptive Statistics" = metrics_col_num)) 

                } else if( !is.na(group_custom_header) & !is.na(custom_header)) {
                     (.) %>% add_header_above(c(group_custom_header_, custom_header_))                      

                } else if( !is.na(group_custom_header) & is.na(custom_header)) {
                     (.) %>% add_header_above(c(" " = group_col_num, "Metric Statistics" = metrics_col_num))  # already inserted group_custom_header using rename so that it appears 

                } else if( !is.na(custom_header) ) {
                     (.) %>% add_header_above(c("Groups" = group_col_num, custom_header_)) 

                } else { 
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Metric Statistics" = metrics_col_num)) 

                }
             } %>%

             # add footnote 
             { 
               if ( !is.na(kbl_settings_note) ) {
                     (.) %>% footnote(general = kbl_settings_note) 

                      } else { 
                     (.) 
              } 
            }      

    # display_html does not accept piped data
    IRdisplay::display_html(as.character(new_df)) 

} 


coL_plotter <- function(data, by, var, y_format=NA) { 

    # add condition to see if library is already loaded 
    # suppressPackageStartupMessages(library(tidyverse))

    single_plot_settings()        

    name = deparse(substitute(var)) 
    max_ = max(data[name]) + (max(data[name]) * 0.05)

    metric_name  <- as_label(enquo(var))
    by_name <- as_label(enquo(by))    

    data %>% 
    mutate('{{by}}' := as.factor({{by}})) %>%
    ggplot(., 
    aes(x = reorder({{by}}, -{{var}}), 
        y = {{var}}, 
        fill = {{by}})) + 
    geom_col() + 
    theme(strip.text.x = element_text(size = 18),
            legend.position="none",  
            axis.text.x = element_text(angle = 90, size = 15)) +
    the_theme + 
    # percent formatting    
    { if( !is.na(y_format)) scale_y_continuous(labels = scales::percent, limits = c(0, max_)) } +
    { if( !is.na(y_format)) geom_text(aes(label = paste0(round(100 * {{var}}, 1), "%")), vjust = -0.25, size = 8) } +
    # count formatting 
    { if( is.na(y_format))  scale_y_continuous(labels = scales::comma, limits = c(0, max_)) } + 
    { if( is.na(y_format))  geom_text(aes(label = paste0(format({{var}}, nsmall = 1, big.mark = ","))), vjust = -0.25, size = 8) } + # geom_text(aes(label = {{var}}), vjust = -0.25, size = 8) 
    scale_color_brewer(palette="Set1") + 
    labs(title    = {if( !is.na(y_format)) glue('Percents by {by_name}') else glue('Counts by {by_name}')}, 
         x        = glue('{by_name}'),
         y        = glue('Count'),
         color    = glue('{by_name}'))

} 


coL_plotter_v2  <- function(data, var) {

        name = deparse(substitute(var))                 # make string 
        name = stringr::str_to_title(gsub('_',' ',name)) # turn undescores to spaces and capitialize first letter of word   

        p <- data %>%
            ggplot() + 
            aes(x = cluster, y = {{var}}, fill = cluster) + 
            geom_col() + 
            geom_text(aes(label = employee_id), vjust = 0, size = 8) + 
            scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) +
            labs(title    = glue('Avg {name}'),
                 # subtitle = 'Count of Employees by Cluster',
                 x        = 'cluster',
                 y        = glue('Avg {name}')) + 
            theme(title =  element_text(size = 16),
                    axis.text.x = element_text(size = 16),
                    axis.text.y = element_text(size = 16))  

        print(p)
} 


plot_dims <- function(width=7, height=7) { 

    # input_list <- list(...) 
    if(!is.null(input_list$width) && !is.null(input_list$height)) { 
        options(repr.plot.width = input_list$width, 
                repr.plot.height = input_list$height) 

    } else if(!is.null(input_list$width)) { 
        options(repr.plot.width = input_list$width, 
                 repr.plot.height  = 7)  

    } else if(!is.null(input_list$height)) { 
        options(repr.plot.width  = 7, 
                repr.plot.height = input_list$height)  

    } else if( is.null(input_list$width) && is.null(input_list$height)) { 
        options(repr.plot.width  = 7, 
                repr.plot.height = 7)  
    }
} 


plot_col <- function(data, by, var, y_format=NA, ...) { 

    # add condition to see if library is already loaded 
    # suppressPackageStartupMessages(library(tidyverse))

    single_plot_settings()     

    plot_dims_args <- list(...) 
    plot_dims(width = plot_dims_args$width , height = plot_dims_args$height)       

    name <- deparse(substitute(var)) 
    max_ <- max(data[name]) + (max(data[name]) * 0.05)

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
            the_theme  
            
    ggplot_format_scale_y_continuous(plot, {{var}}, max_, y_format) + 
    scale_color_brewer(palette="Set1") + 
    labs(title    = {if( !is.na(y_format)) glue('Percents by {by_name}') else glue('Counts by {by_name}')}, 
        x        = glue('{by_name}'),
        y        = glue('Count'),
        color    = glue('{by_name}')) 

} 

