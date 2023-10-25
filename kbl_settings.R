

kbl_settings <- function(df, group_col_num, group_custom_header = NA, desc_stats_label_flag = NA, custom_header = NA, kbl_settings_note = NA) {

    #' @description 
    #' Formats a table for cleaner reporting. 
    #' Cleans column names and provides 'Groups' and 'Metric Statistics' headers that are customizable.
    #' It is a wrapper around kableExtra::kbl(), kableExtra::kable_classic(), and IRdisplay::display_html(as.character()) 
    #' these are the recommended way to report tabular data using R at Meta.
    #'
    #' @details 
    #' using across() enables multiple grouping variables to be provided 
    #' NOTE: need to refine the _outlier_threshold metrics a bit 
    #'
    #' @param df is the data.frame 
    #' @param group_col_num the number of grouping columns before the metrics (usually 0, 1, 2), which is used to set the 'Groups' header location 
    #' @param group_custom_header pass a string to be used as a custom column header for the grouping variable
    #' @param desc_stats_label_flag if this is not null, then "Descriptive Statistics" is used as the header for the metric columns 
    #' @param custom_header pass a string to be used as a custom metrics header, requires the argument 'desc_stats_label_flag' to be NA
    #' @param kbl_settings_note pass a string to be used as a footnote 

    # QUESTION does it make sense to have group_col_num possibly be NA ? 

    metrics_col_num       <- dim(df)[2] - group_col_num                   # subtract the number of grouping columns from the columns in the df, this is used to set the 'Metrics' header locations
    custom_header_        <- setNames(metrics_col_num, custom_header)     # create names character vecrtor 
    group_custom_header_  <- setNames(group_col_num, group_custom_header) # create names character vecrtor 

    new_df <- df %>%  
             { if( !is.na(group_custom_header) ) {  
                  (.) %>% 
                   rename( ' ' := 1)  # remove the actual column header since group_custom_header_ is used to add the label in kable() below
                 } else {
                     (.)
                 }
             } %>%
                kableExtra::kbl() %>%  
                kableExtra::kable_classic() %>%
             { if (group_col_num == 0) {
                    (.) %>% add_header_above(c("Metrics" = metrics_col_num)) 

                } else if( !is.na(desc_stats_label_flag)) {
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Descriptive Statistics" = metrics_col_num)) 

                } else if( !is.na(group_custom_header) & !is.na(custom_header)) {
                     (.) %>% add_header_above(c(group_custom_header_, custom_header_))                      

                } else if( !is.na(group_custom_header) & is.na(custom_header)) {
                     (.) %>% add_header_above(c(group_custom_header_, "Metric Statistics" = metrics_col_num))  # already inserted group_custom_header using rename so that it appears 

                } else if( !is.na(custom_header) ) {
                     (.) %>% add_header_above(c("Groups" = group_col_num, custom_header_)) 

                } else { 
                     (.) %>% add_header_above(c("Groups" = group_col_num, "Metric Statistics" = metrics_col_num)) 
                }
             } %>%
             { if ( !is.na(kbl_settings_note) ) {
                     (.) %>% footnote(general = kbl_settings_note) 
                      } else { 
                     (.) 
              } 
            } 

    # display_html does not accept piped data
    IRdisplay::display_html(as.character(new_df)) 

} 


# examples 
# make an example df
diamonds_agg <- diamonds %>% 
                group_by(cut) %>%
                summarise(color_count   = n_distinct(color),
                          clarity_count = n_distinct(clarity),
                          avg_depth     = mean(depth),
                          stddev_depth  = sd(depth))



cat(cyan("Use kbl_settings() to add kable() formatting and add_header_above with default settings."))   
writeLines('\n')
cat(cyan("diamonds_agg %>%
            kbl_settings(., 
                        group_col_num = 1)"))
diamonds_agg %>%
    kbl_settings(., 
                 group_col_num = 1)

# writeLines('\n')
# cat(cyan("Add a custom header."))
# cat(cyan("diamonds_agg %>%
#             kbl_settings(., 
#                         group_col_num = 1,
#                         group_custom_header = 'Your Label Here :)')"))
# diamonds_agg %>%
#     kbl_settings(., 
#                  group_col_num = 1,
#                  group_custom_header = 'Your Label Here :)')
        
writeLines('\n')
cat(cyan("Add a custom header and a custom metrics header; these can be used seperately too."))
writeLines('\n')
cat(cyan("diamonds_agg %>%
        kbl_settings(., 
                    group_col_num = 1,
                    group_custom_header = 'Custom Label Here',
                    custom_header = 'And / or here too')   "))
diamonds_agg %>%
    kbl_settings(., 
                 group_col_num = 1,
                 group_custom_header = 'Custom Label Here',
                 custom_header = 'And / or here too')     

writeLines('\n')
cat(cyan("Add a footnote below the table."))
writeLines('\n')
cat(cyan("diamonds_agg %>%
            kbl_settings(., 
                        group_col_num = 1,
                        group_custom_header = 'Your Label Here :)',
                        kbl_settings_note = 'Add some info about the table.')  "))

diamonds_agg %>%
    kbl_settings(., 
                 group_col_num = 1,
                 group_custom_header = 'Your Label Here :)',
                 kbl_settings_note = 'Add some info about the table.')      
