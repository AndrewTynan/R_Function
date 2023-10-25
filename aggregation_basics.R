

group_percents <- function(data, by, decimals=2) {

    suppressPackageStartupMessages(library(scales)) 
    
    data %>% 
        group_by({{by}}) %>% 
        count() %>% 
        ungroup() %>%        
        mutate(percent   = round(n / sum(n), decimals),
               percent_f = scales::percent(percent))

} 


#example 
mtcars %>% group_percents(., cyl)



tabyl_one_var <- function(data, var) {

    library(janitor)

    data %>%
        tabyl({{var}}) %>%
        adorn_totals("row") %>%
        adorn_pct_formatting()

} 

#example 
mtcars %>% tabyl_one_var(cyl)


tabyl_two_vars <- function(data, var1, var2, with_percents_by = NA, perc_decimals = 1) {
 
    # Note: with_percents_by can be either 'row', 'col', or 'all' 

    data %>%
    janitor::tabyl({{var1}}, {{var2}}) %>%
    adorn_totals(c("row", "col")) %>% 
    adorn_title("combined") %>%
    {if(is.na(with_percents_by)) { 
        (.) %>%
        dplyr::mutate_if(is.numeric, format, big.mark = ",") 

    } else if( !is.na(with_percents_by) ) {
        (.) %>% 
        janitor::adorn_percentages(with_percents_by) %>% 
        janitor::adorn_pct_formatting(digits = perc_decimals) %>%
        janitor::adorn_ns(position = "front") 
     }
    } 
    
} 


#examples 
diamonds %>% tabyl_two_vars(clarity, cut) 

diamonds %>% tabyl_two_vars(clarity, cut, 'all', perc_decimals = 2)


# example w/ kbl_settings() 
diamonds %>% 
tabyl_two_vars(clarity, cut, 'row') %>% 
kbl_settings(., 
            group_col_num = 1,
            group_custom_header = 'Clarity',
            custom_header = 'Cut')  

