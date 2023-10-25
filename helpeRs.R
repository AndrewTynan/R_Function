# for printing longer data.frames() 

print_df <- function(x) x %>% print(na.print = "", n = Inf) 

ToothGrowth %>% print_df(.) 



print_col_vals <- function(data, col) { 
        data %>% pull({{col}}) %>% unique(.) %>% sort(.) %>% print()
    }    

diamonds %>% print_col_vals(., clarity) 



clean_col_names <- function(df) {

    #' @description 
    #' Provides a way to clean the column names in a data.frame column containing string values. 
    #' The use case is cleaning before reporting in a table or plot. 
    #' Cleaning involes replacing underscores with spaces and capitalizing the first letter in each portion of the string following a space.      
    #' 
    #' @param df the data.frame      

    df %>% 
       rename_with(., ~ gsub('[_]+|[.]+|["]+', " ",  .x)) %>% 
       rename_with(., ~ stringr::str_to_title(.x)) 
       
}  


clean_metric_names <- function(df, metric_cols = 'Metric') {

    #' @description 
    #' Provides a way to clean the values in a data.frame column containing string values. 
    #' The use case is cleaning values before reporting in a table (specifically one created useing skim() where the variable name is displayed).
    #' Cleaning involes replacing underscores with spaces and capitalizing the first letter in each portion of the string following a space.      
    #' 
    #' @param df the data.frame     
    #' @param metric_cols the column(s) in the data.frame that will be cleaned

    df %>%
       mutate_at(vars(matches( {{metric_cols}} )), gsub, pattern = '[_]+|[.]+|["]+', replacement = " ") %>% 
       mutate_at(vars(matches( {{metric_cols}} )), stringr::str_to_title)
}  



library_checkr <- function(package_to_check) { 

    #' check to see if a librray is already loaded, if it's not load and it's installed, then load it. If it's not installed print msg suggesting to install it.
    #' @param package_to_check a string var of a library to check for

    loaded_libraries    <- .packages() 
    installed_libraries <- library()$results[,1] 

     if(package_to_check %in% loaded_libraries) { 
             print(paste('The library', package_to_check, 'is already loaded.'))

         } else if(!package_to_check %in% loaded_libraries && package_to_check %in% installed_libraries) {

            print(paste('Loading the library', package_to_check,'.'))
            suppressPackageStartupMessages(library(package_to_check, character.only = T)) 

         } else if(!package_to_check %in% installed_libraries) { 
             print(paste('The library', package_to_check, 'is required but not installed. Install it to use this function.'))

         } 
    
    # readline(prompt = "Enter any number : "); # NOTE: this seems to cause Bento errors.
} 


#example 
library_checkr('scales') 


# a helper function for getting the name of the object being passed
# I found this on Stack Overflow https://stackoverflow.com/questions/52066097/get-expression-that-evaluated-to-dot-in-function-called-by-magrittr-pipe/52080518#52080518
# just made a small change to add a named output for clarity 
x_expression <- function(x) {
    getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)

    sc <- sys.calls()
    ASTs <- purrr::map( as.list(sc), getAST ) %>%
            purrr::keep( ~identical(.[[1]], quote(`%>%`)) )  # Match first element to %>%

    if( length(ASTs) == 0 ) return( enexpr(x) )              # Not in a pipe

    lhs_name <- dplyr::last( ASTs )[[2]]                     # Second element is the left-hand side
    lhs_name

  } 




