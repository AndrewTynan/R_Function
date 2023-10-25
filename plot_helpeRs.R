

var_max_value_padded <- function(data, var, padding=0.2) max(data[var]) + (max(data[var]) * padding)  


# str_to_title_gsub <- function(var) stringr::str_to_title(gsub('_',' ', deparse(substitute(var))))


gg_the_theme <- function(ggplot_obj) { # was called single_plot_settings    

    the_theme <- theme(title            = element_text(size = 20),
                        axis.text.x     = element_text(size = 20),
                        axis.text.y     = element_text(size = 20), 
                        legend.text     = element_text(size = 16), 
                        legend.position = "right")   
    return(the_theme)
    # assign('the_theme', the_theme, envir = .GlobalEnv)          
                    
} 


gg_theme_default <- function(ggplot_obj) { 

    ggplot_obj <- ggplot_obj + theme(title           = element_text(size = 20),
                                     axis.text.x     = element_text(size = 20),
                                     axis.text.y     = element_text(size = 20), 
                                     legend.text     = element_text(size = 16), 
                                     legend.position = "right")   
    return(ggplot_obj)  
                    
} 


ggplot_format_scale_y_continuous <- function(ggplot_obj, var, y_mmin=0, y_max, y_format=NA) { 

    { if( !is.na(y_format)) { # percent formatting 
        format <- ggplot_obj + 
                  scale_y_continuous(labels = scales::percent, limits = c(y_mmin, y_max)) +
                  geom_text(aes(label = paste0(round(100 * {{var}}, 1), "%")), vjust = -0.25, size = 8) 
    
    } else if( is.na(y_format)) { # count formatting  
        format <- ggplot_obj + 
                  scale_y_continuous(labels = scales::comma, limits = c(y_mmin, y_max)) + 
                  geom_text(aes(label = paste0(format({{var}}, nsmall = 1, big.mark = ","))), vjust = -0.25, size = 8) 
    }
   } 
    return(format)
} 


plot_dims <- function(width=7, height=7) { 

    if(!is.null(width) && !is.null(height)) { 
        options(repr.plot.width = width, 
                repr.plot.height = height) 

    } else if(!is.null(width)) { 
        options(repr.plot.width = width, 
                 repr.plot.height  = 7)  

    } else if(!is.null(height)) { 
        options(repr.plot.width  = 7, 
                repr.plot.height = input_list$height)  

    } else if( is.null(width) && is.null(height)) { 
        options(repr.plot.width  = 7, 
                repr.plot.height = 7)  
    }
} 
