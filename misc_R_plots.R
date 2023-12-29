

gg_density_plot <- function(data, var, by ) { 

    library(glue)   
    options(scipen           = 999,
            repr.plot.width  = 24,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)

    name <- as_label(enquo(var))

    the_theme <- theme(title           = element_text(size = 16),
                       axis.text.x     = element_text(size = 16),
                       axis.text.y     = element_text(size = 16),   
                       legend.position = "none")    

    data %>%  
    ggplot(aes(x = {{var}})) + 
    # ggplot(aes(x = {{var}}, fill = {{by}})) +     
    geom_density(alpha = 0.3) + 
    the_theme
    # labs(title = glue('QQ Plot for {name}'))  

} 

diamonds_tweaked <- diamonds %>%  
                        dplyr::filter(cut %in% c('Ideal', 'Premium')) %>%
                        mutate(cut = as.character(cut)) 

# diamonds_tweaked %>% gg_density_plot(., price, cut)
diamonds_tweaked %>% gg_density_plot(., price)




multiple_density_plots <- function(data, vars, by ) { 

    library(glue)   
    options(scipen           = 999,
            repr.plot.width  = 24,   # in inches (default = 7)
            repr.plot.height = 10)   # in inches (default = 7)

    for(i in vars) {

    the_theme <- theme(title           = element_text(size = 16),
                       axis.text.x     = element_text(size = 16),
                       axis.text.y     = element_text(size = 16),   
                       legend.position = "none") 

    p <- data %>%  
         ggplot(aes(x = .data[[i]], fill = .data[[by]] )) +     
         geom_density(alpha = 0.3) + 
         the_theme +
         labs(title = glue('{i} Density Plot by {by}'))  
    print(p)

    } 
} 


var_names <- as.character(expression( price, depth ))

multiple_density_plots(diamonds_tweaked, var_names, 'cut')


var_names <- as.character(expression( price, depth )) 

multiple_plots(diamonds_tweaked, density_plotter, var_names, 'cut') 


density_plotter <- function(data, metric, by_group) { 

    print(metric) 
    print(by_group) 
    # metric_string   <- metric
    # by_group_string <- by_group   

    data %>%  
        # ggplot(aes(x = {{metric}} , fill = {{by_group}} )) + 
        ggplot(aes(x = .data[[metric]], fill = .data[[by_group]])) +             
        geom_density(alpha = 0.3) + 
        theme(title           = element_text(size = 16),
              axis.text.x     = element_text(size = 16),
              axis.text.y     = element_text(size = 16),   
              legend.position = "none") #+ 
        # labs(title = glue('{metric_string} Density Plot by {by_group_string}'))  

}       

density_plotter(diamonds_tweaked, 'price', 'cut') 


# time series 
ggplot(data,                   
   aes(x = as.Date(x),
       y = y,
       group = group,
       color = group)) +
geom_line() + 
scale_x_date(date_labels = "%m-%d-%y", date_breaks = "1 week") + 
scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +   
theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
      strip.text.x = element_text(size = 10),
      legend.position = "right") + 
labs(title = '',
     subtitle = '',
     y = '',
     x = '',
     color = '') 


# time series with vline 
 ggplot(data,                   
       aes(x = ds, # non date type, needed for geom_vline 
           y = y,
           group = group,
           color = group)) +
  geom_line() +    
  geom_vline(xintercept = 36, 
            linetype="solid", 
            color = "blue", size=1.5) + 
theme(axis.text.x = element_text(angle = 90),     
        strip.text.x = element_text(size = 10),
      legend.position = "right")    +
labs(title = '',
     subtitle = ,
     caption = '',
     y = ''
    )  +
 facet_wrap(~ case_type, ncol = 3)


# area time series 
ggplot(data, 
        aes(x=as.Date(ds),
            y=, 
            fill=)) + 
facet_wrap(~) +
geom_area() + 
scale_x_date(date_labels = "%m-%d-%y", date_breaks = "1 week") + 
theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
      strip.text.x = element_text(size = 10),
      legend.position = "right") 




# boxplot 
ggplot(aes(x = {{by}}, y = {{var}}, fill = {{by}})) +             
geom_boxplot() + 
theme(text            = element_text(size = 20),
      legend.position = "bottom", 
      legend.box      = "horizontal") + 
labs(title    = '_var_str_ ', 
     subtitle = '') 


# stat_ecdf
ggplot(aes(x = {{var}}, color = {{by}})) + 
stat_ecdf(geom = "point") + 
theme(text            = element_text(size = 20),
      legend.position = "bottom", 
      legend.box      = "horizontal") + 
labs(title = '_var_str_ Cumulative Distribution',
     subtitle = 'By _by_str_',  
     x = '_var_str_ ',
     y = 'Cumulative Sum of Observations',
     caption = '',
     color = 'by_str')  


# geom_density
suppressWarnings({  
ggplot(aes(x = {{var}}, fill = {{by}})) + 
geom_density(alpha = 0.3) + 
scale_x_continuous(breaks=seq(0,30,5)) +     
theme(text            = element_text(size = 20),
      legend.position = "bottom", 
      legend.box      = "horizontal") + 
labs(title = '___ Density Plot',
     subtitle = 'By _by_str_',
     caption = '',
     x = '',
     fill = 'By _by_str_') 
})  


#violin 
ggplot(aes(x = as.factor({{by}}), y = {{var}}, fill = as.factor({{by}}))) + 
geom_violin(alpha = 0.3) + 
theme(text = element_text(size = 20)) + 
labs(title = ' Violin Plot',
     subtitle = 'By _by_str_',
     caption = '',
     x = '',
     fill = 'By _by_str_') 


# facet_wrap with reorder 
ggplot(data, 
   aes(x = reorder(cat_var, -y),
       y = y, 
       fill = group)) + 
geom_col() + 
facet_wrap(~group) + 
geom_text(aes(label = event_count), vjust = -0.5, size = 2) + 
ylim(0,  400) + 
theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5), 
        strip.text.x = element_text(size = 10), 
      legend.position = "right") + 
labs(title = '',
     subtitle = '',
     x = '', 
     y = '',
     fill = '') 


ggplot(data=data) + 
  geom_line(aes(x = as.Date(ds), y = metric_var, color = "", group = 1)) + 
  geom_line(aes(x = as.Date(ds), y = metric_var, color = "",  group = 1)) + 
  scale_y_continuous(name = "",
                     sec.axis = sec_axis(~., name = "", labels = scales::label_percent())) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  scale_color_manual(values = c("#F8766D", "#00BFC4")) +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.y = element_text(color = "#F8766D"),
        axis.title.y.right = element_text(color = '#00BFC4')) + 
  labs(title  = '',
       subtitle = '',
       caption = '',
        x = '',
        colour = '') 


library(Scales)
ggplot(data, 
       aes(x = as.Date(date))) + 
  geom_col(aes(y = metric_1, fill = "red")) + 
  geom_col(aes(y = metric_2, fill="blue")) +
  geom_col(aes(y = metric_3, fill="green")) + 
scale_x_date(breaks='days')
# scale_x_date(breaks='days', labels=date_format("%d%b"))
# scale_x_date(date_labels = "%m-%d-%y", date_breaks = "1 week") + 
# scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +   



ggplot(data=data) + 
    aes(x=segment, y=metric_var, fill ='darkgreen') + 
geom_col() + 
scale_y_continuous(labels = scales::percent) + 
geom_errorbar(aes(ymin = lower_err, ymax = upper_err)) 


data %>% 
    mutate(period = as.factor(period)) %>%
    ggplot() + 
    aes(y = mean, x = period, color = period) + 
    geom_point(stat = "identity") + 
    geom_errorbar(aes(ymin = mean - mad, ymax = mean + mad), width = .2,  position = position_dodge(.9)) 

