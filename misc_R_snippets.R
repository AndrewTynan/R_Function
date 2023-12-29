
# ggplot misc 

ggplot(data, aes(y = count, x = x, fill = group)) + 
geom_bar(position = "stack", stat = "identity") 


ggplot(data, aes(fill=origin, y=metric_var, x = month)) + 
    geom_bar(position="fill", stat="identity") + 
    scale_y_continuous(labels = scales::percent) + 

scale_fill_viridis(discrete = TRUE) +
scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
name <- stringr::str_to_title(gsub("_", " ", group_var)) 


geom_col() + 
ylim(0, (max(data['metric_var']) + (max(data['metric_var']) * 0.3)) ) + # dynamic adjustment
geom_text(aes(label = metric_var), vjust = "outward") 


geom_text(aes(label = scales::percent(metric_var)), vjust = -0.5, size = 3) + 
scale_y_continuous(labels = scales::percent, limits = c(0, 0.7)) + 


geom_text(aes(label = metric_var), vjust = -0.5, size = 3) + 

scale_colour_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) + 

scale_x_continuous(breaks=seq(0,30,5)) + 

facet_grid(~ YEAR, scales = "free_x") + 

scale_fill_viridis_d(option = "virdis") + 

geom_text(aes(label = metric_var), vjust = -0.5, size = 3) +     

scale_x_date(date_labels="%B", breaks  = "1 month") + 

guides(color = guide_legend(nrow = 2, byrow = TRUE)) +

scale_colour_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) + 

geom_text(aes(label = paste0(round(100 * {{perc_var}}, 0), "%")), vjust = -0.25, size = 8) + 
geom_text(aes(label = paste0(format({{count_var}}, nsmall = 1, big.mark = ","))), vjust = 1.5, size = 8) 

scale_y_reverse(labels = scales::comma, limits = c(count_var_max_, 0)) + 


# dplyr misc 

select(contains("percent")) %>%  
pivot_longer(cols=c(col_1:col_n),
                names_to='metrics',
                values_to='values') %>%  
mutate(metrics = gsub("_", " ", metrics))  


mutate(across(Min:stddev, ~ exp(.x))) 

#cumsum example 
data %>% 
     mutate(metric_var = if_else( is.na(metric_var), 0, metric_var )) %>% 
     group_by(x) %>%
     arrange(x, ds) %>% 
     mutate(metric_var_cumsum = cumsum(metric_var)) %>% 
     group_by(employee_id) %>%
     mutate(total = max(metric_var_cumsum)) %>%
     ungroup() 

# cumsum example 2 
# pattern is arrange then cumsum
# also ungroup and mutate to get max 
select(emp_id, ds, count, time_spent) %>%
group_by(emp_id) %>%
arrange(emp_id, ds) %>% 
mutate(count_cumsum = cumsum(count)) %>% 
group_by(employee_id) %>%
mutate(total_cases = max(count)) %>% 
ggplot(aes(x = time_spent, fill = as.factor(count_cumsum))) + 
geom_density(alpha = 0.15) 



options(repr.plot.width  = 24,   # in inches (default = 7)
        repr.plot.height = 10)   # in inches (default = 7)


y_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(y_var))))
x_var_str <- stringr::str_to_title(gsub('_',' ', deparse(substitute(x_var))))      


group_var_str <- as_label(enquo(group_var))
group_var_str <- deparse(substitute(group_var)) 
group_var_str <- glue('{group_var_str}')     


scale_y_continuous(labels = scales::comma) + # labels = scales::percent,
scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3)) + 

scale_y_continuous(labels = scales::percent, limits = c(0, perc_var_max_)) +
geom_text(aes(label = paste0(round(100 * {{perc_var}}, 0), "%")), vjust = -0.25, size = 8) + 


unlist(.) %>% 
data.frame(.) %>% 
rename(data = 1) %>%
dplyr::relocate(stats, .before = data) %>% 
mutate(id = rep(c("one", "two"), 3)) %>% 
tibble::rownames_to_column(., "Value") %>% 
mutate(Value = str_sub(Value, end = -2)) %>% 
pivot_wider(names_from = Value, values_from = data) %>% 
dplyr::select(-id) 

            
d <- mtcars %>%
  tidyr::nest(gg = -"am") %>%
  mutate_at(
    "gg",
    purrr::map,
    function(x) ggplot2::qplot(wt, mpg, data = x))

cowplot::plot_grid(plotlist = d$gg, labels = d$am)



data %>% 
# nest_by(period) %>%  
  group_by(period) %>% 
  nest() %>%
mutate(model = map(data, function(df) lm(var1 ~ var2, data = df)))
  mutate(mean_life_exp = map_dbl(data, ~mean(.$var1)), 
         life_exp_ci   = map(data, ~gap_lifeExp_ci(., "mean"))
         ) %>%
 select(period, mean_life_exp)


data %>% 
    group_by(period) %>% 
    nest() %>%
    mutate(mean_life_exp = map_dbl(data, ~mean(.$percent))) %>%
    select(period, mean_life_exp) #%>%
    # group_by(period) %>% 
    # nest() %>% 
    # nest_by(period) %>%   
    # group_map(~ .x ) 
