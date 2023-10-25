

# Fetch package
library('fitdistrplus', lib=library_location)
library(lme4)
library(tidyverse)
library(cowplot)
# library(stringr)
library(kableExtra)


pp_url_case_creation_rate_raw <- pp_url_case_creation_rate_raw %>% 
                                  mutate(pp_url =  if_else(pp_url == 'intern/people/portal', pp_url, str_remove(pp_url, "intern/people/portal/"))) 


pp_url_case_creation_rate_to_fit <- pp_url_case_creation_rate_raw %>% 
                                        filter(case_session_rate > 0,
                                               case_session_rate < 1)


options(
  repr.plot.width  = 16,   # in inches (default = 7)
  repr.plot.height = 8     # in inches (default = 7)
)

p1 <- pp_url_case_creation_rate_to_fit %>% 
        ggplot(aes(x = case_session_rate)) + #
        stat_ecdf(geom = "point",  color="darkgreen") + 
        theme(text = element_text(size = 20),
                legend.position="none") +     
        labs(title = 'Session Case Percent Cumulative Distribution',
            subtitle = 'By URL the Percent of Sessions with Case Creation',    
            x = 'Case Session Percent',
            y = 'Cumulative Sum of Case Session Rate (by URL)') 

p2 <- pp_url_case_creation_rate_to_fit %>% 
        ggplot() + 
        aes(x = case_session_rate) + 
        geom_histogram(aes(y = ..density..),
                        colour = 1, 
                        fill = "aquamarine4",
                        bins = 50) +
        geom_density(lwd = 1, 
                    colour = 1,
                    fill = "aquamarine4", 
                    alpha = 0.7) + 
            scale_x_continuous(breaks=seq(0,50,5)) + 
            theme(text = element_text(size = 20),
                   legend.position="none") +     
            labs(title = 'Session Case Percent Histogram & Density Plot',
                 subtitle = 'By URL the Percent of Sessions with Case Creation',    
                x = 'Session Case Percent',
                y = 'Density') 

plot_grid(p1, p2)         



descdist(pp_url_case_creation_rate_to_fit$case_session_rate, boot = 1000)



m <- MASS::fitdistr(pp_url_case_creation_rate_to_fit$case_session_rate,
                    dbeta,
                    start = list(shape1 = 1, shape2 = 10))

alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]

alpha0
beta0



# NOTE 
# An example of creating the emplirical bayes rate
# all_events is the total sample 
# sub_count_events is the sub set of interest 
# 
# career_eb <- career %>%
#   mutate(eb_estimate = (sub_count_events + alpha0) / (all_events + alpha0 + beta0))

pp_url_case_creation_rate_to_fit <- pp_url_case_creation_rate_to_fit %>%
                                    mutate(eb_estimate = (case_session_count + alpha0) / (session_count + alpha0 + beta0))



# another binomial approach is to model with lmer 
glmer_full = lme4::glmer(cbind(case_session_count, session_count-case_session_count) ~ 1 + (1|pp_url), 
                         data = pp_url_case_creation_rate_to_fit,
                         family = binomial)

pp_url_case_creation_rate_to_fit <- pp_url_case_creation_rate_to_fit %>% 
                                    mutate(glmer_estimate = fitted(glmer_full))                          


pp_url_case_creation_rate_to_fit %>% 
    mutate_at(c("case_session_rate", "eb_estimate", "glmer_estimate"), round, 2) %>%
    filter(case_session_rate !=  eb_estimate) %>% 
    arrange(desc(session_count)) %>% 
    head(10) %>%
    as_tibble() %>%
    clean_col_names() %>% 
    kbl_settings(.,
                group_col_num = 1,
                group_custom_header = 'URL', 
                # desc_stats_label_flag = NA, 
                # custom_header = 'Metric Statistics'
                # kbl_settings_note = NA) 
                ) 

    # print(., n=1000)



pp_url_case_creation_rate_to_fit %>% 
    ggplot(aes(x = case_session_rate, y = eb_estimate)) + 
    geom_point() + 
    geom_abline(color = 'blue') + 
    scale_color_continuous(trans = 'log') + # , breaks = c(.2,)
    # geom_hline(yintercept = tidy())
    labs(title =  'Shruken Case Session Rate',
        x = 'Case Session Rate',
        y = 'Shruken Case Session Rate')


syms('illiteracy_rate') %>% 
    map(., ~ecdf_histogram_qq_plots_purrr(country_litarcy_rate, !!.x)) 



country_litarcy_rate_to_fit <- country_litarcy_rate %>% 
                                dplyr::filter(illiteracy_rate > 0,
                                        illiteracy_rate < 1) #%>%
                                # dplyr::select(country, population, illiterate_population, illiteracy_rate)   

pp_url_case_creation_rate_to_fit$web_page <- paste0("web_page_", 1:nrow(pp_url_case_creation_rate_to_fit))

pp_url_case_creation_rate_to_fit <- pp_url_case_creation_rate_to_fit %>% 
                                    select(web_page,  session_count, case_session_count, case_session_rate) %>% 
                                    mutate(web_page = as.factor(web_page)) 


print(tbl_df(pp_url_case_creation_rate_to_fit), n=10)



glmer_binomial_param_shrink <- function(data, name_var, rate_var, sub_total_count, total_count) { 

    # library(lme4)
    # library(tidyverse)

    df_1 <- data %>% rename(name_var = 1,
                            rate_var = 2,
                            sub_total_count = 3,
                            total_count = 4) %>% 
                      dplyr::filter(rate_var > 0, rate_var < 1) 

    binomial_glmer = lme4::glmer(cbind(sub_total_count, total_count-sub_total_count) ~ 1 + (1|name_var), 
                                 data = df_1,
                                 family = binomial)

    df_1 <- df_1 %>% mutate(binomial_glmer = fitted(binomial_glmer)) 

    data_names <- names(data)
    df_1       <- df_1 %>% rename_at(vars(1:4), ~ data_names)

    # # print(head(df_1,1))    
    # # print(head(data,1))    
    
    output <<- data %>% dplyr::left_join(., df_1) 

}


country_litarcy_rate_to_fit <- country_litarcy_rate_to_fit %>% 
                                mutate(illiterate_population = as.integer(illiterate_population),
                                    population            = as.integer(population))


# glmer_binomial_param_shrink(country_litarcy_rate_to_fit,  
#                             name_var        = country, 
#                             rate_var        = illiteracy_rate, 
#                             sub_total_count = illiterate_population,
#                             total_count     = population
#                             )          

glmer_binomial_param_shrink(pp_url_case_creation_rate_to_fit,  
                            name_var        = web_page, 
                            rate_var        = case_session_rate, 
                            sub_total_count = case_session_count,
                            total_count     = session_count
                            )    

output %>% tail   



