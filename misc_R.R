
###########################
# Misc 
###########################

# https://uc-r.github.io/cleveland-dot-plots
# http://uc-r.github.io/gda

names(which(unlist(eapply(.GlobalEnv, is.data.frame)))) 
ls()
mget(ls())
environment()

print(.packages())
print(library()$results[,1])


#**********************
#**********************
#****** Strings *******
#**********************
#**********************

cat(paste(shQuote(seq(1:3), type="cmd"), collapse=", "))
paste(shQuote(seq(1:3)), collapse=", ")
as.character(expression(lemon, orange, apple))
as.character(seq(1:3))

IRdisplay::display_html(as.character(x))  


#**********************
#**********************
#******* Stats ********
#**********************
#**********************

d          <- sample(data[name] %>% pull(.), 50)
shapiro    <- shapiro.test(d) %>% glance(.)
ks         <- ks.test(d, 'pnorm') %>% glance(.)           
norm_tests <- bind_rows(shapiro, ks)


#************************
#************************
#*** Create data ********
#************************
#************************

make_df <- function() {

    df_temlplate <- data.frame(Doubles=double(),
                               Integers=integer(),
                               Factors=factor(),
                               Logicals=logical(),
                               Characters=character(),
                               stringsAsFactors=FALSE) 
    return(df3)

} 

make_df()
df_temlplate


df4 <- data.frame(Y = rnorm(15), Z = ceiling(rnorm(15)))
df4

# ifelse(sign(rnorm(15))==-1,0,1)

df2 <- data.frame(a = seq(1,16,by=2), 
                  b = LETTERS[1:16], 
                  x= month.abb[1:8], 
                  y = sample(10:20,8, replace = TRUE), 
                  z=letters[1:8]) 
df2

data4 <- data.frame(letter = LETTERS[1:100],
                    rand1 = rnorm(10),            
                    rand2 = runif(10),
                    rand3 = rpois(10, 3),
                    rand4 = rweibull(10, 0.75, 1),
                    rand5 = rbinom(10, 1, prob = 0.7),
                    rand6 = sample(10:20, 10, replace = TRUE),
                    rand7 = seq(1:10)
                    )

#################################################
#################################################
########### examples of control flow ############
#################################################
#################################################

for (i in colnames(data_frame)) { 
        print(i)
}


for (i in colnames(data_frame)) { 

    if (i != 'employee_id' & i != 'cluster') {
        # print(i)

        wee <- syms(i)
        print(wee)

        meh_(data_frame, wee)

    }
} 

#**********************
#**********************
#The packages gt, gtsummary, and modelsummarys datasummary functions are all great ways to 
#**********************
#**********************

# https://www.rdocumentation.org/packages/rstatix/versions/0.7.2/topics/get_summary_stats

library_location <- '/home/atynan/local/'
repo_url <- 'https://mran.revolutionanalytics.com/snapshot/2023-02-01'
# fbr::with_proxy(install.packages('Matrix', version = '1.4.0', lib = library_location, repos = repo_url))    
fbr::with_proxy(install.packages('modelsummary', lib = library_location, repos = repo_url)) 




library_location <- '/home/atynan/local/'
repo_url <- 'https://mran.revolutionanalytics.com/snapshot/2023-02-01'
fbr::with_proxy(install.packages('gtsummary', lib = library_location, repos = repo_url)) 


x <- datasummary_skim(diamonds)
IRdisplay::display_html(as.character(x)) 



library_location <- '/home/atynan/local/'
repo_url <- 'https://mran.revolutionanalytics.com/snapshot/2023-02-01'
fbr::with_proxy(install.packages('modelsummary', lib = library_location, repos = repo_url)) 
library(modelsummary, lib = library_location) 


###################
###################
#### keras ########
###################
###################

# Download package on on-demand server
package_name <- 'keras'; # gglm ggResidpanel
library_location <- '/home/atynan/local/'; 
repo_url <- 'https://cloud.r-project.org';
fbr::with_proxy(
    install.packages(
        package_name,
        lib=library_location,
        repos=repo_url) 
    )

# load package
library('keras', lib=library_location)    

fbr::with_proxy(install.packages('package-name', lib = "/home/parulgupta/R/x86_64-redhat-linux-gnu-library/4.1")) 

library('package-name', lib="/home/parulgupta/R/x86_64-redhat-linux-gnu-library/4.1")

fbr::with_proxy( install.packages( package_name = "babynames", lib="/home/unix_username/local", repos="https://mran.revolutionanalytics.com/snapshot/2023-06-09"))

#################################################
#################################################
#### playing around with kable formating ########
#################################################
#################################################

library(kableExtra)
library(formattable)

options(
    repr.matrix.latex.colspec = list(row_head = 'r|', col = 'l', end = '') # default = list(row_head = 'r|', col = 'l', end = '')
)

kbl(caption = 'test') %>%
 kable_classic(full_width = F, html_font = "Cambria") %>%
 kable_styling(fixed_thead = T) 

# x$employee_count <- color_bar("lightgreen")(x$employee_count)   


#################################################
#################################################
############# ggplot best practices #############
#################################################
#################################################

https://ggplot2.tidyverse.org/reference/index.html
https://ggplot2.tidyverse.org/reference/stat_summary.html?q=stat_bin#null
https://ggplot2.tidyverse.org/reference/geom_linerange.html

ggplot(diamonds, aes(color)) + 
  geom_bar()

ggplot(diamonds, aes(color, price)) + 
  geom_bar(stat = "summary_bin", fun = mean)

  
https://r-graph-gallery.com/index.html


mtcars %>% 
    ggplot(aes(as.factor(cyl),
               qsec)) + 
    
    # stat_summary with arg "fun.y":
    # A function that returns a single number 
    stat_summary(fun.y = mean, 
                 geom = "point") + 
    
    # stat_summary with arg "fun.data": 
    # A function that is given the complete data and should return a data frame
    # with variables ymin, y, and ymax (for use in plotting ranges).
    
    # mean_se( ) is intended for use with stat_summary. It calculates mean and standard error 
    stat_summary(fun.data = mean_se,  
                 geom = "errorbar") + 
    
    scale_y_continuous(limits = c(10, 30))


mtcars %>% 
    ggplot(aes(as.factor(cyl),
               qsec)) + 
    
    # stat_summary with arg "fun.y":
    # A function that returns a single number 
    stat_summary(fun.y = mean, 
                 geom = "point") + 
    
    # stat_summary with arg "fun.data": 
    # A function that is given the complete data and should return a data frame
    # with variables ymin, y, and ymax (for use in plotting ranges).
    
    # mean_cl_normal( ) is intended for use with stat_summary. It calculates
    # sample mean and lower and upper Gaussian confidence limits based on the 
    # t-distribution
    stat_summary(fun.data = mean_cl_normal,  
                 geom = "errorbar") +
    
    scale_y_continuous(limits = c(10, 30))


https://ggplot2tutor.com/tutorials/summary_statistics

gapminder %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               width = .4) +
  stat_summary(fun = "mean", geom = "point")


gapminder %>% 
  ggplot(aes(x = year, y = lifeExp)) +
  stat_summary(fun = mean,
               geom = "pointrange",
               fun.max = function(x) mean(x) + 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)),
               fun.min = function(x) mean(x) - 
                 qt(.975, df = length(x)) * sd(x) / sqrt(length(x)))


https://englelab.gatech.edu/useRguide/plotting-means.html

ggplot(msleep1, aes(vore, sleep_total)) +
  stat_summary(fun.y = mean, na.rm = TRUE, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, na.rm =TRUE, 
               geom = "errorbar", width = .2)


#plot all data points 
ggplot(msleep1, aes(vore, sleep_total)) +
  geom_point(position = position_jitter(width = .2), alpha = .3) +
  stat_summary(fun.y = mean, na.rm = TRUE, 
               geom = "point", color = "dodgerblue", 
               size = 4, shape = "diamond") +
  stat_summary(fun.data = mean_cl_normal, na.rm =TRUE, 
               geom = "errorbar", width = .2, color = "dodgerblue")



https://bookdown.org/yih_huynh/Guide-to-R-Book/important-tidyverse-functions.html

## tidyverse method

### first requires the 'sem' function to be loaded:
sem <- function(x, na.rm = FALSE) {
  out <-sd(x, na.rm = na.rm)/sqrt(length(x))
  return(out)
}

## graphing code
diamonds %>%                          # name of dataset
  group_by(clarity, cut) %>%          # grouping variables
  summarize(m = mean(price),          # calculating mean price
            s = sem(price)) %>%       # calculating standard error
  ggplot(aes(x = clarity,             # x-axis variable
             y = m,                   # y-axis variable 
             group = cut,             # grouping variable
             color = cut)) +          # color the grouping variable
  geom_point() +                      # adding data points
  geom_line() +                       # adding connecting lines
  geom_errorbar(aes(ymin = m - s,     # adding lower error bars
                    ymax = m + s))    # adding upper error bars


## stats_summary method
ggplot(diamonds,                                        # name of dataset
       aes(x = clarity,                                 # x-axis variable
           y = price,                                   # y-axis variable
           group = cut,                                 # grouping variable (legend)
           color = cut)) +                              # coloring the grouping variable
  stat_summary(fun.y = "mean", geom = "point") +        # adding data points
  stat_summary(fun.y = "mean", geom = "line") +         # adding connecting lines
  stat_summary(fun.data = "mean_se", geom = "errorbar") # adding error bars (standard error)


## tidyverse method
diamonds %>% 
  group_by(cut, clarity) %>% 
  summarize(m = mean(price),
            s = sd(price)) %>% # calculating sd
  ggplot(aes(x = clarity, 
           y = m, 
           group = cut,
           color = cut)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = m - s, ymax = m + s)) 


## stat_summary method
ggplot(diamonds, 
       aes(x = clarity, 
           y = price, 
           group = cut,
           color = cut)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.y = "mean", 
               fun.ymax = function(x) mean(x) + sd(x), # calculating sd
               fun.ymin = function(x) mean(x) - sd(x), # calculating sd
               geom = "errorbar") 


###########################
# Misc stat_summary example 
###########################

ggplot(stock_prices.tidy,aes(x=Symbol,y=Prices,fill=Symbol))+
  stat_summary(fun.y = median, geom = "bar")


median.quartile <- function(x){
  out <- quantile(x, probs = c(0.25,0.5,0.75))
  names(out) <- c("ymin","y","ymax")
  return(out)
}


ggplot(stock_prices.tidy, aes(x=Symbol,y=Prices,col=Symbol)) +
  stat_summary(fun.data = median.quartile, geom = "pointrange")


https://financetrain.com/stat_summary-for-statistical-summary-in-ggplot2-r

height_df %>% 
  ggplot(aes(x = group, y = height)) +
  stat_summary(
    geom = "pointrange",
    fun.data = mean_se
  )


https://yjunechoe.github.io/posts/2020-09-26-demystifying-stat-layers-ggplot2/

simple_data %>% 
  ggplot(aes(group, score)) +
  stat_summary(geom = "bar") +
  stat_summary(geom = "errorbar")

height_df <- tibble(group = "A",
                    height = rnorm(30, 170, 10))

height_df %>% 
  ggplot(aes(x = group, y = height)) +
  geom_point()

height_df %>% 
  ggplot(aes(x = group, y = height)) +
  stat_summary()


my_penguins %>% 
  ggplot(aes(sex, body_mass_g)) +
  stat_summary(
    fun.data = ~mean_se(., mult = 1.96), # Increase `mult` value for bigger interval!
    geom = "errorbar",
  )

calc_median_and_color <- function(x, threshold = 40) {
  tibble(y = median(x)) %>% 
    mutate(fill = ifelse(y < threshold, "pink", "grey35"))
}

my_penguins %>% 
  ggplot(aes(species, bill_length_mm)) +
  stat_summary(
    fun.data = calc_median_and_color,
    geom = "bar"
  )

group_split(my_penguins, species) %>%
  map(~ pull(., bill_length_mm)) %>% 
  map_dfr(calc_median_and_color)


###########################
# regression  
###########################

lm(qsec ~ cyl, 
   data = mtcars) %>% 
    tidy %>% 
    kableExtra::kable()


lm(qsec ~ as.factor(cyl), 
   data = mtcars) %>% 
    tidy %>% 
    kableExtra::kable()


###########################
# Boxplots  
###########################

https://appsilon.com/ggplot2-boxplots/

get_box_stats <- function(y, upper_limit = max(df$mpg) * 1.15) {
  return(data.frame(
    y = 0.95 * upper_limit,
    label = paste(
      "Count =", length(y), "\n",
      "Mean =", round(mean(y), 2), "\n",
      "Median =", round(median(y), 2), "\n"
    )
  ))
}

ggplot(df, aes(x = cyl, y = mpg, fill = cyl)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#0099f8", "#e74c3c", "#2ecc71")) +
  stat_summary(fun.data = get_box_stats, geom = "text", hjust = 0.5, vjust = 0.9) +
  theme_classic()



# https://stackoverflow.com/questions/42560389/get-name-of-dataframe-passed-through-pipe-in-r
x_expression <- function(x) {
    getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)

    sc <- sys.calls()
    ASTs <- purrr::map( as.list(sc), getAST ) %>%
      purrr::keep( ~identical(.[[1]], quote(`%>%`)) )  # Match first element to %>%

    if( length(ASTs) == 0 ) return( enexpr(x) )        # Not in a pipe
    dplyr::last( ASTs )[[2]]    # Second element is the left-hand side
  }

data %>% x_expression() -> x 
as_string(x) -> x


