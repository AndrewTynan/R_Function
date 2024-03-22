# Databricks notebook source
# MAGIC %md
# MAGIC User Level Granular Analysis Below 
# MAGIC * T-test and other EDA
# MAGIC * Reports on both the cumulative as well as the period to period retention
# MAGIC * The t-test script can also be updated to use engagement metrics, rather than just looking at retention
# MAGIC
# MAGIC I added an ANOVA test in order to compare the means across the UCG status (e.g 'is_longterm_ucg', 'tier_type', or 'payment_provider'
# MAGIC
# MAGIC The following functions are defined below: 
# MAGIC extract_t_test()  
# MAGIC     * Gets the  t statistic, pvalue, & confidence intervals 
# MAGIC
# MAGIC t_test() 
# MAGIC     * Wrapper for the functions CompareMeans() and DescrStatsW() from statsmodels.stats.weightstats 
# MAGIC
# MAGIC report_metric() 
# MAGIC     * The stats models t-test functions do not seem to report the means for the two groups, so I made this function to report the means 
# MAGIC
# MAGIC test_runner() 
# MAGIC     * Runs either the t_test() function mentioned above or the anova() from the pingouin package 
# MAGIC
# MAGIC The test_runner() function can run either the t-test or the ANOVA.  
# MAGIC
# MAGIC NOTE: reminder that as the title says, this is just the LONG-TERM US UCG
# MAGIC * Jialu will need to work with Shalini to determine the correct SQL conditions for the short-term UCG for the existing subs 
# MAGIC * I will add the very early stage SQL file to the SQL folder in SharePoint 

# COMMAND ----------

# MAGIC %md
# MAGIC NOTE: More work is needed in order to use the test_runner() function with engagement metrics 
# MAGIC The report_metric() function has a section commented out for adding the reporting for engagement metrics
# MAGIC Also, the other functions need to be checked, I added the 'metric' variable later on and there might be some places where 'is_retained' is hard-coded still

# COMMAND ----------

pip install plotnine==0.12.4

# COMMAND ----------

pip install matplotlib==3.7.1 

# COMMAND ----------

pip install pingouin

# COMMAND ----------

dbutils.library.restartPython() 

# COMMAND ----------

import pandas as pd 
import numpy as np 
import pingouin as pg
from plotnine import * 
from mizani.formatters import percent_format, comma_format
from statsmodels.stats.weightstats import CompareMeans, DescrStatsW  
from statsmodels.stats.proportion import test_proportions_2indep, confint_proportions_2indep 
# pip install jinja2==3.1.2

# COMMAND ----------

# DEFINE col_plot() ad facet() 

def facet(plot, facet_col, facet_scales=None, nrow=1): 

    if facet_scales is not None: # NOTE scales : Literal["fixed", "free", "free_x", "free_y"] = "fixed" 
        plt = (plot + facet_wrap(facet_col, nrow=nrow, scales=facet_scales))  
    else: 
        plt = (plot + facet_wrap(facet_col, nrow=nrow)) 

    return plt 


def col_plot(df, x, y, fill, **kwargs):  

    # kwargs options: 'position', 'facet', 'text', or 'percent' 

    max_val     = df.agg(max = (y, 'max')).iloc[0,0]
    max_val_pad = max_val + (max_val * 0.1)
    text_size = kwargs.get('text_size', 10)
    percent_deciamls = kwargs.get('percent_deciamls', 2)
    nudge_y = kwargs.get('nudge_y', 0)   
    
    if percent_deciamls == 1:
        deciamls = "{:.1f}%" 
    elif percent_deciamls == 0:
        deciamls = "{:.0f}%"        
    
    if 'position' in kwargs: 
        position = kwargs.get('position') 
    else:  
        position = 'stack' 

    plt = (ggplot(df) + 
            geom_col(position=position) + 
            aes(x = x, y = y, fill = fill))
    
    if 'facet' in kwargs:
        plt = facet(plt, 
                    facet_col = kwargs.get('facet'),
                    facet_scales = kwargs.get('facet_scales', 'fixed'), 
                    nrow = kwargs.get('nrow', 1))  

    if 'text' in kwargs and 'percent' in kwargs and position == 'dodge': # 'position' in kwargs:
        text = kwargs.get('text')    
        plt = (plt + geom_text(aes(label = text), position = position_dodge(width = 1), size = text_size, va="bottom", format_string=deciamls) #, nudge_y = nudge_y) 
                   + scale_y_continuous(labels = lambda l: ["%d%%" % v for v in l],
                                        limits = [0, 100])) 
        
    if 'text' in kwargs and 'percent' in kwargs and position == 'stack':
        text = kwargs.get('text')    
        plt = (plt + geom_text(aes(label = text), position = position_stack(vjust = 0.5), size = text_size, va="bottom", format_string=deciamls, nudge_y = nudge_y) 
                   + scale_y_continuous(labels = lambda l: ["%d%%" % v for v in l],
                                        limits = [0, 100]))  
    
    if 'text' in kwargs and 'percent' not in kwargs:
        text = kwargs.get('text')    
        if 'position' in kwargs:
            position = kwargs.get('position') 
            if position == 'dodge': # NOTE default position = "stack", which might be useful sometimes.. 
                plt = (plt + geom_text(aes(label = text), position = position_dodge(width = 1), size = text_size, va="bottom", format_string="{:,}"))
        else: 
            plt = (plt + geom_text(aes(label = text), size = text_size, va="bottom", format_string="{:,}")) 
        
        plt = (plt + scale_y_continuous(labels = comma_format(),
                                        limits = [0, max_val_pad]))
        
    plt = (plt + labs(x=x.replace("_", " ").title(), 
                      y=y.replace("_", " ").title()) 
               + guides(fill = guide_legend(title = y.replace("_", " ").title())))   
    
    return plt 

# COMMAND ----------

# DEFINE five_num_sum_by_group()

def five_num_sum_by_group(df, by, metric):

    def q1(x):
        return x.quantile(0.25)

    def q3(x):
        return x.quantile(0.75)

    df = (df
            .groupby(by)
            .agg(count  = (metric, 'count'),
                mean    = (metric, 'mean'),
                std     = (metric, 'std'),
                min     = (metric, 'min'),
                # q1      = (metric, q1),
                median  = (metric, 'median'),
                # q3      = (metric, q3),
                max     = (metric, 'max'))
            .reset_index() 
            .assign(metric = metric)) 

    return df

# COMMAND ----------

# MAGIC %sql
# MAGIC select 
# MAGIC         user_id,
# MAGIC         global_subscription_id,        
# MAGIC         sub_type, 
# MAGIC         DATE(start_ts) as start_date,
# MAGIC         date_format(start_ts, 'yyyy-MM') as start_month,         
# MAGIC         DATE(paid_start_ts) as paid_start_date,
# MAGIC         date_format(paid_start_ts, 'yyyy-MM') as paid_start_month, 
# MAGIC         tenure_months as lifetime_month, 
# MAGIC         FLOOR(months_between(batch_date, start_ts)) as potential_lifetime_month,
# MAGIC         FLOOR(months_between(paid_start_ts, start_ts)) as months_to_paid, 
# MAGIC         b.grp, 
# MAGIC         IF(grp IS NOT NULL, 'Yes', 'No') as is_longterm_ucg, 
# MAGIC         IF(terminated_ind = 1 AND sub_status = 'STATUS_TERMINATED', DATE(termination_ts), null) AS terminated_date,
# MAGIC         payment_period,
# MAGIC         tier_type, 
# MAGIC         payment_provider
# MAGIC     FROM bolt_growthml_prod.gold.max_subscription_daily_snapshot a 
# MAGIC     LEFT JOIN bolt_dai_martech_prod.gold.lt_ucg_sampling b --long-term ucg (for new subs)
# MAGIC         ON a.user_id = b.userid 
# MAGIC     WHERE batch_date = date_sub(current_date(), 1) 
# MAGIC     AND date(start_ts) >= '2023-06-11' 
# MAGIC     AND market = 'TG_MARKET_UNITED_STATES'  
# MAGIC     AND payment_period = 'PERIOD_MONTH' 
# MAGIC     AND retail_ind = 'true'        
# MAGIC     AND subs_valid_ind = 1 
# MAGIC     AND sub_number = 1 
# MAGIC group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 

# COMMAND ----------

new_subs_user_level = _sqldf.toPandas() 
new_subs_user_level['start_date'] = pd.to_datetime(new_subs_user_level['start_date']) 
new_subs_user_level.head(2) 

# COMMAND ----------

# MAGIC %md
# MAGIC ### Checks
# MAGIC
# MAGIC * Look at the counts of subs by date and UCG
# MAGIC * Plus some summary stats 

# COMMAND ----------

daily_agg = (new_subs_user_level
            .groupby(['start_date', 'is_longterm_ucg'])   
            .agg(user_id_count  = ('user_id', 'count'))
            .reset_index())

(ggplot(daily_agg) + 
    aes(x = 'start_date', y = 'user_id_count', color = "is_longterm_ucg") + 
    scale_y_continuous(labels=comma_format()) +
    geom_line() + 
    theme(figure_size = (16, 4),
          legend_position='bottom') +
    labs(title='US New Subscribers Daily by UCG Status'))

# COMMAND ----------

pd.options.display.float_format = '{:.0f}'.format    

print("average of daily average by is_longterm_ucg")
(new_subs_user_level
    .groupby(['start_date', 'is_longterm_ucg'])   
    .agg(user_id_count  = ('user_id', 'count'))
    .reset_index() 
    .groupby(['is_longterm_ucg']) 
    .agg(mean = ('user_id_count', 'mean'), 
         std  = ('user_id_count', 'std')) 
    .reset_index())  

# COMMAND ----------

pd.options.display.float_format = '{:.1f}'.format    
(new_subs_user_level 
    .groupby(['start_date', 'is_longterm_ucg']) 
    .agg(user_id_count  = ('user_id', 'count')) 
    .pipe(five_num_sum_by_group, 'is_longterm_ucg', 'user_id_count')) 

# COMMAND ----------

monthly_cohort_counts_ul = (new_subs_user_level
                            .groupby(["start_month", "is_longterm_ucg"]) 
                            .agg(cohort_count  = ('user_id', 'count'))
                            .reset_index())

(col_plot(monthly_cohort_counts_ul, x = 'start_month', y = 'cohort_count', fill = 'start_month',  
          text = 'cohort_count', 
          facet = 'is_longterm_ucg') + 
theme(figure_size = (16, 5),
      legend_position='bottom') + 
labs(title = "New Subscriber Monthly Counts by Long-Term UCG Status",
     subtitle = 'For New Subs in the US',
     x= ''))   

# COMMAND ----------

# DEFINE extract_t_test()

def extract_t_test(t_test_results, t_test_CIs): 

    # NOTE: a helper function to extract the values of interest from the t-test output
    t_test_output = pd.DataFrame({'t statistic': t_test_results[0],
                                  'pvalue':      t_test_results[1], 
                                  'CI Lower':    t_test_CIs[0],
                                  'CI Upper':    t_test_CIs[1]}, index=[0])  

    return t_test_output

# COMMAND ----------

# DEFINE t_test() 

def t_test(df, month, metric): 
    # NOTE month is just for adding a data labels 

    control = (df
                .query("is_longterm_ucg == 'Yes'")
                .loc[:, metric] 
                .to_list()) 
    exposed = (df
                .query("is_longterm_ucg == 'No'")
                .loc[:, metric] 
                .to_list()) 

    cm         = CompareMeans(DescrStatsW(exposed), DescrStatsW(control))
    t_test     = cm.ttest_ind(usevar = 'unequal')     # NOTE: If unequal, then Welch ttest is used.   alternative = 'larger',
    t_test_CIs = cm.tconfint_diff(usevar = 'unequal') # alternative = 'larger', 

    t_test_output = extract_t_test(t_test, t_test_CIs) 
    t_test_output.insert(0, "test", ['t test'], True) 

    params        = pd.DataFrame({'month': month}, index=[0]) 
    t_test_output = pd.concat([params, t_test_output], axis=1)  

    return t_test_output


# COMMAND ----------

# DEFINE report_metric()

def report_metric(df, stat, groups): 

    if stat == "retention_rate": 

        groupby = ['lifetime_month_ref'] + groups     # lifetime_month_ref
        
        df_base = (df
                    .groupby(groupby)
                    .agg(total_count = ('user_id', 'count'))  
                    .reset_index()) 

        df_retained = (df
                        .query('is_retained == 1') 
                        .groupby(groupby)
                        .agg(churned_count = ('user_id', 'count')))  

        df_agg_output = (pd.merge(df_base,
                                  df_retained, 
                                  on = groupby)
                            .assign(retention_rate = lambda x: ((x['churned_count'] / x['total_count']) * 100).round(3)))    
    
    # elif stat IN ('list of engagement metrics'):  
    #     df_base = (df
    #                 .query("is_base_population == 'Yes'") 
    #                 .groupby(['lifetime_month_ref', 'is_longterm_ucg'])
    #                 .agg(total_user_id_count  = ('user_id', 'count'), 
    #                      mean                 = (metric, 'mean'),
    #                      std                  = (metric, 'std'))
    #                 .reset_index())  

    #     df_base
    #     df_agg_output 
        
    return df_agg_output


# COMMAND ----------

# POC t_test_runner() 

lifetime_months = new_subs_user_level.loc[:, 'lifetime_month'].unique().tolist()
lifetime_months = [1] 

if 't_test_results' in locals(): 
    del(t_test_results)

for month in lifetime_months: 
    
    lifetime_months_var               = f"lifetime_month == {month}"
    new_subs_user_level['is_churned'] = new_subs_user_level['lifetime_month'].apply(lambda x: 1 if x > lifetime_month else 0) 

    df = (new_subs_user_level
            .query(lifetime_months_var) 
            .loc[:, ['user_id', 'is_churned', 'is_longterm_ucg']]) 

    t_test_output             = t_test(df) 
    t_test_output['stat_sig'] = t_test_output['pvalue'].apply(lambda x: 'Yes' if x <= 0.05 else 'No') 

    if 't_test_results' not in locals(): 
        t_test_results = t_test_output
    else: 
        t_test_results = pd.concat([t_test_results, t_test_output], ignore_index=True, axis=0) 

pd.options.display.float_format = '{:.5f}'.format
t_test_results 

# COMMAND ----------

# DEFINE test_runner() 

def test_runner(df_n, test, metric, period, groups): 
    # test:   't-test', 'anova' 
    # metric: 'is_retained'           # NOTE can update to engagement metrics 
    # period: 'prior', 'cumulative'     
    # groups: a list with any single value or combination of values: 'is_longterm_ucg', tier_type' & 'payment_provider' 

    lifetime_months = df_n.loc[:, 'lifetime_month'].unique().tolist()
    lifetime_months.sort() 

    if period == 'prior': 
        lifetime_months = lifetime_months[:-1] 
    elif period == 'cumulative': 
        lifetime_months.pop(0) 
        lifetime_months = lifetime_months[:-1] 

    for lifetime_month in lifetime_months: 
        df = df_n
        df['lifetime_month_ref'] = lifetime_month
        df['is_retained']        = df['lifetime_month'].apply(lambda x: 1 if x > lifetime_month else 0) 

        if period == 'prior':
            df['is_eligible_population'] = np.where((df['lifetime_month'] >= lifetime_month)  # (lifetime_month - 1)
                                                    & (df['potential_lifetime_month'] >= lifetime_month), 'Yes','No') 

        elif period == 'cumulative':
            df['is_eligible_population'] = df['potential_lifetime_month'].apply(lambda x: 'Yes' if x >= lifetime_month else 'No') 

        df = df.query("is_eligible_population == 'Yes'") 

        if test == 't-test': 
            test_output             = t_test(df, lifetime_month, metric) 
            test_output['stat_sig'] = test_output['pvalue'].apply(lambda x: 'Yes' if x <= 0.05 else 'No') 

        elif test == 'anova': 
            test_output = df.anova(dv      = metric, 
                                   between = groups).round(2)   
            params      = pd.DataFrame({'month': lifetime_month}, index=[0]) 
            test_output = pd.concat([params, test_output], axis=1)  
            test_output['month'] = lifetime_month # corrects nulls in month caused by concat-ing a scale params df to test_output

        df_agg = report_metric(df, 'retention_rate', groups) 

        df = df.drop(['is_retained', 'lifetime_month_ref', 'is_eligible_population'],  axis=1) # not sure this is needed..? 

        if 'test_results' not in locals(): 
            test_results = test_output 
            df_agg_results = df_agg 
        else: 
            test_results   = pd.concat([test_results, test_output], ignore_index=True, axis=0) 
            df_agg_results = pd.concat([df_agg_results, df_agg],    ignore_index=True, axis=0) 

    if period == 'prior': 
        test_results   =  test_results.query("month > 0")
        df_agg_results =  df_agg_results.query("lifetime_month_ref > 0")    

    df_agg_results = df_agg_results.rename(columns = {'lifetime_month_ref': 'lifetime_month'})

    return test_results, df_agg_results 

# COMMAND ----------

# MAGIC %md
# MAGIC ## t-test Cumulative Retention by UCG Status 

# COMMAND ----------

df_2023_06 = new_subs_user_level.query("start_month == '2023-06'") 

t_test_results, t_test_agg_results = test_runner(new_subs_user_level,              # new_subs_user_level, df_2023_06
                                                 test   = 't-test',       
                                                 metric = 'is_retained', 
                                                 period = 'cumulative',   
                                                 groups = ['is_longterm_ucg']) 

pd.options.display.float_format = '{:.2f}'.format 
pd.set_option('display.max_rows', None) 
t_test_results 

# COMMAND ----------

pd.options.display.float_format = '{:.2f}'.format 
t_test_agg_results.sort_values(by =  ['lifetime_month', 'is_longterm_ucg'], ascending = True) 

# COMMAND ----------

(col_plot(t_test_agg_results, 'factor(lifetime_month)', 'retention_rate', 'is_longterm_ucg', 
          text = 'retention_rate', 
          percent = 'retention_rate',   
          position = 'dodge',
          percent_deciamls = 1) + 
theme(figure_size = (16, 4),
      legend_position='bottom') + 
labs(title="Cumulative Retention by Long Term UCG Status Lifetime Month",
     x = 'Lifetime Month')) 

# COMMAND ----------

# MAGIC %md
# MAGIC ## t-test Period to Period Retention by UCG Status 

# COMMAND ----------

t_test_results, t_test_agg_results = test_runner(new_subs_user_level,              # new_subs_user_level, df_2023_06 
                                                 test   = 't-test',      
                                                 metric = 'is_retained', 
                                                 period = 'prior',  
                                                 groups = ['is_longterm_ucg']) 
t_test_results 

# COMMAND ----------

pd.options.display.float_format = '{:.2f}'.format 
t_test_agg_results.sort_values(by = ['lifetime_month', 'is_longterm_ucg'], ascending = True) 

# COMMAND ----------

(col_plot(t_test_agg_results, 'factor(lifetime_month)', 'retention_rate', 'is_longterm_ucg', 
          text = 'retention_rate', 
          percent = 'retention_rate', 
          position = 'dodge',
          percent_deciamls = 1) + 
theme(figure_size = (16, 4),
      legend_position='bottom') + 
labs(title="Prior Month Retention by Long Term UCG Status Lifetime Month",
     subtitle='Note: the large drop in month 4 is due to the large November cohort',
     x = 'Lifetime Month')) 

# COMMAND ----------

# MAGIC %md
# MAGIC ## ANOVA Cumulative Retention by UCG Status 

# COMMAND ----------

anova_tier_type_test_results, anova_tier_type_df_agg_results = test_runner(new_subs_user_level, 
                                                                            test   = 'anova',        # 't-test', 
                                                                            metric = 'is_retained', 
                                                                            period = 'cumulative',   # 'prior'
                                                                            groups = ['is_longterm_ucg', 'tier_type']) # payment_provider
pd.options.display.float_format = '{:.2f}'.format 
pd.set_option('display.max_rows', None)
anova_tier_type_test_results 

# COMMAND ----------

pd.options.display.float_format = '{:.2f}'.format 
anova_tier_type_df_agg_results.sort_values(by = ['is_longterm_ucg', 'tier_type', 'lifetime_month'], ascending = True) # 'payment_provider',

# COMMAND ----------

(col_plot(anova_tier_type_df_agg_results, 'factor(lifetime_month)', 'retention_rate', 'is_longterm_ucg', 
          text = 'retention_rate', 
          percent = 'retention_rate', 
          position = 'dodge', 
          percent_deciamls = 0,
          facet = 'tier_type',
          nrow = 3) + 
theme(figure_size = (16, 10),
      legend_position='bottom') + 
labs(title="Cumulative Retention by Long Term UCG Status Lifetime Month",
     subtitle='By Tier Type',
     x = 'Lifetime Month')) 

# COMMAND ----------

anova_provider_test_results, anova_provider_df_agg_results = test_runner(new_subs_user_level, 
                                                                         test   = 'anova',       
                                                                         metric = 'is_retained', 
                                                                         period = 'cumulative',  
                                                                         groups = ['is_longterm_ucg', 'payment_provider']) 

pd.options.display.float_format = '{:.2f}'.format 
pd.set_option('display.max_rows', None)
anova_provider_test_results 

# COMMAND ----------

pd.options.display.float_format = '{:.2f}'.format 
anova_provider_df_agg_results.sort_values(by = ['is_longterm_ucg', 'payment_provider', 'lifetime_month_ref'], ascending = True) 

# COMMAND ----------

(col_plot(anova_provider_df_agg_results, 'factor(lifetime_month)', 'retention_rate', 'is_longterm_ucg', 
          text = 'retention_rate', 
          percent = 'retention_rate', 
          position = 'dodge', 
          percent_deciamls = 0,
          facet = 'payment_provider',
          nrow = 7) + 
theme(figure_size = (16, 10),
      legend_position='bottom') + 
labs(title="Cumulative Retention by Long Term UCG Status Lifetime Month",
     subtitle='By Tier Type',
     x = 'Lifetime Month')) 

# COMMAND ----------



# COMMAND ----------

# MAGIC %md
# MAGIC # Check Retention Rates 
# MAGIC * get overall segment  

# COMMAND ----------

new_subs_user_level_2023_06 = new_subs_user_level.query("start_month == '2023-06'")

cohort = (new_subs_user_level_2023_06
            .query("lifetime_month > 0") 
            .agg(count = ('user_id', 'count'))) 

retained_cohort = (new_subs_user_level_2023_06
                    .query("lifetime_month > 0") 
                    .groupby(['lifetime_month']) 
                    .agg(churned_count = ('user_id', 'count'))
                    .assign(cohort               = 218536,
                            churned_count_cumsum = lambda x: x['churned_count'].cumsum(), 
                            churn_cumsum         = lambda x: ((x['churned_count_cumsum'] / x['cohort']) * 100).round(2),
                            re_rate              = lambda x: ((x['churned_count'] / x['cohort']) * 100).round(2))
                    .reset_index()) 
retained_cohort 
# new_subs_user_level_2023_06.head()

# COMMAND ----------

# new_subs_user_level_2023_06 = new_subs_user_level.query("start_month == '2023-06'")

cohort = (new_subs_user_level
            .query("lifetime_month > 0") 
            .agg(count = ('user_id', 'count'))) 

retained_cohort = (new_subs_user_level_2023_06
                    .query("lifetime_month > 0") 
                    .groupby(['lifetime_month']) 
                    .agg(churned_count = ('user_id', 'count'))
                    .assign(cohort               = 218536,
                            churned_count_cumsum = lambda x: x['churned_count'].cumsum(), 
                            churn_cumsum         = lambda x: ((x['churned_count_cumsum'] / x['cohort']) * 100).round(2),
                            re_rate              = lambda x: ((x['churned_count'] / x['cohort']) * 100).round(2))
                    .reset_index()) 
retained_cohort 
# new_subs_user_level_2023_06.head()

# COMMAND ----------

new_subs_user_level['lifetime_month'].unique().tolist()

# COMMAND ----------

is_longterm_ucg_cohort = (new_subs_user_level
                            .groupby(['is_longterm_ucg']) 
                            .agg(count = ('user_id', 'count')) 
                            .reset_index()) 
is_longterm_ucg_cohort  

# COMMAND ----------

(new_subs_user_level
 .query("lifetime_month > 0") 
  .query("lifetime_month <= potential_lifetime_month")
    # .groupby(['lifetime_month','potential_lifetime_month']) 
     .groupby(['is_longterm_ucg']) 
    .agg(count = ('user_id', 'count')) 
    .reset_index()) 


# COMMAND ----------

is_longterm_ucg_cohort_lifetimes = (new_subs_user_level
                                    .query("lifetime_month > 0") # only filters out approx 125 users w/ odd data 
                                    .query("lifetime_month <= potential_lifetime_month")
                                    .groupby(['lifetime_month', 'is_longterm_ucg']) # lifetime_month changes as users retain longer and new cohorts are added  
                                    .agg(churn = ('user_id', 'count'))
                                    .reset_index())

df_l = (pd.merge(is_longterm_ucg_cohort_lifetimes,
                 is_longterm_ucg_cohort, 
                 on = ['is_longterm_ucg'])
           .assign(churn_rate = lambda x: ((x['churn'] / x['count']) * 100).round(3))) 

# COMMAND ----------

df_l['cumsum_churn_rate'] = df_l.groupby(['is_longterm_ucg'])['churn_rate'].cumsum() 
df_l['retained_rate'] = 100 - df_l['cumsum_churn_rate']

df_l.sort_values(by = ['is_longterm_ucg','lifetime_month'], ascending = True)  

# COMMAND ----------

(new_subs_user_level
    .query("lifetime_month > 0") 
    .groupby(['lifetime_month']) 
    .agg(user_id_count = ('user_id', 'count'))
    .reset_index()
    )#.head()   

# COMMAND ----------

(new_subs_user_level
    .query("lifetime_month > 0") 
    .groupby(['lifetime_month', 'is_longterm_ucg', 'is_churned']) 
    .agg(user_id_count = ('user_id', 'count'))
    .reset_index()
    )#.head() 

# COMMAND ----------

new_subs_agg = (new_subs_user_level
                .query("lifetime_month > 0") 
                .groupby(['seamless_paid_start_month', 'is_longterm_ucg'])
                ["user_id"]
                .count()
                .reset_index()
                .rename(columns = {'user_id': 'cohort_count'}))

new_subs_agg 

# COMMAND ----------

new_subs_agg_lifetimes = (new_subs_user_level
                            .query("lifetime_month > 0") 
                            .groupby(['seamless_paid_start_month', 'is_longterm_ucg', 'lifetime_month'])
                            ["user_id"]
                            .count() 
                            .reset_index()
                            .rename(columns = {'user_id': 'user_id_count'}))
new_subs_agg_lifetimes                            

# COMMAND ----------

new_subs_user_level['user_id'].agg(['nunique','count','size'])

# COMMAND ----------

# del(df_t) 
df_t = (new_subs_user_level 
        # .query("lifetime_month > 0") 
        .query("is_longterm_ucg == 'No'") 
        .groupby(['is_longterm_ucg', 'potential_lifetime_month'])  
        ["user_id"]
        .count()
        .reset_index()
        .rename(columns = {'user_id': 'user_id_count'})
        .assign(cumsum_rev = lambda x: x.loc[::-1, 'user_id_count'].cumsum())  
        ) 
df_t
# df_t['cumsum_rev'] = df_t.loc[::-1, 'user_id_count'].cumsum()[::-1]    
# df_t.groupby(by=['is_longterm_ucg', 'potential_lifetime_month']).sum().iloc[::-1].groupby(level=[0]).cumsum().iloc[::-1] 
