# Databricks notebook source
# MAGIC %md
# MAGIC **US UCG LONG-TERM Analysis**
# MAGIC     
# MAGIC Analysis of the impact of the US UCG.  
# MAGIC The existing analysis in the Sharepoint file is more descriptive and does not show an statistical testing.  
# MAGIC It focuses on data valiation.  
# MAGIC   
# MAGIC During this first phase, the plan is to:  
# MAGIC 1. Identify the data sources
# MAGIC 2. Do exploratory analysis 
# MAGIC 3. Run tests 
# MAGIC 4. Create reporting for this work 
# MAGIC
# MAGIC NOTE: reminder that as the title says, this is just the LONG-TERM US UCG
# MAGIC * Jialu will need to work with Shalini to determine the correct SQL conditions for the short-term UCG for the existing subs 
# MAGIC * I will add the very early stage SQL file to the SQL folder in SharePoint 

# COMMAND ----------

# MAGIC %md
# MAGIC This was the first UCG analysis that I did, I mainly ended-up using this as a check against the t-test user level data.
# MAGIC It seems like the t-test user level data script is better because it is more flexible and reports on both the cumulative as well as the period to period retention.
# MAGIC The t-test script can also be updated to use engagement metrics, rather than just looking at retention.

# COMMAND ----------

pip install plotnine==0.12.4

# COMMAND ----------

pip install matplotlib==3.7.1

# COMMAND ----------

dbutils.library.restartPython() 

# COMMAND ----------

import pandas as pd 
import numpy as np 
from plotnine import *
from mizani.formatters import percent_format, comma_format

from statsmodels.stats.weightstats import CompareMeans, DescrStatsW  
from statsmodels.stats.proportion import test_proportions_2indep, confint_proportions_2indep 

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
    print(nudge_y) 
    
    if percent_deciamls == 1:
        deciamls = "{:.1f}%" 
    elif percent_deciamls == 0:
        deciamls = "{:.0f}%"     
    else: 
        deciamls = "{:.2f}%"            
    
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
        plt = (plt + geom_text(aes(label = text), position = position_dodge(width = 1), size = text_size, va="bottom", format_string=deciamls, nudge_y = nudge_y) 
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
                q1      = (metric, q1),
                median  = (metric, 'median'),
                q3      = (metric, q3),
                max     = (metric, 'max'))
            .reset_index() 
            .assign(metric = metric)) 

    return df

# COMMAND ----------

# MAGIC %md
# MAGIC Descriptive Analysis for Retention Rates

# COMMAND ----------

# MAGIC %sql
# MAGIC -- retention rates for new subs by monthly cohort ucg vs non-ucg 
# MAGIC WITH new_subs as (
# MAGIC select 
# MAGIC         user_id,
# MAGIC         global_subscription_id, 
# MAGIC         DATE(start_ts) as start_date,
# MAGIC         date_format(start_ts, 'yyyy-MM') as start_month, 
# MAGIC         tenure_months as lifetime_month, 
# MAGIC         batch_date, 
# MAGIC         b.grp, 
# MAGIC         IF(grp IS NOT NULL, 'Yes', 'No') as is_longterm_ucg, 
# MAGIC         IF(terminated_ind = 1 AND sub_status = 'STATUS_TERMINATED', DATE(termination_ts), null) AS terminated_date 
# MAGIC     FROM bolt_growthml_prod.gold.max_subscription_daily_snapshot a 
# MAGIC     LEFT JOIN bolt_dai_martech_prod.gold.lt_ucg_sampling b -- long-term ucg (for new subs)
# MAGIC         ON a.user_id = b.userid 
# MAGIC     WHERE batch_date = date_sub(current_date(), 1) 
# MAGIC     AND date(start_ts) >= '2023-06-11' 
# MAGIC     AND market = 'TG_MARKET_UNITED_STATES'  
# MAGIC     AND payment_period = 'PERIOD_MONTH' 
# MAGIC     AND retail_ind = 'true'        
# MAGIC     AND subs_valid_ind = 1 
# MAGIC     AND sub_number = 1 -- remove this for existing active 
# MAGIC     -- AND closing_active_ind = 1 --add this for existing active 
# MAGIC     -- AND end_ts -- need to use this with exisitng active 
# MAGIC )
# MAGIC
# MAGIC , new_subs_agg as ( 
# MAGIC Select 
# MAGIC     start_month, 
# MAGIC     is_longterm_ucg, 
# MAGIC     COUNT(DISTINCT user_id) as cohort_count 
# MAGIC     From new_subs
# MAGIC GROUP BY 1,2  
# MAGIC ORDER BY 2,1 
# MAGIC ) 
# MAGIC
# MAGIC , new_subs_agg_lifetimes as ( --NOTE: grouping by the lifetime_month is counting churn for past lifetime_month and retained for the current lifetime_month 
# MAGIC Select 
# MAGIC     start_month, 
# MAGIC     is_longterm_ucg, 
# MAGIC     lifetime_month,  
# MAGIC     COUNT(DISTINCT user_id) as churned_count    
# MAGIC     From new_subs 
# MAGIC GROUP BY 1,2,3
# MAGIC order by 1,2,3
# MAGIC ) 
# MAGIC
# MAGIC , new_subs_cumsum_churn as ( 
# MAGIC Select 
# MAGIC     a.start_month, 
# MAGIC     a.is_longterm_ucg, 
# MAGIC     a.lifetime_month, 
# MAGIC     cohort_count, 
# MAGIC     churned_count, 
# MAGIC     SUM(churned_count)  OVER(PARTITION BY a.is_longterm_ucg, a.start_month ORDER BY a.lifetime_month) AS cumsum_churned_count, 
# MAGIC     MAX(lifetime_month) OVER(PARTITION BY a.is_longterm_ucg, a.start_month) AS max_lifetime_month 
# MAGIC     FROM new_subs_agg_lifetimes a 
# MAGIC     JOIN new_subs_agg b 
# MAGIC         ON a.start_month = b.start_month
# MAGIC         AND a.is_longterm_ucg = b.is_longterm_ucg
# MAGIC ) 
# MAGIC
# MAGIC SELECT 
# MAGIC     start_month, 
# MAGIC     is_longterm_ucg, 
# MAGIC     lifetime_month, 
# MAGIC     cohort_count, 
# MAGIC     cumsum_churned_count,
# MAGIC     cohort_count - cumsum_churned_count AS retained_count, 
# MAGIC     ROUND(100. * (cohort_count - cumsum_churned_count) / cohort_count, 2) as retention_rate 
# MAGIC     FROM new_subs_cumsum_churn 
# MAGIC     WHERE lifetime_month > 0 
# MAGIC     AND lifetime_month < (max_lifetime_month - 1) -- remove the partial retained months 
# MAGIC ORDER BY is_longterm_ucg, start_month, lifetime_month 

# COMMAND ----------

new_subs_cohort_retention = _sqldf.toPandas() 
new_subs_cohort_retention['retention_rate'] = new_subs_cohort_retention['retention_rate'].astype(float)

# COMMAND ----------

# pd.set_option('display.max_columns', None)
# pd.set_option('display.max_rows', None)
# display(new_subs_cohort_retention)

# print(new_subs_cohort_retention)
# print(new_subs_cohort_retention.to_markdown())

# COMMAND ----------

(ggplot(new_subs_cohort_retention) + 
    aes(x = 'lifetime_month', y = 'retention_rate', color = "factor(start_month)") + 
    facet_wrap(' ~ is_longterm_ucg', scales='free') + 
    scale_y_continuous(labels = lambda l: ["%d%%" % v for v in l],
                                            limits = [0, 100]) +
    geom_line() + 
    guides(color = guide_legend(title = 'Start Month')) + 
    theme(figure_size = (12, 4),
          legend_position='bottom') + 
    labs(title = 'Montly Retention by Cohort Month & Long-Term UCG Status',
         subtitle = 'For New Subs in the US'))

# COMMAND ----------

(col_plot(new_subs_cohort_retention, x = 'lifetime_month', y = 'retention_rate', fill = 'is_longterm_ucg',  
          text = 'retention_rate', 
          percent = 'retention_rate',
          facet = 'start_month', 
          facet_scales = 'free', 
          nrow = 3, 
          position = 'dodge',
          text_size = 8,
          percent_deciamls = 0) + 
theme(figure_size = (16, 6),
      legend_position='bottom') + 
labs(title = 'Montly Retention by Cohort Month & Long-Term UCG Status',
     subtitle = 'For New Subs in the US',
     x= '')) 

# COMMAND ----------

monthly_cohort_counts = (new_subs_cohort_retention
                        .query('lifetime_month == 1')
                        .loc[:,['start_month', 'is_longterm_ucg', 'cohort_count']]
                        .reset_index())

(col_plot(monthly_cohort_counts, x = 'start_month', y = 'cohort_count', fill = 'start_month',  
          text = 'cohort_count', 
          facet = 'is_longterm_ucg') + 
theme(figure_size = (12, 5),
      legend_position='bottom') + 
labs(title = "New Subscriber Monthly Counts by Long-Term UCG Status",
     subtitle = 'For New Subs in the US',
     x= '')) 

# COMMAND ----------

print('The table below shows Monthly Retention summary stats by UCG Status')
new_subs_cohort_retention_SUMMARY = five_num_sum_by_group(new_subs_cohort_retention, 
                                                          ['is_longterm_ucg', 'lifetime_month'], 
                                                          'retention_rate') 
new_subs_cohort_retention_SUMMARY 

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
        plt = (plt + geom_text(aes(label = text), position = position_dodge(width = 1), size = text_size, va="bottom", format_string=deciamls, nudge_y = nudge_y) 
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

(col_plot(new_subs_cohort_retention_SUMMARY, x = 'is_longterm_ucg', y = 'mean', fill = 'is_longterm_ucg',  
          text = 'mean', 
          percent = 'mean',
          facet = 'lifetime_month', 
          facet_scales = 'free',
          position = 'dodge',
          text_size = 8,
          percent_deciamls = 0,
          nudge_y = 8) + 
geom_errorbar(aes(x="is_longterm_ucg", ymin="mean-std", ymax="mean+std")) +  
theme(figure_size = (16, 4),
      legend_position='bottom') + 
labs(title = 'New Subscriber First Month Avg Retention by Long-Term UCG Status',
     subtitle = 'For New Subs in the US',
     x= '')) 

# COMMAND ----------



# COMMAND ----------

# POC (proof of concept) for sim() 

ucg_retained = int(df.query("is_longterm_ucg == 'Yes'").loc[:,['retained_count_total']].values[0])
ucg_total    = int(df.query("is_longterm_ucg == 'Yes'").loc[:,['cohort_count_total']].values[0])

non_ucg_retained = int(df.query("is_longterm_ucg == 'No'").loc[:,['retained_count_total']].values[0])
non_ucg_total    = int(df.query("is_longterm_ucg == 'No'").loc[:,['cohort_count_total']].values[0])

prop_test = test_proportions_2indep(count1      = ucg_retained,
                                    nobs1       = ucg_total,
                                    count2      = non_ucg_retained,
                                    nobs2       = non_ucg_total, 
                                    alternative = 'larger') 

prop_test_ci = confint_proportions_2indep(count1      = ucg_retained,
                                        nobs1       = ucg_total,
                                        count2      = non_ucg_retained,
                                        nobs2       = non_ucg_total) 

prop_test_output = pd.DataFrame({'test':        ['prop test'],
                                't statistic': [prop_test.statistic],
                                'pvalue':      [prop_test.pvalue],
                                'CI':      [prop_test_ci[0]]}) 

params  = pd.DataFrame({'month': month}, index=[0]) 

prop_test_output = pd.concat([params, prop_test_output], axis=1)  

prop_test_output 

# COMMAND ----------

# DEFINE prop_test() 
def prop_test(df, month): 
    # NOTE month is just for adding a data labels in params    

    ucg_retained = int(df.query("is_longterm_ucg == 'Yes'").loc[:,['retained_count_total']].values[0])
    ucg_total    = int(df.query("is_longterm_ucg == 'Yes'").loc[:,['cohort_count_total']].values[0])

    non_ucg_retained = int(df.query("is_longterm_ucg == 'No'").loc[:,['retained_count_total']].values[0])
    non_ucg_total    = int(df.query("is_longterm_ucg == 'No'").loc[:,['cohort_count_total']].values[0])    

    prop_test = test_proportions_2indep(count1      = non_ucg_retained, 
                                        nobs1       = non_ucg_total,
                                        count2      = ucg_retained,
                                        nobs2       = ucg_total) # alternative = 'larger') 

    prop_test_ci = confint_proportions_2indep(count1 = non_ucg_retained,
                                              nobs1  = non_ucg_total,
                                              count2 = ucg_retained,
                                              nobs2  = ucg_total) 

    prop_test_output = pd.DataFrame({'test':      ['prop test'],
                                     'statistic': [prop_test.statistic],
                                     'pvalue':    [prop_test.pvalue],
                                     'CI Lower':  [prop_test_ci[0]],
                                     'CI Upper':  [prop_test_ci[1]]}) 

    params           = pd.DataFrame({'month': month}, index=[0]) 
    prop_test_output = pd.concat([params, prop_test_output], axis=1)  

    return prop_test_output 

# COMMAND ----------

# POC prop_test_runner() 
lifetime_months = new_subs_cohort_retention.loc[:, 'lifetime_month'].unique()

if 'prop_test_results' in locals(): 
    del(prop_test_results)

for month in lifetime_months: 
    
    lifetime_months_var = f"lifetime_month == {month}"
    # print(lifetime_months_var)

    df = (new_subs_cohort_retention
            .query(lifetime_months_var) 
            .loc[:, ['lifetime_month', 'is_longterm_ucg', 'cohort_count', 'retained_count', ]]
            .groupby(['lifetime_month', 'is_longterm_ucg'])
            .agg(cohort_count_total   = ('cohort_count', 'sum'),
                 retained_count_total = ('retained_count', 'sum'))
            .reset_index())  
    # print(df)

    prop_test_output             = prop_test(df)
    prop_test_output['stat_sig'] = prop_test_output['pvalue'].apply(lambda x: 'Yes' if x <= 0.05 else 'No') 

    if 'prop_test_results' not in locals(): 
        prop_test_results = prop_test_output
    else: 
        prop_test_results = pd.concat([prop_test_results, prop_test_output], ignore_index=True, axis=0) 

pd.options.display.float_format = '{:.5f}'.format 
prop_test_results 

# COMMAND ----------

# DEFINE prop_test_runner() 

def prop_test_runner(df): 

    lifetime_months = df.loc[:, 'lifetime_month'].unique()

    for month in lifetime_months: 
        
        lifetime_months_var = f"lifetime_month == {month}" 

        df_agg = (df
                    .query(lifetime_months_var) 
                    .loc[:, ['lifetime_month', 'is_longterm_ucg', 'cohort_count', 'retained_count']]
                    .groupby(['lifetime_month', 'is_longterm_ucg'])
                    .agg(cohort_count_total  = ('cohort_count', 'sum'),
                        retained_count_total = ('retained_count', 'sum'))
                    .reset_index())  

        prop_test_output             = prop_test(df_agg, month)
        prop_test_output['stat_sig'] = prop_test_output['pvalue'].apply(lambda x: 'Yes' if x <= 0.05 else 'No') 

        df_agg = (df_agg
                  .assign(retained_percent = lambda x: ((x['retained_count_total'] / x['cohort_count_total']) * 100).round(2)))  

        if 'prop_test_results' not in locals(): 
            prop_test_results = prop_test_output 
            df_agg_results    = df_agg

        else: 
            prop_test_results = pd.concat([prop_test_results, prop_test_output], ignore_index=True, axis=0) 
            df_agg_results    = pd.concat([df_agg_results, df_agg], ignore_index=True, axis=0) 

    return prop_test_results, df_agg_results 


prop_test_results, df_agg_results  = prop_test_runner(new_subs_cohort_retention) 

print('Reporting the retained_percent by lifetime_month & is_longterm_ucg metrics to contextualize the pre.test results') 
pd.options.display.float_format    = '{:.3f}'.format 
prop_test_results 

# COMMAND ----------

pd.options.display.float_format = '{:.2f}'.format 
df_agg_results.sort_values(by = ['is_longterm_ucg'], ascending = True) 

# COMMAND ----------

# MAGIC %md
# MAGIC ### Check 
# MAGIC
# MAGIC * Using the June 2023 cohort 
# MAGIC * Run the tests on just this cohort and compare the df_2023_06_df_agg_results to the plot of the monthly cohort retnetion by lifetime_month above
# MAGIC * It matches! 

# COMMAND ----------

# # just 2023-06 cohort 
df_2023_06 = (new_subs_cohort_retention
              .query("start_month == '2023-06'"))

df_2023_06_prop_test_results, df_2023_06_df_agg_results = prop_test_runner(df_2023_06) 

print('June cohort 2023-06') 
pd.options.display.float_format    = '{:.5f}'.format 
df_2023_06_prop_test_results 

# COMMAND ----------

print('June cohort 2023-06') 
print('Reporting the retained_percent by lifetime_month & is_longterm_ucg metrics to contextualize the pre.test results') 
pd.options.display.float_format = '{:.2f}'.format 
df_2023_06_df_agg_results.sort_values('lifetime_month') 

# COMMAND ----------


