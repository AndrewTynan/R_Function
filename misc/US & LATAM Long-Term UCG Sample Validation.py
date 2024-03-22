# Databricks notebook source
# MAGIC %md
# MAGIC Checking the Samples Sizes for the US LONG-TERM UCG Daily Cohorts   
# MAGIC   * Reporting the UCG percent by month 
# MAGIC   * Also by the dimensions used in the 'grp' field: 
# MAGIC     * vars = ['payment_period', 'tier_type', 'payment_provider', 'subscribed_in_territory'] 
# MAGIC       * I added 'subscribed_in_territory' to the LATAM checks, but it is not part of the 'grp' field used in the LATAM sampling process 
# MAGIC
# MAGIC NOTE: 
# MAGIC * Jialu will need to work with Shalini to determine the correct SQL conditions for the short-term UCG for the existing subs 
# MAGIC * I will add the very early stage SQL file to the SQL folder in SharePoint 

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
from mizani.formatters import percent_format
from datetime import datetime, date 
pd.options.mode.chained_assignment = None # suppress warnings :) 

# from mizani.formatters import percent_format

# COMMAND ----------

# MAGIC %md
# MAGIC # Aggregate check 
# MAGIC
# MAGIC ## The aggregate check is compared to the subsequent granular user level SQL pull aggregations donw with Pandas
# MAGIC
# MAGIC I did this because I'm still learning Pandas and needed a check ;) 

# COMMAND ----------

# MAGIC %sql
# MAGIC ------------------------------------------------balance checks on 1 day of the month-----------------------------------------------------
# MAGIC -----------------------------------------------------------------------------------------------------------------------------------------
# MAGIC -- is there a new version of max_prod.subscription_gold.max_launch_signup_holdout_ids ? 
# MAGIC with max_users_since_ucg as ( 
# MAGIC select 
# MAGIC         user_id,
# MAGIC         global_subscription_id,        
# MAGIC         sub_type, 
# MAGIC         DATE(start_ts) as start_date,
# MAGIC         date_format(start_ts, 'yyyy-MM') as seamless_paid_start_month, 
# MAGIC         tenure_months as lifetime_month, 
# MAGIC         FLOOR(months_between(batch_date, start_ts)) as potential_lifetime_month,
# MAGIC         FLOOR(months_between(paid_start_ts, start_ts)) as months_to_paid, 
# MAGIC         case when paid_start_ts is null    then 'non-payer'
# MAGIC              when paid_start_ts = start_ts then 'payer from start'
# MAGIC              when paid_start_ts > start_ts then 'converted to payer'
# MAGIC              else 'other'
# MAGIC              end payer_type, 
# MAGIC         b.grp, 
# MAGIC         IF(grp IS NOT NULL, 'Yes', 'No') as is_longterm_ucg, 
# MAGIC         IF(terminated_ind = 1 AND sub_status = 'STATUS_TERMINATED', DATE(termination_ts), null) AS terminated_date 
# MAGIC     FROM bolt_growthml_prod.gold.max_subscription_daily_snapshot a 
# MAGIC     LEFT JOIN bolt_dai_martech_prod.gold.lt_ucg_sampling b --long-term ucg (for new subs)
# MAGIC         ON a.user_id = b.userid 
# MAGIC     WHERE batch_date = date_sub(current_date, 1) 
# MAGIC     AND date(start_ts) >= '2023-06-11' 
# MAGIC     -- AND (sub_type is null or sub_type in ('TYPE_PAYING','TYPE_NON_PAYING'))
# MAGIC     AND market = 'TG_MARKET_UNITED_STATES' 
# MAGIC     AND payment_period = 'PERIOD_MONTH' 
# MAGIC     AND retail_ind = 'true' 
# MAGIC     AND subs_valid_ind = 1 
# MAGIC     AND sub_number = 1 
# MAGIC group by 1,2,3,4,5,6,7,8,9,10,11,12 
# MAGIC ) 
# MAGIC Select 
# MAGIC     start_date, 
# MAGIC     is_longterm_ucg, 
# MAGIC     user_id_count, 
# MAGIC     ROUND(1. * user_id_count / sum(user_id_count) over(partition by start_date),2) as daily_ucg_percent 
# MAGIC From (Select 
# MAGIC           start_date, 
# MAGIC           is_longterm_ucg, 
# MAGIC           count(distinct user_id) as user_id_count
# MAGIC           from max_users_since_ucg
# MAGIC       group by 1,2 
# MAGIC      ) a 
# MAGIC order by 1,2 

# COMMAND ----------

ucg_date_cohort = _sqldf.toPandas() 

# COMMAND ----------

ucg_date_cohort['start_date'] = pd.to_datetime(ucg_date_cohort['start_date']) 
ucg_date_cohort['start_year_month'] = ucg_date_cohort['start_date'].dt.strftime('%Y_%m') 

pd.options.display.float_format = '{:.2f}'.format 
ucg_date_cohort_5_num_sum = five_num_sum_by_group(ucg_date_cohort, ['start_year_month', 'is_longterm_ucg'], 'daily_ucg_percent') 
# ucg_date_cohort_5_num_sum.dtypes

ucg_date_cohort_5_num_sum["mean"] = pd.to_numeric(ucg_date_cohort_5_num_sum["mean"])

# COMMAND ----------

(ggplot(ucg_date_cohort_5_num_sum) + 
    aes(x = 'is_longterm_ucg', y = 'mean', fill = 'is_longterm_ucg') + 
    facet_wrap(' ~ start_year_month', nrow=1) + #  scales='free_x',
    geom_col() + 
    geom_errorbar(aes(x="is_longterm_ucg", ymin="mean-std", ymax="mean+std")) + 
    scale_y_continuous() + 
    theme(figure_size = (16, 4),
          legend_position='bottom') + 
    labs(title="Average Long Term UCG Percent by Month")) 

# COMMAND ----------

# MAGIC %md
# MAGIC # UCG Long Term Sample Validation
# MAGIC
# MAGIC ## For US and LATAM
# MAGIC
# MAGIC Can be extended to EMEA by updating region_filter() 

# COMMAND ----------

# MAGIC %sql 
# MAGIC select 
# MAGIC         user_id,
# MAGIC         global_subscription_id,        
# MAGIC         sub_type, 
# MAGIC         DATE(start_ts) as start_date,
# MAGIC         date_format(start_ts, 'yyyy-MM') as seamless_paid_start_month, 
# MAGIC         tenure_months as lifetime_month, 
# MAGIC         FLOOR(months_between(batch_date, start_ts)) as potential_lifetime_month,
# MAGIC         FLOOR(months_between(paid_start_ts, start_ts)) as months_to_paid, 
# MAGIC         case when paid_start_ts is null    then 'non-payer'
# MAGIC              when paid_start_ts = start_ts then 'payer from start'
# MAGIC              when paid_start_ts > start_ts then 'converted to payer'
# MAGIC              else 'other'
# MAGIC              end payer_type, 
# MAGIC         coalesce(b.grp, c.grp) as grp, 
# MAGIC         IF(coalesce(b.grp, c.grp) IS NOT NULL, 'Yes', 'No') as is_longterm_ucg,
# MAGIC         IF(terminated_ind = 1 AND sub_status = 'STATUS_TERMINATED', DATE(termination_ts), null) AS terminated_date,
# MAGIC         payment_period,
# MAGIC         tier_type, 
# MAGIC         payment_provider, 
# MAGIC         -- concat(subscription_product_most_recent, term_length_most_recent) as service_type, 
# MAGIC         signup_offer, 
# MAGIC         subscribed_in_territory,
# MAGIC         market 
# MAGIC     FROM bolt_growthml_prod.gold.max_subscription_daily_snapshot a 
# MAGIC     LEFT JOIN bolt_dai_martech_prod.gold.lt_ucg_sampling b -- US long-term ucg (for new subs)
# MAGIC         ON a.user_id = b.userid 
# MAGIC     LEFT JOIN bolt_dai_martech_prod.gold.lt_ucg_sampling_latam c -- LATAM long-term ucg (for new subs)
# MAGIC         ON a.user_id = c.userid 
# MAGIC     WHERE batch_date = date_sub(current_date, 1) 
# MAGIC     AND date(start_ts) >= '2023-06-11' 
# MAGIC     AND retail_ind = 'true' 
# MAGIC     AND subs_valid_ind = 1 
# MAGIC     AND sub_number = 1 
# MAGIC group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18 

# COMMAND ----------

max_users_since_ucg = _sqldf.toPandas() 

max_users_since_ucg['start_date'] = pd.to_datetime(max_users_since_ucg['start_date']) 

# COMMAND ----------

# MAGIC %md
# MAGIC ### Create Functions
# MAGIC
# MAGIC * region_filter() determines the user_id's region  
# MAGIC   
# MAGIC * col_plot() for plotting 
# MAGIC   
# MAGIC * ucg_sample_checks() plots the UCG percent for the metrics:  
# MAGIC     
# MAGIC   * payment_period, tier_type, payment_provider
# MAGIC   
# MAGIC * ucg_daily_trend()  plots an average daily UCG percent per monthly 
# MAGIC   
# MAGIC ##### NOTE These function names can be improved, I will rename them.  

# COMMAND ----------

# DEFINE 
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
            .assign(metric = metric)
            ) 

    return df

# COMMAND ----------

# run this cell to get emea countries 

# see all the countires 
# max_users_since_ucg['market'].unique() 

latam_countries = ['TG_MARKET_BRAZIL', 'TG_MARKET_COSTA_RICA', 'TG_MARKET_MEXICO','TG_MARKET_COLOMBIA', 'TG_MARKET_PERU', 'TG_MARKET_CHILE',
                    'TG_MARKET_ARGENTINA', 'TG_MARKET_JAMAICA', 'TG_MARKET_GUATEMALA','TG_MARKET_ECUADOR', 'TG_MARKET_HONDURAS', 'TG_MARKET_URUGUAY',
                    'TG_MARKET_DOMINICAN_REPUBLIC','TG_MARKET_BOLIVIA', 'TG_MARKET_NICARAGUA', 'TG_MARKET_PANAMA',
                    'TG_MARKET_EL_SALVADOR', 'TG_MARKET_CURACAO', 'TG_MARKET_PARAGUAY','TG_MARKET_BAHAMAS', 'TG_MARKET_VENEZUELA',
                    'TG_MARKET_CAYMAN_ISLANDS', 'TG_MARKET_TRINIDAD_AND_TOBAGO','TG_MARKET_ST_KITTS_AND_NEVIS', 'TG_MARKET_ARUBA',
                    'TG_MARKET_ST_LUCIA', 'TG_MARKET_BELIZE', 'TG_MARKET_BARBADOS','TG_MARKET_GRENADA', 'TG_MARKET_GUYANA',
                    'TG_MARKET_ST_VINCENT_AND_GRENADINES', 'TG_MARKET_ANGUILLA','TG_MARKET_TURKS_AND_CAICOS', 'TG_MARKET_SURINAME',
                    'TG_MARKET_HAITI', 'TG_MARKET_DOMINICA', 'TG_MARKET_BRITISH_VIRGIN_ISLANDS','TG_MARKET_ANTIGUA_AND_BARBUDA', 'TG_MARKET_MONTSERRAT']

# get the list of emea countries 
(max_users_since_ucg
    .loc[max_users_since_ucg['market'].notna()]
    .query("market not in @latam_countries & market != 'TG_MARKET_UNITED_STATES'")
    .market
    .unique().tolist()) 

# COMMAND ----------

# MAGIC %md
# MAGIC NOTE: 
# MAGIC * The region_filter() function in the cell below needs to be checked when EMEA is added.
# MAGIC   * The Pandas now excludes the US and LATAM countires, and this should work fine for EMEA 
# MAGIC   * However, if another region is added this function will need to be updated

# COMMAND ----------

# DEFINE region_filter()

def region_filter(df, region): 

    latam_countries = ['TG_MARKET_BRAZIL', 'TG_MARKET_COSTA_RICA', 'TG_MARKET_MEXICO','TG_MARKET_COLOMBIA', 'TG_MARKET_PERU','TG_MARKET_CAYMAN_ISLANDS',
                        'TG_MARKET_ARGENTINA', 'TG_MARKET_JAMAICA', 'TG_MARKET_GUATEMALA','TG_MARKET_ECUADOR', 'TG_MARKET_HONDURAS', 'TG_MARKET_URUGUAY',
                        'TG_MARKET_DOMINICAN_REPUBLIC','TG_MARKET_BOLIVIA', 'TG_MARKET_NICARAGUA', 'TG_MARKET_PANAMA','TG_MARKET_ANTIGUA_AND_BARBUDA', 
                        'TG_MARKET_CURACAO', 'TG_MARKET_PARAGUAY','TG_MARKET_BAHAMAS', 'TG_MARKET_VENEZUELA','TG_MARKET_BRITISH_VIRGIN_ISLANDS',
                        'TG_MARKET_TRINIDAD_AND_TOBAGO','TG_MARKET_ST_KITTS_AND_NEVIS', 'TG_MARKET_ARUBA','TG_MARKET_ST_LUCIA', 'TG_MARKET_BELIZE',
                         'TG_MARKET_BARBADOS','TG_MARKET_GRENADA', 'TG_MARKET_GUYANA', 'TG_MARKET_ANGUILLA','TG_MARKET_ST_VINCENT_AND_GRENADINES',
                        'TG_MARKET_TURKS_AND_CAICOS', 'TG_MARKET_SURINAME','TG_MARKET_DOMINICA', 'TG_MARKET_EL_SALVADOR', 'TG_MARKET_MONTSERRAT'
                        'TG_MARKET_HAITI', 'TG_MARKET_CHILE']   

    emea_countries = (df
                        .loc[df['market'].notna()]
                        .query("market not in @latam_countries & market != 'TG_MARKET_UNITED_STATES'")
                        .market
                        .unique().tolist())     

    if region == 'US':
        df = df.query("market == 'TG_MARKET_UNITED_STATES'") 

    elif region == 'LATAM':
        df = df.query("market in @latam_countries & start_date >= '2024-02-27'")
        
    elif region == 'EMEA':
        df = df.query("market in @emea_countries & start_date >= '2024-02-27'") # NOTE need to update start_date
        
    return df

# COMMAND ----------

# CHECK 
(max_users_since_ucg
    .groupby(['market'])   # market != 'TG_MARKET_UNITED_STATES' 
    .agg(user_id_count  = ('user_id', 'count'))
    .reset_index()
    .sort_values('user_id_count', ascending=False))  

# COMMAND ----------

# DEFINE col_plot()
def col_plot(df, x, y, fill, **kwargs):
    
    if 'position' in kwargs:
        position = kwargs.get('position')    
        plt = (ggplot(df) + 
               geom_col(position=position) + 
               aes(x = x, y = y, fill = fill)) 
    else: 
        plt = (ggplot(df) + 
               geom_col() + 
               aes(x = x, y = y, fill = fill)) 
    
    if 'facet' in kwargs:
        facet_col = kwargs.get('facet')    
        plt = (plt + facet_wrap(facet_col, nrow=1))        

    if 'text' in kwargs:
        text = kwargs.get('text')    
        plt = (plt + geom_text(aes(label = text), position = position_dodge(width = 1), format_string="{:.1f}%", size = 10, va="bottom"))  # nudge_y = 5)) 
    
    return plt 

# COMMAND ----------

# DEFINE ucg_dimension_sample_checks() 

def ucg_dimension_sample_checks(df, region, vars): 

    # df is the dataframe 
    # region is the region 
    
    for v in vars: 
        group_vars      = ['start_date', 'is_longterm_ucg'] 
        group_vars_plus = group_vars + [v]

        df_n = region_filter(df, region)       

        df_base = (df_n
                    .groupby(group_vars_plus)
                    .agg(user_id_count = ('user_id', 'count'))
                    .reset_index())

        join_vars = ['start_date'] + [v]
        df_agg = (df_base
                    .groupby(join_vars)
                    .agg(user_id_count_total = ('user_id_count', 'sum'))
                    .reset_index())

        df_output = (pd.merge(df_base,
                                df_agg, 
                                on = join_vars) 
                        .assign(daily_ucg_percent = lambda x: (100 * (x['user_id_count'] / x['user_id_count_total'])).round(3)))

        df_output['start_date'] = pd.to_datetime(df_output['start_date']) 
        df_output['start_year_month'] = df_output['start_date'].dt.strftime('%Y_%m')             

        df_5_num_sum = five_num_sum_by_group(df_output, 
                                            ['is_longterm_ucg'] + [v], 
                                            'daily_ucg_percent')  
                                                                             
        plot = (col_plot(df_5_num_sum, 'is_longterm_ucg', 'mean', 'is_longterm_ucg', 
                         facet = "~ {var}".format(var = v),
                         text = 'mean') + 
                geom_errorbar(aes(x="is_longterm_ucg", ymin="mean-std", ymax="mean+std")) + 
                scale_y_continuous(labels = lambda l: ["%d%%" % (v * 1) for v in l], 
                                limits = [0, 100]) + 
                theme(figure_size = (16, 5),
                    legend_position='bottom') + 
                labs(title= "{region} Long Term UCG Average Percent by {var}".format(region = region, var = v.replace("_", " ").title()),
                     x='')) 

        print(plot) 

# COMMAND ----------

# DEFINE ucg_sample_checks() 
 
def ucg_monthly_trend(df, region): 

    df = region_filter(df, region)    

    df['start_date'] = pd.to_datetime(df['start_date'], format='%d/%m/%y')
    df['start_year_month'] = df['start_date'].dt.strftime('%Y_%m')
    df['today'] = date.today() # - timedelta(days=1)

    df = (df.query("start_date < today")) 

    start_date_ucg_counts  = (df
                                .groupby(['start_year_month', 'start_date', 'is_longterm_ucg'])   
                                .agg(user_id_count  = ('user_id', 'count'))
                                .reset_index()) 

    start_date_ucg_percents = (start_date_ucg_counts
                                .assign(daily_user_id_count = (start_date_ucg_counts.groupby('start_date')['user_id_count'].transform('sum')),
                                        daily_ucg_percent   = lambda x: ((100 * x['user_id_count'] / x['daily_user_id_count'])).round(2))) 
        
    start_year_month_ucg_counts  = (start_date_ucg_percents
                                    .groupby(['start_year_month', 'is_longterm_ucg'])   
                                    .agg(avg_daily_ucg_percent = ('daily_ucg_percent', 'mean'))
                                    .reset_index()) 

    plot = (col_plot(start_year_month_ucg_counts, 'start_year_month', 'avg_daily_ucg_percent', 'is_longterm_ucg', 
                     text = 'avg_daily_ucg_percent', 
                     position = 'dodge') + 
            scale_y_continuous(labels = lambda l: ["%d%%" % (v * 1) for v in l], 
                               limits = [0, 100]) + 
            theme(figure_size = (16, 5),
                  legend_position = 'bottom') + 
            labs(title = "{region} Long Term UCG Percent by Start Date".format(region = region),
                 x = ''))

    print(plot) 

# COMMAND ----------

# MAGIC %md
# MAGIC ### US Long Term UCG
# MAGIC
# MAGIC NOTE: US uses a 10% sampling rate for long-term UCG.
# MAGIC   
# MAGIC We are seeing percents around 9.4% because the UCG sample is done overall and we use some conditions.

# COMMAND ----------

ucg_monthly_trend(max_users_since_ucg, 'US') 

# COMMAND ----------

vars = ['payment_period', 'tier_type', 'payment_provider'] 

ucg_dimension_sample_checks(max_users_since_ucg, 'US', vars) 

# COMMAND ----------

# MAGIC %md
# MAGIC ### LATAM Long Term UCG
# MAGIC
# MAGIC NOTE: LATAM uses a 5% sampling rate for long-term UCG.
# MAGIC   
# MAGIC We are seeing percents around 4.5% because the UCG sample is done overall and we use some conditions.

# COMMAND ----------

ucg_monthly_trend(max_users_since_ucg, 'LATAM') 

# COMMAND ----------

vars = ['payment_period', 'tier_type', 'payment_provider', 'subscribed_in_territory'] 

ucg_dimension_sample_checks(max_users_since_ucg, 'LATAM', vars) 
