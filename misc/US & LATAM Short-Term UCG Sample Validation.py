# Databricks notebook source
# MAGIC %md
# MAGIC ## Checking the Samples Sizes for the US SHORT-TERM UCG Daily Cohorts  
# MAGIC
# MAGIC

# COMMAND ----------



# COMMAND ----------

# MAGIC %sql
# MAGIC select 
# MAGIC         user_id,
# MAGIC         global_subscription_id,        
# MAGIC         sub_type, 
# MAGIC         -- renewal_ts,
# MAGIC         -- next_renewal_ts
# MAGIC         -- period_end_ts,
# MAGIC         -- period_start_ts, 
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
# MAGIC     LEFT JOIN bolt_dai_martech_prod.gold.ucg_sampling_vw b --short-term ucg (for existing subs) 
# MAGIC         ON a.user_id = b.userid 
# MAGIC     WHERE batch_date >= '2023-06-11' -- add these 
# MAGIC     and DATE(start_ts) < date(batch_date) -- existing users 
# MAGIC     AND market = 'TG_MARKET_UNITED_STATES'  
# MAGIC     AND payment_period = 'PERIOD_MONTH' 
# MAGIC     AND retail_ind = 'true'        
# MAGIC     AND subs_valid_ind = 1 
# MAGIC     and closing_active_ind = 1
# MAGIC group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 
# MAGIC limit 100 

# COMMAND ----------



# COMMAND ----------



# COMMAND ----------



# COMMAND ----------


