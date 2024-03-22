# Databricks notebook source
pip install plotnine==0.12.4

# COMMAND ----------

pip install matplotlib==3.7.1

# COMMAND ----------

dbutils.library.restartPython() 

# COMMAND ----------

from plotnine import *
from plotnine.data import mtcars
import matplotlib 

# COMMAND ----------

pip show matplotlib 

# COMMAND ----------

pip show plotnine 

# COMMAND ----------

(ggplot(mtcars, aes("wt", "mpg")) + 
 geom_point())
