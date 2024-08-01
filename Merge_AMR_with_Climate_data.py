#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np


# In[10]:


data = pd.read_csv("D:/AMR/climate_change_data.csv/climate_change_data.csv")


# In[13]:


import pandas as pd

# Create DataFrame
df = pd.DataFrame(data)

# Convert the date_time column to datetime type
df['Date'] = pd.to_datetime(df['Date'])

# Extract day, month, year, and time
df['day'] = df['Date'].dt.day
df['month'] = df['Date'].dt.month
df['Year'] = df['Date'].dt.year
df['time'] = df['Date'].dt.time

print(df)


# In[15]:


# Write the DataFrame to a CSV file
df.to_csv('D:/AMR/climate_change_data.csv/edited_climate.csv', index=False)


# In[10]:


import pandas as pd

# Load your datasets
amr_data = pd.read_csv("D:/AMR/2024_05_28 atlas_antibiotics.csv")  
weather_data = pd.read_csv("D:/AMR/global weather data/edited_weather_dataframe.csv")  

# Aggregate weather data by State and Year 
weather_data_agg = weather_data.groupby(['State','Year']).mean()

# Merge the datasets on State and Year
merged_data = pd.merge(amr_data, weather_data_agg, on=['State', 'Year'], how='left')

# Save the merged dataset to a new CSV file
merged_data.to_csv('D:/AMR/merged_dataset.csv', index=False)


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:




