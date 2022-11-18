# -*- coding: utf-8 -*-
"""
Created on Fri Sep  9 07:40:10 2022

@author: mprtr
"""

import pandas as pd
import seaborn as sns
import os
# from string import ascii_letters
import numpy as np
import matplotlib.pyplot as plt

os.chdir("G:/My Drive/Research/UofSC Research/UofSC Research")
print(os.getcwd())


f, ax = plt.subplots(figsize=(11, 9))

df = pd.read_csv("Data/timeseries.csv")

df['timestamp'] = pd.to_datetime(df['timestamp'], format = '%Y-%m-%d')

print(df.columns)

df_treatment = df.loc[df['treatment'] == 1]
df_control = df.loc[df['treatment'] == 0]


sns.lineplot(x = 'timestamp', y = 'mean_vaxintent', data = df_treatment, 
             label = "treatment")

sns.lineplot(x = 'timestamp', y = 'mean_vaxintent', data = df_control, 
             label = "control")

plt.ylabel('Mean of Knowledge Scores')
plt.xlabel('Date')
plt.xticks(rotation = 45)
plt.legend()


y_min = df.mean_vaxintent.min()
y_max = df.mean_vaxintent.max()

ax.vlines(x=['2020-11-10', '2020-11-10'],
          ymin=y_min, ymax=y_max, colors='purple', ls='--', 
          lw=2, label='Cutt-Off Point')

#ax.legend(bbox_to_anchor=(1.04, 0.5), loc="upper left")

ax.legend()
plt.savefig('timeseries.pdf', format = 'pdf', 
              dpi=1080)


#%% Multiple Time Series Analysis

f, ax = plt.subplots(figsize=(11, 9))

df = pd.read_csv('Data/df_timestamped.csv')
df['timestamp'] = pd.to_datetime(df['timestamp'], format = '%Y-%m-%d').dt.date


df = df.groupby('timestamp').agg({'Policy_Support_POLSUP_AVG': 'mean', 
                                  'Emotion_proportion_ANG_PRO': 'mean', 
                                  'Emotion_proportion_HAP_PRO': 'mean', 
                                  'True.False_Knowledge_Questions_KNOW_AVG': 'mean', 
                                  'Willingness_Range_VAXINTENT': 'mean'})


sns.lineplot(x = 'timestamp', y = 'Policy_Support_POLSUP_AVG', data = df, 
             label = "Policy Support", color = 'brown')

sns.lineplot(x = 'timestamp', y = 'Willingness_Range_VAXINTENT', data = df, 
             label = "Vaccine Intent", color = 'darkgreen')

sns.lineplot(x = 'timestamp', y = 'Emotion_proportion_ANG_PRO', data = df, 
              label = "Anger Proportion", color='lightblue')
sns.lineplot(x = 'timestamp', y = 'Emotion_proportion_HAP_PRO', data = df, 
              label = "Happiness Proportion", color = 'darkblue')
sns.lineplot(x = 'timestamp', y = 'True.False_Knowledge_Questions_KNOW_AVG', data = df, 
              label = "Knowledge Score", color = 'brown')




plt.ylabel('Score/Proportion Average')
plt.xlabel('Date')
plt.xticks(rotation = 45)
plt.legend()

ax.legend()

plt.savefig('polsup_vaccine.pdf', format = 'pdf', 
              dpi=1080)

#%%  

# f, ax = plt.subplots(figsize=(11, 9))

# sns.lineplot(x = 'timestamp', y = 'Emotion_proportion_ANG_PRO', data = df, 
#               label = "Anger Proportion", color='lightblue')
# sns.lineplot(x = 'timestamp', y = 'Emotion_proportion_HAP_PRO', data = df, 
#               label = "Happiness Proportion", color = 'darkblue')
# sns.lineplot(x = 'timestamp', y = 'True.False_Knowledge_Questions_KNOW_AVG', data = df, 
#               label = "Knowledge Score", color = 'brown')

stacked = df[['Emotion_proportion_ANG_PRO', 'Emotion_proportion_HAP_PRO', 
              'True.False_Knowledge_Questions_KNOW_AVG']]


stacked.plot(kind = 'bar', stacked = True)



# plt.ylabel('Daily Score/Proportion Average')
# plt.xlabel('Date')
# plt.xticks(rotation = 45)
# plt.legend()

# ax.legend()
# plt.savefig('anger_happy_knowledge.pdf', format = 'pdf', 
#               dpi=1080)








