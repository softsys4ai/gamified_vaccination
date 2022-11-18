import pandas as pd
import seaborn as sns
import os
# from string import ascii_letters
import numpy as np
import matplotlib.pyplot as plt

os.chdir("G:/My Drive/Research/UofSC Research/UofSC Research")
print(os.getcwd())



df = pd.read_csv("Data/df_complete.csv")

print(df.columns)


#% Correlation Plot 

sns.set_theme(style="white")
df = df[["HAP_PRO", "ANG_PRO", "EANX_PRO", "SKP_PRO", 
                            "POLSUP_AVG", "KNOW_AVG", "SHARINTENT", 
                            "VAXINTENT"]]

colnames = ["Happiness", "Anger", "Anxiety", "Skepticism", 
                        "Policy Support", "Knowledge", "Game Sharing", 
                        "Vaccine Intent"]


df.set_axis(colnames, axis=1, inplace=True)

d = df
# Compute the correlation matrix
corr = d.corr()


# Generate a mask for the upper triangle
mask = np.triu(np.ones_like(corr, dtype=bool))
# sns.set(font_scale=1.5)

# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))

# Generate a custom diverging colormap

cmap = sns.color_palette("RdBu",  311) 

# Draw the heatmap with the mask and correct aspect ratio

sns.heatmap(corr,  mask=mask, 
            cmap=cmap, vmax=1 , vmin = -1,  center=0,
            square=True, linewidths=.5,
            cbar_kws = dict(use_gridspec=True, location="top",
                                                       shrink = .3))

# plt.legend(loc='upper center')


plt.savefig('correlation.pdf', format = 'pdf', 
              dpi=720)









