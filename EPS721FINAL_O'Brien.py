#!/usr/bin/env python
# coding: utf-8

# In[4]:


import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt
import seaborn as sns
import openpyxl


# In[3]:


yankees = pd.read_csv("yankees_2000-2026.csv")
yankees.head(10)


# In[4]:


yankees["OPS"] = yankees["OBP"]+ yankees["SLG"].copy()
yankees["Walk_to_K"]= yankees["BB%"] / yankees["K%"].copy()
yankees["HR_efficiency"]=yankees["HR"] / yankees["PA"].copy()
yankees.head(10)


# In[5]:


df= yankees
metrics= ["AVG", "OBP", "OPS", "HR", "K%", "BB%"]
mean_vs_med= pd.DataFrame({'Mean':df[metrics].mean(), 'Median': df[metrics].median()})
print("---Mean vs Median---")
print("\n---Skewness---")
print(df[metrics].skew())


# In[6]:


df= yankees
yankees_descriptives= df[metrics].describe().T[['min', '25%','50%','75%','max','std']]
yankees_descriptives['IQR']=yankees_descriptives['75%']- yankees_descriptives['25%']
print("---Range Descriptives---")
print(yankees_descriptives[['min','max','std','IQR']])


# In[8]:


yankees_temp_PA = yankees[yankees["PA"]>= 100].copy()
yankees_temp_PA.head(5)


# In[9]:


yankees_temp_PA.hist(column="AVG", bins= 20, rwidth=0.9)
plt.show()


# In[10]:


yankees_temp_PA.hist(column="OBP", bins= 20, rwidth=0.9)
plt.show()


# In[11]:


yankees_temp_PA.hist(column="SLG", bins= 20, rwidth=0.9)
plt.show()


# In[12]:


yankees_temp_PA.hist(column="OPS", bins= 20, rwidth=0.9)
plt.show()


# In[13]:


yankees_compare=pd.DataFrame({'Raw Data(All Players)': yankees[["AVG", "OBP","SLG","OPS"]].mean(), 
                             'Threshold(PA>=100)': yankees_temp_PA[["AVG","OBP","SLG","OPS"]].mean()})
print(yankees_compare)


# In[14]:


pa_threshold= 300
yankees_bench= yankees[yankees["PA"] < pa_threshold].copy()
yankees_bench.head(5)


# In[15]:


yankees_starter= yankees[yankees["PA"] >= pa_threshold].copy()
yankees_starter.head(5)


# In[16]:


yankees_starter.describe()


# In[17]:


high_walks= yankees_starter[(yankees_starter['OBP'] >=0.330) & (yankees_starter['AVG']<= 0.250)][['Name','PA','AVG','OBP','BB%', 'HR']].sort_values(by='OBP', ascending= False)
high_walks.head()


# In[18]:


low_strikeout = yankees_starter[(yankees_starter['HR'] >=25) & (yankees_starter['K%']<= 0.15)][['Name','PA','HR','K%','AVG', 'OPS']].sort_values(by='HR', ascending= False)
low_strikeout.head()


# In[19]:


yankees['Role']= np.where(yankees['PA'] >= pa_threshold, 'Starter', 'Bench').copy()
yankees.groupby('Role')[['AVG','OBP','SLG','OPS','HR_efficiency']].mean()


# In[20]:


plt.figure(figsize=(8,5))
sns.barplot(data=yankees, x = 'Role', y='OPS', errorbar=None, hue= 'Role',palette='Blues_d', legend= False)
plt.title('Average OPS comparison between Starter and Bench players')
plt.xlabel('Player Role')
plt.ylabel('OPS')
plt.show()


# In[21]:


yankees_HR = yankees[yankees["HR"]>5].sort_values(by= "HR", ascending = False).reset_index().copy()
yankees_HR.head(10)


# In[22]:


plt.figure(figsize=(10,6))
sns.regplot(data=yankees_HR,x="K%", y="OBP", scatter_kws={"alpha":0.3,"color":"navy"}, 
            line_kws={"color":"purple", "linewidth":2})
plt.title("Strikeout Rate vs On Base Percentage", fontsize=14)
plt.xlabel("Srikeout Rate", fontsize=12)
plt.ylabel("On Base Percentage", fontsize=12)
plt.grid(True, linestyle="--", alpha=0.5)
plt.tight_layout()
plt.show()


# In[23]:


kvsobp= yankees_HR["K%"].corr(yankees_HR["OBP"])
print(f"correlation between K% and OBP:{kvsobp:.3f}")


# In[24]:


kvsobp2=yankees_starter["K%"].corr(yankees_starter["OBP"])
print(f"Correlation of K% + OBP controlled for PA:{kvsobp2:.3f}")


# In[26]:


slgvshr= yankees_starter["HR"].corr(yankees_starter["SLG"])
slgvsavg=yankees_starter["AVG"].corr(yankees_starter["SLG"])
print(f"HR vs SLG Correlation is: {slgvshr:.3f}")
print(f"AVG vs SLG Correlation is:{slgvsavg:.3f}")


# In[27]:


top_hr= yankees.sort_values(by="HR", ascending = False).head(10).copy()


# In[28]:


plt.figure(figsize=(10,6))
sns.barplot(data=top_hr, x = "Name", y= "HR", hue="Name", palette= "Blues_r", legend= False)
plt.title("Top 10 Yankee Home Run Hitters", fontsize=14)
plt.xlabel("Player", fontsize=12)
plt.ylabel("Home Runs", fontsize=12)
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()


# In[29]:


import statsmodels.formula.api as smf
OPS_pred = smf.ols(formula= "R ~ OPS + PA", data= yankees_starter).fit()
print(OPS_pred.summary())


# In[30]:


Plate_pred= smf.ols(formula= "Q('R') ~ Q('BB%') + Q('K%') + AVG + PA", data= yankees_starter).fit()
print(Plate_pred.summary())


# In[2]:


yankees_by_szn= pd.read_csv("yankees_by_szn.csv")
yankees_by_szn.head(20)


# In[13]:


yanks_2009= yankees_by_szn[yankees_by_szn['Season'] == 2009].copy()
rs= yanks_2009['R'].sum()
ra= 753
pyth_win_pct= (rs**2)/ ((rs**2) + (ra**2))
exp_wins= pyth_win_pct * 162
print(f"Estimated 2009 Runs Scored (rs): {rs:.0f}")
print(f"Estimated 2009 Runs Allowed (ra): {ra}")
print(f"Estimated 2009 Pythagorean Win %: {pyth_win_pct:.3f}")
print(f"Estimated 2009 Expected Wins: {exp_wins:.1f}")


# In[8]:


import pandas as pd

season_summary = yankees_by_szn.groupby("Season")["R"].sum().reset_index()
season_summary.rename(columns={"R": "RS"}, inplace=True)

real_ra = {
    2000: 814,
    2001: 713,
    2002: 697,
    2003: 716,
    2004: 808,
    2005: 789,
    2006: 767,
    2007: 777,
    2008: 727,
    2009: 753, 
    2010: 693,
    2011: 657,
    2012: 668,
    2013: 671,
    2014: 664,
    2015: 698,
    2016: 702,
    2017: 660,
    2018: 669,
    2019: 739,
    2020: 270, 
    2021: 669,
    2022: 567,
    2023: 698,
    2024: 668,
    2025: 685,
    2026: 449  
}

season_summary["RA"] = season_summary["Season"].map(real_ra)

season_summary["pyth_win_pct"] = (season_summary["RS"] ** 2) / (
    (season_summary["RS"] ** 2) + (season_summary["RA"] ** 2)
)

season_summary["exp_wins"] = season_summary["pyth_win_pct"] * 162

season_summary["games_played"] = 162
season_summary.loc[season_summary["Season"] == 2020, "games_played"] = 60
season_summary.loc[season_summary["Season"] == 2026, "games_played"] = 118

season_summary["exp_wins"] = season_summary["pyth_win_pct"] * season_summary["games_played"]

season_summary = season_summary.sort_values(by="exp_wins", ascending=False).reset_index(drop=True)

print("--- Best Season (Actual RA) ---")
print(season_summary.head(1))

print("\n--- Full Season Rankings ---")
print(season_summary)


# In[10]:


import matplotlib.pyplot as plt
chronological_szns = season_summary.sort_values("Season")

plt.figure(figsize=(12, 5))
plt.plot(chronological_szns["Season"], chronological_szns["pyth_win_pct"], marker="o", color="navy", linewidth=2)

plt.title("Yankees Pythagorean Win % Trend (2000–2026)", fontsize=14)
plt.xlabel("Season", fontsize=12)
plt.ylabel("Pythagorean Win %", fontsize=12)
plt.axhline(0.500, color="red", linestyle="--", alpha=0.7, label=".500 Baseline")
plt.grid(True, linestyle="--", alpha=0.5)
plt.legend()
plt.tight_layout()
plt.show()


# In[ ]:




