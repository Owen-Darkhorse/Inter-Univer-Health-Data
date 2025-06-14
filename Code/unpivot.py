import os
import pandas as pd
import numpy as np
# -*- coding: utf-8 -*-

def unpivotMetric(df, metricName, idCol="HHIDPN"):
    """
    Helper function: unpivot a metric from wide to long format.
    
    Parameters:
    df (DataFrame): The DataFrame containing the metric.
    metricName (str): The name of the metric to unpivot, in regex pattern.
    idCol (str): The identifier column name.
    
    Returns:
    DataFrame: The unpivoted DataFrame.
    """
    metricName = metricName.replace("Rw", "R\\d+")
    idx = df.filter(items = ["HHIDPN"])
    df = df.filter(regex = metricName+"$")
    df = pd.concat([idx, df], axis=1)
    # print(df.columns[1:])

    valueNames = df.columns[df.columns.str.contains(metricName)].to_list()
    metricNewName = metricName.replace("R\\d+", "Rw")
    df = df.melt(id_vars=[idCol], value_vars=valueNames, 
                 var_name="Wave", value_name=metricNewName)
    
    df["Wave"] = df["Wave"].str.extract(r'(\d+)').astype(int)  # Extract the wave number from the column name
    df[idCol] = df[idCol].astype(int)
    df.sort_values(by=[idCol, "Wave"], inplace=True)
    # df[metricNewName].to_csv(fileNameWave, mode='a', header=False, index=False) # append to the end of the file    
    return df

def joinMetrics(df, metrics, fileNameWave, idCol="HHIDPN"):
    """
    Helper function: join multiple metrics in multiple waves into a single DataFrame.
    
    Parameters:
    df (DataFrame): The DataFrame containing the metrics.
    metrics (list): List of metric names to join.
    idCol (str): The identifier column name.

    Returns: None 
    Side Effects: 
    Write a joined DataFrame to a CSV file with all metrics for each wave.
    The DataFrame contains the same id column that enables them to be joined.
    """
    unique_ids = df[idCol].astype(int).unique()
    joinedDf = pd.DataFrame({
        "HHIDPN": np.repeat(unique_ids, 16),
        "Wave": np.tile([i for i in range(1, 17)], len(unique_ids))
    })

    # print(joinedDf[["HHIDPN", "Wave"]].dtypes)

    joinedDf.sort_values(by=[idCol, "Wave"], inplace=True)
    joinedDf.to_csv(fileNameWave, index=False)
    
    # joinedDf[idCol] = joinedDf[idCol].astype(int)  

    for metric in metrics:
        unpivoted = unpivotMetric(df, metric)
        # print(unpivoted.info())
        # unpivoted.to_csv(fileNameWave, mode='a', header=False, index=False) # append to the end of the file
        joinedDf = pd.merge(joinedDf, unpivoted, ## linking table
                            on=["HHIDPN", "Wave"], 
                            how="left")
        # joinedDf = pd.concat([joinedDf, unpivoted], axis=1, ignore_index=True)
        joinedDf.to_csv(fileNameWave, index=False) # append to the end of the file
        
    return None


def selectMetricforAll(df, metricNames, fileNameAll, idCol="HHIDPN"):
    """
    Helper function: select variables not specific any single wave as well as the id column
    
    Parameters:
    df (DataFrame): The DataFrame containing the metrics.
    metricsNames (list): List of metric names to join.
    idCol: the identfier column of each individual

    Returns:
    DataFrame: The joined DataFrame with metrics not specific to any single wave
    """
    targetDf = df[metricNames]
    targetDf.dropna(axis = 0, how = "all", inplace = True)
    targetDf = pd.concat([df[[idCol]], targetDf], axis=1)
    targetDf.to_csv(fileNameAll, index=False)
    return None


def unPivotSelectedVars(df, metricNames, fileNameAll, fileNameWave, idCol="HHIDPN"):
    """
    A wrapper function that calls helper functions, converting wide data to long format for selected metrics.
    This function separates metrics that are not specific to any single wave from those that are specific to every wave.
    It returns two data frames: one for metrics not specific to any single wave and another for metrics specific to every wave.
        

    Parameters:
    df (DataFrame): The DataFrame containing the metrics.
    metricNames (list): List of metric names to join, case sensitive.
    fileNameAll: file name for the data frame containing metrics not specific to any single wave
    fileNameWave:file name for the data frame containing metrics specific to any single wave
    idCol: the identfier column of each individual

    Return: None 

    Side Effects:
    Write two data frames: 
    the first data frame contains metrics not specific to any single wave, 
    the second data frame contains metric specific to every wave.
    Each data frame contains the same id column that enables them to be joined
    """
    
    ## Select all columns names that starts with RA
    varAll = [s for s in metricNames if s.startswith("RA")]
    selectMetricforAll(df, varAll, fileNameAll)

    ## Select all columns names that starts with Rw
    varWaves =  [s for s in metricNames if s.startswith("Rw")]
    joinMetrics(df, varWaves, fileNameWave)

    return None

# Example:
## Specifiy variable categories
# idx = ["HHIDPN"]
# cogniScore = [
#     "RwSTATUS", ## 1=cognitive measure is provided, 2=interview by proxy, w=not responded
#     "RwIMRC", ## Immediate word recall score
#     "RwDLRC", # delayed word recall
#     "RwSER7", # serial 7s score
#     "RwVOCAB", # vocabulary score, categorical, correct, incorrect, or partially correct
#     "RwBWC86", # counting backwards from 86
#     "RwBWC20", # counting backwards from 20
#     "RwTR20", ## recall index summary
#     "RwMSTOT", # mental status index summary
#     "RwLOST", # cognitive impairment: getting lost
#     "RwWANDER", # cognitive impairment: wandering off
#     "RwALONE", # cognitive impairment: left alone
#     "RwHALUC", # cognitive impairment: getting lost
# ]

# demographics = ["RAGENDER", # Reference person gender
#                 "RARACEM", # Reference person race
#                 "RwCENREG", # Reference person census region
#                 "RAEDYRS", # Reference person education years
#                 "RwMSTAT", # Reference person marital status
#                 "RwAGEM_B", #W1 R Age (months) at Ivw BegMonth
#                 "RwLIVBRO", # Reference person number of siblings
#                 "RAEVBRN"] # Reference person number of children ever born]

# employmentVars = [
#     "RwWORK", # W1 R working for pay
#     "RwJHOURS", # W1 Hours worked/week main job
#     "RwWGIHR", # W1 Imputed Wage Rate-Hrly
#     "RwJCIND", # Industru code for the current job
#     "RwJCOCC", # W1 Current Job Occup/1980 Census
#     "RwJPHYS", # W1 Cur job req lots phys effort Categ
#     "RwJLIFT", # W1 Cur job req lift heavy loads
#     "RwJSTRES", # W1 Cur job involves much stress
#     "RwJSTOOP", # W1 Cur job req stoop/kneel/crouch
#     "RwJSIGHT", # W1 Cur job req good eyesight
# ]

# interestVars =  demographics + cogniScore + employmentVars

# ## Modifiable data path
# workingDir = os.getcwd()
# hrsFullPath = os.path.join(workingDir,'..', 'Data', 'randhrs1992_2022v1.sas7bdat')

# ## Due to big file size, only select the first 10000 rows as the sample for EDA
# hrsFull = pd.read_sas(hrsFullPath, chunksize=10000)

# ## Get only thef first chunk of the data for EDA
# hrsFull = next(hrsFull)
# print(hrsFull.shape)

# ## Unpivot all interest variables and save them into two data frames
# unPivotSelectedVars(hrsFull, interestVars, 'joinedAll10kRows.csv', 'joinedWaves10kRows.csv')