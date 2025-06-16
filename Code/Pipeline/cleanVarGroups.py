## Tranforms variables into different scales, types
## Do this in bacthes
## This is a class of operations
import pandas as pd
import numpy as np

class hrsSet:
    def __init__(self, hrsAll, hrsWave):
        '''
        hrsAll is a DataFrame containing all variables that are not specific to any single wave.
        hrsWave is a DataFrame containing variables that are specific to every wave.
        '''
        self.All = hrsAll
        self.Wave = hrsWave

    def logFinance(self, financeVars):
        """
        Transform financial variables to log scale.
        """    
        for var in financeVars:
            if var in self.Wave.columns:
                self.Wave[var] = self.Wave[var].apply(lambda x: 
                                                      np.log1p(np.add(x, 1)) 
                                                      if pd.notnull(x) else x)
        
        return None


    def standardizeAge(self, ageVars):
        """
        Standardize age variables.
        """
        for var in ageVars:
            if var in self.Wave.columns:
                self.Wave[var] = (self.Wave[var] - 65) / 12   
        return None
    
    def discretize(self, mapper, varList):
        '''
        Discretize continuous or discrete variables of many values into a simpler value structure.

        Parameters:
        mapper (dict): A lambda function that maps the original values to new values.
        varList (list): A list of variable names to be discretized.
        '''
        for var in varList:
            if var in self.Wave.columns:
                self.Wave[var] = self.Wave[var].map(mapper)
                self.Wave[var] = self.Wave[var].astype('category')


    def castType(self, varList, dtype):
        """
        Cast the type of variables in varList to the specified dtype.
        
        Parameters:
        varList (list): A list of variable names to be casted.
        dtype: The target data type to cast the variables to.
        """
        for var in varList:
            if var in self.Wave.columns:
                self.Wave[var] = self.Wave[var].astype(dtype)
        
        return None
    
    def rewriteOccupations(self, occupationMap, occVarList):
        """
        Creates the new column in self.Wave called "RwJOCCSD", with all occupations 
        Drop the old occupation column list "RwOCC" if it exists.
        Relabel occupation variables based on a mapping dictionary.
        
        Parameters:
        occupationMap (dict): A dictionary mapping old occupation codes to new labels.
        """
        self.Wave["RwJOCCSD"] = self.Wave[occVarList].map(occupationMap)
        self.Wave.drop(columns=occVarList, inplace=True, errors='ignore')
        
        return None
        
    def dealNAs(self, varList, fillValue=0):
        """
        Deal with missing values in the specified variables.
        
        Parameters:
        varList (list): A list of variable names to handle missing values for.
        fillValue: The value to fill in for missing values. Default is 0.
        """
        for var in varList:
            if var in self.Wave.columns:
                self.Wave[var].fillna(fillValue, inplace=True)
        
        return None
    