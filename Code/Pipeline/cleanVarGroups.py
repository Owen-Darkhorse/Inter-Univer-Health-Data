## Tranforms variables into different scales, types
## Do this in bacthes
## This is a class of operations
import pandas as pd
import numpy as np
import Pipeline.occupationLookUp as occLookUp

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
    
    def groupOccupations(self):
        """
        Creates the new column in self.Wave called "RwJOCCSD", with all occupations 
        Drop the old occupation column list "RwOCC" if it exists.
        Relabel occupation variables based on a mapping dictionary.
        
        Parameters:
        occupationMap (dict): A dictionary mapping old occupation codes to new labels.
        """
        ## Map numbers to occupation names in different census years
        self.Wave["RwJCOCC"] = self.Wave["RwJCOCC"].map(occLookUp.occupation_1980)
        self.Wave["RwJCOCCA"] = self.Wave["RwJCOCCA"].map(occLookUp.occupation_ahead)
        self.Wave["RwJCOCCB"] = self.Wave["RwJCOCCB"].map(occLookUp.occupation_2000)
        self.Wave["RwJCOCCC"] = self.Wave["RwJCOCCC"].map(occLookUp.occupation_2010)
        
        ## Map occupation codes to standardized groups
        allOccs = ["RwJCOCC", "RwJCOCCA", "RwJCOCCB", "RwJCOCCC"]
        self.Wave[allOccs] = self.Wave[allOccs].apply(
            lambda x: x.map(occLookUp.codeToGroup),
            axis=0
        )
        print("Occupation codes across waves are:")
        print(self.Wave[allOccs].head(10))

        ## Take the most common occupation grouping across all waves as the final occupation
        self.Wave["RwJOCCSD"] = self.Wave[allOccs].apply(lambda x: x.mode()[0] if not x.mode().empty else np.nan, 
                                                         axis=1) 
        # print(self.Wave["RwJOCCSD"].head(10))      
        # # ## Drop the old occupation columns if they exist
        self.Wave.drop(columns=allOccs, inplace=True)
                
        return None

    def combineColumns(self, newColName, colList, method = "all", combFunc = None):
        """
        Combine multiple columns into a single column by taking OR or AND operation.
        
        Parameters:
        newColName (str): The name of the new column to be created.
        colList (list): A list of column names to be combined.
        method: a str, "all" or "any"
        combFunc: a lambda function to be applied to every row of colList

        Note:
        Only one of method and combFunc should be non null
        """
        if (method not in ["all", "any"]) and (combFunc is None):
            raise ValueError("Method is not 'all' or 'any', or combFunc is None.")
        elif method == "all":
            self.Wave[newColName] = self.Wave[colList].all(axis=1)
        elif method == "any":
            self.Wave[newColName] = self.Wave[colList].any(axis=1)
        else:
            self.Wave[newColName] = self.Wave[colList].apply(combFunc, axis = 0)
        
        self.Wave.drop(columns = colList, inplace = True)
        return None
    
    def extractNonNull(self, how):
        """
        Extract individuals with non-null cognition variables, RwTR20 and ReMSTOT, or non-null cognitive impairement
        For individyuals with part of their cognitive status null,  
        It imputes missing values with forward fill and back fill
        
        Returns: none
        Mutate: self.Wave
        """
        non_null_response = (
            self.Wave[['RwTR20', 'RwMSTOT']].notnull().all(axis=1) |
            self.Wave[["RwLOST", "RwWANDER", "RwALONE", "RwHALUC"]].notnull().all(axis=1)
        )
        self.Wave = self.Wave[non_null_response]
        

    def dealNAs(self, varList, fillValue=0):
        """
        Deal with missing values in the specified variables.
        
        Parameters:
        varList (list): A list of variable names to handle missing values for.
        fillValue: The value to fill in for missing values. Default is 0.
        """
        self.Wave.fillna(method='ffill', axis = 0, inplace = True)
        self.Wave.fillna(method='bfill', axis = 0, inplace = True)
        
        return None
    