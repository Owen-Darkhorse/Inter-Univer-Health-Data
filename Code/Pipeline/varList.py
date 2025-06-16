idx = ["HHIDPN"]

cogniScore = [
    "RwTR20", ## recall index summary
    "RwMSTOT" # mental status index summary
]

cognImp = [
    "RwLOST", # cognitive impairment: getting lost
    "RwWANDER", # cognitive impairment: wandering off
    "RwALONE", # cognitive impairment: left alone
    "RwHALUC", # cognitive impairment: getting lost
]

employmentVars = [
    "RwWORK", # W1 R working for pay
    "RwJHOURS", # W1 Hours worked/week main job
    "RwWGIHR", # W1 Imputed Wage Rate-Hrly
    "RwJCOCC", # W1 Current Job Occup/1980 Census
    "RwJCOCCA", # W1 Current Job Occup/AHEAD Census
    "RwJCOCCB", # W1 Current Job Occup/2000 Census
    "RwJCOCCC", # W1 Current Job Occup/2010 Census
    "RwJPHYS", # W1 Cur job req lots phys effort Categ --------
    "RwJLIFT", # W1 Cur job req lift heavy loads
    "RwJSTRES", # W1 Cur job involves much stress
    "RwJSTOOP", # W1 Cur job req stoop/kneel/crouch
    "RwJSIGHT", # W1 Cur job req good eyesight
]

demographics = ["RAGENDER", # Reference person gender
                "RARACEM", # Reference person race
                "RwCENREG", # Reference person census region
                "RAEDYRS", # Reference person education years
                "RwMSTAT", # Reference person marital status
                "RwAGEM_B", #W1 R Age (months) at Ivw BegMonth
                "RwLIVBRO", # Reference person number of siblings
                "RAEVBRN", # The number of children ever born
                "HACOHORT"
] # sample cohort] # Reference person number of children ever born]

funcLimits = [
    "RwHLHLM", # W1 R reports health limitation
] ## Any functional limitations

medicalConds = [
    "RwHIBP",  # R1HIBP:W1 R reports high BP this wv
    "RwDIAB", # R1DIAB:W1 R reports diabetes this wv
    "RwCANCR",  # R1CANCR:W1 R reports cancer this wv
    "RwLUNG",   # R1LUNG:W1 R reports lung disease this wv
    "RwHEART",  # R1HEART:W1 R reports heart disease this wv
    "RwSTROK",  # R1STROK:W1 R reports stroke this wv
    "RwPSYCH",  # R1PSYCH:W1 R reports psych condition this wv
    "RwARTHR"   # R1ARTHR:W1 R reports arthritis this wv
]

financials = [
    "RwEARN", # Individual earnings
    "RwITOT", # Total household income
    "RwATRAN", # Net value of viehicles
    "RwAHOUS", # Net value of house
    "RwACHCK" # Net value of checking accounts
] # To be mapped to log scale

healthBehaviors = [
    "RwHSWRRF", # W1 frequency of doing heavy housework
    "RwVIGACT", # W1 frequency of doing vigorous activities over 3 times a week
    "RwLTACTF", # Frequency of doing light activities
    "RwSMOKEV", # W1 R smoking status
    "RwDRINK", # W1 R alcohol consumption
] # such as doing chores, exercises

interestVars = cogniScore + cognImp + employmentVars + demographics + funcLimits + medicalConds + financials + healthBehaviors

import pyreadstat as pr

hrsFull = pr.pyreadstat.read_sas7bdat(
    filename_path = "../../Data/randhrs1992_2022v1.sas7bdat",
    # usecols = interestVars,
    row_limit = 1
)[0]

import gc

# Read the first row of the dataset to get the column names
hrsColumns = hrsFull.columns.to_list()
del hrsFull
gc.collect()

# print(hrsColumns[0:6])

## Now, we want to match all columns that starts with RA and Rw
import re

# Variables specific to single waves
varWave = ["R(A|E|\\d+)"+s[2:] + "$" for s in interestVars]

# if s.startswith("Rw")

def selectMetricforAll(varWave, hrsColumns):
    """
    Select all columns from df that match the variable names in varList
    and save to a file.
    """
    ## Create a list of variables in with their waves specified
    varComplete = ["HHIDPN"] # Initialize with HHIDPN

    for var in varWave:
        matched = [col for col in hrsColumns if re.match(var, col)]
        varComplete.extend(matched)
        # print(f"{var}: {matched[:5]} ... ({len(matched)} matches)")

    return varComplete

varComplete = selectMetricforAll(varWave, hrsColumns)

print(varComplete[:10])