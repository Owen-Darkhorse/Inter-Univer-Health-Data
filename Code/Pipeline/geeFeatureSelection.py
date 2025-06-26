import re
import statsmodels.api as sm
import statsmodels.formula.api as smf
import pandas as pd
import numpy as np


def stepwise_selection_qic(data, groups, cov_struct, family, start_formula, end_formula, verbose=True, qic_threshold=0):
    """
    Perform stepwise (forward and backward) feature selection based on QIC for GEE models.

    Parameters:
        data: pandas.DataFrame
            The dataset containing all variables used in the formulas.
        groups: array-like
            Grouping variable for GEE (e.g., subject or cluster IDs).
        cov_struct: statsmodels.genmod.cov_struct.CovStruct
            Covariance structure for GEE (e.g., Exchangeable, Autoregressive).
        family: statsmodels.genmod.families.Family
            The family object for GEE (e.g., Gaussian, Binomial).
        start_formula: str
            The starting model formula (patsy syntax).
        end_formula: str
            The full model formula (patsy syntax, includes all candidate variables).
        verbose: bool, optional
            If True, prints progress at each step.
        qic_threshold: float, optional
            Minimum QIC improvement required to continue selection.

    Returns:
        best_formula: str, formula of the best model found
        best_result: fitted GEE result object
        history: list of (formula, QIC)
    """
    import statsmodels.formula.api as smf

    def get_terms(formula):
        rhs = formula.split('~')[1]
        terms = [t.strip() for t in re.split(r'\s*\+\s*', rhs) if t.strip() != '']
        terms = [t for t in terms if t != '1']
        return set(terms)

    def build_formula(lhs, terms):
        if not terms:
            return f"{lhs} ~ 1"
        return f"{lhs} ~ {' + '.join(sorted(terms))}"

    def calc_qic(result):
        try:
            return result.qic()
        except Exception:
            return np.nan

    lhs = start_formula.split('~')[0].strip()
    start_terms = get_terms(start_formula)
    end_terms = get_terms(end_formula)
    current_terms = set(start_terms)
    history = []

    # Fit initial model
    current_formula = build_formula(lhs, current_terms)
    model = smf.gee(current_formula, groups=groups, data=data, cov_struct=cov_struct, family=family)
    result = model.fit(cov_type="robust")
    best_qic = calc_qic(result)
    best_formula = current_formula
    best_result = result
    history.append((current_formula, best_qic))

    improved = True
    while improved:
        improved = False
        # Forward step
        qic_candidates = []
        formulas = []
        term_changes = []
        for term in sorted(end_terms - current_terms):
            new_terms = current_terms | {term}
            formula = build_formula(lhs, new_terms)
            try:
                model = smf.gee(formula, groups=groups, data=data, cov_struct=cov_struct, family=family)
                result = model.fit(cov_type="robust")
                qic = calc_qic(result)
            except Exception:
                qic = np.nan
            qic_candidates.append(qic)
            formulas.append(formula)
            term_changes.append(('add', term))

        # Backward step
        for term in sorted(current_terms - start_terms):
            new_terms = current_terms - {term}
            formula = build_formula(lhs, new_terms)
            try:
                model = smf.gee(formula, groups=groups, data=data, cov_struct=cov_struct, family=family)
                result = model.fit(cov_type="robust")
                qic = calc_qic(result)
            except Exception:
                qic = np.nan
            qic_candidates.append(qic)
            formulas.append(formula)
            term_changes.append(('remove', term))

        if qic_candidates:
            min_idx = np.nanargmin(qic_candidates)
            min_qic = qic_candidates[min_idx]
            if (best_qic - min_qic) > qic_threshold:
                improved = True
                best_qic = min_qic
                best_formula = formulas[min_idx]
                action, term = term_changes[min_idx]
                if action == 'add':
                    current_terms.add(term)
                elif action == 'remove':
                    current_terms.remove(term)
                model = smf.gee(best_formula, groups=groups, data=data, cov_struct=cov_struct, family=family)
                best_result = model.fit(cov_type="robust")
                history.append((best_formula, best_qic))
                if verbose:
                    print(f"Step: {action}, QIC: {best_qic:.2f}, Formula: {best_formula}")
            else:
                if verbose:
                    print("No QIC improvement above threshold, stopping.")
        else:
            if verbose:
                print("No candidates left, stopping.")

    return best_formula, best_result, history

## Test Cases
hrsWave = pd.read_csv("../Data/hrsWaveCleaned.csv")

n = 1000
hhidpn = np.random.choice(hrsWave["HHIDPN"].unique(), size = n)
idx = hrsWave["HHIDPN"].isin(hhidpn)
df = hrsWave.loc[idx, :]

controlVars = ['RwWORK', 'RwJHOURS', 'RwWGIHR',
       # 'RwJPHYS', 'RwJLIFT', 'RwJSTRES', 'RwJSTOOP', 'RwJSIGHT', 'RwCENREG',
       # 'RwMSTAT', 'RwLIVBRO', 'RwHIBP', 'RwDIAB', 'RwCANCR',
       # 'RwLUNG', 'RwHEART', 'RwSTROK', 'RwPSYCH', 'RwVIGACT', 'RwSMOKEV',
       # 'RwDRINK', 'RwPhyLim', 'RwCogLim', 'RwAnyCogImp', 'RwLOST',
       # 'RwWANDER', 'RwHALUC', 'RwALONE', 'HwATOTB', 'HwADEBT', 'HwACHCK',
       'HwAMRTB', 'HwITOT']

baseFormula = "RwRecProp ~ RwAGEM_B * C(RwJOCCSD, Treatment(reference='Retired'))"
fullFormula = baseFormula + ' + ' + ' + '.join(controlVars)

indep_corr = sm.cov_struct.Independence()

optimal_form, optimal_result, history =\
    stepwise_selection_qic(df, df["HHIDPN"], 
                           indep_corr, 
                            sm.families.Binomial(), 
                            baseFormula, fullFormula, 
                            verbose=True, 
                            qic_threshold=10)