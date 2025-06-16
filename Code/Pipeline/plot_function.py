import pandas as pd
import numpy as np

## Plotting Libraries
import matplotlib.pyplot as plt
import seaborn as sns

## Machine Learning Libraries
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
import statsmodels.api as sm

## File dirtectory
import os



def plot_missing_data_heatmap(df, title="Missing Data Heatmap",color = ["white", "gray"]):
    """
    绘制数据缺失热力图：灰色表示有数据，白色表示缺失值。
    
    参数:
    df -- pandas DataFrame
    title -- 图表标题
    """
    plt.figure(figsize=(16, 10))
    sns.heatmap(
        df.notnull(),  # True（有数据）为灰色，False（NaN）为白色
        cmap=color,
        cbar=False
    )
    plt.title(title, fontsize=16)
    plt.xlabel("Columns")
    plt.ylabel("Samples")
    plt.xticks(rotation=45, ha="right")
    plt.tight_layout()
    plt.show()


# (x, y)-dot-plot
def plot_dots(plot_vars,data_set,plot_vars_name='',outcomeVars=['R1TR40']):
    n_cols = 4
    n_rows = (len(plot_vars) + n_cols - 1) // n_cols

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(5 * n_cols, 4 * n_rows))
    fig.suptitle("Joint Distribution of "+ plot_vars_name + " Variables and Recall Index", fontsize=16)

    # 把 axes 展平为一维数组，方便遍历
    axes = axes.flatten()

    for idx, col in enumerate(plot_vars):
        ax = axes[idx]
        sns.scatterplot(
            data=data_set,
            x=col,
            y=outcomeVars[0],
            ax=ax,
            alpha=0.6
        )
        sns.regplot(
            data=data_set,
            x=col,
            y=outcomeVars[0],
            scatter=False,
            lowess=True,
            line_kws={'color': 'red', 'lw': 2},
            ax=ax
        )
        ax.set_title(f"{col} vs Recall")
        ax.set_xlabel(col)
        ax.set_ylabel("Recall Score")

    # 删除多余的 subplot（如果变量数不是4的倍数）
    for j in range(len(plot_vars), len(axes)):
        fig.delaxes(axes[j])

    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.show()


# (x, y)-dot-plot
def plot_box_plot(vars_to_plot, response_var, data_set, title_prefix=''):
    fig, axes = plt.subplots(4, 4, figsize=(20, 16))
    fig.suptitle(f"Relationship between {title_prefix} Variables and {response_var}", fontsize=18)
    axes = axes.flatten()

    for idx, var in enumerate(vars_to_plot):
        ax = axes[idx]

        # 尝试将变量转换为数值型，失败的会变为 NaN
        data_set[var] = pd.to_numeric(data_set[var], errors='coerce')

        # 检查是否为有效的数值列
        if data_set[var].dropna().nunique() < 2:
            ax.text(0.5, 0.5, f"{var}: too few unique values", ha='center', va='center')
            ax.set_title(f"{var} - Skipped")
            ax.axis("off")
            continue


        try:
            binned = pd.qcut(data_set[var], q=10, duplicates='drop')
            sns.boxplot(x=binned, y=data_set[response_var], palette="Set2", ax=ax)
            ax.set_xlabel(f"{var} (deciles)")
            ax.set_ylabel(response_var)
            ax.set_title(f"{var} vs {response_var}")
            ax.set_xticklabels(ax.get_xticklabels(), rotation=45)
        except Exception as e:
            ax.text(0.5, 0.5, f"Error: {e}", ha='center', va='center')
            ax.set_title(f"{var} - Error")
            ax.axis("off")

    # 删除多余子图
    for j in range(len(vars_to_plot), len(axes)):
        fig.delaxes(axes[j])

    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.show()


def plot_histogram(plot_vars, data_set, plot_vars_name='', ncols=2):
    nrows = 2
    n_per_page = nrows * ncols
    n_pages = (len(plot_vars) + n_per_page - 1) // n_per_page  # 向上取整

    for page in range(n_pages):
        fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
        fig.suptitle(f"Distribution of {plot_vars_name} Variables (Page {page+1})", fontsize=16)

        sub_vars = plot_vars[page * n_per_page : (page + 1) * n_per_page]
        axes = axes.flatten()

        for idx, col in enumerate(sub_vars):
            ax = axes[idx]
            try:
                # 判断是否为连续变量
                if pd.api.types.is_numeric_dtype(data_set[col]) and data_set[col].nunique() > 10:
                    sns.histplot(data=data_set, x=col, bins=30, kde=True, color="blue", ax=ax)
                else:
                    sns.countplot(data=data_set, x=col, palette="Set2", ax=ax)
                    ax.set_xticklabels(ax.get_xticklabels(), rotation=45)
                ax.set_title(f"Distribution of {col}")
                ax.set_xlabel(col)
                ax.set_ylabel("Count")
            except Exception as e:
                ax.text(0.5, 0.5, f"Error: {e}", ha='center', va='center')
                ax.set_title(f"{col} - Error")
                ax.axis("off")

        # 删除多余子图
        for j in range(len(sub_vars), len(axes)):
            fig.delaxes(axes[j])

        plt.tight_layout(rect=[0, 0.03, 1, 0.95])
        plt.show()



def plot_correlation_between_variables(vars_to_check, data_set, title='Correlation Matrix', annotate=True, figsize=(12, 10)):
    """
    Plots the correlation matrix heatmap for selected variables.

    Parameters:
    - vars_to_check: list of variable names
    - data_set: pandas DataFrame
    - title: title of the plot
    - annotate: whether to annotate correlation values
    - figsize: size of the figure
    """
    # 取所需数据并删除缺失值
    corr_data = data_set[vars_to_check].dropna()

    # 计算相关系数
    corr_matrix = corr_data.corr()

    # 绘制热力图
    plt.figure(figsize=figsize)
    sns.heatmap(corr_matrix, annot=annotate, fmt=".2f", cmap="coolwarm", square=True, linewidths=0.5)
    plt.title(title, fontsize=16)
    plt.xticks(rotation=45, ha='right')
    plt.tight_layout()
    plt.show()



def plot_correlations_between_variable_and_response(vars_to_check, response_var, data_set, method='pearson', sort=True, show_plot=True):
    """
    Plots correlation between selected variables and a response variable.

    Parameters:
    - vars_to_check: list of variable names
    - response_var: the name of the response variable
    - data_set: pandas DataFrame
    - method: 'pearson', 'spearman', or 'kendall'
    - sort: whether to sort by correlation magnitude
    - show_plot: if False, only returns the correlation series
    """
    correlations = {}

    for var in vars_to_check:
        try:
            sub_data = data_set[[var, response_var]].dropna()
            if sub_data[var].nunique() > 1:  # Skip constant variables
                corr = sub_data[var].corr(sub_data[response_var], method=method)
                correlations[var] = corr
        except Exception as e:
            print(f"Error calculating correlation between {var} and {response_var}: {e}")

    # 转为Series，排序
    corr_series = pd.Series(correlations)
    if sort:
        corr_series = corr_series.sort_values(key=lambda x: abs(x), ascending=False)

    if show_plot:
        plt.figure(figsize=(10, 6))
        sns.barplot(x=corr_series.values, y=corr_series.index, palette='coolwarm')
        plt.title(f"{method.capitalize()} Correlation with {response_var}")
        plt.xlabel("Correlation")
        plt.ylabel("Variables")
        plt.grid(True, axis='x', linestyle='--', alpha=0.7)
        plt.tight_layout()
        plt.show()

    return corr_series

def print_summary_of_data(df, var, top_n=10):
    if var not in df.columns:
        print(f"[Error] Column '{var}' not found in DataFrame.")
        return

    print(f"\n=== Summary of '{var}' ===")
    
    # 显示描述性统计
    print("\nDescriptive Statistics:")
    print(df[var].describe())

    # 显示频数统计（适合分类变量）
    print(f"\nTop {top_n} Most Frequent Values:")
    print(df[var].value_counts().head(top_n))
    
    print("="*40)


if __name__ == '__main__':
    script_path = os.path.abspath(__file__)
    folder_path = os.path.dirname(script_path)
    hrsFullPath = os.path.join(folder_path , 'Data', 'randhrs1992_2022v1.sas7bdat')
    print("="*40)
    print(folder_path )
    print("="*40)

    hrsFull = pd.read_sas(hrsFullPath, chunksize=10000)
    hrsFull = next(hrsFull)

    outcomeVars = [
    "R1TR40" # W1 TOTAL WORD RECALL SUMMARY SCORE
    ]
    SavingVars = [
        "H1ARLES", # Net value of real estate(not primary residence)
        "H1ATRAN", # Net Value Of Vehicles
        "H1ABSNS", # Net Value Of Businesses
        "H1AIRA", # Net value of IRA,Keogh Accounts
        "H1ASTCK", # Net Value Of Stocks,mutual funds,and investment trusts
        "H1ACHCK", # Value Of Checking,savings,or money market accounts
        "H1ACD", # Value of,government savings bonds,and T-bills
        "H1ABOND", # Net Value Of Bonds And Bond Funds
        "H1AOTHR", # Net value of all other savings
        "H1ADEBT", # Value Of Other Debt
        "H1AHOUS", # Value Of Primary Residence
        "H1AMORT", # Value Of All Mortgages/land contracts(primary residence)
        "H1AHMLN", # Value Of Other Home Loans(primary residence)
        "H1ATOTH", #  Net value of primary residence
        "H1ATOTB", # Total Wealth
    ]

    # Note that the data set should include both x and y
    waveOneSaving = hrsFull.loc[hrsFull["INW1"] == 1, ["HHIDPN"] + outcomeVars + SavingVars]

    plot_missing_data_heatmap(waveOneSaving, 'Saving missing data Heatmap', color=['red', 'green'])
    