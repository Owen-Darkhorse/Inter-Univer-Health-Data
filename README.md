# Inter-University Health Data Inquiry

## Overview

### Background

Nowadays, Canada is facing a serious threat, with the quickly aging population, the risk of cognitive decline largely impacts the public-health area and economic area. Moreover, the protective value of continued work after retirement is still unclear.

### Research Goal

This project aims to investigate the impact of post-retirement occupational engagement on cognitive and mental health. We studied 22,374 adults aged 65 and older who were part of the 2022 RAND Health and Retirement Study. The study tracked every two years across nine survey waves. Brain health was measured in three ways: the Mental Status Index, the Recall Index, and whether someone showed signs of serious cognitive trouble. We compared those scores with job status, job type, weekly hours, pay rate, and how much physical or mental effort each job demanded, while also adjusting for age, income,lifestyle, and medical factors. GEE models let us follow changes over time.

### Findings

The results showed that individuals in service-sector jobs, particularly those in household and cleaning, food service, and personal care, had significantly lower mental health scores and experienced faster cognitive decline compared to individuals in managerial or professional roles. In the Recall Index model, main effects of all occupational categories were negative, and the skilled trades group experienced significantly faster memory decline relative to the retired group. Meanwhile, mental health scores were positively associated with moderate alcohol use and being in a close personal relationship, suggesting that lifestyle and social context also shape later-life well-being. Overall, the impact of post-retirement occupational engagement on cognitive and mental health varies by job type and should be interpreted contextually. While differences in memory performance across occupations were largely insignificant after adjusting for age, individuals in skilled trades and manual roles exhibited faster memory decline. In contrast, individuals in professional or managerial roles exhibited higher mental health scores compared to those in service-oriented occupations, reflecting potential differences in job-related stressors or resources.

## File Directory

This repository is organized as follows:

| Directory | Description |
| :--- | :--- |
|[Data][Data]| This folder contains cleaned and imputed data for training GEE model. The raw data is not uploaded due to large file size.|
| [Code](Code) | This folder contains data manipulation Python scripts in `\Pipeline` subfolder, Jupyter notebooks for EDA and modelling, and R markdown for final GEE modelling.|
| [Images](Images) | This folder contains all images generated in EDA and modelling stage
| [Notes](Notes) | This folder contains importanmt meeting notes and manuscript drafts. |
| [Proposal](Proposal) | This folder contains the research proposal. |

## How to Clone

To clone this repository, run the following command in your terminal:

```bash
git clone https://github.com/<your-username>/Inter-Univer-Health-Data.git
```