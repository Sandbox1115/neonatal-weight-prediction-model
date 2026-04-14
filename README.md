
# Neonatal weight prediction statistical model

This project is my solution to an inferential statistics task assigned using a neonatal healtcare dataset containing raw pregnancies data from three hospitals. I used R, RStudio and relevant R packages to extract core informations from the dataset, through Exploratory Data Analysis, and use the results to build a multiple linear regression model for neonatal weight prediction. This was developed for a first-level postgraduate master’s program at ProfessionAi.

## Overview and goals

For this task i was provided with a 2500 samples .CSV dataset compiled with entries from three different hospitals managed by a parent private health company. The goal for this assignment was to start grasping the pipeline of a data science project from EDA to modeling through the lens of inferential statistics, therefore there is greater attention on the statistic pipeline accuracy and less on the machine learning pipeline accuracy which is still not fully enclosed into this assignment; things like data leakage, splitting the dataset and cleaning it through preprocessing will not be discussed here.

Regarding the company/project goals i was assigned with the following tasks:

- Starting by an extended Exploratory Data Analysis from a descriptive statistics standpoint to identify anomalies, outliers and data distribution;

- Development of a Multiple Linear Regression model to predict newborn weight based onto relevant independent variables relations established through EDA;

- Testing various models using AIC or BIC techniques, to identify the best one in order to balance variance and model efficiency;

- After choosing the best model, extraction of model performances through model performance indicators and residual analysis.

- Usage of charts and plots where it makes sense to do so.
## Content

The following content is in this repo:

- main.R: RStudio main worksheet for this project;

- neonati.CSV: The original raw dataset the assignment was based on.

- renv.lock: needed to recover the project dependencies used in RStudio;

- main_markdown R HTML file: The knit file required to generate either the PDF or HTML output through Latex contaning the whole assignment.

- main_markdown HTML: The completed assignment knitted in HTML. You may open it with any browser and evaluate my solution to the project.

- renv folder with settings.json: needed for renv.lock extraction and renv operations to restore the dependencies.
## RStudio Package Requirements

As usual with any data science project, some external libraries were installed to reach the assignment end goals. 

If you want to replicate your own version of this project you will have to load all the required libraries into your own RStudio environment.

To properly list all of the packages required for this repo, i used renv to create a renv.lock file the user can load into RStudio in order to install all of the required packages.

Follow these instructions:

- clone this github repo

```
 git clone https://github.com/Sandbox1115/neonatal-weight-prediction-model.git
```

- Open RStudio;

- Set the repo directory contaning the renv.lock file as working directory in RStudio;

- If you never installed the renv package, then do it now:

```
install.packages("renv")
```
- When renv is installed proceed to restore the packages by calling the follwoing command:

```
renv::restore()
```

renv should now reinstall all the required packages listed in the lock file to their original version.
## Credits and Contributions

This project was developed by Sandbox1115.

If you have any question, advice, or want to share your own findings and solution to this project raw dataset please get in touch, i'd be glad to evaluate your work and grow up professionally toghether.

### License

You are allowed to share and adapt this project by copying, redistributing, remixing, transforming or building on the original content made by me through the **Creative Commons Attribution-NonCommercial 4.0 International CC BY-NC 4.0** License. Please do not forget attrbituion the the original content creator. 