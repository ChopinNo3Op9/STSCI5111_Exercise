# Titanic Survivor Analysis

## Overview
The project aims to analyze the survival data of passengers from the Titanic disaster. The analysis involves scraping data from `titanic_survivor.html` and `titanic_victim.html`, followed by multivariate analysis to explore various factors affecting survival rates.

## Data Sources
- `titanic_survivor.html`: HTML file containing information about survivors of the Titanic disaster.
- `titanic_victim.html`: HTML file containing information about victims of the Titanic disaster.

## Project Goals
1. Scrape data from the provided HTML files.
2. Clean and preprocess the data for analysis.
3. Conduct multivariate analysis to explore factors influencing survival rates.
4. Generate insights and visualize findings.

## Methodology
- **Data Collection**: Data is collected by scraping HTML files using R.
- **Data Cleaning**: The scraped data undergoes cleaning and preprocessing to eliminate inconsistencies and missing values.
- **Analysis**: Multivariate analysis techniques are employed to explore the relationships between different factors and survival rates.
- **Visualization**: Findings are presented visually through plots and charts to aid interpretation.

- **To begin**:
1. `scrape_titanic_survivor.R` to scrape Titanic survivor data.
2. `scrape_titanic_victim_and_combine.R` to scrape Titanic victim data and combine it with survivor data.
3. `relationship_between_variables.R` to explore relationships between variables by running.

## Hypotheses
1. Passengers with higher passenger class have better chances of survival.
2. Women and children are more likely to survive compared to men.
3. Passengers traveling with family members have higher survival rates.

## Results
- The analysis revealed a significant correlation between passenger class and survival rates.
- Women and children indeed had higher survival rates compared to men.
- Passengers traveling with family members showed slightly higher survival rates.

## Future Modeling
For future modeling and prediction tasks related to the Titanic dataset, you may refer to the following resource: [Titanic: Machine Learning from Disaster - Kaggle Notebook](https://github.com/agconti/kaggle-titanic/blob/master/Titanic.ipynb). This notebook provides a comprehensive approach to modeling and predicting survival outcomes using machine learning techniques on the Kaggle Titanic dataset. It can serve as a valuable reference for building predictive models and further analysis.

## Conclusion
The Titanic Survivor Analysis project provides valuable insights into the factors influencing survival rates during the Titanic disaster. The findings contribute to a better understanding of historical events and human behavior in crisis situations.

