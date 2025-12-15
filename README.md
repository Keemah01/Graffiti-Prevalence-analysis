# Graffiti-Prevalence-analysis
Data analysis project using UK Crime Survey data to examine graffiti prevalence and compare logistic regression and random forest models.

# Graffiti Prevalence Analysis Using UK Crime Survey Data

## Project Overview
This project analyses the prevalence of graffiti across urban and rural areas in the UK using data from the 2013 Crime Survey. The aim is to identify key socio-economic and environmental factors associated with graffiti occurrence and to compare predictive performance across statistical and machine learning models.

The analysis applies data-driven methods to support evidence-based decision making for urban planning and resource allocation.



## Research Objective
- Examine whether graffiti prevalence differs between urban and rural areas  
- Identify environmental and socio-economic factors associated with graffiti  
- Compare predictive performance between logistic regression and random forest models  
- Generate actionable insights to improve quality of life in affected areas  



## Dataset
- **Source:** UK Crime Survey (2013)
- **Observations:** 8,086
- **Variables:** 20 after cleaning and transformation
- **Outcome Variable:** Graffiti prevalence (binary)



## Methodology
- Data cleaning and feature engineering
- Exploratory data analysis
- Linear probability models
- Logistic regression with average marginal effects
- Random forest classification
- Train-test split (80:20)
- Model comparison using accuracy and interpretability



## Key Findings
- Graffiti is significantly more prevalent in urban areas
- Poor housing conditions, littered environments, and deprivation are strong predictors
- Logistic regression achieved an accuracy of **92.4%**
- Random forest achieved comparable accuracy (**92.1%**) but with lower interpretability


## Model Comparison
| Model | Accuracy | Strength |
|------|---------|----------|
| Logistic Regression | 92.4% | High interpretability |
| Random Forest | 92.1% | Captures complex interactions |



## Tools & Technologies
- **Language:** R  
- **Models:** Logistic Regression, Random Forest  
- **Techniques:** Classification, feature selection, out-of-sample prediction  
- **Visualisation:** ggplot2  



## Practical Implications
The findings support targeted interventions such as:
- Improving housing conditions
- Organising community cleaning initiatives
- Prioritising high-risk urban areas
- Data-informed resource allocation

---

## Author
**Hikmat**  
Data Analyst | Quantitative Research | R  

---

## Notes
This project is part of a personal data analytics portfolio and has been adapted for public presentation.

