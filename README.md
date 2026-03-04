# Post-Pandemic Remote Work Health Impact Analysis (2025)
A Business Analysis Report

## Executive Summary
The shift to remote and hybrid work after the COVID-19 pandemic has transformed workplace structures globally. While flexibility has improved for many employees, concerns about burnout, social isolation, and declining work-life balance have emerged.

This Business Intelligence study analyses employee survey data to:

Identify key drivers of burnout
Measure the impact of work arrangements on work-life balance
Understand how working hours and isolation influence mental health
Provide predictive insights for HR and leadership teams

Using R (tidyverse, ggplot2, corrplot, nnet), the project applies data cleaning, exploratory analysis, correlation modeling, and regression techniques to generate actionable recommendations.
---

## Business Problem
Organisations are facing increasing reports of:

- High employee burnout
- Declining mental health
- Poor work-life balance
- Increased social isolation in remote roles

Key Business Questions:

1. Does working remotely increase burnout risk?
2. How do working hours impact work-life balance?
3. Is social isolation a strong predictor of burnout?
4. Which work arrangement (Remote, Hybrid, On-site) supports better employee well-being?
5. Can we predict high burnout risk using employee behavioral indicators?

## Dataset Overview
*Source: post_pandemic_remote_work_health_impact_2025.csv* 
**Observations:** Employee survey responses

**Variables Included:**

**Demographics:**
- Age
- Gender
- Region
- Industry
- Job Role

**Work Patterns:**
- Work Arrangement (Remote / Hybrid / On-site)
- Hours Per Week
  
**Health & Well-being**
- Burnout Level (Low / Medium / High)
- Mental Health Status
- Work-Life Balance Score
- Social Isolation Score
- Physical Health Issues

## Data Preparation and Cleaning

The following steps were performed:

- Standardised categorical text fields (e.g., Gender, Industry, Work Arrangement)
- Converted burnout levels into ordered factors
- Separated multiple physical health issues using separate_rows()
 
Created binary variable:

  - Burnout_High (1 = High Burnout, 0 = Otherwise)
    
- Converted relevant numeric variables for correlation analysis
  
This ensured consistency and accurate modeling.

## Exploratory Data Analysis (EDA)
**Age & Gender Distribution**

Histogram analysis revealed workforce demographic spread and gender participation balance.

**Business Insight:**
Burnout and work-life balance trends should be segmented by demographic group in future HR dashboards.

**Weekly Work Hours by Work Arrangement**

Key finding:
Employees working longer hours showed significantly higher burnout levels.

**Business Insight:**
Excessive weekly hours strongly correlate with burnout risk.

**Burnout vs Hours Worked (Boxplot)**

Clear trend observed:
Higher weekly hours → Higher burnout category.

This visually confirms operational overload as a primary burnout driver.

**Work-Life Balance by Work Arrangement**

Hybrid workers showed more balanced distributions compared to fully remote or fully on-site employees.

**Business Insight:**
Hybrid work may provide structural balance between flexibility and social interaction.

**Top 10 Physical Health Issues**

Common issues included:
- Back pain
- Eye strain
- Headaches
- Neck strain

**Business Implication:**
Remote ergonomic support programs are necessary to reduce long-term health costs.

## Correlation Analysis 
A correlation matrix revealed:

**Strong negative correlation between:**
Hours Per Week and Work-Life Balance

**Positive correlation between:**
Social Isolation Score and Burnout Level

**Higher isolation → Lower work-life satisfaction**

**Key Insight:**
Social isolation is a critical hidden driver of burnout.

## Predictive Modeling
### Linear Regression
**Target**: Work-Life Balance Score

Predictors:
- Hours Per Week
- Age
- Social Isolation Score
- Work Arrangement

**Findings:**

- Hours per week negatively impacts work-life balance.
- Social isolation significantly reduces work-life balance scores.
- Work arrangement influences balance outcomes.

Conclusion:
Managing workload and social connection programs improves employee satisfaction.

### Logistic Regression
**Target:** High Burnout (Binary)

- Model predicts probability of high burnout using:
- Hours Per Week
- Age
- Social Isolation Score
- Work Arrangement

**Key Drivers of High Burnout:**

- Increased working hours
- Higher isolation scores
- Certain work arrangements
The predicted probability plot shows:
- Burnout risk rises sharply beyond higher weekly hour thresholds.

**Business Application:**
HR teams can build early-warning dashboards using these predictors.

### Multinomial Logistic Regression

**Target:** Burnout Level (Low / Medium / High)

This model allows companies to:
- Identify likelihood of each burnout category
- Segment employees by risk level
- Proactively intervene before burnout escalates

## Key Business Insights

1. Long working hours are the strongest predictor of burnout.
2. Social isolation significantly impacts mental health and work-life balance.
3. Hybrid work models appear more balanced than extreme remote or on-site structures.
4. Physical health issues are emerging due to prolonged remote work environments.
5. Predictive models can successfully estimate high burnout probability.

# Strategic Recommendations
**1. Implement Work Hour Monitoring**
   - Flag employees consistently exceeding healthy thresholds.
   - Introduce workload redistribution policies.
**2. Strengthen Hybrid Work Models**
   - Encourage flexible structures that maintain social interaction.
   - Design in-office collaboration days.
**3. Develop Isolation Mitigation Programs**
  - Virtual social events
  - Mentorship programs
  - Regular manager check-ins
**4. Introduce Ergonomic Support**
  - Provide home-office stipends
  - Offer posture & wellness training
**5. Build a Burnout Risk Dashboard (For BI route)**
    Use Power BI or Tableau to:
  - Track burnout risk probability
  - Monitor isolation trends
  - Compare departments and regions

# Business Value

This project demonstrates:
* End-to-end data cleaning
* Exploratory data analysis
* Correlation insights
* Predictive modeling (Linear, Logistic, Multinomial)
* Translation of statistical output into executive recommendations

The aim of this analysis is to support HR strategy, workforce planning, and organisational health monitoring.

**In future improvements that can be made are to:**
1. Integrate time-series tracking
2. Add department-level analysis
3. Automate burnout risk scoring model





