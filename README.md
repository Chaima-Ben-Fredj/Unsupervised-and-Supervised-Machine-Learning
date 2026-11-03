# Supervised and Unsupervised Machine Learning in R

Statistical Learning project, MSc Data Science for Economics, University of Milan.

This project applies two families of machine learning methods to two datasets:

- **Supervised learning:** predicting heart disease from clinical data.
- **Unsupervised learning:** grouping countries into development stages from socioeconomic and health indicators.

## Results at a glance

| Part | Methods | Key result |
| --- | --- | --- |
| Heart disease prediction | Logistic Regression, SVM (polynomial), Decision Tree, Random Forest | Random Forest performed best: 85.3% accuracy, 90.6% sensitivity, 18 false positives on the test set |
| Country development clustering | PCA, K-means, Hierarchical clustering (Ward) | Countries split into Developed, Emerging and Least-developed groups (k = 3) |

---

## 1. Supervised learning: heart disease prediction

**Goal:** predict whether a patient has heart disease, with a focus on catching true cases (high sensitivity) for early screening.

**Data:** Kaggle heart disease dataset, 918 patients and 12 variables (age, sex, chest pain type, resting blood pressure, cholesterol, fasting blood sugar, resting ECG, maximum heart rate, exercise-induced angina, Oldpeak, ST slope, and the target `HeartDisease`). Around 55% of patients have heart disease.

**Preprocessing**
- Removed one impossible record (`RestingBP = 0`).
- Found 172 records with `Cholesterol = 0`, which is medically implausible. Replaced them with `NA` and imputed values with K-Nearest Neighbors.
- Converted categorical variables to dummy variables and standardized numeric variables (z-score).
- Stratified 80/20 train-test split, with 10-fold cross-validation on the training data.

**Model comparison (test set)**

| Model | Accuracy | True Negatives | False Positives | False Negatives | True Positives |
| --- | --- | --- | --- | --- | --- |
| Logistic Regression | 76.0% | 88 | 36 | 8 | 51 |
| SVM (polynomial) | 84.2% | 86 | 19 | 10 | 68 |
| Decision Tree | 77.6% | 87 | 32 | 9 | 55 |
| Random Forest | **85.3%** | 87 | **18** | 9 | 69 |

**Takeaways**
- All four models kept sensitivity above 90%, which suits an early screening use case.
- Random Forest gave the best balance of accuracy and false alarms. SVM was a close second.
- The Decision Tree is the most interpretable. Its main splits are ST slope and chest pain type, which fits clinical knowledge.
- Logistic Regression is the simplest model, but it produced the most false positives.

---

## 2. Unsupervised learning: country development clusters

**Goal:** find natural groupings of countries by development level without predefined labels.

**Data:** Kaggle country dataset, 166 countries. Economic indicators: GDP per capita, income, inflation, exports, imports. Health indicators: child mortality, life expectancy, fertility rate, health spending.

**Methods**
- **Exploratory analysis:** distributions, scatter plots, choropleth world maps and a correlation heatmap. For example, income and GDP per capita correlate at 0.90, and child mortality and fertility at 0.85.
- **Preprocessing:** removed `income` (highly correlated with GDP per capita) and standardized all numeric variables.
- **PCA:** the first three components explain about 77.8% of the variance. PC1 (44.5%) mostly separates countries by economic development.
- **K-means:** the elbow method pointed to k = 3. Clusters were labelled Developed, Emerging and Least-developed using GDP per capita and child mortality. A k = 4 run was also tested and gave very unbalanced clusters.
- **K-means on PCA data:** clusters overlapped in the 2D projection, so the K-means results on the full scaled data were preferred.
- **Hierarchical clustering:** complete, average and Ward linkage were compared. Ward was selected for its more compact clusters. Average silhouette width was 0.29 and cophenetic correlation was 0.532.

---

## Repository structure

```
├── country-data/         # Kaggle dataset for unsupervised learning
├── heart/                # Kaggle dataset for supervised learning
├── supervised/           # Heart disease prediction code
├── unsupervised/         # Country clustering code
├── report/               # Full project report (PDF)
└── README.md
```

## How to run

1. Install R (and RStudio, if you like).
2. Install the packages used in the scripts. 
3. Download the datasets from Kaggle and place them in the data folder.
4. Open the scripts and run them in order: preprocessing, models, then evaluation.

## Tools

R, with packages for data manipulation, visualization, clustering, PCA and classification.

## Author

Chaima Ben Fredj, MSc Data Science for Economics, University of Milan.
[LinkedIn](https://linkedin.com/in/chaima-ben-fredj)

## License

Apache License 2.0
