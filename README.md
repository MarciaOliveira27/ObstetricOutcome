# ObstetricOutcome

The dissertation titled "Prediction of obstetric outcome based on the fetal weight/placental weight ratio indicator" was conducted as part of the Master's in Bioinformatics, with the purpose of developing a web application that allows professionals to obtain an estimate of the obstetric outcome.

# Abstract
Throughout gestation, accurately predicting obstetric outcome is of utmost importance; however, it continues to represent a significant challenge in medical practice. The Fetal Weight/Placental Weight Ratio (FPW-R ) has gained interest in this field, as it can be used to enhance predictive capacity and, consequently, minimize adverse effects while optimizing maternal-fetal health. In this context, the topic of this dissertation aims to predict obstetric outcomes based on the value of the FPW-R indicator. For this purpose, a dataset from the third trimester of pregnant women was utilized, with three groups defined for the response variable: newborn, intrauterine fetal death, and neonatal death.

In this study, multinomial logistic regression techniques were employed to analyze the fetoplacental biometric data and identify the most contributive variables. Prior to implementing the algorithm, data preprocessing was necessary (including cleaning and transformation). Due to data imbalance, the SMOTE technique was applied to generate synthetic samples. Among the variable selection methods, the one that demonstrated the best performance was Elastic Net with an alpha of 0.1. For the intrauterine fetal death category, it was possible to select 4 variables, achieving an AUC-PR of 0.715 and an AUC-ROC of 0.841, with the most contributive variable being female fetal gender. Regarding the neonatal death class, the model selected 7 variables, yielding an AUC-PR of 0.023 and an AUC-ROC of 0.620. The variables with the greatest impact in this category were diameter 2 (D2), male gender, and maternal age. It is believed that the model's difficulty in identifying more consistent patterns is related to the scarcity of data in some categories of the response variable.

Following this stage, the selected model was utilized for the development of a web application, available at https://obstetricoutcome.shinyapps.io/mo_uminho/, developed in R using the RStudio environment. This application can be a significant contribution to obstetric practice, providing an intuitive tool for healthcare professionals. It thus allows for real-time predictions of possible obstetric outcomes, ensuring better monitoring of maternal-fetal health
