# Regression logistic

install.packages(glm)
library(glm)

model <- glm(résultats_IVA ~ age + status_matrimonial + niveau_instruction + contracception+
                       age_premier_rappport + Gestité + Parité +  nbres_partenaires_sexuels_vie +
                       infection_hépatite_b + conn_dépistage_cancer + CDD,
                     data = DataFM,
                     family = "binomial")
model

summary(model)


## Report des résultats du model final

library(gtsummary)

tbl_regression(model, exponentiate = TRUE)

t1 <- tbl_regression(model, exponentiate = TRUE)

t1

# Création du vecteur de probabilités de prédiction

preds <- predict(model,
                 newdata = select(DataFM, -résultats_IVA), # remove real outcomes
                 type = "response"
)

# si probabilité < seuil, le patient est considéré comme n'ayant pas la maladie
preds_outcome <- ifelse(preds < 0.5,
                        0,
                        1
)

# transformer les prédictions en facteurs et définir des étiquettes
preds_outcome <- factor(preds_outcome,
                        levels = c(0, 1),
                        labels = c("no disease", "disease")
)

# comparer les résultats observés et prévus
tab <- table(DataFM$résultats_IVA, preds_outcome,
             dnn = c("observed", "predicted")
)

# imprimer les résultats
tab

accuracy <- sum(diag(tab)) / sum(tab)
accuracy

## Sensitivity and specificity

# sensitivity
sensitivity <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
sensitivity

# specificity
specificity <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
specificity

--------------------------------------------------------------------------------
### AUC and ROC curve
  
# load package
library(pROC)

# save roc object
res <- roc(résultats_IVA~ fitted(model),
           data = DataFM
)

# plot ROC curve
ggroc(res, legacy.axes = TRUE)

# print AUC
res$auc

# plot ROC curve with AUC in title
ggroc(res, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res$auc, 2)))