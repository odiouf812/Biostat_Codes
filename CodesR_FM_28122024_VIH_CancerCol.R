# Chargement des données VIH/Cancer

Data_VIH_Cancer_Col <- read_excel("Data_VIH_Cancer_Col.xlsx")


# Codification VIH et Cancer col

Data_VIH_Cancer_Col$résultats_IVA <- factor(Data_VIH_Cancer_Col$résultats_IVA, 
                               levels = c(0, 1), 
                               labels = c("Négatif", "Col suspect"))

Data_VIH_Cancer_Col$VIH <- factor(Data_VIH_Cancer_Col$VIH, 
                     levels = c(0, 1), 
                     labels = c("VIH négatif", "VIH positif"))


# Tableau croisé VIH / Cancer du col

install.packages("cardx")
library(cards)

Data_VIH_Cancer_Col$résultats_IVA <- factor(Data_VIH_Cancer_Col$résultats_IVA, 
                               levels = c(0, 1), 
                               labels = c("Négatif", "Col suspect"))
table3 <-
  tbl_summary(
    Data_VIH_Cancer_Col,
    include = c(résultats_IVA),
    by = VIH, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p() |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()
table3





