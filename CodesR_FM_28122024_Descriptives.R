## Chargement des données
Data_FM_19122024 <- read_excel("C:/Users/Lenovo/Desktop/Dr Fatou Mbaye/Data_FM_19122024.xlsx")
DataFM<- Data_FM_19122024
--------------------------------------------------------------------------------
## ANALYSE DESCRIPTIVE

### Codifications (Table sans données manquantes)

DataFM$résultats_IVA <- factor(DataFM$résultats_IVA, 
                levels = c(0, 1), 
                labels = c("Négatif", "Col suspect"))

DataFM$CDD <- factor(DataFM$CDD, 
                levels = c(0, 1), 
                labels = c("Aucun symptôme", "Présence de symptômes"))

DataFM$conn_dépistage_cancer <- factor(DataFM$conn_dépistage_cancer, 
                levels = c(0,1,2), 
                labels = c("Aucune", "Moyenne","Bonne"))

DataFM$infection_hépatite_b <- factor(DataFM$infection_hépatite_b, 
                levels = c(0,1), 
                labels = c("Non", "Oui"))

DataFM$Gestité <- factor(DataFM$Gestité, 
                levels = c(0,1,2,3,4), 
                labels = c("nulligeste", "primigeste","paucigeste", "multigeste", "grande multigeste"))
                        
DataFM$Parité <- factor(DataFM$Parité, 
                levels = c(0,1,2,3,4), 
                labels = c("nullipare", "primipare","paucipare", "multipare", "grande multipare"))
                                                 
DataFM$contracception <- factor(DataFM$contracception, 
                levels = c(0,1), 
                labels = c("Non", "Oui"))
                                                 
DataFM$niveau_instruction <- factor(DataFM$niveau_instruction, 
                levels = c(0,1,2,3,4), 
                labels = c("Non scolarise", "Arabe","Niveau primaire","Niveau secondaire","Niveau supérieur"))
                                                 
DataFM$status_matrimonial <- factor(DataFM$status_matrimonial, 
                levels = c(1,2,3), 
                labels = c("Mariee", "Célibataire","Divorcée/Veuve"))
                                                 
--------------------------------------------------------------------------------

# Tableau des résultats descriptifs

install.packages("gtsummary")
library(gtsummary)
trial2 <- DataFM |> select(résultats_IVA , age, status_matrimonial, niveau_instruction, 
                          contracception, age_premier_rappport, Gestité, 
                          Parité,  nbres_partenaires_sexuels_vie, 
                          infection_hépatite_b, conn_dépistage_cancer, CDD)


trial2 |> tbl_summary()
--------------------------------------------------------------------------------
# Tableau croisé des facteurs avec la variable dépendante

install.packages("cardx")
library(cards)

table3 <-
  tbl_summary(
    DataFM,
    include = c(age, status_matrimonial, niveau_instruction, 
                contracception, age_premier_rappport, Gestité, 
                Parité,  nbres_partenaires_sexuels_vie, 
                infection_hépatite_b, conn_dépistage_cancer, CDD),
    by = résultats_IVA, # split table by group
    missing = "no" # don't list missing data separately
  ) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p() |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()
table3
--------------------------------------------------------------------------------
# Graphiques de visualisation de l'associaton des facteurs à la variable dépendante

library(ggplot2)
library(dplyr)
library(gcookbook)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

## status_matrimonial

p1 <- table(DataFM$status_matrimonial, DataFM$résultats_IVA)
p11<-prop.table(p1, 2)
tb1<-as.data.frame(p11)
tb1$Freq <- tb1$Freq*100
tb1$Percentage<-tb1$Freq
tb1$status_matrimonial<- tb1$Var1
tb1$résultats_IVA<- tb1$Var2
tb1 <- select(tb1, -Var1)
tb1 <- select(tb1, -Var2)
tb1 <- select(tb1, -Freq)
tb1$Percentage<- round(tb1$Percentage, digits = 0)

sm<- ggplot(tb1,aes(x = résultats_IVA, y=Percentage, fill=status_matrimonial))+
     geom_col(position = position_fill()) +
     geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
sm<- sm + scale_y_continuous(labels = scales::percent_format())
sm

## niveau_instruction

p2 <- table(DataFM$niveau_instruction, DataFM$résultats_IVA)
p22<-prop.table(p2, 2)
tb2<-as.data.frame(p22)
tb2$Freq <- tb2$Freq*100
tb2$Percentage<-tb2$Freq
tb2$niveau_instruction<- tb2$Var1
tb2$résultats_IVA<- tb2$Var2
tb2 <- select(tb2, -Var1)
tb2 <- select(tb2, -Var2)
tb2 <- select(tb2, -Freq)
tb2$Percentage<- round(tb2$Percentage, digits = 0)

ni<- ggplot(tb2,aes(x = résultats_IVA, y=Percentage, fill=niveau_instruction))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
ni<- ni + scale_y_continuous(labels = scales::percent_format())
ni

## Parité

p7 <- table(DataFM$Parité, DataFM$résultats_IVA)
p77<-prop.table(p7, 2)
tb7<-as.data.frame(p77)
tb7$Freq <- tb7$Freq*100
tb7$Percentage<-tb7$Freq
tb7$Parité<- tb7$Var1
tb7$résultats_IVA<- tb7$Var2
tb7 <- select(tb7, -Var1)
tb7 <- select(tb7, -Var2)
tb7 <- select(tb7, -Freq)
tb7$Percentage<- round(tb7$Percentage, digits = 0)

par<- ggplot(tb7,aes(x = résultats_IVA, y=Percentage, fill=Parité))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
par<- par + scale_y_continuous(labels = scales::percent_format())
par

## Gestité

p8 <- table(DataFM$Gestité, DataFM$résultats_IVA)
p88<-prop.table(p8, 2)
tb8<-as.data.frame(p88)
tb8$Freq <- tb8$Freq*100
tb8$Percentage<-tb8$Freq
tb8$Gestité<- tb8$Var1
tb8$résultats_IVA<- tb8$Var2
tb8 <- select(tb8, -Var1)
tb8 <- select(tb8, -Var2)
tb8 <- select(tb8, -Freq)
tb8$Percentage<- round(tb8$Percentage, digits = 0)

ges<- ggplot(tb8,aes(x = résultats_IVA, y=Percentage, fill=Gestité))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
ges<- ges + scale_y_continuous(labels = scales::percent_format())
ges


## Contraception

p3 <- table(DataFM$contracception, DataFM$résultats_IVA)
p33<-prop.table(p3, 2)
tb3<-as.data.frame(p33)
tb3$Freq <- tb3$Freq*100
tb3$Percentage<-tb3$Freq
tb3$contracception<- tb3$Var1
tb3$résultats_IVA<- tb3$Var2
tb3 <- select(tb3, -Var1)
tb3 <- select(tb3, -Var2)
tb3 <- select(tb3, -Freq)
tb3$Percentage<- round(tb3$Percentage, digits = 0)

cc<- ggplot(tb3,aes(x = résultats_IVA, y=Percentage, fill=contracception))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
cc<- cc + scale_y_continuous(labels = scales::percent_format())
cc

## infection_hépatite_b

p4 <- table(DataFM$infection_hépatite_b, DataFM$résultats_IVA)
p44<-prop.table(p4, 2)
tb4<-as.data.frame(p44)
tb4$Freq <- tb4$Freq*100
tb4$Percentage<-tb4$Freq
tb4$infection_hépatite_b<- tb4$Var1
tb4$résultats_IVA<- tb4$Var2
tb4 <- select(tb4, -Var1)
tb4 <- select(tb4, -Var2)
tb4 <- select(tb4, -Freq)
tb4$Percentage<- round(tb4$Percentage, digits = 0)

hb<- ggplot(tb4,aes(x = résultats_IVA, y=Percentage, fill=infection_hépatite_b))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
hb<- hb + scale_y_continuous(labels = scales::percent_format())
hb

## Connaissance du dépistage du cancer

p5 <- table(DataFM$conn_dépistage_cancer, DataFM$résultats_IVA)
p55<-prop.table(p5, 2)
tb5<-as.data.frame(p55)
tb5$Freq <- tb5$Freq*100
tb5$Percentage<-tb5$Freq
tb5$conn_dépistage_cancer<- tb5$Var1
tb5$résultats_IVA<- tb5$Var2
tb5 <- select(tb5, -Var1)
tb5 <- select(tb5, -Var2)
tb5 <- select(tb5, -Freq)
tb5$Percentage<- round(tb5$Percentage, digits = 0)

cdc<- ggplot(tb5,aes(x = résultats_IVA, y=Percentage, fill=conn_dépistage_cancer))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
cdc<- cdc + scale_y_continuous(labels = scales::percent_format())
cdc

## CDD

p6 <- table(DataFM$CDD, DataFM$résultats_IVA)
p66<-prop.table(p6, 2)
tb6<-as.data.frame(p66)
tb6$Freq <- tb6$Freq*100
tb6$Percentage<-tb6$Freq
tb6$CDD<- tb6$Var1
tb6$résultats_IVA<- tb6$Var2
tb6 <- select(tb6, -Var1)
tb6 <- select(tb6, -Var2)
tb6 <- select(tb6, -Freq)
tb6$Percentage<- round(tb6$Percentage, digits = 0)

cdd<- ggplot(tb6,aes(x = résultats_IVA, y=Percentage, fill=CDD))+
  geom_col(position = position_fill()) +
  geom_text(aes(label = paste0(Percentage,"%")),
            position = position_fill(vjust = .5))
cdd<- cdd + scale_y_continuous(labels = scales::percent_format())
cdd

## Age

ggplot(DataFM, aes(x=résultats_IVA, y=age)) + 
  geom_boxplot()

## Age au premier rapport sexuel

ggplot(DataFM, aes(x=résultats_IVA, y=age_premier_rappport)) + 
  geom_boxplot()

## Nombre de partenaires sexuels

ggplot(DataFM, aes(x=résultats_IVA, y=nbres_partenaires_sexuels_vie)) + 
  geom_boxplot()

  



















