        ### Introduction a la regression logistique ###

#QUESTION 1 : Charger la base de donnees
FRCV = read.csv2(file.choose())

#QUESTION 2 : Convertir les variables aux types recommandes
str(FRCV)
FRCV$Sexe = as.factor(FRCV$Sexe)
FRCV$Age = as.numeric(FRCV$Age)
FRCV$Scolarisation = as.factor(FRCV$Scolarisation)
FRCV$Tabagisme = as.factor(FRCV$Tabagisme)
FRCV$Sedentarite = as.factor(FRCV$Sedentarite)
FRCV$HTA = as.factor(FRCV$HTA)
FRCV$Diabete = as.factor(FRCV$Diabete)
FRCV$ECG = as.factor(FRCV$ECG)
FRCV$Creatininemie = as.numeric(FRCV$Creatininemie)
FRCV$Uricemie = as.numeric(FRCV$Uricemie)

#QUESTION 3 : Creer la variable IMC
FRCV$IMC = FRCV$Poids*10000/(FRCV$Taille^2)


#QUESTION 4 : Description des variables
##Generer les parametres de position et de dispersion
library(Hmisc)
describe(FRCV)
sd(FRCV$Age)
sd(FRCV$Creatininemie)
sd(FRCV$Uricemie)
sd(FRCV$IMC, na.rm=T)

##Generer les graphiques d'usage
hist(FRCV$Age,
     xlab="Ages en annees",
     ylab="Frequences absolues",
     main="",
     col=heat.colors(10),
     breaks=seq(to=5, from=100, length=15))

boxplot(FRCV$Creatininemie,
     ylab="Creatininemie (mg/l)",
     col=topo.colors(10))

hist(FRCV$Uricemie,
     xlab="Uricemie (en)",
     ylab="Frequences absolues",
     main="",
     col=heat.colors(10),
     breaks=seq(to=0, from=200, length=21))

boxplot(FRCV$IMC, col="blue",ylab="IMC")


#QUESTION 5 : Faire les croisements deux a deux avec la variable diabete
#Variables avec p-value inf. a 25% : 

summary(Diabete~HTA + Age + IMC + Obesite.abdo + Scolarisation +
                Sedentarite + Tabagisme + Sexe + Creatininemie + Uricemie +
                ECG, method = "reverse", test = T, data = FRCV)
names(FRCV)
        # HTA, Age, IMC, Obesite abdo, Scolarisation, Sedentarite, Tabagisme, Sexe

#QUESTION 6 : Creer un modele de depart comportant toutes les variables avec p-value inf. a 25%

##Creer une nouvelle dataframe sans donnees manquantes
        # Excluant toutes les opbservations avec données manquantes
                FRCV0 = na.omit(FRCV)

        # Excluant uniquement les observations presentant des na sur une variable donnée                
                Manquantes <-is.na(FRCV$IMC)
                FRCVNew <- FRCV[!Manquantes,]
library(epiDisplay)
library(MASS)
library(Hmisc)
##Creer le modèle complet avec les variables presentant p-value inf 25%
                names(FRCVNew)
                mod1 = glm(Diabete~HTA + Age + IMC + Obesite.abdo + Scolarisation + Sedentarite + Tabagisme, family=binomial, data=FRCVNew)
                
                #QUESTION 7 : Proceder a une modelisation pas-a-pas descendante manuelle basee sur le test du rapport de vraisemblance
                summary(mod1)
                #Scolarisation va être enleve pour creer un modele 2 et tester son apport dans le modèle
                
                mod2 = glm(Diabete~HTA + Age + IMC + Obesite.abdo + Sedentarite + Tabagisme, family=binomial, data=FRCVNew)
                lrtest(mod1,mod2)
                #pas de difference statistiquement significative, le modele parcimonieux est meilleur. Scolarisation est definitivement supprime
                
                summary(mod2)
                mod3 = glm(Diabete~HTA + Age + IMC + Obesite.abdo + Tabagisme, family=binomial, data=FRCVNew)
                lrtest(mod2,mod3)
                
                summary(mod3)
                mod4 = glm(Diabete ~ Age + IMC + Obesite.abdo + Tabagisme, family=binomial, data=FRCVNew)
                lrtest(mod3,mod4)
                
                summary(mod4)
                mod5 = glm(Diabete~ Age + Obesite.abdo + Tabagisme, family=binomial, data=FRCVNew)
                lrtest(mod4,mod5)
                
                summary(mod5)
                mod6 = glm(Diabete~ Age + Obesite.abdo, family=binomial, data=FRCVNew)
                lrtest(mod5,mod6)
                
                summary(mod6)
                mod7 = glm(Diabete~Age, family=binomial, data=FRCVNew)
                lrtest(mod6,mod7)
                
                #Modele retenu = mod6 
                
#QUESTION 8 : MODELISATION AVEC UNE STRATEGIE PAS A PAS ASCENDANTE AUTOMATISEE AVEC LES 11 VARIABLES
                modVide = glm (Diabete ~ 1, family=binomial , data = FRCVNew)
                modFull = glm(Diabete~Sexe + Age + Scolarisation + Tabagisme + Sedentarite + HTA + ECG + Creatininemie + Uricemie + IMC + Obesite.abdo, family=binomial, data=FRCVNew)
                stepAIC(modVide, direction="forward", scope=list(lower=modVide, upper=modFull))
                
                modForw = glm(Diabete ~ Age + Obesite.abdo + Sexe + Creatininemie + ECG + Tabagisme, family = binomial, data = FRCVNew)
                
#QUESTION 9 : MODELISATION AVEC UNE STRATEGIE MIXTE AVEC LES 11 VARIABLES
                stepAIC(modFull, direction ="both")

#QUESTION 9 : DETERMINER LES CRITERES AIC, BIC et PSEUDO-R2
                AIC (mod6)
                AIC (modForw)
                
                BIC (mod6)
                BIC (modForw)
                
                library(blorr)      
                blr_rsq_cox_snell(mod6)
                blr_rsq_cox_snell(modForw)
                
# QUESTION 10 : Etudier les residus du modele
                par(mfrow=c(1,1))
                plot(modForw, which=1:6)
                
        # Creer un sous-repertoire sans les observations 171, 544 et 307
                FRCV3 = FRCVNew[-c(171,544,307),]
                mod8 = glm(Diabete ~ Age + Obesite.abdo + Sexe + Creatininemie + ECG + Tabagisme, family = binomial, data = FRCV3)
                AIC(mod8)
                AIC(modForw)
                
# QUESTION 11 : Etudier les interactions
                mod9 = glm(Diabete ~ Obesite.abdo + Sexe*Age + 
                                   Creatininemie + ECG + Tabagisme, 
                           family = binomial, data = FRCV3)
                summary(mod9)   
                
# INTERPRETATION DU MODELE
                summary(mod8)
                exp(0.065417)
                logistic.display(mod8)
                
                logistic.display(mod8, alpha = 0.03, crude = TRUE, 
                                 crude.p.value = T, decimal = 2, 
                                 simplified = F) 
                
# CALIBRATION AVEC TEST DE HOSMER LEMESHOW
                blr_test_hosmer_lemeshow(mod8)
                
# ETUDIER LES PERFORMANCES DU MODELE
                Predictions = predict(mod8, type="response")
                plot(Predictions, jitter(as.numeric(FRCV3$Diabete),.5), 
                     cex=0.5, ylab="Diabetiques (1=Non, 2=Oui)",
                     xlab = "Probalite predite d'etre diabetique",
                     xlim=c(0,1))
                
                
                FRCV3$Predictions2 = predict(mod8, newdata=FRCV3,type="response")
                describe(Predictions2)
                str(FRCV3)
                FRCV3$Predicts = ifelse(FRCV3$Predictions2 >.5, 1, 0)
                FRCV3$Predicts = as.factor(FRCV3$Predicts)
                table(FRCV3$Predicts, FRCV3$Diabete)
                FRCV3$Predicts=relevel(FRCV3$Predicts, ref="1")
                FRCV3$Diabete=relevel(FRCV3$Diabete, ref="1")
                
                library(epibasix)
                
                A = sensSpec(table(FRCV3$PredictsF, FRCV3$Diabete))
                summary(A)
                
# TRACER LA COURBE ROC DU MODELE AVEC SON AUC 
                library(Deducer)
                rocplot(mod8)
                
                library(blorr)
                blr_roc_curve(blr_gains_table(mod8))
                
                library(pROC)
                roc(FRCV3$Diabete ~ FRCV3$Predictions2, plot = T, print.auc=T)
        
                
                
                
                
                