packageQCA <- ("https://cran.r-project.org/src/contrib/Archive/QCA/QCA_3.13.tar.gz")
install.packages(packageQCA, repos=NULL, type="source")
#install.packages("QCA", dependencies = TRUE)
install.packages("Hmisc")
library("Hmisc")
library(QCA)
library(venn)


#Import dataset
QCA_Data_R <- read.csv("C:/Users/katha/OneDrive/Dokumente/01_Uni/KIT/02_Master/10_Master Thesis/Inhaltlich/R/QCA_Data_R.csv", sep = ";",
                       header = TRUE, dec = ",", row.names = 1)
head(QCA_Data_R)
project_acronym <-row.names(QCA_Data_R)


#Original Feature Calibration
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.185, c=0.295, e=0.355")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2000000, c=3500000, i=10000000")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=36, c=48, i=60")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=5.5, c=9.5, i=15")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.186, c=0.501, i=0.609")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.395, i=0.505")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.181, c=0.275, i=0.361")

#Calibrate the features (e=original, c=AVERAGE, i=original)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.185, c=0.260, e=0.355")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2000000, c=9570048.34, i=10000000")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=36, c=45.3, i=60")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=5.5, c=11.41, i=15")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.186, c=0.39, i=0.609")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.55, i=0.505")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.181, c=0.28, i=0.361")

#Calibrate the features (e=original, c=Median, i=original)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.185, c=0.221, e=0.355")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2000000, c=3189816.1, i=10000000")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=36, c=44.1, i=60")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=5.5, c=9.1, i=15")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.186, c=0.41, i=0.609")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.561, i=0.505")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.181, c=0.271, i=0.361")

#Calibrate the features (e=0.1, c=original, i=0.9)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.158, c=0.295, e=0.38")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=1995765.50, c=3500000, i=15549067.05")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=35, c=48, i=60.2")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=6, c=9.5, i=19")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.187, c=0.501, i=0.60")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.333, c=0.395, i=0.75")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.163, c=0.275, i=0.406")

#Calibrate the features (e=0.1, c=average, i=0.9)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.158, c=0.261, e=0.38")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=1995765.50, c=9570048.341, i=15549067.05")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=35, c=45.31, i=60.2")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=6, c=11.411, i=19")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.187, c=0.391, i=0.60")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.333, c=0.551, i=0.75")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.163, c=0.281, i=0.406")

#Calibrate the features (e=0.1, c=median, i=0.9)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.158, c=0.221, e=0.38")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=1995765.50, c=3189816.1, i=15549067.05")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=35, c=44.1, i=60.2")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=6, c=9.1, i=19")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.187, c=0.41, i=0.60")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.333, c=0.561, i=0.75")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.163, c=0.271, i=0.406")

#Calibrate the features (e=0.2, c=original, i=0.8)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.183, c=0.295, e=0.32")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2331841.5, c=3500000, i=7638060.06")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=35, c=48, i=56.2")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=7, c=9.5, i=13")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.25, c=0.501, i=0.5")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.4, i=0.703")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.195, c=0.275, i=0.360")

#Calibrate the features (e=0.2, c=average, i=0.8)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.183, c=0.261, e=0.32")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2331841.5, c=9570048.341, i=7638060.06")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=35, c=45.31, i=56.2")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=7, c=11.411, i=13")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.25, c=0.391, i=0.5")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.551, i=0.703")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.195, c=0.281, i=0.360")

#Calibrate the features (e=0.2, c=median, i=0.8)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.183, c=0.221, e=0.32")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2331841.5, c=3189816.01, i=7638060.06")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=35, c=44.1, i=56.2")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=7, c=9.1, i=13")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.25, c=0.41, i=0.5")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.551, i=0.703")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.195, c=0.271, i=0.360")




#New dataframe with calibrated features
QCA_Data_R_C_original <-data.frame(cross_sectoral_diversity_c, project_total_cost_c, project_duration_c, project_partners_c, commercial_partner_rate_c, 
                          internationality_c, locality_c, row.names=project_acronym)
head(QCA_Data_R_C_original)


#Export calibrated features
write.csv(QCA_Data_R_C_original,"C:/Users/katha/OneDrive/Dokumente/01_Uni/KIT/02_Master/10_Master Thesis/Inhaltlich/R/QCA_Data_R_C.csv", row.names = TRUE)




#------------------------------Truthtable and Minimization-----------------------------------------#

#Truth table with Commerz 
ttQCADataRC_original <- truthTable(QCA_Data_R_C_original, outcome = "cross_sectoral_diversity_c", conditions = "project_total_cost_c, project_duration_c, 
                          commercial_partner_rate_c, project_partners_c, internationality_c, locality_c",  incl.cut=0.8, pri.cut=0.65, exclude =NULL, 
                          complete = TRUE, use.letters=FALSE,show.cases=TRUE, dcc=TRUE, sort.by = "incl", n.cut=1)
ttQCADataRC_original
write.table(ttQCADataRC_original$tt, 
            file="C:/Users/katha/OneDrive/Dokumente/01_Uni/KIT/02_Master/10_Master Thesis/Inhaltlich/R/QCA_Data_RC_original_Truthtable.txt",
            sep="\t", quote=FALSE)


#Minimization Parsimonious
mmQCADataRC_original_P <- minimize(ttQCADataRC_original, include = "?", details=TRUE, show.case=TRUE)
mmQCADataRC_original_P
mmQCADataRC_original_P$PIchart

#Minimization intermediate Commercial, Locality
mmQCADataRC_original_I <- minimize(ttQCADataRC_original, include = "?", dir.exp="1,-,1,1,-,1", details=TRUE, show.case=TRUE)
mmQCADataRC_original_I
mmQCADataRC_original_I$PIchart

#---------------------------------------------------------------------------------------------------------------#

