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


#Calibrate the features (Excel)
cross_sectoral_diversity_c <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.185, c=0.295, e=0.355")
cross_sectoral_diversity_c_INVERSE <- calibrate(QCA_Data_R$cross_sectoral_diversity, thresholds = "i=0.355, c=0.295, e=0.185")
project_total_cost_c <- calibrate(QCA_Data_R$project_total_cost, thresholds = "e=2000000, c=3500000, i=10000000")
project_duration_c <- calibrate(QCA_Data_R$project_duration, thresholds = "e=36, c=48, i=60")
project_partners_c <- calibrate(QCA_Data_R$project_partners, thresholds = "e=5.5, c=9.5, i=15")
commercial_partner_rate_c <- calibrate(QCA_Data_R$commercial_partner_rate, thresholds = "e=0.186, c=0.501, i=0.609")
internationality_c <- calibrate(QCA_Data_R$internationality, thresholds = "e=0.251, c=0.395, i=0.505")
locality_c <- calibrate(QCA_Data_R$locality, thresholds = "e=0.181, c=0.275, i=0.361")


#New dataframe with calibrated features
QCA_Data_R_C_original <-data.frame(cross_sectoral_diversity_c, project_total_cost_c, project_duration_c, project_partners_c, commercial_partner_rate_c, 
                          internationality_c, locality_c, row.names=project_acronym)
head(QCA_Data_R_C_original)


#Export calibrated features
write.csv(QCA_Data_R_C_original,"C:/Users/katha/OneDrive/Dokumente/01_Uni/KIT/02_Master/10_Master Thesis/Inhaltlich/R/QCA_Data_R_C.csv", row.names = TRUE)


#Correlation analysis with Pearsson (since metric data)
corr_tab <- cor(QCA_Data_R)
corr_tab_sign <-rcorr(as.matrix(QCA_Data_R))
corr_tab
corr_tab_sign

#Plot the calibrated features
par(mfrow = c(1, 2))
plot(QCA_Data_R$cross_sectoral_diversity, cross_sectoral_diversity_c, main = "Cross-sectoral diversity", xlab = "Raw data",
     ylab = "Calibrated data")
plot(QCA_Data_R$cross_sectoral_diversity, cross_sectoral_diversity_c_INVERSE, main = "Cross-sectoral diversity INVERSE", xlab = "Raw data",
     ylab = "Calibrated data")

par(mfrow = c(1, 2))
plot(QCA_Data_R$project_total_cost, project_total_cost_c, main = "Project total cost", xlab = "Raw data",
     ylab = "Calibrated data")
plot(QCA_Data_R$project_duration, project_duration_c, main = "Project duration", xlab = "Raw data",
     ylab = "Calibrated data")

par(mfrow = c(1, 2))
plot(QCA_Data_R$project_partners, project_partners_c, main = "Project partners", xlab = "Raw data",
     ylab = "Calibrated data")
plot(QCA_Data_R$commercial_partner_rate, commercial_partner_rate_c, main = "Commercialization", xlab = "Raw data",
     ylab = "Calibrated data")

par(mfrow = c(1, 2))
plot(QCA_Data_R$internationality, internationality_c, main = "Internationality", xlab = "Raw data",
     ylab = "Calibrated data")
plot(QCA_Data_R$locality, locality_c, main = "Locality", xlab = "Raw data",
     ylab = "Calibrated data")


#Introduce new dataframe without commercial_partner rate
#QCA_Data_R_C <-data.frame(cross_sectoral_diversity_c, project_total_cost_c, project_duration_c, project_partners_c,
                          #internationality_c, locality_c, row.names=project_acronym)


#Analysis of necessity with relevance threshold (ron.cut=0.6) (Dusa 2019)
superSubset(QCA_Data_R_C_original, outcome = "cross_sectoral_diversity_c", incl.cut=0.9, ron.cut=0.6)

#Analysis of sufficieny
superSubset(QCA_Data_R_C_original, outcome = "cross_sectoral_diversity_c", relation = "sufficiency", incl.cut=0.9, ron.cut=0.6)

#Analysis of sufficieny INVERSE
superSubset(QCA_Data_R_C_original, outcome = "~cross_sectoral_diversity_c", relation = "sufficiency", incl.cut=0.9, ron.cut=0.6)

#XYplots
#XYplot(1-QCA_Data_R_C$"internationality",QCA_Data_R_C$"cross_sectoral_diversity_c", QCA_Data_R_C_original, relation = "sufficiency", mguides = TRUE,
 #      jitter = FALSE, clabels = NULL, enhance = FALSE, model = FALSE)
#XYplot(1-QCA_Data_R_C$"internationality",QCA_Data_R_C$"cross_sectoral_diversity_c", QCA_Data_R_C_original, enhance = TRUE, jitter=TRUE)
#XYplot("~INTERNATIONALITY_C=>CROSS_SECTORAL_DIVERSITY_C", data=QCA_Data_R_C_original, relation = "sufficiency", mguides = TRUE,
 #      jitter = FALSE, clabels = NULL, enhance = FALSE, model = FALSE)

#------------------------------Truthtable and Minimization with Commerz-----------------------------------------#

#Truth table with Commerz 
ttQCADataRC_original <- truthTable(QCA_Data_R_C_original, outcome = "cross_sectoral_diversity_c", conditions = "project_total_cost_c, project_duration_c, 
                          commercial_partner_rate_c, project_partners_c, internationality_c, locality_c",  incl.cut=0.8, pri.cut=0.65, exclude =NULL, 
                          complete = TRUE, use.letters=FALSE,show.cases=TRUE, dcc=TRUE, sort.by = "incl", n.cut=1)
ttQCADataRC_original
write.table(ttQCADataRC_original$tt, 
            file="C:/Users/katha/OneDrive/Dokumente/01_Uni/KIT/02_Master/10_Master Thesis/Inhaltlich/R/QCA_Data_RC_original_Truthtable.txt",
            sep="\t", quote=FALSE)

#Check for CSAs in the truthtable
CSA1 <- findRows(obj = ttQCADataRC_original, type=2)
CSA2 <- findRows(obj = ttQCADataRC_original, type=3)
CSA1
CSA2

#Venn diagram for truth table
venn(ttQCADataRC_original, counts=TRUE, opacity = ttQCADataRC_original$tt$incl, ilabels=TRUE)

#Minimization Parsimonious
mmQCADataRC_original_P <- minimize(ttQCADataRC_original, include = "?", details=TRUE, show.case=TRUE)
mmQCADataRC_original_P
mmQCADataRC_original_P$PIchart
M1_P <- "commercial_partner_rate_c + ~internationality_c + project_total_cost_c*project_partners_c + project_partners_c*locality_c"
XYplot(M1_P, "cross_sectoral_diversity_c", data = QCA_Data_R_C_original, enhance=FALSE, model=TRUE)

#Minimization intermediate Commercial, Locality
mmQCADataRC_original_I <- minimize(ttQCADataRC_original, include = "?", dir.exp="1,-,1,1,-,1", details=TRUE, show.case=TRUE)
mmQCADataRC_original_I
mmQCADataRC_original_I$PIchart
M1_I <- "project_total_cost_c*project_partners_c + commercial_partner_rate_c*locality_c + ~internationality_c*locality_c + project_total_cost_c*commercial_partner_rate_c*internationality_c +
       ~project_duration_c*commercial_partner_rate_c*internationality_c + ~project_duration_c*project_partners_c*~internationality_c + ~project_duration_c*project_partners_c*locality_c"
XYplot(M1_I, "cross_sectoral_diversity_c", data = QCA_Data_R_C_original, enhance=FALSE, model=TRUE)

#Venn
#venn("project_total_cost_c*project_partners_c + commercial_partner_rate_c*locality_c + ~internationality_c*locality_c + project_total_cost_c*commercial_partner_rate_c*internationality_c +
   #    ~project_duration_c*commercial_partner_rate_c*internationality_c + ~project_duration_c*project_partners_c*~internationality_c + ~project_duration_c*project_partners_c*locality_c", ilabels = TRUE)


#Minimization Complex
mmQCADataRC_original_C <- minimize(ttQCADataRC_original, details=TRUE, show.case=TRUE)
mmQCADataRC_original_C
mmQCADataRC_original_C$PIchart
M1_C <- "project_total_cost_c*project_duration_c*~commercial_partner_rate_c*project_partners_c +
    project_total_cost_c*~commercial_partner_rate_c*project_partners_c*~locality_c +
    project_total_cost_c*commercial_partner_rate_c*~project_partners_c*internationality_c +
    ~project_duration_c*~commercial_partner_rate_c*project_partners_c*~internationality_c +
    ~project_duration_c*commercial_partner_rate_c*~project_partners_c*internationality_c +
    ~project_duration_c*~commercial_partner_rate_c*~internationality_c*locality_c +
    commercial_partner_rate_c*~project_partners_c*internationality_c*locality_c +
    ~project_total_cost_c*~project_duration_c*~commercial_partner_rate_c*project_partners_c*locality_c +
    ~project_total_cost_c*~commercial_partner_rate_c*~project_partners_c*~internationality_c*locality_c"
XYplot(M1_C, "cross_sectoral_diversity_c", data = QCA_Data_R_C_original, enhance=FALSE, model=TRUE)

#---------------------------------------------------------------------------------------------------------------#



#------------------------------Truthtable and Minimization without Commerz-----------------------------------------#

#Truth table forumla without Commerz 
#ttQCADataRC_nocommerz <- truthTable(QCA_Data_R_C_nocommerz, outcome = "cross_sectoral_diversity_c", conditions = "project_total_cost_c, project_duration_c, 
 #                         project_partners_c, internationality_c, locality_c",  incl.cut=0.8, pri.cut=0, exclude =NULL, 
  #                        complete = TRUE, use.letters=FALSE,show.cases=TRUE, dcc=FALSE, sort.by = "incl", n.cut=0)
#ttQCADataRC_nocommerz
#Minimization
#mmQCADataRC_nocommerz <- minimize(ttQCADataRC_nocommerz, include = "?", details=TRUE, show.case=TRUE)
#mmQCADataRC_nocommerz
#mmQCADataRC_nocommerz$PIchart
#Sufficiency relation of the model
#sol <- "~internationality_c + project_total_cost_c*project_partners_c + project_duration_c*locality_c"
#XYplot(sol, "cross_sectoral_diversity_c", data = QCA_Data_R_C, enhance=FALSE, model=TRUE)

#---------------------------------INVERSE CALCULATION---------------------------------#

#New dataframe with calibrated features INVERSE CALCULATION
QCA_Data_R_C_INVERSE <-data.frame(cross_sectoral_diversity_c_INVERSE, project_total_cost_c, project_duration_c, project_partners_c, commercial_partner_rate_c, 
                         internationality_c, locality_c, row.names=project_acronym)
head (QCA_Data_R_C_INVERSE)

#Truth table  with commerz, dass lediglich ein positiver outcome rauskommt, liegt an incl.cut=0.8
ttQCADataRC_INVERSE <- truthTable(QCA_Data_R_C_INVERSE, outcome = "cross_sectoral_diversity_c_INVERSE", conditions = "project_total_cost_c, project_duration_c, 
 project_partners_c, commercial_partner_rate_c, internationality_c, locality_c",  incl.cut=0.8, pri.cut=0.65, exclude =NULL, 
                         complete = TRUE, use.letters=FALSE,show.cases=TRUE, dcc=FALSE, sort.by = "incl", n.cut=0)
ttQCADataRC_INVERSE


#Minimization
mmQCADataR_C_INVERSE <- minimize(ttQCADataRC_INVERSE, include = "?", details=TRUE, show.case=TRUE)
mmQCADataR_C_INVERSE
mmQCADataR_C_INVERSE$PIchart

#dor <- "~project_total_cost_c + ~project_duration_c + internationality_c + ~project_partners_c*~locality_c"
#XYplot(dor, "cross_sectoral_diversity_c_INVERSE", data = QCA_Data_R_C_INVERSE, enhance=FALSE, model=TRUE)

