install.packages("cgdsr")
library (cgdsr)

# Create a connection to the cBioPortal for Cancer Genomics
mycgds = CGDS("http://www.cbioportal.org/public-portal/")
cancerStudies= getCancerStudies(mycgds)
studies<- cancerStudies[,1]
#create a subset of a small number of studies (acute myloid lukemia, kidney and lung cancers)
studies1<- c("laml_tcga_pub", "laml_tcga", "blca_tcga_pub", "blca_tcga", 
             "kirc_tcga_pub", "kirc_tcga", "luad_tcga_pub", 
             "luad_tcga")
#These are cancer specific genes (25 selected genes)
genes<- c("FGFR3","TACC3", "TERT", "PIK3CA", "VHL", "WT1", "MET", "FLCN","TSC2", 
          "FH","SDHB","FLT3","IDH1","IDH2","RUNX1","CEBPA","KIT","PTPN11", "KRAS",
          "PTENP1","FAS","ALK","ERBB2","AKT1","MAP2K1")
# The names of the genes must follow HUGO Gene Nomenclature Committee
# HGNC (http://www.genenames.org/)
study_data<- data.frame()
combined_data<-data.frame()
#to get the mutation data for each study and combine them together
for (i in studies1){                  
      
      profile=paste(i, "mutations", sep="_")
      case=paste(i, "sequenced", sep="_")
      study_data<- getProfileData(mycgds,genes, profile, case)
      ROWS<-gsub("TCGA",i, rownames(study_data))
      rownames(study_data)<- ROWS
      combined_data<- rbind(combined_data, study_data)
      print(dim(combined_data))
      
}
#the "print'was just to make sure that everything is working fine
write.table (combined_data, file="combined_data.txt", sep = "\t", quote = FALSE)
combined<-read.table("combined_data.txt", colClasses= "character", na.strings=c("NaN","NA"))

#to convert all values in the "combined" dataset into numerical values, all mutations
#were give the number"1"while when no mutation was there the "NA" was replaced with "0"
combined[!is.na(combined[,1:25])]<- "1"
combined[is.na(combined[,1:25])]<- "0"

samples.names<- row.names(combined)

combined<-data.frame(lapply(combined, as.numeric))
row.names(combined)<- samples.names
head(combined)
cancertypes<-vector("character", length=1854)
cancertypes<-(c(rep("Leukemia", times= 396),rep("Bladder", times=158), 
                rep("Kidney", times=841), rep("Lung", times=459)))
combined2<-cbind(cancertypes,combined)
head(combined2)

#just to make sure that the columns became numeric except that of the cancertype
class(combined[,1])
class(combined[,2])


cancertype <- combined2[, 1]

# apply PCA 
pca <- prcomp(combined2[,2:26],
              center = TRUE,
              scale. = TRUE) 
print(pca)
# plot method
plot(pca, type = "l")
# summary method
summary(pca)
library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(pca,choices=1:2, obs.scale = 1, var.scale = 1, 
              groups = cancertype, ellipse = TRUE, 
              circle = TRUE, var.axes=FALSE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)