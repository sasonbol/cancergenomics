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
for (i in studies1){                  
      
      profile=paste(i, "rna_seq_v2_mrna_median_Zscores", sep="_")
      case=paste(i, "sequenced", sep="_")
      study_data<- getProfileData(mycgds,genes, profile, case)
      ROWS<-gsub("TCGA",i, rownames(study_data))
      rownames(study_data)<- ROWS
      combined_data<- rbind(combined_data, study_data)
      print(dim(combined_data))
      
}
#the "print'was just to make sure that everything is working fine
write.table (combined_data, file="combined_data.txt", sep = "\t", quote = FALSE)
all.data<-read.table("combined_data.txt", header=TRUE)
#to make sure that the values in the columns are considered numeric, test the class of 1 column
class(all.data$AKT1)
nrow(all.data)
complete<-complete.cases(all.data)
all.data2<- all.data[complete,] 
nrow(all.data2)
# to know the number of samples of each cancer type in all.data2 dataset
grep("laml", row.names(all.data2))
grep("blca", row.names(all.data2))
grep("kirc", row.names(all.data2))
grep("luad", row.names(all.data2))

cancertypes<-vector("character", length=1670)
cancertypes<-(c(rep("Leukemia", times= 343),rep("Bladder", times=156), 
                rep("Kidney", times=812), rep("Lung", times=359)))
data<-cbind(cancertypes,all.data2)
head(data)
# to get the mean value for the expression of each gene in each cancer type
s<- split(data, data$cancertypes)
means<-sapply(s,function(x) colMeans(x[,genes]))
#sapply is used instead of lapply to generate a matrix instead of a list
means
#to make the varialbles(genes) in the columns, matrix transpose was done
means<- t(means)
means
boxplot(means, las=2, main=" means of expression levels of diferent genes in 4 different cancer types")


