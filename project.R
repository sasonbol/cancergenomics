library (cgdsr)

# Create a connection to the cBioPortal for Cancer Genomics
mycgds = CGDS("http://www.cbioportal.org/public-portal/")
cancerStudies= getCancerStudies(mycgds)
studies<- cancerStudies[,1]
#create a subset of a small number of studies (acute myloid lukemia, kidney and lung cancers)
studies1<- c("laml_tcga_pub", "laml_tcga", "blca_tcga_pub", "blca_tcga", 
              "kirc_tcga_pub", "kirc_tcga", "luad_tcga_pub", 
             "luad_tcga")
#these genes are just use for the sake of trying the script
genes = c("EGFR", "HER1", "MYC", "RB1", "TP53", "PTEN")

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
combined[!is.na(combined_data[,1:6])]<- "1"
combined[is.na(combined_data[,1:6])]<- "0"

combined<-data.frame(lapply(combined, as.numeric))
combined<-as.matrix(combined)
#just to make sure that the columns became numeric
class(combined[,1])
# Generate a heatmap
heatmap(combined, cexCol=0.5, cexRow=0.9)

#this line always gets me many errors,as shown below:
#Error in lapply(args, is.character) : node stack overflow
#In addition: Warning messages:
#1: In min(x) : no non-missing arguments to min; returning Inf
#2: In max(x) : no non-missing arguments to max; returning -Inf
#Error during wrapup: node stack overflow
#Error in dev.flush() : node stack overflow
#Error during wrapup: node stack overflow
#Error in dev.flush() : node stack overflow
#Error during wrapup: node stack overflow


