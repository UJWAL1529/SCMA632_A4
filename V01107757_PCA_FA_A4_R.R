install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}


# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv("C:\\A4\\Survey.csv") 

# Check the dimensions, names, and structure of the data
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)


#A)Do principal component analysis and factor analysis and identify the dimensions in the data. 
is.na(survey_df) 

# Check for missing values
sum(is.na(survey_df)) 

# Subset the relevant columns for analysis (assuming columns 20 to 46 are relevant)
sur_int=survey_df[,20:46] 

# Verify the structure and dimensions of the subset data
str(sur_int) 
dim(sur_int) 
library(GPArotation) 

# Perform Principal Component Analysis (PCA)
pca <- principal(sur_int,5,n.obs =162, rotate ="promax") 
pca 


# Perform Factor Analysis using the omega function from the psych package
om.h<-omega(sur_int,n.obs=162,sl=FALSE) 
op<-par(mfrow=c(1,1)) 
om<-omega(sur_int,n.obs=162) 
library(FactoMineR) 

# Perform PCA using the FactoMineR package
pca<-PCA(sur_int,scale.unit = TRUE) 
summary(pca)

# Create a biplot for visualization
biplot(pca_result) 

# Display the structure and dimensions of the subset data again for verification
str(sur_int) 
dim(sur_int) 
show(sur_int) 

#Factor Analysis 

factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 