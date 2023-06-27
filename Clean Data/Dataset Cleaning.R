#### Read-Me ####
##  This script pulls in the CPDat and CSCP excel files saved on Github and 
##  cleans them.
##  The cleaned datasets are exported and saved in the Cleaned Data folder on Github

# Libraries we might need
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", 
                                         repos = "http://cran.us.r-project.org")
if(!require(writexl)) install.packages("writexl", 
                                      repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", 
                                       repos = "http://cran.us.r-project.org")


library(tidyverse)
library(readxl)
library(writexl)
library(stringr)


# From Github, download the CPDat files save them in a local directory. Note that
# the working directory here is very similar to how the folders and files are organized
# on Github. For the code to work create a working directory as follows:
#     setwd("./Harmonization-of-Product-Categories/Raw Data/CPDat")
setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Raw Data/CPDat")

# Read in the CPDat files
cpact <- read_excel(path = "chemical_and_product_categories.xlsx")
cpcat_terms <- read_excel(path = "chemical_and_product_category_terms.xlsx")
substances <- read_excel(path = "cpdat_source_substances.xlsx")
rep_uses <- read_excel(path = "functional_uses.xlsx")
ingredients <- read_excel(path = "ingredients.xlsx")
pred_uses <- read_excel(path = "predicted_functional_uses.xlsx")
products <- read_excel(path = "products.xlsx")

# From Github also download the California Safe Cosmetics Product Database (CSCP).
# This is titled as "CDPH_Search_results.xlsx". This file is located in the Raw Data
# folder. Download the file and save it in a local directory using the convention:
#       setwd("./Harmonization-of-Product-Categories/Raw Data")
setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Raw Data")

# Read in the CSCP dataset
CSCP <- read_excel(path = "CDPH_Search_results.xlsx")

# Now we construct a dataframe containing the CPDat data. For our purposes we
# to have product names, product categories, and ingredients.
# To get this data we merge the dataframes:
#   1.  products
#   2.  ingredients
#   3.  substances


CPDat <- merge(x=products,y=ingredients,by="products_id")
CPDat <- merge(x=CPDat,y=substances,by ="cpdat_substance_record_id")

# View the CPDat dataframe to check the merge was performed successfully
View(CPDat)

# From the CPDat dataframe select the columns
#   product_name
#   product_use_category
#   product_use_category_description
#   chemical_name
#   casrn

CPDat <- CPDat %>% select(product_name,product_use_category,
                          product_use_category_description,chemical_name,casrn)

# Then rename column headers
CPDat <- CPDat %>% rename(ProductName = product_name,
                          ProductUseCategory = product_use_category,
                          ProductUseCategoryDescription = product_use_category_description,
                          ChemicalName = chemical_name,
                          CASRN = casrn)

# Set empty CASRN entries to No CASRN
CPDat$CASRN <- ifelse(is.na(CPDat$CASRN),"No CASRN",CPDat$CASRN)

# Separate out the entries in the ProductUseCategory column into separate columns. First
# split by the colon separator

CPDat[c("ProductCategory1","ProductCategory2")] <- str_split_fixed(CPDat$ProductUseCategory,":",2)

# Then split by the semicolon separator
CPDat[c("ProductCategory2","ProductCategory3")] <- str_split_fixed(CPDat$ProductCategory2,";",2)

# Looking at the CPDat dataframe some entries in the ProductUseCategory column are NA. We
# set blank entries in ProductCategory1, ProductCategory2 and ProductCategory3 to NA, so
# it matches with the ProductCategory column.

CPDat$ProductCategory1 <- ifelse(CPDat$ProductCategory1=="","NA",CPDat$ProductCategory1)
CPDat$ProductCategory2 <- ifelse(CPDat$ProductCategory2=="","NA",CPDat$ProductCategory2)
CPDat$ProductCategory3 <- ifelse(CPDat$ProductCategory3=="","NA",CPDat$ProductCategory3)

# Now drop the original ProductUseCategory column and rearrange column orders
CPDat <- CPDat %>% select(-ProductUseCategory)
CPDat <- CPDat %>% select(ProductName,ProductCategory1,ProductCategory2,
                          ProductCategory3,ProductUseCategoryDescription,
                          ChemicalName,CASRN)

# Use the head function to take a look at the entries in the ChemicalName column
head(CPDat)

# By looking at the results of the head function, we can see that the ChemicalName
# column needs to be cleaned. This requires to first export the CPDat dataframe as
# a flat excel file and then performing a manual cleanup. Part of this manual cleanup
# involves running the ChemicalName entries through the United States Environmental
# Protection Agency's (U.S. EPA) Comptox Dashboard Batch search feature. This Batch
# search feature is explained in more detail in the accompanying report which can
# found on Github. The reason for using the Batch search feature is to find more
# common chemical names for these ingredients.

# First filter the CPDat dataframe to only include beauty and personal care products
# as this is the dataset which will be used for machine learning.
# This level of product categorization is in the ProductCategory1 column
CPDat <- CPDat %>% filter(ProductCategory1 == "personal care")

# Set working directory for saving the CDPat dataframe. This dataframe will be saved
# in a folder called Clean Data which mimics how folders and files are organized on
# Github.

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Clean Data")

write_xlsx(CPDat, "CPDat_Clean.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

# Reload the CPDat dataframe.
# Check that we're in the right working directory
getwd()

# Read in the CPDat_Clean flat excel file which now contains an additional column
# called PREFERRED_NAME. This column contains entries of chemical names from running
# a batch search on EPA's Comptox Dashboard of CASRN entries.

CPDat_Clean <- read_excel(path = "CPDat_Clean.xlsx")

# Rename the PREFERRED_NAME column header
CPDat_Clean <- CPDat_Clean %>% rename(PreferredName = PREFERRED_NAME)

# Set empty entries in the PreferredName column to NA
CPDat_Clean$PreferredName <- ifelse(is.na(CPDat_Clean$PreferredName),"NA",CPDat_Clean$PreferredName)

# Now we are done cleaning the CPDat_Clean dataframe

### Now we clean the CSCP dataframe
# Use the head function to inspect the first few rows of the CSCP dataframe
head(CSCP)
# An inspection of the first few rows of the dataframe indicates the presence of 
# variant products. Variants are similar products but differ in certain aspects such
# color. In this case we only want to keep one of the variants and remove the others.

# First we remove Product Id and Variant columns since they are not needed
CSCP <- CSCP %>% select(-`Product Id`,-Variant)

# Then we use distinct and remove duplicates in the Company column
CSCP <- distinct(CSCP,Company,.keep_all = TRUE)

# Use the head function to see if duplicates were correctly removed
head(CSCP)

# Now assign a vector called Chemicals to hold the entries in the Ingredient Name
# column to see if the entries in the Ingredient Name column need to be cleaned

Chemicals <- CSCP$`Ingredient Name`
Chemicals

# An inspection of this vector shows a mixture of entries listing chemical names
# along with their associated CAS numbers, chemical names by themselves without their
# associated CAS numbers, and inconsistencies in chemical names. This will involve a manual
# cleanup similar to the type of cleanup performed for the CPDat dataset above.

# The CSCP dataset will be saved as a flat file in the following local directory
#     ./Harmonization-of-Product-Categories/Raw Data

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Raw Data")

write_xlsx(CSCP,"CDPH.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)