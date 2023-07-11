#### Read-Me ####
##  This script performs data exploration on the cleaned CPDat and CDPH datasets which
##  can be found on Github.


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

# The cleaned CPDat and CDPH datasets are located in the following Github repository:
# https://github.com/moradkhantig/Harmonization-of-Product-Categories/tree/main/Clean%20Data
# They are titled CPDat_Clean and CDPH_Clean.
# These files should be downloaded and saved in a local directory with a folder organization
# similar to how the Github repository is organized.
# The local directory should be set as:
#       ./Harmonization-of-Product-Categories/Clean Data

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Clean Data")

CPDat_Clean <- read_excel(path = "CPDat_Clean.xlsx")
CSCP <- read_excel(path = "CDPH_Clean.xlsx")

# Note that the CSCP dataframe contains a column listing functional use data for each
# ingredient. We attempted to do that for the CPDat_Clean dataset as functional use data
# is provided as part of the CPDat database download. However, when exporting CPDat_Clean
# as a flat file from R, R was not able to write the data to excel as the number of rows
# were above the maximum allowed that an excel spreadsheet can handle. Therefore to
# also explore functional use information with the CPDat we have to load the functional
# use data provided in the CPDat. These are found in two separate files provided as part
# of the CPDat database download and are saved in the Raw Data folder in the Github
# repository. These two files should be downloaded and saved to a local directory as
# follows:
#   ./Harmonization-of-Product-Categories/Raw Data/CPDat

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Raw Data/CPDat")

substances <- read_excel(path = "cpdat_source_substances.xlsx")
rep_uses <- read_excel(path = "functional_uses.xlsx")

### First we explore the CPDat dataset
# View the CPDat dataset to see the type of columns in the dataset
View(CPDat_Clean)

# For visualizations looking at chemicals, we will use the PREFERRED_NAME column
# as the chemical names in this column are more systematic. See the report for
# more details about this.

# First we look at the distribution of the types of personal care products in the
# CPDat database. This level of product categorization is found in the column titled
# ProductCategory2. We will hold the results of the distribution of product types
# in a dataframe called Tab1. Before we do that we need to remove NA entries in the
# PREFERRED_NAME column

CPDat_Clean <- CPDat_Clean[-(which(CPDat_Clean$PREFERRED_NAME %in% "NA")), ]


Tab1 <- CPDat_Clean %>% group_by(ProductCategory2) %>% 
  count() %>%
  rename(ProductType = ProductCategory2,NumberOfProducts = n) %>%
  mutate(Frequency = (NumberOfProducts/nrow(CPDat_Clean))*100) %>%
  select(ProductType,Frequency)

# Creatw figure based on Tab1 which is titled CPDat_Fig2, then save CPDat_Fig2
# as a png file to a local directory set as:
#   .Harmonization-of-Product-Categories/Data Visualizations
# This local directory exacly mirrors how folders and files are organized on Github.
CPDatFig2 <- Tab1 %>% ggplot(aes(ProductType,Frequency)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Product Type") + ylab("Frequency of Product Types in CPDat")
CPDatFig2

ggsave("CPDat_ProdTypes.png",CPDatFig2,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

# Looking at Fig2, there are more shampoos, hand/body lotions, hair styling, 
# hair conditioners, and face creams/moisturizers. We filter the CPDat database to look
# at the chemicals that are present, specifically counting the number of chemicals. The
# results are saved in Tab2 which we then export as a flat file saving it to the following
# directory: ./Harmonization-of-Product-Categories/Data Visualizations"

CommonProducts <- CPDat_Clean %>% 
  filter(ProductCategory2 %in% c("shampoo","hand/body lotion","hair styling",
                                 "hair conditioner","face cream/moisturizer")) %>%
  select(ProductName,ProductCategory2,PREFERRED_NAME,CASRN) %>%
  rename(ProductCategory = ProductCategory2,IngredientName = PREFERRED_NAME)

Tab2 <- CommonProducts %>% 
  group_by(ProductCategory,IngredientName,CASRN) %>% 
  count(CASRN)

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")
write_xlsx(Tab2,"CPDatCommonProducts_ChemCounts.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)


# From Tab2 we want to get the chemicals who have been reported with high and low
# frequencies. A brief look at Tab 2 shows reported frequencies that are:
#   higher than 1000
#   between 100 and 1000
#   between 30 and 100
#   less than 30
# We therefore use these bins to filter Tab2. 
# We first filter Tab2 using the first bin, higher than 1000, and store the results
# in a dataframe called Tab3.
Tab3 <- Tab2 %>% filter(n>1000)


# However looking at the results of Tab3 we see that there are no ingredients reported
# at such high frequencies. So we next consider the second bin. The results of the next
# filtering will be stored in Tab4
Tab4 <- Tab2 %>% filter(n<1000 && n>100)


# We then create several figures for each product category in Tab4 and save each
# figure in .Harmonization-of-Product-Categories/Data Visualizations

FaceCream <- Tab4 %>% filter(ProductCategory == "face cream/moisturizer") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
FaceCream
ggsave("CPDat_FaceCream.png",FaceCream,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

HairCond <- Tab4 %>% filter(ProductCategory == "hair conditioner") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
HairCond
ggsave("CPDat_HairCond.png",HairCond,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)
  

HairStyling <- Tab4 %>% filter(ProductCategory == "hair styling") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
HairStyling
ggsave("CPDat_HairStyling.png",HairStyling,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

HandLotion <- Tab4 %>% filter(ProductCategory == "hand/body lotion") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
HandLotion
ggsave("CPDat_HandLotion.png",HandLotion,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

Shampoo <- Tab4 %>% filter(ProductCategory == "shampoo") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
Shampoo
ggsave("CPDat_Shampoo.png",Shampoo,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

# Then we apply the last filter smaller than 100 on Tab2 and save the results in
# Tab5
Tab5 <- Tab2 %>% filter(n<100 && n>30)

# Now we plot the results in Tab5 and save the plots.

FaceCream_10030 <- Tab5 %>% filter(ProductCategory == "face cream/moisturizer") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
FaceCream_10030
ggsave("CPDat_FaceCream10030.png",FaceCream_10030,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

HairCond_10030 <- Tab5 %>% filter(ProductCategory == "hair conditioner") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
HairCond_10030
ggsave("CPDat_HairCond10030.png",HairCond_10030,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

HairStyling_10030 <- Tab5 %>% filter(ProductCategory == "hair styling") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
HairStyling_10030
ggsave("CPDat_HairStyling_10030.png",HairStyling_10030,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

HandLotion_10030 <- Tab5 %>% filter(ProductCategory == "hand/body lotion") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
HandLotion_10030
ggsave("CPDat_HandLotion_10030.png",HandLotion_10030,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

Shampoo_10030 <- Tab5 %>% filter(ProductCategory == "shampoo") %>%
  ggplot(aes(IngredientName,n)) + geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical Name") + ylab("Frequency of Reported Chemicals")
Shampoo_10030
ggsave("CPDat_Shampoo_10030.png",Shampoo_10030,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

# Finally we apply the filter less than 30 to Tab2. The results will be stored
# in CPDatChem_lessThan30 which is arranged to contain the top 10 reported chemicals. 
# Then we save the dataframe as an excel flat file

CPDatChem_lessThan30 <- Tab2 %>% 
  filter(n<30) %>%
  group_by(ProductCategory,IngredientName,CASRN,n) %>%
  arrange(desc(n)) %>%
  top_n(10)

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")
write_xlsx(CPDatChem_lessThan30,"CPDatChem_lessThan30.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

  
# Next we want to see which of the reported chemicals have specific hazard traits.
# To do this we import SCP's Candidate Chemicals List. This is saved in the Clean Data
# folder on Github. As discussed in the report, the list contains chemicals which 
# authoritative bodies have reported having hazard traits.

# First check that we are in the right working directory
getwd()

# If we're not in the right working directory use setwd() to change the working
# directory to   ./Harmonization-of-Product-Categories/Clean Data

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Clean Data")

# Now read the excel file titled Candidate Chemicals List into a dataframe called
# CCL
CCL <- read_excel(path = "Candidate Chemicals List.xlsx")

# Use the head function to view the first few entries and column headers
head(CCL)

# We see that the CCL dataframe has the following columns
#     Chemical Name: The name of each chemical on the Candidate Chemicals List
#     CAS RN: The respective CAS number for each chemical
#     Synonyms: Any alternative chemical names if available for each chemical
#               on the Candidate Chemicals List
#     Hazard Traits: Associated hazard traits for each chemical on the Candidate
#                    Chemicals List as determined by authoritative bodies
#     Authoritative List: The associated authoritative list that lists specific
#                         hazard traits for chemicals


# Now we add a column called CandidateChemical which would have entries of True of False. An entry
# of True means the reported chemical in CommonProducts is a Candidate Chemical and
# an entry of False means that reported chemical is not a Candidate Chemical.

CommonProducts$CandidateChemical <- CommonProducts$CASRN %in% CCL$`CAS RN`

# First we want to see how many of the reported chemicals in CommonProducts are
# are Candidate Chemicals and how many are non-Candidate Chemicals.

CommonProductsChems_Total <- CommonProducts %>% 
  group_by(CandidateChemical) %>%
  count(CandidateChemical)

# write_xlsx(CommonProductsChems_Total,"CommonProdTest.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

CPDatCommonProdCCComp <- CommonProductsChems_Total %>%
  ggplot(aes(CandidateChemical,n,fill = CandidateChemical)) +
  geom_bar(stat = "identity") +
  xlab("Candidate Chemical") + ylab("Frequency of reported chemicals") +
  theme_bw()
CPDatCommonProdCCComp
ggsave("CPDat_CommonProdCC_Count.png",CPDatCommonProdCCComp,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

# Now we filter CommonProducts to only include Candidate Chemicals. The filtering
# will be on the CandidateChemical column. The result will be a dataframe only
# reporting chemicals in products which are Candidate Chemicals. We will store
# these results in a dataframe called CommonProductsCC

CommonProductsCC <- CommonProducts %>% filter(CandidateChemical == "TRUE")

# Now we count the number of reported chemicals similar to what was done above and
# store the number of reported Candidate Chemicals in Tab7
Tab7 <- CommonProductsCC %>%
  select(ProductCategory,IngredientName,CASRN) %>%
  group_by(ProductCategory,IngredientName,CASRN) %>%
  count(CASRN)

# Looking at Tab7 we see that we can bin the frequency of reported Candidate
# Chemicals as follows:
#   greater than 100
#   between 10 and 100
#   smaller than 10
# We now apply these bins to Tab7 and store the results in separate tables and save
# each of the tables

# First apply the bin greater than 100 to Tab7
Tab8 <- Tab7 %>% 
  filter(n>100) %>%
  group_by(ProductCategory,IngredientName,CASRN,n) %>%
  arrange(desc(n))

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")
write_xlsx(Tab8,"CPDatCommonProdCC_100.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)


# Next apply the bin between 10 and 100 to Tab7
Tab9 <- Tab7 %>% 
  filter(n>10 && n<100) %>%
  group_by(ProductCategory,IngredientName,CASRN,n) %>%
  arrange(desc(n))

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")
write_xlsx(Tab9,"CPDatCommonProdCC_10_100.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

# Finally apply the bin smaller than 10 to Tab7
Tab10 <- Tab7 %>% 
  filter(n<10) %>%
  group_by(ProductCategory,IngredientName,CASRN,n) %>%
  arrange(desc(n))

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")
write_xlsx(Tab10,"CPDatCommonProdCC_10.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)


### This concludes data analysis of the CPDat dataset ###

###########################################################################
## This begins the data exploration of the CDPH dataset which is loaded as
## CSCP
###########################################################################

# We begin looking at types of product categories in the CDPH dataset.
Tab11 <- CSCP %>%
  group_by(`Product Category`) %>%
  count(`Product Category`) %>%
  rename(ProductType = `Product Category`,NumberOfProducts = n) %>%
  mutate(Frequency = (NumberOfProducts/nrow(CSCP))*100) %>%
  select(ProductType,Frequency) %>%
  arrange(desc(Frequency)) %>%
  top_n(10)

# Save Tab11 as a flat file in the following working directory
#   ./Harmonization-of-Product-Categories/Data Visualizations
setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")

write_xlsx(Tab11,"CDPH_ProdTypes.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)  

# From Tab11, we see that the most frequently found product types in the
# CSCP dataset are Body Washes and Soaps, Anti-Wrinkle/Anti-Aging products, and
# Blushes. We now want to look at the distribution of reported chemicals in
# these products. The results of the distribution of reported chemicals are stored
# in Tab12 which we also want to export as a flat file and save it in the following
# directory: ./Harmonization-of-Product-Categories/Data Visualizations 

CSCPCommonProducts <- CSCP %>%
  filter(`Product Category` %in% c("Body Washes and Soaps","Anti-Wrinkle/Anti-Aging Products",
                            "Blushes")) %>%
  select(`Product Category`,PreferredIngredientName,CASRN)

Tab12 <- CSCPCommonProducts %>%
  group_by(`Product Category`,PreferredIngredientName,CASRN) %>%
  count(CASRN) %>%
  rename(ProductType = `Product Category`, NumberOfProducts = n) %>%
  arrange(desc(NumberOfProducts)) %>%
  top_n(10)

setwd("C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations")
write_xlsx(Tab12,"CSCPCommonProducts_ChemCounts.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)


# Next we want to look at the subset of reported chemicals which are 
# Candidate Chemicals. We do this using the CCL dataframe which is already
# loaded. As we have already discussed the CCL dataframe is SCP's Candidate Chemicals
# List which contains a list of chemicals which authoritative bodies have determined
# to have specific hazard traits.
# To get reported Candidate Chemicals, we add a column titled CandidateChemical to
# CSCPCommonProducts and then check if any of the CASRN entries in CSCPCommonProducts
# appear in the CCL dataframe. If there is a match, then the entry for a chemical 
# in the CandidateChemical column of the CSCPCommonProducts will be TRUE, and 
# FALSE if there is no match.

CSCPCommonProducts$CandidateChemical <- CSCPCommonProducts$CASRN %in% CCL$`CAS RN`
  
# Now we look to see how many of the reported chemicals in CSCPCommonProducts are
# Candidate Chemicals and how many are non-Candidate Chemicals

CSCPCommonProd_CC <- CSCPCommonProducts %>%
  group_by(CandidateChemical) %>%
  count(CandidateChemical) %>%
  ggplot(aes(CandidateChemical,n,fill = CandidateChemical)) +
  geom_bar(stat = "identity") +
  xlab("Candidate Chemical") + ylab("Frequency of reported chemicals") +
  theme_bw()
CSCPCommonProd_CC
ggsave("CSCP_CommonProdCC_Count.png",CSCPCommonProd_CC,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)

# Next we filter CSCPCommonProducts to only include Candidate Chemicals and re-create
# a similar dataframe as Tab12 which will report the number of Candidate Chemicals
# for each of the product types in CSCPCommonProducts.

CSCPCommonProductsCC <- CSCPCommonProducts %>% filter(CandidateChemical == "TRUE")

Tab13 <- CSCPCommonProductsCC %>%
  group_by(`Product Category`,PreferredIngredientName,CASRN) %>%
  count(CASRN) %>%
  rename(ProductType = `Product Category`, NumberOfProducts = n)
  
CPDatCommonProdType_CC <- Tab13 %>%
  ggplot(aes(PreferredIngredientName,NumberOfProducts,fill = ProductType)) +
  geom_bar(position = "dodge",stat = "identity") +
  theme_bw() +
  coord_flip() +
  xlab("Chemical name") + ylab("Frequency of reported chemicals")
CPDatCommonProdType_CC
ggsave("CSCP_CommonProdType_CC.png",CPDatCommonProdType_CC,
       device ="png",
       "C:/Users/tig_m/Documents/Data Science/R Captsone/Harmonization-of-Product-Categories/Data Visualizations",
       width=7, height=5)
