
## Define the location of the table files
path_to_tables <- "L:/Lab/HEM/CPDat/cpdat_searcher/cpdat_tables/"

## Use the path to get a list of all of the CPDat table files
files <- list.files(path=path_to_tables,pattern="*.xlsx")

## Now make a named list where the names of the list are the names of the CPDat tables,
## and the values of the list is the names of the MS Excel file containing the table.
keys <- unlist(lapply(files,FUN=function(x){paste(path_to_tables,x,sep="")}))
values <- unlist(lapply(list.files(path=path_to_tables,pattern="*.xlsx"),
                        FUN=function(x){stringr::str_split(x,'[.]')[[1]][1]}))

## Here's that names list (it works like a dictionary in Python)
files <- setNames(as.list(keys),values)


## Load all of the tables
substances  <- readxl::read_excel(files[['cpdat_source_substances']])
products    <- readxl::read_excel(files[['products']])
ingredients <- readxl::read_excel(files[['ingredients']])
rep_uses    <- readxl::read_excel(files[['functional_uses']])
pred_uses   <- readxl::read_excel(files[['predicted_functional_uses']])
cpcat       <- readxl::read_excel(files[['chemical_and_product_categories']])
cpcat_terms <- readxl::read_excel(files[['chemical_and_product_category_terms']])

## Let's look for for chemicals that are ingredients in products labeled as shampoos
## First, we need to combine the products, ingredients, and chemical substances tables
## in order to have all the information we need in a single data frame.
table <- merge(x=products,y=ingredients,by='products_id')
table <- merge(x=table,y=substances,by='cpdat_substance_record_id')

## Now that the table is constructed, we can just look for any record whose product_name
## contains the substring "shampoo"
shampoos_name <- table[which(grepl("shampoo",table$product_name)),]

## We can also use the product_use_category to find products categorized as shampoos
shampoos_puc <- table[which(grepl("shampoo",table$product_use_category)),]


## We can follow a similar excercise to get at which chemicals are reported as having a
## functional use of 'solvent'
table <- merge(x=substances,y=rep_uses,by='cpdat_substance_record_id')

solvents <- table[which(grepl("solvent",table$reported_functional_use)),]
