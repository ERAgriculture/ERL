# Focus on ERA animal feed data

# The purpose of this notebook is to describe how to subset the ERA dataset to get information on the nutritional composition and digestibility of animal feeds.
# ERA is a synthesis of peer-reviewed research conducted in sub-Saharan Africa.

# [ERA Publication](https://www.nature.com/articles/s41597-024-03805-z)
# [ERA Codebase](https://github.com/CIAT/ERA_dev)

# 0) Set-up workspace ####
## 0.1) Load libraries and functions ####
# Install and load pacman if not already installed
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(data.table,s3fs,clipr,miceadds,httr,readxl,DT)

## 0.2) Set directories ####
dl_dir<-"downloaded_data"

# 1) Download & import data ####

## 1.1) ERA data tables ####
# Download
s3<-s3fs::S3FileSystem$new(anonymous = T)
era_s3<-"s3://digital-atlas/era"

# List the files in the s3 bucket
files<-s3$dir_ls(file.path(era_s3,"data"))
# This is the most recent version of the datas3://digital-atlas/era/data/skinny_cow_2022-YYYY-MM-DD.RData (substitute most recent date into filepath)
files<-tail(grep(".RData",grep("skinny_cow_2022",files,value=T),value=T),1)

# Set a save location for the dataset (amend to something more suitable for your needs)
save_path<-file.path(getwd(),dl_dir,basename(files))

if(!file.exists(save_path)){
  s3$file_download(files,save_path,overwrite = T)
}

# Load
livestock_data<- miceadds::load.Rdata2(file=basename(save_path),path=dirname(save_path))

## 1.2) ERA controlled vocabulary ####
### 1.2.1) Download most recent version era vocab from gtihub ####
era_vocab_url<-"https://github.com/peetmate/era_codes/raw/main/era_master_sheet.xlsx"
era_vocab_local<-file.path(getwd(),dl_dir,"era_master_sheet.xlsx")

update<-T
if(update){
  download.file(era_vocab_url, era_vocab_local, mode = "wb")  # Download and write in binary mode
}  

### 1.2.2) Load vocab ####
# Get names of all sheets in the workbook
sheet_names <- readxl::excel_sheets(era_vocab_local)
sheet_names<-sheet_names[!grepl("sheet|Sheet",sheet_names)]

# Read each sheet into a list of tables
master_codes <-suppressWarnings(suppressMessages(sapply(sheet_names, FUN=function(x){data.table(readxl::read_excel(era_vocab_local, sheet = x))},USE.NAMES=T)))

### 1.2.3) Explore livestock diets elements of the vocab ####
# Pull out the information on table and field descriptions from the vocab
field_descriptions<-copy(master_codes$era_fields_v2)

#### 1.2.3.1) Animal diet ingredients (ani_diet) ####
table_name<-"ani_diet"
ani_diet<-copy(master_codes[[table_name]])
descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# Remove depreciated columns
depreciated<-fields[grepl("Depreciated legacy column",Field_Description),Field]
ani_diet[,(depreciated):=NULL]

# How do we describe animal diets in ERA?
cat(descrip)

# Fields in the animal diet table
datatable(fields)

#### 1.2.3.2) AOM controlled vocabulary (aom) ####
table_name<-"AOM"
aom<-copy(master_codes[[table_name]])
descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# What is the aom vocab?
cat(descrip)

# Fields in the aom vocab
datatable(fields)

# 3) Extracted diet data ####

## 3.0) Animal diet
table_name<-"Animals.Out"
diet_overview<-livestock_data[[table_name]]

descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# Table description
cat(descrip)

cat("Fields in the", table_name,"table:")
datatable(fields)

## 3.1) Animal diet ingredients ####
table_name<-"Animals.Diet"
diet_ingredients<-livestock_data[[table_name]]
descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# Table description
cat(descrip)

cat("Fields in the", table_name,"table:")
datatable(fields)


# The diet ingredients table contains multiple types of information:
  # - How much of an entire diet is fed (Diet.Item=="Entire Diet")
  # - How much of a diet ingredient is fed
  # - The identities and amounts of ingredients within a diet
  # - The identities and amounts of ingredients within a composite diet item. In tern the composite diet item is part of a diet itself.
# This table is not required to meet our aim of creating a dataset of feed item nutritional composition and digestibility, but it provides 
# important insights into the structure of ERA animal diet information that will aid you in understanding the next sections.

# Note the ERA team is working on an update that will simplify the diet_ingredient table by splitting it into multiple tables based on the above.

## 3.2) Nutritional composition ####
# Information on the nutritional composition of animal diets or diet ingredients is stored in the Animals.
table_name<-"Animals.Diet.Comp"
diet_nutrition<-livestock_data[[table_name]]

descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# Table description
cat(descrip)

cat("Fields in the", table_name,"table:")
datatable(fields)

# We are interested in individual ingredients rather than entire diets, so we will remove descriptions of entire diets
diet_nutrition<-diet_nutrition[is_entire_diet==F][,is_entire_diet:=NULL]

# We also need to remove descriptions of composite diet items
diet_nutrition<-diet_nutrition[is_group==F][,is_group:=NULL]

# We also want only measured values
diet_nutrition<-diet_nutrition[DC.Method!="Estimated"][,DC.Method:=NULL]

# Diet items are described using multiple fields, let's use the most simplified version of the diet item names int he D.Item.Root.Comp.Proc_Major field. 
# This field combines harmonized values for root name of the diet item (e.g., Maize), the component (e.g., Bran), and major processes (e.g., Ground).
diet_nutrition<-diet_nutrition[,!c("D.Item","D.Item.Root.Other.Comp.Proc_All","D.Item.raw")]

# Rename the item name field
setnames(diet_nutrition,"D.Item.Root.Comp.Proc_Major","D.Item")

# Note that removing and renaming fields does have implications for linking records back to the diet ingredients table, if you need to do thisensure you retain the D.Item.raw field.
data.table(diet_nutrition)

### 3.2.1) Add AOM ####
# Add information about the diet item from the AOM vocab
diet_nutrition<-merge(diet_nutrition,
                      aom[,.(AOM,`Scientific Name`,NCBI,WFO,Feedipedia,CPC_Code_Product,CPC_Code_Component)],
                      by.x="D.Item.AOM",
                      by.y="AOM",
                      all.x=T)

datatable(diet_nutrition[!grepl("No Match",D.Item.AOM)])

# Some items have no match in AOM, why is this? They are reported as combined items and there is a technical reason why then cannot be split
# to their component items.
unique(diet_nutrition[grep("No Match",D.Item.AOM),.(B.Code,D.Item)])

## 3.3) Digestibility ####
table_name<-"Animals.Diet.Digest"
diet_digest<-livestock_data[[table_name]]

descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# Table description
cat(descrip)

cat("Fields in the", table_name,"table:")
datatable(fields)

# Remove descriptions of entire diets
full_length<-nrow(diet_digest)
diet_digest<-diet_digest[is_entire_diet==F][,is_entire_diet:=NULL]
sub_length<-nrow(diet_digest)

cat("Most the digestibility data in ERA is for entire diets,",round(100*(full_length-sub_length)/full_length,1),"% of records.")
# The ERA team will be updating this document in 2025 to also include the description of entire diets.

# Remove descriptions of composite diet items
diet_digest<-diet_digest[is_group==F][,is_group:=NULL]

# Remove estimated values
diet_digest<-diet_digest[!grepl("Estimated",DD.Method)]

# Remove and rename fields as per the nutrient composition table
diet_digest<-diet_digest[,!c("D.Item","D.Item.Root.Other.Comp.Proc_All","D.Item.raw")]

# Rename the item name field
setnames(diet_digest,"D.Item.Root.Comp.Proc_Major","D.Item")

### 3.3.1) Add AOM ####
# Add information about the diet item from the AOM vocab
diet_nutrition<-merge(diet_digest,
                      aom[,.(AOM,`Scientific Name`,NCBI,WFO,Feedipedia,CPC_Code_Product,CPC_Code_Component)],
                      by.x="D.Item.AOM",
                      by.y="AOM",
                      all.x=T)

nrow(diet_digest)
datatable(diet_digest[!grepl("No Match",D.Item.AOM)])

## 3.4) Bibliographic information ####
# The diet description tables all contain a `B.Code` field this can be linked back to bibliographic information about the publication.
table_name<-"Pub.Out"
bib_data<-livestock_data[[table_name]]

descrip<-field_descriptions[Table==table_name,unique(Table_Description)]
fields<-field_descriptions[Table==table_name,.(Field,Field_Description)]

# Table description
cat(descrip)

cat("Fields in the", table_name,"table:")
datatable(fields)

# Subset to studies that contain feed nutrition or digestibility info
bib_data<-bib_data[B.Code %in% unique(c(diet_nutrition[,B.Code],diet_digest[,B.Code]))]
datatable(bib_data)

# 4) Save processed data for easy access ####
save_dir<-"diet_data"
if(!dir.exists(save_dir)){
  dir.create(save_dir)
}

fwrite(diet_nutrition,file.path(save_dir,"feeds_nutrition.csv"))
fwrite(diet_digest,file.path(save_dir,"feeds_digestibility.csv"))
fwrite(bib_data,file.path(save_dir,"bibliograpy.csv"))
