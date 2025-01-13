# Focus on ERA animal feed data

# 0) Set-up workspace ####
## 0.1) Load libraries and functions ####
# Install and load pacman if not already installed
if (!require("pacman", character.only = TRUE)) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(data.table,s3fs,clipr,miceadds,httr,readxl)

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
# Download
era_vocab_url<-"https://github.com/peetmate/era_codes/raw/main/era_master_sheet.xlsx"
era_vocab_local<-file.path(getwd(),dl_dir,"era_master_sheet.xlsx")

update<-T
if(update){
  download.file(era_vocab_url, era_vocab_local, mode = "wb")  # Download and write in binary mode
}  

# Load
# Get names of all sheets in the workbook
sheet_names <- readxl::excel_sheets(era_vocab_local)
sheet_names<-sheet_names[!grepl("sheet|Sheet",sheet_names)]

# Read each sheet into a list
master_codes <-suppressWarnings(suppressMessages(sapply(sheet_names, FUN=function(x){data.table(readxl::read_excel(era_vocab_local, sheet = x))},USE.NAMES=T)))

aom<-master_codes$AOM
ani_diet<-master_codes$ani_diet

# How do we describe animal diets in ERA?

# D.Item (Diet Items) 
  # - free text values derived from publications
  # - these are a concatenation of the diet item name and processing applied to it. Item and process are presented in the form "name||process_1/process_2"
  # - synonyms are separated using a ";" delimiter, for example "name||process_1/process_2;name2||process_3;name_3||process_1;name4"

# D.Item.Root
  # A harmonized basal name of the diet item with information processing and components removed. Components are sub-parts of the diet item, for example `Vine` is the component of 
  # in the D.Item name `Watermelon Vine` therefore D.Item.Root is simply `Watermelon`.

# D.Item.Comp
  # The harmonized name of any sub-component mentioned in diet item name. (D.Item). For example `Vine` is the component of in the D.Item name `Watermelon Vine` therefore 
  # D.Item.Root is simply `Watermelon`.

# D.Item.Proc_All
  # Detailed and largely complete process names as described in the diet item name (D.Item). The purpose of this field to retain the unique identity of 
  # similar diet items with minor differences in processing when recombining root, component and processing fields to recreate a diet item name. Note
  # the diet D.Item.Proc_Major and D.Item.Proc_Minor fields contain more useful harmonized process names that connect the diet item to AOM.

# D.Item.Proc_Major
  # A harmonized field containing major processes that have been applied to the diet. These are practices that will have a major impact on the 
  # chemical composition or physical structure of the diet item.

ani_diet[,unique(unlist(strsplit(D.Item.Proc_Major,";")))]

# D.Item.Proc_Minor
  # A harmonized field containing minor processes that have been applied to the diet. These are practices that will not have a major impact on the 
  # chemical composition or physical structure of the diet item.

ani_diet[,unique(unlist(strsplit(D.Item.Proc_Minor,";")))]

# # D.Item.Other
  #

# D.Item.Root.Comp
  # D.Item.Root and D.Item.Comp concatenated together using a space.

# D.Item.Root.Comp.Proc_Major
  # 

# D.Item.Root.Other.Comp.Proc_All
  #

# D.Item.AOM
  #

# D.Item.Species 
  #

# D.Item.Species.Comp
  #

# Concentrate/Mechanically Processed/Chemically Processed/Diet.Sub/Diet.Add/Sub.Codes/Add.Codes
  # Depreciated columns, a legacy of an earlier version of the data extraction system



# 3) Prepare diet data

diet_descrip<-livestock_data$Animals.Diet

# Get meta-data that describes fields
master_codes$era_fields_v2[,unique(Table)]

no_match<-colnames(diet_descrip)[!colnames(diet_descrip) %in% master_codes$era_fields_v2[Table=="Animals.Diet",Field]]
write_clip(no_match)

diet_descrip_groups<-diet_descrip[!is.na(D.Item.Group)]
diet_descrip_entire<-diet_descrip[D.Type=="Entire Diet"]
diet_descrip<-diet_descrip[D.Is.Group!=T & !is.na(D.Item.Group)]

diet_descrip[D.Item.Raw!=D.Item_raw]

