#!/usr/bin/env Rscript

#FileCompareR v2.0.1


##################
#
# USAGE
#
##################

#This script takes two data files (.tsv, .csv, .xlsx), a clinical and a metadata file in most cases, and compares two selected columns.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla FileCompareR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr",
                   "readr",
                   "stringi",
                   "janitor",
                   "readxl",
                   "optparse",
                   "tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-c", "--clinical"), type="character", default=NULL, 
              help="clinical dataset file (.xlsx, .tsv, .csv)", metavar="character"),
  make_option(c("-l", "--clinical_sheet"), type="character", default=NULL, 
              help="clinical dataset file sheet if the file is an .xlsx", metavar="character"),
  make_option(c("-m", "--metadata"), type="character", default=NULL, 
              help="metadata dataset file (.xlsx, .tsv, .csv)", metavar="character"),
  make_option(c("-e", "--metadata_sheet"), type="character", default=NULL, 
              help="clinical dataset file sheet if the file is an .xlsx", metavar="character"),
  make_option(c("-t", "--translate"), type="character", default="No", 
              help="Flag 'Yes' if you would like to use a second column in the metadata file to translate a column in the clinical data file.", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nFileCompareR v2.0.1")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$clinical) | is.null(opt$metadata)){
  print_help(opt_parser)
  cat("Please supply both the clinical data file (-c) and metadata file (-m).\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Data file pathway
clinical_path=file_path_as_absolute(opt$clinical)

#Template file pathway
metadata_path=file_path_as_absolute(opt$metadata)

clinical_sheet=opt$clinical_sheet
metadata_sheet=opt$metadata_sheet
translate_flag=tolower(opt$translate)

#############
#
# Function and data frame work
#
#############


#Function to determine the file path
determine_path <- function(file_path){
  path=paste(dirname(file_path),"/",sep = "")
}

#Function to determine the output file name
determine_out <- function(file_path){
  file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
  
  #Output file name based on input file name and date/time stamped.
  output_file=paste(file_name,
                    "_Compare",
                    stri_replace_all_fixed(
                      str = Sys.Date(),
                      pattern = "-",
                      replacement = ""),
                    sep="")
}

#Function to determine the extension of the file
determine_ext <- function(file_path){
  ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
  
  return(ext)
}

#Read in page/file. 
#Further logic has been setup to accept the original XLSX as well as a TSV or CSV format.
read_in <- function(file_path, sheet){
  #Rework the file path to obtain a file name, this will be used for the output file.
  file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
  ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))

  if (ext == "tsv"){
    df=suppressMessages(read_tsv(file = file_path, guess_max = 1000000, col_types = cols(.default = col_character(),),trim_ws = FALSE))
  }else if (ext == "csv"){
    df=suppressMessages(read_csv(file = file_path, guess_max = 1000000, col_types = cols(.default = col_character()), trim_ws = FALSE))
  }else if (ext == "xlsx"){
    df=suppressMessages(read_xlsx(path = file_path,sheet = sheet, guess_max = 1000000, col_types = "text",trim_ws = FALSE))
  }else{
    stop("\n\nERROR: Please submit a data file that is in either xlsx, tsv or csv format.\n\n")
  }
  return(df)
}

#Function to allow user input mid script run
typeline <- function(msg="Enter text: ") {
  if (interactive() ) {
    txt <- readline(msg)
  } else {
    cat(msg);
    txt <- readLines("stdin",n=1);
  }
  return(txt)
}

#Function to check user input and prompt for an expected input when input does not recognize available options.
check_input <- function(dataframe,whichdf){
  double_check=0
  col_name=typeline(paste("\nBased on the following columns, which column should the ",whichdf," file use:\n\n",paste(colnames(dataframe),collapse = "\n"),"\n\n",sep = ""))
  while(double_check==0){
    if (!(col_name %in% colnames(dataframe))){
      cat("\n********\nPlease select one of the following viable options:\n********\n")
      col_name=typeline(paste("\n",paste(colnames(dataframe),collapse = "\n"),"\n\n",sep = ""))
    }else{
      double_check=1
    }
  }
  return(col_name)
}


#pull information for clinical
df_clinical=read_in(clinical_path,clinical_sheet)
path=determine_path(clinical_path)
output_file=determine_out(clinical_path)

#pull information for metadata
df_metadata=read_in(metadata_path,metadata_sheet)

#Prompt inputs from user and save that input
clinical_col=check_input(df_clinical, "clincal")
metadata_col=check_input(df_metadata, "metadata")

#Check for values in clinical that are not in metadata
clinical_uniques=unique(df_clinical[!(df_clinical[clinical_col][[1]] %in% df_metadata[metadata_col][[1]]),clinical_col][[1]])

#Check for values in metadata that are not in clinical
metadata_uniques=unique(df_metadata[!(df_metadata[metadata_col][[1]] %in% df_clinical[clinical_col][[1]]),metadata_col][[1]])

#If translating a column in the clinical file
if (translate_flag=="yes"){
  translate_list=c()
  cat("\nThe flag for translation has been selected. Please choose a column to translate to.")
  translate_col=check_input(df_metadata,"metadata")
  for (row in 1:dim(df_clinical)[1]){
    value=df_clinical[clinical_col][row,]
    #look for array delimiter
    if (grepl(pattern = ";",x = value)){
      value_array=stri_split_fixed(str = value, pattern = ';')[[1]]
      new_value_array=c()
      for (value_array_element in value_array){
        grep_pos=grep(pattern = TRUE, x = (df_metadata[metadata_col][[1]] %in% value_array_element))
        if (length(grep_pos)!=0){
          if (length(grep_pos)>1){
            translate_list=c(translate_list,(paste("\nWARNING: Multiple positions were found for the value, ",value_array_element,", and the first instance, ", df_metadata[translate_col][[1]][grep_pos[1]] ,", at position, ", grep_pos[1]+1,", was used for the translation.", sep="")))
          }
          new_value_array=c(new_value_array,df_metadata[translate_col][[1]][grep_pos[1]])
        }else{
          translate_list=c(translate_list,paste("\nERROR: The following value, ", value_array_element,", was not found in the translation file.", sep = ""))
          new_value_array=c(new_value_array,value_array_element)
        }
      }
      df_clinical[clinical_col][row,]=paste(new_value_array,collapse = ";")
    }else{
      grep_pos=grep(pattern = TRUE, x = (df_metadata[metadata_col][[1]] %in% value))
      if (length(grep_pos)!=0){
        if (length(grep_pos)>1){
          translate_list=c(translate_list,paste("\nWARNING: Multiple positions were found for the value, ",value,", and the first instance, ",df_metadata[translate_col][[1]][grep_pos[1]],", at position, ", grep_pos[1]+1,", was used for the translation.", sep=""))
        }
        df_clinical[clinical_col][row,]=df_metadata[translate_col][[1]][grep_pos[1]]
      }else{
        translate_list=c(translate_list,paste("\nERROR: The following value, ", value,", was not found in the translation file.", sep = ""))
      }
    }
  }
}


###############
#
# Write out
#
###############

#Start write out
sink(paste(path,output_file,".txt",sep = ""))

#If there is a decent enough difference, this note will print out suggesting the user to look at the columns they chose for each file.
if (translate_flag!="yes"){
  if (length(metadata_uniques)/length(unique(df_clinical[clinical_col][[1]])) >0.5 | length(clinical_uniques)/length(unique(df_metadata[metadata_col][[1]])) >0.5){
    cat("\nThere is a large proportion (greater than 50%) of unique values that appears in one file and not the other.\n\nThis suggests choosing a different column for one or both of the two files.\n\n")
  }
}

#If there are values found in clinical unique that are not in metadata
if (length(clinical_uniques)>0){
  cat(paste("\nUnique ids that are found in the clinical data file, but not in the metadata file:\n", paste(clinical_uniques,collapse = "\n",sep = ""),"\n",sep = ""))
}

#If there are values found in metadata unique that are not in clinical
if (translate_flag!="yes"){
  if (length(metadata_uniques)>0){
    cat(paste("\nUnique ids that are found in the metadata data file, but not in the clinical file:\n", paste(metadata_uniques,collapse = "\n",sep = ""),"\n",sep = ""))
  }
}

#If everything matches and there are not differences
if (length(clinical_uniques)==0 & length(metadata_uniques)==0){
  cat("\nThe id values for each file's column are found in the other.\n")
}

if (translate_flag=="yes"){
  cat(paste("\nThe following is output for the translation of the ",clinical_col," in the ",clinical_path, " file, compared to the column ",metadata_col," found in the metadata file ",metadata_path," and translated from that file's column ",translate_col,".\n\n" ,sep=""))
  cat(unique(translate_list))
}

sink()


#Write out for translate
if (translate_flag=="yes"){
  
  ext=determine_ext(file_path = clinical_path)
  
  if (ext == "tsv"){
    suppressMessages(write_tsv(df, file = paste(output_file,".tsv",sep = ""), na=""))
  }else if (ext == "csv"){
    suppressMessages(write_csv(df, file = paste(output_file,".csv",sep = ""), na=""))
  }else if (ext == "xlsx"){
    wb=openxlsx::loadWorkbook(file = clinical_path)
    
    openxlsx::deleteData(wb, sheet = clinical_sheet,rows = 1:(dim(df_clinical)[1]+1),cols=1:(dim(df_clinical)[2]+1),gridExpand = TRUE)
    
    openxlsx::writeData(wb=wb, sheet=clinical_sheet, df_clinical,keepNA = FALSE)
    
    openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)
  }
  
}


cat(paste("\n\nProcess Complete.\n\nThe output files can be found here: ",path,"\n\n",sep = "")) 
