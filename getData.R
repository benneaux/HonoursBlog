require(tm)
require(pdftools)
require(stringi)
require(httr)
require(rvest)
require(tidyverse)
library(stringr)
library(lubridate)


# # run the following after restarting R
# #Sys.setenv(http_proxy = "http://<username>:<password>@proxy.newcastle.edu.au:8080")

page <- read_html("http://flutracking.net/Info/Reports/")

page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("/[0-9]{3,}$") -> filenames # find those that end a numeric string > length(3)

filenames %>%                    # extract the numeric id from each name
  str_extract("[0-9]+") %>% # i.e. '201728' instead of 'Info/Reports/201728'
  as.data.frame() -> filenames

prev_retrieved_files <- data.frame(
  files = readRDS(
    "~/R/HonoursBlog/data/retrieved_files_list.RDS"),
  stringsAsFactors = FALSE)

colnames(prev_retrieved_files) <- c("files")

names <- data.frame(
  files = setdiff(
    filenames$.,
    prev_retrieved_files[,1])) -> names

if(nrow(names)>0){
saveRDS(rbind(
  prev_retrieved_files,
  names),
  "~/R/HonoursBlog/data/retrieved_files_list.RDS")

rm(prev_retrieved_files, filenames)
#
# # ###############################################################################
# # #
# # # Dowload the files
# # #
# # ###############################################################################
# #
#
fname <- vector(mode = "character", length = nrow(names))
if((nrow(names) > 0) == TRUE){
  for(i in 1:nrow(names)){

    fileurl  = paste0("http://flutracking.net/Info/Reports/",
                      as.character(names[i,1]))
    fil      = GET(fileurl,
                   write_disk("~/R/HonoursBlog/data/files/tmp.fil",
                              TRUE))
    fname[i]    = str_match(headers(fil)$`content-disposition`,
                            "=(.*)")[2]

    file.rename("~/R/HonoursBlog/data/files/tmp.fil",
                paste0("~/R/HonoursBlog/data/files/",
                       fname[i]))

    Sys.sleep(2)
  }

  ###############################################################################
  #
  # Setup the container for the data
  #
  ###############################################################################

  files <- list.files(path = "~/R/HonoursBlog/data/files/",pattern = "pdf$")
  files <- files[which(files %in% fname)]
  data <- tbl_df(matrix(nrow=nrow(names),ncol = 13))

  ###############################################################################
  #
  # Import regex codes
  #
  ###############################################################################

  codes <- readRDS("~/R/HonoursBlog/data/flutracking_regex_codes.RDS")

  ###############################################################################
  #
  # Function definitions
  #
  ###############################################################################

  # datefunc is used to get the week ending date (field 1)

  datefunc <- function(x, codenum){
    unlist(
      stri_extract_first_regex(
        x,
        codes$String[codenum]))
  }

  # numberfunc is used to get all of the raw numbers (fields 2:8)

  numberfunc <- function(x, codenum){
    unlist(
      stri_extract_all_charclass(
        stri_extract_all_regex(
          x,
          codes$String[codenum]),
        codes$CharClass[codenum]))
  }

  # The perc functions are used to get the various percentages (fields 9:12)
  # Because they come in pairs, I've defined variants for the first and
  # second values returned.

  percfirstfunc <- function(x, codenum){
    unlist(
      stri_extract_all_regex(
        unlist(stri_extract_first_regex(
          x,
          codes$String[codenum])),
        codes$CharClass[codenum]))
  }

  perclastfunc <- function(x, codenum){
    unlist(
      stri_extract_all_regex(
        unlist(stri_extract_last_regex(
          x,
          codes$String[codenum])),
        codes$CharClass[codenum]))
  }

  convertWEdatefunc <- function(x){

    WEday = stri_extract_first_regex(x,"[0-9]{1,2}")
    WEmonth = match(
      stri_extract_first_regex(
        x,
        "\\b[A-z][a-z]*\\b")[[1]],
      month.name)
    WEyear = stri_extract_first_regex(x,"[0-9]{4}")
    make_date(day = WEday, month = WEmonth, year = WEyear)
  }


  ###############################################################################
  #
  # Data scraping
  #
  ###############################################################################
  setwd("~~/R/HonoursBlog/data/files/")
  for(i in 1:nrow(names)){

    tryCatch({

      txt     <- as.list(pdf_text(files[i])[1]) # scrapes all txt from the pdf

      WEdate  <- ifelse(is.na(datefunc(txt,1)), # the 'week ending' date.
                        datefunc(txt,2),
                        datefunc(txt,1))

      data[i,1]  <- convertWEdatefunc(WEdate)

      data[i,2]  <- numberfunc(txt,3)              # the # of responses
      data[i,3]  <- numberfunc(txt,4)              # the # of self-reporters
      data[i,4]  <- numberfunc(txt,5)              # the # of reports for others
      data[i,5]  <- numberfunc(txt,6)              # the # of vacc. respondents
      data[i,6]  <- numberfunc(txt,7)              # the # of total respondents
      data[i,7]  <- numberfunc(txt,8)              # the # clinical staff
      data[i,8]  <- numberfunc(txt,9)              # the # clinical staff (vacc.)
      data[i,9]  <- percfirstfunc(txt,10)          # % ILI (vacc.)
      data[i,10] <- percfirstfunc(txt, 11)         # % ILI (unvacc.)
      data[i,11] <- perclastfunc(txt,12)           # % ILI (vacc. + absence)
      data[i,12] <- perclastfunc(txt,13)           # % ILI (unvacc. + absence)
      data[i,13] <- as.character(names[i,1])       # the file reference code.
    },
    error=function(e){})
  }

  colnames(data) <- c("Week_end",
                      "Responses",
                      "Self_Report",
                      "Other_Report",
                      "Vaccinated_Respondents",
                      "Total_Respondents",
                      "Clinical_Staff",
                      "Clinical_Staff_Vaccinated",
                      "ILI_Vaccinated",
                      "ILI_Unvaccinated",
                      "ILI_wAbsence_Vaccinated",
                      "ILI_wAbsence_Unvaccinated",
                      "fileref")

  data[[1]] <- as.factor(as_date(data[[1]]))
  data[[6]] <- NULL # Total Respondents is reported twice - all but the first
  # few are identical figures, so I delete the field here.
  # easier than removing from the code; maybe it'll change?

  for(i in c(2:7)){
    data[[i]] <- as.integer(data[[i]])
  }
  for(i in c(8:11)){
    data[[i]] <- as.numeric(str_replace_all(data[[i]],"%",""))
  }

  setwd("..")
  ###############################################################################
  #
  # Export
  #
  ###############################################################################

  old_data <- readRDS("~/R/HonoursBlog/data/fludata.rds")
  new_data <- rbind(old_data,data)
  saveRDS(new_data,file="~/R/HonoursBlog/data/fludata.rds")
  write.csv(new_data,file="~/R/HonoursBlog/data/fludata.csv")
}
rm(names, page)
} else {
  print("No new data")
}
