library(RCurl)

url <- "ftp://ftp.ceda.ac.uk/badc/cru/data/cru_cy/cru_cy_4.03/data/tmp/"

dest_file <- paste0(PFUSetup::get_abs_paths()$project_path, "/Data/Temperature Data/CEDA_2018", sep ="")

# protocol <- "sftp"

up <- "zmarshall:ParrotIron4"

filenames <- RCurl::getURL(url, userpwd = up,
                    ftp.use.epsv = TRUE, dirlistonly = TRUE)

filenames <- paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")

con <- getCurlHandle(ftp.use.epsv = TRUE, userpwd = up)


for(file in filenames) {writeBin(getBinaryURL(file, curl = con, dirlistonly = FALSE),
                            paste(dest_file, substr(file, nchar(url), nchar(file)), sep = ""))
}


# Code to correctly read .per files
World <- readr::read_table2("C:/Users/Zeke Marshall/Dropbox/Fellowship 1960-2015 PFU database/Data/Temperature Data/crucy.v4.03.1901.2018.all.tmp.per", skip = 3)

