rm(list = ls())   

library(pacman)
p_load(stringr, stm, magrittr, readxl, writexl, stringr, stringdist, tidyr, data.table, quanteda)
setwd("~/Dropbox/BA_neu/Daten/Datasets for Content Analysis")     

#Load data
file <- read_excel("FULL DATA EXCEL.xlsx", sheet = 1)

#Take whats important
subset <- subset(file, plenum == "0", select = c("Statement", "country",	"day", "groups", "topic", "topturn", "wordcount", "gndr", "grouptop"))


#Split dfs
foo <- split(subset, subset$grouptop)

#Create lagged var and merge back together
newdata <- data.frame()
for (df in foo){
  df$prev_speaker <- shift(df$gndr, 1L, type="lag")
  df <- subset(df, is.na(prev_speaker) == F)
  df$turn_type <- as.factor(paste(df$prev_speaker, df$gndr, sep = "-"))
  newdata <- rbind(newdata, df)
}

#Clean up
newdata$Statement <- str_replace(newdata$Statement, "^[A-Z]\\d{1,2}[:]", "") %>%
  str_replace("^[FM][:]\\d{1,2}", "") %>% 
  str_replace("^\\d{1,2}[:]", "") %>% 
  str_replace("^P\\d{1,2}", "") %>%
  str_replace("^\\d{1,2}", "") %>%
  str_replace("^[FM] ", "") %>%
  str_replace_all("\\[.+?\\]", "") %>%
  str_trim()

#Get the beginning of term
#Get first three words
newdata$Statement_start <- str_extract(newdata$Statement, "^[A-Za-z[:punct:]]+ [A-Za-z[:punct:]]+ [A-Za-z[:punct:]]+")
#If shorter get first two
newdata$Statement_start <- ifelse(is.na(newdata$Statement_start) == T, str_extract(newdata$Statement, "^[A-Za-z[:punct:]]+ [A-Za-z[:punct:]]+"), newdata$Statement_start)
#And first one
newdata$Statement_start <- ifelse(is.na(newdata$Statement_start) == T, str_extract(newdata$Statement, "^[A-Za-z[:punct:]]+"), newdata$Statement_start)
  


#Take out min stems
SENTENCES_DTM <- dfm(newdata$Statement_start, stem = F , remove_punct = F, tolower = F) %>% dfm_trim(min_docfreq = 30) %>% as.data.frame()






#Write to csv
write.csv(newdata, "~/Dropbox/BA_neu/Daten/Datasets for Content Analysis/Data_week 6.csv")


