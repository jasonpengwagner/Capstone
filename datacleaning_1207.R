library(stringr)
library(doBy)
library(tidyverse)
library(dplyr)

#set working directory on own computer
#setwd("~/Desktop/Capstone/2022 Fall/Data")


cases <- read.csv('FOIA TRAC Report 20221003/A_TblCase.csv', sep = "\t", header = T, skipNul = T)

cases <- cases[!duplicated(cases$IDNCASE),] # remove rows with duplicate case ID number
colnames(cases) <- tolower(colnames(cases))

cases_columns <- c("idncase","alien_city","alien_state","update_site","nat","lang","gender","custody","case_type","lpr","date_detained","date_released")
cases <- subset(cases, select = cases_columns)

countries <- read.csv('FOIA TRAC Report 20221003/Lookup/tblLookupCountry.csv', sep = "\t", header = T, skipNul = T)

#determine the race/ethnicity of these subjects 
countries <- countries[order(countries$strDescription),]
countries$strDescription <- str_to_title(countries$strDescription)
countries <- countries[,1:3]

#process the country data
africa_countries <- c(4,8,25,16,26,35,54,47,52,39,48,41,109,247,60,65,67,69,71,83,82,86,94,177,114,128,126,131,132,138,139,145,143,142,151,231,158,179,183,190,213,189,187,192,195,188,253,200,234,212,215,221,219,232,238,239)

caribbean_countries <- c(12,2,1,19,15,229,45,53,62,63,88,91,97,112,133,137,191,199,227,207,209)

latin_american_countries <- c(149,49,9,170,228,44,93,64,23,97,53,63,100,168,166,70,51,174,225,112,207,95,165,21,19,15,199,88,28,63,191)

asian_countries <- c(43,104,103,172,20,111,181,230,216,106,208,24,119,110,3,184,226,150,235,162,116,40,122,203,38,113,13,206,205,107,123,124,115,214,194,147,249,121,90,136,6,178,14,57,30,148,34)

nat_grp <-  c(4,8,25,16,26,35,54,47,52,39,48,41,109,247,60,65,67,69,71,83,82,86,94,177,114,128,126,131,132,138,139,145,143,142,151,231,158,179,183,190,213,189,187,192,195,188,253,200,234,212,215,221,219,232,238,239,12,2,1,19,15,229,45,53,62,63,88,91,97,112,133,137,191,199,227,207,209,149,49,9,170,228,44,93,64,23,97,53,63,100,168,166,70,51,174,225,112,207,95,165,21,19,15,199,88,28,63,191,43,104,103,172,20,111,181,230,216,106,208,24,119,110,3,184,226,150,235,162,116,40,122,203,38,113,13,206,205,107,123,124,115,214,194,147,249,121,90,136,6,178,14,57,30,148,34)

nat_dat <- data.frame(nat_grp, rep(0, length(nat_grp)))
colnames(nat_dat) <- c("country_id", "immi_type")
nat_dat[1:56,2] <- rep(1,56)
nat_dat[57:77,2] <- rep(2,21)
nat_dat[78:108,2] <- rep(3,31)
nat_dat[109:155,2] <- rep(4,47)

nat_dat$immi_type <- as.factor(nat_dat$immi_type)
levels(nat_dat$immi_type) <- c("African","Caribbean","Latin-American","Asian")

nat <- merge(nat_dat, countries, by.x = "country_id", by.y = "idnCountry", all.y = T)
nat <- nat[!duplicated(nat$strCode),]
cases_nat <- merge(cases, nat, by.x = "nat", by.y = "strCode", all.x = T)
cases_nat <- cases_nat[!is.na(cases_nat$strDescription),]
cases_nat <- cases_nat[cases_nat$nat!="??",]
cases_nat <- cases_nat[!is.na(cases_nat$nat),]
colnames(cases_nat)[15]<-"country_name"

cases_nat$case_type <- as.factor(cases_nat$case_type)




#clean proceeding table, use osc_date as time stamp in proceeding 

proc <- read.csv('FOIA TRAC Report 20221003/B_TblProceeding.csv', sep = "\t", header = T, skipNul = T)
colnames(proc) <-tolower(colnames(proc))

proc_columns <- c("idnproceeding","idncase","osc_date","ij_code","hearing_date","dec_code","other_comp","comp_date","crim_ind","aggravate_felon")
proc <- proc[, proc_columns]

proc <- proc[proc$idnproceeding!= "",]
proc$crim_ind <- as.factor(proc$crim_ind)

table(proc$crim_ind)
proc <- proc[proc$crim_ind == "N"|proc$crim_ind == "Y",]
proc <- proc[!duplicated(proc),]



#schedule? already used the time stamp in proceedings table
#time_tbl <- read.csv("FOIA TRAC Report 20221003/tbl_schedule.csv", sep = "\t", header = T, skipNul = T)

#detention
#det <- read.csv('FOIA TRAC Report 20221003/tbl_CustodyHistory.csv', sep = "\t", header = T, skipNul = T)
#colnames(det) <-tolower(colnames(det))
#det <- det[,c("idncase","custody")]




#bond
bond <- read.csv('FOIA TRAC Report 20221003/D_TblAssociatedBond.csv',sep = "\t", header = T, skipNul = T)
bond <- bond[!duplicated(bond$IDNASSOCBOND),]
colnames(bond) <- tolower(colnames(bond))
bond_keep <- c("idncase", "bond_hear_req_date","initial_bond","new_bond")

bond <- subset(bond, select = bond_keep)

#  Mail Question 3: bond hearings only happen when immigrants ask the court to adjust the bond, which may involve judges' decision. 
#  Would this restrict us from using judge fixed effects? 
#  Every bond record involves a bond hearing, as the bond_hear_req_date field tells us
#  How could we exclude those who appeal the bond?
sum(is.na(bond$bond_hear_req_date))

bond$bond_hear_req_date <- substr(bond$bond_hear_req_date, 1, 10)
bond$bond_hear_req_date  <- as.Date(bond$bond_hear_req_date, format = "%Y-%m-%d")

#bond <- bond[!is.na(bond$bond_hear_req_date),] #no need because there is no NAs in this field

bond$initial_bond <- as.numeric(bond$initial_bond)
bond$new_bond <- as.numeric(bond$new_bond)

bond$initial_bond[is.na(bond$initial_bond)] <- 0 
bond$new_bond[is.na(bond$new_bond)] <- 0 

bond$bond_amount <- 0 
bond$bond_amount[bond$initial_bond==0 & bond$new_bond!=0] <- bond$new_bond[bond$initial_bond==0 & bond$new_bond!=0] #no initial bond but new bond
bond$bond_amount[bond$initial_bond!=0 & bond$new_bond!=0] <- bond$new_bond[bond$initial_bond!=0 & bond$new_bond!=0] #has initial bond but new bond
bond$bond_amount[bond$initial_bond!=0 & bond$new_bond==0] <- bond$initial_bond[bond$initial_bond!=0 & bond$new_bond==0] #has initial bond but no new bond
bond$bond_amount[bond$initial_bond==0 & bond$new_bond==0] <- bond$initial_bond[bond$initial_bond==0 & bond$new_bond==0] #no initial bond or new bond either, i.e., requested bond but did not get one for this proceeding/bond id

# averaging the bond amount for each case-id (immigrant)

bond_case <- summaryBy( cbind(bond_amount,initial_bond) ~ idncase, FUN = c(sum, mean), data = bond)
bond_case <- bond_case[2:nrow(bond_case),] #drop a blank obs

bond_case$req_bond <- 1  #flag for those requested for bond


countries <- NULL
cases <- NULL

#matching

#unique case id matches multiple proceedings
case_proc <- merge(proc, cases_nat, by.x = "idncase", by.y = "idncase", all.x = T)

case_proc <- case_proc[case_proc$idncase!="",]

case_proc$osc_year <- strtoi(substr(case_proc$osc_date, start=1,stop=4))

#year range subject to change, now avoiding major events like Great Depression and COVID... may conduct year-specific analysis? not year fixed effect
case_proc <- case_proc[case_proc$osc_year>2009,]
case_proc <- case_proc[case_proc2$osc_year<2020,]

case_proc$date_detained <- substr(case_proc$date_detained , 1, 10)
case_proc$date_detained <- as.Date(case_proc$date_detained, format = "%Y-%m-%d")

case_proc$date_released <- substr(case_proc$date_released  , 1, 10)
case_proc$date_released <- as.Date(case_proc$date_released , format = "%Y-%m-%d")

#case_proc$time_detained <- case_proc$date_released - case_proc$date_detained
#case_proc$time_detained[case_proc$time_detained<=0] <- NA

case_proc$osc_date <- substr(case_proc$osc_date  , 1,10)
case_proc$osc_date <- as.Date(case_proc$osc_date  ,format = "%Y-%m-%d")

case_proc$hearing_date <- substr(case_proc$hearing_date  , 1,10)
case_proc$hearing_date <- as.Date(case_proc$hearing_date  ,format = "%Y-%m-%d")

case_proc$comp_date <- substr(case_proc$comp_date  , 1,10)
case_proc$comp_date <- as.Date(case_proc$comp_date  ,format = "%Y-%m-%d")

cases_nat <- NULL #to save disk space
proc <- NULL

bond <- NULL

main <- merge(case_proc, bond_case, by = "idncase", all.x = T)
main <- main[!is.na(main$nat),]


main$custody[main$custody=="R"] <- "D" #group released as detained since question of interest is whether one was ever detained
main <- main[main$custody!="",]



case_proc <- NULL

bond_case <- NULL


table(main$case_type)
#select only the removal case (99.9% of all data), deleted column "case_type"
main <- main[main$case_type =="RMV",]
main <- subset(main, select=-case_type)



#add reps data
rep <- read.csv('FOIA TRAC Report 20221003/tbl_RepsAssigned.csv', sep = "\t", header = T, skipNul = T)
colnames(rep) <-tolower(colnames(rep))

#filter out attorneys representing the DHS
rep2 <- rep[rep$strattytype == "ALIEN", ]
rep2$parent_table[rep2$parent_table == "A_Tblcase"] <- "a_tblcase"
rep2$parent_table[rep2$parent_table == "A_TblCase"] <- "a_tblcase"
rep2$parent_table[rep2$parent_table == "B_TblProceeding"] <- "b_tblproceeding"
rep2$parent_table[rep2$parent_table == "tblAppeal"] <- "tblappeal"
rep2 <- rep2[rep2$parent_table!="tblappeal", ]

#in rep, as long as an immigrant is represented for once, there's at least one record in rep2 table, they are counted as represented
rep <- rep2[!duplicated(rep2$idncase), ]
rep$represent <- 1
rep_merge <- subset(rep, select = c("idncase","represent")) # the only required data? 

#Delete records with hearing date in the future, since there will be no decision made / decision pending for appeal
main <- main %>% filter(hearing_date < '2022-11-01')

#merge dec_code with other_comp -- they both record decision codes

main$dec_code[main$dec_code==" "] <- ""
main$dec_code[main$dec_code=="  "] <- ""
main$other_comp[main$other_comp==" "] <- ""
main$other_comp[main$other_comp=="  "] <- ""
main$new_dec_code <- paste(main$dec_code, main$other_comp, sep="")
main$dec_code[main$new_dec_code=="OT"] <- "T"

idncase_vector <- table(main$idncase)
unique_idncase_vec <- idncase_vector[idncase_vector==1]

unique_id_data <- main[which(main$idncase %in% names(unique_idncase_vec)),] #unique idncase, unique proceeding
dupMain <- main[-which(main$idncase %in% names(unique_idncase_vec)),] #one idncase, multiple proceedings

dup_decision_id_data <- dupMain %>% 
  group_by(idncase) %>% 
  filter(any(new_dec_code %in% c("E","G","J","O","R","T","U","V","W","X","Z",
                                 "A","C","F","M","O","P","T","Y"))) #with non-empty dec_code

dupMain_empty <- dupMain[-which(dupMain$idncase %in% dup_decision_id_data$idncase),] #4 individuals/8 proceedings with no decision issued here
#Put the latest proceeding on the top of every case, so that we can keep the latest data of this case when use duplicate to delete the earlier ones
dup_decision_id_data <- dup_decision_id_data %>% 
  arrange(idncase, desc(hearing_date))

dupMain <- NULL
#in duplicated cases, one individual corresponding to more proceedings
#Delete duplicates in idncase, keep the latest proceeding only
dup_latest_dec <- dup_decision_id_data[!duplicated(dup_decision_id_data$idncase), ]

dupMain_empty <- dupMain_empty %>%  #similarly
  arrange(idncase, desc(hearing_date))
dupMain_empty <- dupMain_empty[!duplicated(dupMain_empty$idncase), ]

table(dup_latest_dec$new_dec_code)
table(unique_id_data$new_dec_code)

#New control variable 12/7/22: how many proceedings does a person go through
numproc <- data.frame(names(idncase_vector),idncase_vector)

uniq_main <- rbind(dup_latest_dec,unique_id_data)
uniq_main <- rbind(uniq_main,dupMain_empty)
uniq_main <- uniq_main %>% arrange(idncase)

numproc <- subset(numproc, select = Freq)
colnames(numproc) <- c("num_proc")
main <- cbind(uniq_main,numproc)

table(main$new_dec_code) #1.1M with empty decision codes
main <- main[, !(colnames(main) %in% c("dec_code", "other_comp"))]


dup_decision_id_data <- NULL
dup_latest_dec <- NULL
dupMain <- NULL
dupMain_empty <- NULL
unique_id_data <- NULL
uniq_main <- NULL
numproc <- NULL
nat <- NULL
nat_dat <- NULL

#merge with representation/attorney data
main <- merge(main, rep_merge, by = "idncase", all.x = T)

#convert bond amount na to 0
main$bond_amount.sum[is.na(main$bond_amount.sum)] <- 0
main$bond_amount.mean[is.na(main$bond_amount.mean)] <- 0
main$initial_bond.sum[is.na(main$initial_bond.sum)] <- 0
main$initial_bond.mean[is.na(main$initial_bond.mean)] <- 0

#convert to binary variables
main$det <- ifelse(main$custody == "D", 1,0)
main$represent[is.na(main$represent)] <- 0
main$req_bond[is.na(main$req_bond)] <- 0

#Mail Question 2: change every Dec_code not equal to removal to "not removal"?
table(main$new_dec_code) # *** what does all these decision code mean? Doesn't match with the code book, and many missing values...
main$remove <- ifelse(main$new_dec_code == "X", 1,0)




#save disk space
rep_merge <- NULL
rep2 <- NULL
rep <- NULL

#do we need to add dummies just for regression? Or we can use factor?
#main$i_africa <- ifelse(main$immi_type == "African", 1,0)
#main$i_carib <- ifelse(main$immi_type == "Caribbean", 1,0)
#main$i_africa[is.na(main$i_africa)] <- 0
#main$i_carib[is.na(main$i_carib)] <- 0

#export
library(data.table)
fwrite(main, "regression_1207.csv", row.names=FALSE, col.names=TRUE)
