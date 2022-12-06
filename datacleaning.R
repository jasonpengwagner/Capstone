library(stringr)
library(doBy)
library(tidyverse)
library(dplyr)

#On Mu's Computer
#setwd("~/Desktop/Capstone/2022 Fall/Data")

cases <- read.csv('FOIA TRAC Report 20221003/A_TblCase.csv', sep = "\t", header = T, skipNul = T)

cases <- cases[!duplicated(cases$IDNCASE),] # remove rows with duplicate case ID number
colnames(cases) <- tolower(colnames(cases))

cases_columns <- c("idncase","alien_city","alien_state","update_site","nat","lang","custody","case_type","lpr","date_detained","date_released")
cases <- subset(cases, select = cases_columns)

#cases <- cases %>% filter(alien_state %in% c("CA","NY","IL","TX","FL","VA" ))

countries <- read.csv('FOIA TRAC Report 20221003/Lookup/tblLookupCountry.csv', sep = "\t", header = T, skipNul = T)

#determine the race/ethnicity of these subjects 
countries <- countries[order(countries$strDescription),]
countries$strDescription <- str_to_title(countries$strDescription)
countries <- countries[,1:3]

#process the country data
africa_countries <- c(4,8,25,16,26,35,54,47,52,39,48,41,109,247,60,65,67,69,71,83,82,86,94,177,114,128,126,131,132,138,139,145,143,142,151,231,158,179,183,190,213,189,187,192,195,188,253,200,234,212,215,221,219,232,238,239)

caribbean_countries <- c(12,2,1,19,15,229,45,53,62,63,88,91,97,112,133,137,191,199,227,207,209)

blk_nat <-  c(4,8,25,16,26,35,54,47,52,39,48,41,109,247,60,65,67,69,71,83,82,86,94,177,114,128,126,131,132,138,139,145,143,142,151,231,158,179,183,190,213,189,187,192,195,188,253,200,234,212,215,221,219,232,238,239,12,2,1,19,15,229,45,53,62,63,88,91,97,112,133,137,191,199,227,207,209)

blk_nat_dat <- data.frame(blk_nat, rep(0, length(blk_nat)))
colnames(blk_nat_dat) <- c("country_id", "immi_type")
blk_nat_dat[1:56,2] <- rep(1,56)
blk_nat_dat[57:77,2] <- rep(2,21)

blk_nat_dat$immi_type <- as.factor(blk_nat_dat$immi_type)
levels(blk_nat_dat$immi_type) <- c("African","Caribbean")

nat <- merge(blk_nat_dat, countries, by.x = "country_id", by.y = "idnCountry", all.y = T)
cases_nat <- merge(cases, nat, by.x = "nat", by.y = "strCode", all.x = T)
cases_nat <- cases_nat[!is.na(cases_nat$strDescription),]
cases_nat <- cases_nat[cases_nat$nat!="??",]
cases_nat <- cases_nat[!is.na(cases_nat$nat),]
colnames(cases_nat)[11]<-"country_name"

cases_nat$case_type <- as.factor(cases_nat$case_type)




#clean proceeding table, use osc_date as time stamp in proceeding 

proc <- read.csv('FOIA TRAC Report 20221003/B_TblProceeding.csv', sep = "\t", header = T, skipNul = T)
colnames(proc) <-tolower(colnames(proc))

proc_columns <- c("idnproceeding","idncase","osc_date","ij_code","hearing_date","dec_code","other_comp","comp_date","crim_ind")
proc <- proc[, proc_columns]

proc$osc_year <- strtoi(substr(proc$osc_date, start=1,stop=4))



proc <- proc[proc$idnproceeding!= "",]
proc$crim_ind <- as.factor(proc$crim_ind)

table(proc$crim_ind)
proc <- proc[proc$crim_ind == "N"|proc$crim_ind == "Y",]
proc <- proc[!duplicated(proc),]



#schedule? might use the one in proceedings table
#time_tbl <- read.csv("FOIA TRAC Report 20221003/tbl_schedule.csv", sep = "\t", header = T, skipNul = T)



#detention
#det <- read.csv('FOIA TRAC Report 20221003/tbl_CustodyHistory.csv', sep = "\t", header = T, skipNul = T)
#colnames(det) <-tolower(colnames(det))
#det <- det[,c("idncase","custody")]

#workspace saved in before_merge.Rdata


#bond
bond <- read.csv('FOIA TRAC Report 20221003/D_TblAssociatedBond.csv',sep = "\t", header = T, skipNul = T)
bond <- bond[!duplicated(bond$IDNASSOCBOND),]
colnames(bond) <- tolower(colnames(bond))
bond_keep <- c("idncase", "bond_hear_req_date","initial_bond","new_bond")
bond <- subset(bond, select = bond_keep)

bond$bond_hear_req_date <- substr(bond$bond_hear_req_date, 1, 10)
bond$bond_hear_req_date  <- as.Date(bond$bond_hear_req_date, format = "%Y-%m-%d")

bond <- bond[!is.na(bond$bond_hear_req_date),]

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

bond_case$req_bond <- 1



countries <- NULL
cases <- NULL
#matching

case_proc <- merge(proc, cases_nat, by.x = "idncase", by.y = "idncase", all.x = T)

case_proc <- case_proc[case_proc$idncase!="",]

case_proc$osc_year <- strtoi(substr(case_proc$osc_date, start=1,stop=4))

case_proc <- case_proc[case_proc$osc_year>2012,]
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

#newest record of custody
#colnames(det)[2] <- "hist_custody"
#det$hist_custody <- as.factor(det$hist_custody)
#det$hist_custody <- factor(det$hist_custody, levels=c("R","D","N"))
#det <- with(det, det[order(idncase, hist_custody),])
#latest_det <- aggregate(det,by=list(det$hist_custody),head,n=1) how to refresh?

cases_nat <- NULL #to save disk space
proc <- NULL
prochar <- NULL
bond <- NULL

main <- merge(case_proc, bond_case, by = "idncase", all.x = T)

main <- main[!is.na(main$nat),]
#main$crim_ind <- as.character(main$crim_ind)
#main$crim_ind <- as.factor(main$crim_ind)

main$custody[main$custody=="R"] <- "D" #group released as detained since question of interest is whether one was ever detained
main <- main[main$custody!="",]



case_proc <- NULL
prochar2 <- NULL
bond_case <- NULL
case_proc_char <- NULL
#saved as "transfer.Rdata", Up to sample row 130 calculate detention time part

table(main$case_type)
#select only the removal case, deleted column "case_type"
main <- main[main$case_type =="RMV",]
main <- subset(main, select=-case_type)


#change every Dec_code not equal to removal to "not removal"?
main$remove <- ifelse(main$dec_code == "X", 1,0)
#change to binary variables
main$crim <- ifelse(as.numeric(main$crim_ind)==1,0,1)
main$crim_ind <- main$crim

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

#saved
main <- main %>% 
  arrange(idncase, desc(hearing_date))

#Delete records with hearing date in the future, since there will be no decision made / decision pending for appeal
main <- main %>% filter(hearing_date < '2022-11-01')

table(main$dec_code)

#in rep, as long as an immigrant is represented for once, there's at least one record in rep2 table, they are counted as represented
rep <- rep2[!duplicated(rep2$idncase), ]
rep$represent <- 1
rep_merge <- subset(rep, select = c("idncase","represent")) # the only required data? 


#in main, now unique proceedings, one individual corresponding to more proceedings
#Delete duplicates in idncase
before_unique_main <- main
main <- main[!duplicated(main$idncase), ]


main <- merge(main, rep_merge, by = "idncase", all.x = T)

#validate bond amount
main$bond_amount.sum[is.na(main$bond_amount.sum)] <- 0
main$bond_amount.mean[is.na(main$bond_amount.mean)] <- 0
main$initial_bond.sum[is.na(main$initial_bond.sum)] <- 0
main$initial_bond.mean[is.na(main$initial_bond.mean)] <- 0


main$det <- ifelse(main$custody == "D", 1,0)
main$represent[is.na(main$represent)] <- 0
main$req_bond[is.na(main$req_bond)] <- 0

#save disk space
rep_merge <- NULL
rep2 <- NULL

#saved

main <- subset(main, select = -atty_nbr)

#dummies for regression
main$i_africa <- ifelse(main$immi_type == "African", 1,0)
main$i_carib <- ifelse(main$immi_type == "Caribbean", 1,0)
main <- subset(main, select = -immi_type)

main$i_africa[is.na(main$i_africa)] <- 0
main$i_carib[is.na(main$i_carib)] <- 0

#export
library(data.table)
fwrite(main, "1206.csv", row.names=FALSE, col.names=TRUE)

