setwd("C:/Users/Upamanyu/Documents/R Codes/Digitization")

rm(list=ls())

library(rio)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)

load("C:/Users/Upamanyu/Documents/R Codes/Imported.RData") # Loads a pre-saved set of objects from disk into environment
rm(loan_detail, repayment, transactions) #Selectively remove unnecessary objects (for this script)

x<-list(district, block, panchayat, village) # Forms a list of all the objects, for an operation on all objects of the list

#Loop within a loop. Using for & lapply, together.
#grep() function-Give a pattern, specify the names to look for the pattern

for (i in 1:4){ 
  x[[i]][grep("_id", names(x[[i]]), value=TRUE)]<-lapply(x[[i]][grep("_id", names(x[[i]]), value=TRUE)], as.numeric)
}
#What did the loop do?
#For loop defined for i, which will go from 1 to 4, which is the number of objects in list x. 
#Note the subscripting of x's elements. x[[1]], picks up the 1st element of x, which is the object district
#Left Hand says that for the i-th element, use grep to identify the variables of that element whose names end with "_id"
#Right hand says that for all such variable-element combination, apply the function, as.numeric to convert the same variables into numeric format
#Thus, for loop here, in conjunction with grep() and name() identifies every relevant variable for all objects that are elemnts of list x
# And lapply applies the same function across all such identified variables to convert them into numeric

district<-data.frame(x[[1]])#Convert first element of list x into a data frame called district
block<-data.frame(x[[2]])
panchayat<-data.frame(x[[3]])
village<-data.frame(x[[4]])

rm(x, i)

#Joining datasets

geography<-full_join(district, block) 
count(filter(geography, is.na(district_id))) 
count(filter(geography, is.na(block_id)))

geography<-full_join(geography, village)
count(filter(geography, is.na(block_id)))
count(filter(geography, is.na(village_id)))

geography<-full_join(geography, panchayat)
count(filter(geography, is.na(panchayat_id)))
count(filter(geography, is.na(village_id)))

geography<-select(geography, district_id:block_name, panchayat_id:panchayat_name, village_id:village_name, -state_id)
names(geography)<-c("District_ID", "District", "Scheme", "Block_ID", "Block", "Panchayat_ID", "Panchayat", "Village_ID", "Village")
#Assigned names to variables
rm(district, block, panchayat, village)


last_voucher<-select(voucher_master, cbo_id, voucher_date)%>%
  group_by(cbo_id)%>% #group_by function: repeats calculations for each unique set defined by the group_by function
  mutate(last_voucher_date=max(voucher_date, na.rm=TRUE))%>%
  distinct(cbo_id)%>%select(-voucher_date)%>%ungroup #end calculations for unique sets defined by previous group_by
last_voucher$last_voucher_date<-as.Date(last_voucher$last_voucher_date, "%Y-%m-%d") #converts into date, as per the format provided.

ac_master<-filter(ac_master, acc_opening_status==2)%>% #filter: selects rows depending on given condition
  group_by(cbo_id)%>%
  mutate(accounts=n())%>%
  distinct(cbo_id)%>%select(-acc_opening_status)%>%ungroup

cbo_mapping<-distinct(cbo_mapping, cbo_id)

cbo<-full_join(cbo, ac_master)
cbo$accounts<-replace(cbo$accounts, is.na(cbo$accounts), 0)#replace: take variable, provide what you want to replace, and what to replace with
count(filter(cbo, is.na(cbo_id)))
count(filter(cbo, is.na(accounts)))
cbo$entry_date_cbo<-NULL #Assigning NULL drops the variable

#Note the format in which cbo formation date is given. substr(): given variable, select starting position, and number of positions to parse text
#Then use paste command to paste the parsed text, 20, and parsed text again to generate date in desired format
cbo$formation_date<-paste(substr(cbo$formation_date, 1,6), "20", substr(cbo$formation_date, 7,8), sep="")
cbo$formation_date<-as.Date(cbo$formation_date, "%d-%m-%Y")

cbo<-full_join(cbo, last_voucher) 
count(filter(cbo, is.na(cbo_id)))
count(filter(cbo, is.na(last_voucher_date)))

cbo<-full_join(cbo, cbo_mapping)%>%select(-id)
count(filter(cbo, is.na(cbo_id)))
count(filter(cbo, is.na(parent_cbo_id)))
cbo[grep("_id", names(cbo), value=TRUE)]<-lapply(cbo[grep("_id", names(cbo), value=TRUE)], as.numeric)

rm(ac_master, last_voucher, cbo_mapping, voucher_master)


CLF<-filter(cbo, cbo_type_id==1)%>%
  select(c(-parent_cbo_id, -cbo_type_id, -village_id))%>%
  filter(!is.na(`block_id`))
names(CLF)<-c("District_ID", "Block_ID", "CLF_ID", "CLF", "CLF DoF", "CLF Status", "CLF Accounts", "Last Voucher CLF")

VO<-filter(cbo, cbo_type_id==2)%>%
  select(c(-cbo_type_id, -village_id))%>%
  filter(!is.na(`block_id`))
names(VO)<-c("District_ID", "Block_ID", "VO_ID", "VO", "VO DoF", "VO Status", "VO Accounts", "Last Voucher VO", "CLF_ID")

SHG<-filter(cbo, cbo_type_id==3)%>%
  select(c(-cbo_type_id))%>%
  select(-last_voucher_date)
names(SHG)<-c("District_ID", "Block_ID", "Village_ID", "SHG_ID", "SHG", "SHG DoF", "SHG Status", "SHG Accounts", "VO_ID")

member3<-filter(member3, designation_id==12)%>%
  group_by(cbo_id)%>%
  mutate("Total Member"=n())%>%
  distinct(cbo_id) %>% 
  select(cbo_id, `Total Member`)%>% rename("SHG_ID"=cbo_id)

SHG<-full_join(SHG, member3) %>% 
  filter(!is.na(`SHG`)) %>%
  filter(!is.na(`Village_ID`))%>%
  mutate(`Total Member`=ifelse(is.na(`Total Member`), 0, `Total Member`))

CBOs<-full_join(SHG, VO)
count(filter(CBOs, is.na(`VO_ID`)))
count(filter(CBOs, is.na(`SHG_ID`)))

CBOs<-full_join(CBOs, CLF)
count(filter(CBOs, is.na(`CLF_ID`)))
count(filter(CBOs, !is.na(`CLF_ID`) & !is.na(`SHG_ID`) & is.na(`VO_ID`)))

CBOs<-mutate(CBOs, "Date"=Sys.Date())%>% #Creates a column which will contain date as per the system
  filter(is.na(`CLF Status`) | `CLF Status`==1)%>%
  filter(is.na(`VO Status`) | `VO Status`==1)%>%
  filter(is.na(`SHG Status`) | `SHG Status`==1)%>%
  select(-ends_with("Status"))%>%
  select(`District_ID`,`Block_ID`, starts_with("CLF"), `Last Voucher CLF`, `Village_ID`, starts_with("VO"), `Last Voucher VO`, starts_with("SHG"), `Total Member`, `Date`)

rm(CLF, VO, SHG, cbo, member3)

save.image(file="Setupped.RData")
