setwd("C:/Users/Upamanyu/Documents/R Codes/Digitization")
rm(list=ls())
library(rio)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
rm(list=ls())
load(file="Setupped.RData")

SHGs<-CBOs%>%
  filter(!is.na(`SHG_ID`))%>%
  distinct(`SHG_ID`)%>%
  mutate("Alert Profile"=ifelse(`Total Member`>9, "OK", 
                                ifelse(`Total Member`==0, "Empty", "Check")))%>%
  mutate("Alert Account"=ifelse(`SHG Accounts`==0 & `Date`-`SHG DoF`>100, "No Account", ""))%>%
  select(`District_ID`, `Block_ID`, `Village_ID`, `CLF`, `CLF_ID`, `VO`, `VO_ID`, `SHG`, `SHG_ID`, `SHG DoF`, `Total Member`, `Alert Profile`, `Alert Account`)
SHGs<-right_join(geography, SHGs)%>%filter(!is.na(`SHG_ID`))%>%distinct(`SHG_ID`)

VOs<-CBOs%>%
  filter(!is.na(`VO_ID`))%>%  
  distinct(`VO_ID`, `SHG_ID`)%>%
  group_by(`VO_ID`) %>% 
  mutate(`Total SHG`=n())%>%
  mutate(`Total SHG`=as.numeric(ifelse(is.na(`SHG_ID`), 0, `Total SHG`)))%>%
  distinct(`VO_ID`) %>%ungroup()%>%
  mutate("Alert Profile"=ifelse(`Total SHG`>7, "OK", 
                                ifelse(`Total SHG`==0, "Empty", "Check")))%>%
  mutate("Alert Account"=ifelse(`VO Accounts`==0 & `Date`-`VO DoF`>100, "No Account", ""))%>%
  mutate(period=12*(year(`Date`)-year(`Last Voucher VO`))+(month(`Date`)-month(`Last Voucher VO`)))%>%
  mutate("Alert Transactions"=
                  ifelse(is.na(`Last Voucher VO`), "No Entry", 
                         ifelse(period<=3, "LIVE", "LAG")))%>%
  select(`District_ID`, `Block_ID`, `CLF`, `CLF_ID`, `VO`, `VO_ID`, `VO DoF`, `Total SHG`, `Alert Profile`, `Alert Account`, `Last Voucher VO`, `Alert Transactions`)
VOs<-right_join(geography, VOs)%>%filter(!is.na(`VO_ID`))%>%distinct(`VO_ID`)

CLFs<-CBOs%>%
  filter(!is.na(`CLF_ID`))%>% distinct(`CLF_ID`, `VO_ID`)%>% 
  group_by(`CLF_ID`) %>% 
  mutate(`Total VO`=n())%>%
  mutate(`Total VO`=as.numeric(ifelse(is.na(`VO_ID`), 0, `Total VO`)))%>%  
  distinct(`CLF_ID`) %>%ungroup()%>%
  mutate("Alert Profile"=ifelse(`Total VO`>19, "OK", 
                                ifelse(`Total VO`==0, "Empty", "Check")))%>%
  mutate("Alert Account"=ifelse(`CLF Accounts`==0 & `Date`-`CLF DoF`>100, "No Account", ""))%>%
  mutate(period=12*(year(`Date`)-year(`Last Voucher CLF`))+(month(`Date`)-month(`Last Voucher CLF`)))%>%
  mutate("Alert Transactions"=
           ifelse(is.na(`Last Voucher CLF`), "No Entry", 
                         ifelse(period<=3, "LIVE", "LAG")))%>%
  select(`District_ID`, `Block_ID`, `CLF`, `CLF_ID`, `CLF DoF`, `Total VO`, `Alert Profile`, `Alert Account`, `Last Voucher CLF`, `Alert Transactions`)
CLFs<-right_join(geography, CLFs)%>%filter(!is.na(`CLF_ID`))%>%distinct(`CLF_ID`)


Block<-select(geography, `District_ID`:`Block`)%>%
  distinct(`District_ID`, `Block_ID`)

Block_SHG<-SHGs%>% select(-`District`, -`Block`, -`Village`, -`Panchayat`, -`Panchayat_ID`)%>%
  mutate(`Empty SHG`=ifelse(`Alert Profile`=="Empty", 1, 0))%>%
  mutate(`SHG wo Accounts`=ifelse(`Alert Account`=="No Account", 1, 0))%>%
  group_by(`District_ID`, `Block_ID`)%>%
  mutate(`SHGs`=n())%>%
  mutate(`Members`=sum(`Total Member`, na.rm=TRUE))%>%
  mutate(`Empty SHGs`=sum(`Empty SHG`, na.rm=TRUE))%>%
  mutate(`SHGs wo Accounts`=sum(`SHG wo Accounts`, na.rm=TRUE))%>%ungroup()%>%
  distinct(`District_ID`, `Block_ID`)
Block_SHG<-full_join(Block, Block_SHG)%>%
  mutate(`SHGs`=ifelse(is.na(`SHGs`),0, `SHGs`))%>%
  mutate(`Members`=ifelse(is.na(`Members`),0, `Members`))%>%
  select(`District_ID`:`Block`, `Members`, `SHGs`, `Empty SHGs`, `SHGs wo Accounts`)

Block_VO<-VOs%>% select(-`District`, -`Block`, -`Village`, -`Village_ID`, -`Panchayat`, -`Panchayat_ID`)%>%
  mutate(`Empty VO`=ifelse(`Alert Profile`=="Empty", 1, 0))%>%
  mutate(`VO wo Accounts`=ifelse(`Alert Account`=="No Account", 1, 0))%>%
  mutate(`VO wo Transactions`=ifelse(`Alert Transactions`=="No Entry", 1, 0))%>%
  mutate(`VO in LAG`=ifelse(`Alert Transactions`=="LAG", 1, 0))%>%
  mutate(`VO LIVE`=ifelse(`Alert Transactions`=="LIVE", 1, 0))%>%
  mutate(`VO with Transactions`=ifelse(`Alert Transactions`!="No Entry", 1, 0))%>%
  group_by(`District_ID`, `Block_ID`)%>%
  mutate(`VOs`=n())%>%
  mutate(`SHGs Federated`=sum(`Total SHG`, na.rm=TRUE))%>%
  mutate(`Empty VOs`=sum(`Empty VO`, na.rm=TRUE))%>%
  mutate(`VOs wo Accounts`=sum(`VO wo Accounts`, na.rm=TRUE))%>%
  mutate(`VOs with Transactions`=sum(`VO with Transactions`, na.rm=TRUE))%>%
  mutate(`VOs wo Transactions`=sum(`VO wo Transactions`, na.rm=TRUE))%>%
  mutate(`VOs in LAG`=sum(`VO in LAG`, na.rm=TRUE))%>%
  mutate(`VOs LIVE`=sum(`VO LIVE`, na.rm=TRUE))%>%
  ungroup()%>%
  distinct(`District_ID`, `Block_ID`)
Block_VO<-full_join(Block, Block_VO)%>%
  mutate(`VOs`=ifelse(is.na(`VOs`),0, `VOs`))%>%
  mutate(`SHGs Federated`=ifelse(is.na(`SHGs Federated`),0, `SHGs Federated`))%>%
  select(`District_ID`:`Block`,`VOs`, `SHGs Federated`, `Empty VOs`, `VOs wo Accounts`, `VOs with Transactions`, `VOs wo Transactions`, `VOs in LAG`, `VOs LIVE`)

Block_CLF<-CLFs%>% select(-`District`, -`Block`, -`Village`, -`Village_ID`, -`Panchayat`, -`Panchayat_ID`)%>%
  mutate(`Empty CLF`=ifelse(`Alert Profile`=="Empty", 1, 0))%>%
  mutate(`CLF wo Accounts`=ifelse(`Alert Account`=="No Account", 1, 0))%>%
  mutate(`CLF wo Transactions`=ifelse(`Alert Transactions`=="No Entry", 1, 0))%>%
  mutate(`CLF in LAG`=ifelse(`Alert Transactions`=="LAG", 1, 0))%>%
  mutate(`CLF LIVE`=ifelse(`Alert Transactions`=="LIVE", 1, 0))%>%
  mutate(`CLF with Transactions`=ifelse(`Alert Transactions`!="No Entry", 1, 0))%>%
  group_by(`District_ID`, `Block_ID`)%>%
  mutate(`CLFs`=n())%>%
  mutate(`VOs Federated`=sum(`Total VO`, na.rm=TRUE))%>%
  mutate(`Empty CLFs`=sum(`Empty CLF`, na.rm=TRUE))%>%
  mutate(`CLFs wo Accounts`=sum(`CLF wo Accounts`, na.rm=TRUE))%>%
  mutate(`CLFs with Transactions`=sum(`CLF with Transactions`, na.rm=TRUE))%>%
  mutate(`CLFs wo Transactions`=sum(`CLF wo Transactions`, na.rm=TRUE))%>%
  mutate(`CLFs in LAG`=sum(`CLF in LAG`, na.rm=TRUE))%>%
  mutate(`CLFs LIVE`=sum(`CLF LIVE`, na.rm=TRUE))%>%
  ungroup()%>%
  distinct(`District_ID`, `Block_ID`)
Block_CLF<-full_join(Block, Block_CLF)%>%
  mutate(`CLFs`=ifelse(is.na(`CLFs`),0, `CLFs`))%>%
  mutate(`VOs Federated`=ifelse(is.na(`VOs Federated`),0, `VOs Federated`))%>%
  select(`District_ID`:`Block`,`CLFs`, `VOs Federated`, `Empty CLFs`, `CLFs wo Accounts`, `CLFs with Transactions`, `CLFs wo Transactions`, `CLFs in LAG`, `CLFs LIVE`)

Block_Facesheet<-full_join(Block, Block_CLF)
Block_Facesheet<-full_join(Block_CLF, Block_VO)
Block_Facesheet<-full_join(Block_Facesheet, Block_SHG)

District<-select(geography, `District_ID`:`Scheme`)%>%
  distinct(`District_ID`)

District_SHG<-SHGs%>% select(-`District`, -`Block`, -`Village`, -`Panchayat`, -`Panchayat_ID`)%>%
  mutate(`Empty SHG`=ifelse(`Alert Profile`=="Empty", 1, 0))%>%
  mutate(`SHG wo Accounts`=ifelse(`Alert Account`=="No Account", 1, 0))%>%
  group_by(`District_ID`)%>%
  mutate(`SHGs`=n())%>%
  mutate(`Members`=sum(`Total Member`, na.rm=TRUE))%>%
  mutate(`Empty SHGs`=sum(`Empty SHG`, na.rm=TRUE))%>%
  mutate(`SHGs wo Accounts`=sum(`SHG wo Accounts`, na.rm=TRUE))%>%ungroup()%>%
  distinct(`District_ID`)
District_SHG<-full_join(District, District_SHG)%>%
  mutate(`SHGs`=ifelse(is.na(`SHGs`),0, `SHGs`))%>%
  mutate(`Members`=ifelse(is.na(`Members`),0, `Members`))%>%
  select(`District_ID`:`Scheme`, `Members`, `SHGs`, `Empty SHGs`, `SHGs wo Accounts`)

District_VO<-VOs%>% select(-`District`, -`Block`, -`Village`, -`Village_ID`, -`Panchayat`, -`Panchayat_ID`)%>%
  mutate(`Empty VO`=ifelse(`Alert Profile`=="Empty", 1, 0))%>%
  mutate(`VO wo Accounts`=ifelse(`Alert Account`=="No Account", 1, 0))%>%
  mutate(`VO wo Transactions`=ifelse(`Alert Transactions`=="No Entry", 1, 0))%>%
  mutate(`VO in LAG`=ifelse(`Alert Transactions`=="LAG", 1, 0))%>%
  mutate(`VO LIVE`=ifelse(`Alert Transactions`=="LIVE", 1, 0))%>%
  mutate(`VO with Transactions`=ifelse(`Alert Transactions`!="No Entry", 1, 0))%>%
  group_by(`District_ID`)%>%
  mutate(`VOs`=n())%>%
  mutate(`SHGs Federated`=sum(`Total SHG`, na.rm=TRUE))%>%
  mutate(`Empty VOs`=sum(`Empty VO`, na.rm=TRUE))%>%
  mutate(`VOs wo Accounts`=sum(`VO wo Accounts`, na.rm=TRUE))%>%
  mutate(`VOs with Transactions`=sum(`VO with Transactions`, na.rm=TRUE))%>%
  mutate(`VOs wo Transactions`=sum(`VO wo Transactions`, na.rm=TRUE))%>%
  mutate(`VOs in LAG`=sum(`VO in LAG`, na.rm=TRUE))%>%
  mutate(`VOs LIVE`=sum(`VO LIVE`, na.rm=TRUE))%>%
  ungroup()%>%
  distinct(`District_ID`)
District_VO<-full_join(District, District_VO)%>%
  mutate(`VOs`=ifelse(is.na(`VOs`),0, `VOs`))%>%
  mutate(`SHGs Federated`=ifelse(is.na(`SHGs Federated`),0, `SHGs Federated`))%>%
  select(`District_ID`:`Scheme`,`VOs`, `SHGs Federated`, `Empty VOs`, `VOs wo Accounts`, `VOs with Transactions`, `VOs wo Transactions`, `VOs in LAG`, `VOs LIVE`)

District_CLF<-CLFs%>% select(-`District`, -`Block`, -`Village`, -`Village_ID`, -`Panchayat`, -`Panchayat_ID`)%>%
  mutate(`Empty CLF`=ifelse(`Alert Profile`=="Empty", 1, 0))%>%
  mutate(`CLF wo Accounts`=ifelse(`Alert Account`=="No Account", 1, 0))%>%
  mutate(`CLF wo Transactions`=ifelse(`Alert Transactions`=="No Entry", 1, 0))%>%
  mutate(`CLF in LAG`=ifelse(`Alert Transactions`=="LAG", 1, 0))%>%
  mutate(`CLF LIVE`=ifelse(`Alert Transactions`=="LIVE", 1, 0))%>%
  mutate(`CLF with Transactions`=ifelse(`Alert Transactions`!="No Entry", 1, 0))%>%
  group_by(`District_ID`)%>%
  mutate(`CLFs`=n())%>%
  mutate(`VOs Federated`=sum(`Total VO`, na.rm=TRUE))%>%
  mutate(`Empty CLFs`=sum(`Empty CLF`, na.rm=TRUE))%>%
  mutate(`CLFs wo Accounts`=sum(`CLF wo Accounts`, na.rm=TRUE))%>%
  mutate(`CLFs with Transactions`=sum(`CLF with Transactions`, na.rm=TRUE))%>%
  mutate(`CLFs wo Transactions`=sum(`CLF wo Transactions`, na.rm=TRUE))%>%
  mutate(`CLFs in LAG`=sum(`CLF in LAG`, na.rm=TRUE))%>%
  mutate(`CLFs LIVE`=sum(`CLF LIVE`, na.rm=TRUE))%>%
  ungroup()%>%
  distinct(`District_ID`)
District_CLF<-full_join(District, District_CLF)%>%
  mutate(`CLFs`=ifelse(is.na(`CLFs`),0, `CLFs`))%>%
  mutate(`VOs Federated`=ifelse(is.na(`VOs Federated`),0, `VOs Federated`))%>%
  select(`District_ID`:`Scheme`,`CLFs`, `VOs Federated`, `Empty CLFs`, `CLFs wo Accounts`, `CLFs with Transactions`, `CLFs wo Transactions`, `CLFs in LAG`, `CLFs LIVE`)

District_Facesheet<-full_join(District, District_CLF)
District_Facesheet<-full_join(District_CLF, District_VO)
District_Facesheet<-full_join(District_Facesheet, District_SHG)


MPPR_District<-import("C:/Users/Upamanyu/Documents/R Codes/Progress.xlsx")
District_Facesheet<-full_join(District_Facesheet, MPPR_District)%>%
  
  mutate(`Ratio Members`=100*Members/MPPR_Members)%>%
  mutate(`Ratio SHGs`=100*SHGs/MPPR_SHGs)%>%
  mutate(`Ratio VOs`=100*VOs/MPPR_VOs)%>%
  mutate(`Ratio CLFs`=100*CLFs/MPPR_CLFs)%>%
  
  mutate(`Ratio VOs with Transactions`=100*`VOs with Transactions`/`VOs`)%>%
  mutate(`Ratio CLFs with Transactions`=100*`CLFs with Transactions`/`CLFs`)%>%
  
  mutate(`Ratio VOs LIVE`=100*`VOs LIVE`/`VOs`)%>%
  mutate(`Ratio CLFs LIVE`=100*`CLFs LIVE`/`CLFs`)%>%
  
  mutate(`Ratio Mapped SHGs`=100-(100*`Empty SHGs`/`SHGs`))%>%
  mutate(`Ratio Mapped VOs`=100-(100*`Empty VOs`/`VOs`))%>%  
  mutate(`Ratio Mapped CLFs`=100-(100*`Empty CLFs`/`CLFs`))%>%
  
  mutate(`Ratio SHGs with Accounts`=100-(100*`SHGs wo Accounts`/`SHGs`))%>%
  mutate(`Ratio VOs with Accounts`=100-(100*`VOs wo Accounts`/`VOs`))%>%  
  mutate(`Ratio CLFs with Accounts`=100-(100*`CLFs wo Accounts`/`CLFs`))

cleaner<-function(x){
  replace(x, !is.finite(x), NA)
  round(x,2)
}

District_Facesheet[grep("Ratio", names(District_Facesheet), value=TRUE)]<-
  lapply(District_Facesheet[grep("Ratio", names(District_Facesheet), value=TRUE)], cleaner)
District_Facesheet$`Ratio CLFs`<-replace(District_Facesheet$`Ratio CLFs`, !is.finite(District_Facesheet$`Ratio CLFs`), NA)


schemer<-function(x){sum(x, na.rm=TRUE)}
Scheme_Facesheet<-District_Facesheet%>%select(`District_ID`:`MPPR_CLFs`)%>%
  group_by(Scheme)%>%
  mutate_each(funs(schemer), `CLFs`:`MPPR_CLFs`)%>%
  ungroup()%>%
  distinct(Scheme)%>%select(-(District_ID:District))

Bihar<-Scheme_Facesheet%>%
  mutate(Scheme="Bihar")%>%
  mutate_each(funs(schemer), `CLFs`:`MPPR_CLFs`)%>%
  distinct(Scheme)
Scheme_Facesheet<-rbind(Scheme_Facesheet, Bihar)

Scheme_Facesheet<-Scheme_Facesheet%>%
  
  mutate(`Ratio Members`=100*Members/MPPR_Members)%>%
  mutate(`Ratio SHGs`=100*SHGs/MPPR_SHGs)%>%
  mutate(`Ratio VOs`=100*VOs/MPPR_VOs)%>%
  mutate(`Ratio CLFs`=100*CLFs/MPPR_CLFs)%>%
  
  mutate(`Ratio VOs with Transactions`=100*`VOs with Transactions`/`VOs`)%>%
  mutate(`Ratio CLFs with Transactions`=100*`CLFs with Transactions`/`CLFs`)%>%
  
  mutate(`Ratio VOs LIVE`=100*`VOs LIVE`/`VOs`)%>%
  mutate(`Ratio CLFs LIVE`=100*`CLFs LIVE`/`CLFs`)%>%
  
  mutate(`Ratio Mapped SHGs`=100-(100*`Empty SHGs`/`SHGs`))%>%
  mutate(`Ratio Mapped VOs`=100-(100*`Empty VOs`/`VOs`))%>%  
  mutate(`Ratio Mapped CLFs`=100-(100*`Empty CLFs`/`CLFs`))%>%
  
  mutate(`Ratio SHGs with Accounts`=100-(100*`SHGs wo Accounts`/`SHGs`))%>%
  mutate(`Ratio VOs with Accounts`=100-(100*`VOs wo Accounts`/`VOs`))%>%  
  mutate(`Ratio CLFs with Accounts`=100-(100*`CLFs wo Accounts`/`CLFs`))

cleaner<-function(x){
  replace(x, !is.finite(x), NA)
  round(x,2)
}

Scheme_Facesheet[grep("Ratio", names(Scheme_Facesheet), value=TRUE)]<-
  lapply(Scheme_Facesheet[grep("Ratio", names(Scheme_Facesheet), value=TRUE)], cleaner)
Scheme_Facesheet$`Ratio CLFs`<-replace(Scheme_Facesheet$`Ratio CLFs`, !is.finite(Scheme_Facesheet$`Ratio CLFs`), NA)


rm(Block_CLF, Block_VO, Block_SHG, District_CLF, District_VO, District_SHG, geography, CBOs, schemer, Bihar, cleaner, MPPR_District)

District_Facesheet[paste("Rank_", grep("Ratio", names(District_Facesheet), value = TRUE), sep="")]<-
  lapply(District_Facesheet[grep("Ratio", names(District_Facesheet), value = TRUE)], function(x){min_rank(desc(x))})


library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)

DistrictGIS<-readOGR("C:/Users/Upamanyu/Documents/GIS Database/Census 2011 Districts", "2011_Dist")
DistrictGIS<-subset(DistrictGIS, ST_NM=="Bihar")

DistrictGIS@data<-mutate_each(DistrictGIS@data, funs(toupper), DISTRICT)

DistrictGIS@data$DISTRICT<-gsub("SARAN (CHHAPRA)", "SARAN", DistrictGIS@data$DISTRICT, fixed=TRUE)
DistrictGIS@data$DISTRICT<-gsub("KAIMUR (BHABUA)", "KAIMUR", DistrictGIS$DISTRICT, fixed=TRUE)
DistrictGIS@data$DISTRICT<-gsub("PURBA", "PURBI", DistrictGIS@data$DISTRICT, fixed=TRUE)
DistrictGIS@data<-full_join(DistrictGIS@data, District_Facesheet, by=c("DISTRICT"="District"))

DistrictCoord<-fortify(DistrictGIS, region="censuscode")
centroid <- aggregate(cbind(DistrictCoord $long, DistrictCoord$lat) ,by=list(DistrictCoord$id), FUN=function(x) mean(range(x)))
rm(DistrictCoord)
centroid$Group.1<-as.numeric(centroid$Group.1)
DistrictGIS@data<-full_join(DistrictGIS@data, centroid, by=c("censuscode"="Group.1"))

rm(centroid)
save.image(file="Dashboard/Cooked.RData")
