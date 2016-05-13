library(rvest)
library(XML)
library(RMySQL)
library(stringr)
library(lubridate)
library(RCurl)
library(tidyr)

proj_user <- "sql5118904"
proj_pwd  <- "wUQcwMzwUl"
proj_db   <- "sql5118904"
proj_host <- "sql5.freemysqlhosting.net"

# IPO Issue Details
IPO_Issue_Details = data.frame(stringsAsFactors = FALSE)
for (i in 2007:2016){
    url <- paste0('http://www.chittorgarh.com/ipo/reports/ipo_report_yearly_ipo_list.asp?y=', i)
    data_table = readHTMLTable(url, header=FALSE, as.data.frame = TRUE, trim=TRUE, stringsAsFactors = FALSE, colClasses = c("character", "character", "character", "numeric", "character", "numeric"))
    ipo_data <- data.frame(data_table, stringsAsFactors = FALSE)
    names(ipo_data) <- c("IssuerCompany", "IssueOpenDate", "IssueCloseDate", "IssuePrice", "IssueType", "IssueSize")
    IPO_Issue_Details <- rbind(IPO_Issue_Details, cbind(ipo_data, IPOYear=i))    
    Sys.sleep(time=1)
}

IPO_Issue_Details$IssueOpenDate<-as.Date(IPO_Issue_Details$IssueOpenDate,  "%B%d, %Y")
IPO_Issue_Details$IssueCloseDate<-as.Date(IPO_Issue_Details$IssueCloseDate,  "%B%d, %Y")
IPO_Issue_Details$IssuerCompany <- str_replace(IPO_Issue_Details$IssuerCompany, " IPO", "")

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
dbWriteTable(mydb,"IPO_Issue_Details",IPO_Issue_Details,overwrite=T)
dbDisconnect(mydb)

write.csv(IPO_Issue_Details, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/IPO_Issue_Details.csv")


# Over subscription vs listing gain
IPO_OverSub_ListingGains = data.frame(stringsAsFactors = FALSE)
for (i in 1:13){
    url <- paste0('http://www.chittorgarh.com/ipo/reports/ipo_report_listing_day_gain.asp?FormIPO_Page=', i)
    data_table = readHTMLTable(url,  as.data.frame = TRUE, trim=TRUE, stringsAsFactors = FALSE, colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
    ipo_data <- as.data.frame(data_table)
    IPO_OverSub_ListingGains <- rbind(IPO_OverSub_ListingGains, ipo_data)    
    Sys.sleep(time=1)
}

IPO_OverSub_ListingGains <- IPO_OverSub_ListingGains[,c(1, 4:13)] 
names(IPO_OverSub_ListingGains) <- c('IssuerCompany', 'QIB', 'NII', 'RII', 'EMP', 'Total', 'OpenPrice', 'LowPrice', 'HighPrice', 'ClosePrice', 'ListingGains')
IPO_OverSub_ListingGains$IssuerCompany <- str_replace(IPO_OverSub_ListingGains$IssuerCompany, " IPO", "")

write.csv(IPO_OverSub_ListingGains, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/IPO_OverSub_ListingGains.csv")

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
dbWriteTable(mydb,"IPO_OverSub_ListingGains",IPO_OverSub_ListingGains,overwrite=T)
dbDisconnect(mydb)


# IPO Listing Dates
IPO_ListingDates = data.frame(stringsAsFactors = FALSE)
for (i in 1:19){
    url <- paste0('http://www.chittorgarh.com/ipo/ipo_perf_tracker.asp?FormIPOPT_Page=', i)

    IssuerCompany <- url %>%
        read_html() %>%
        xml_nodes(xpath = '/html/body/div[3]/div/div[1]/div/div[3]/table/tbody/tr/td/a/div/div') %>%
        xml_text() 

    ListingDate <- url %>%
        read_html() %>%
        xml_nodes(xpath = '/html/body/div[3]/div/div[1]/div/div[3]/table/tbody/tr/td[2]') %>%
        xml_text() 

    CurPrice <- url %>%
        read_html() %>%
        xml_nodes(xpath = '/html/body/div[3]/div/div[1]/div/div[3]/table/tbody/tr/td[4]') %>%
        xml_text() 
    
        
    IssuerCompany <- as.data.frame(IssuerCompany, stringsAsFactors = FALSE)
    ListingDate <- as.data.frame(ListingDate, stringsAsFactors = FALSE)
    CurPrice <- as.data.frame(as.numeric(CurPrice))
    
    IPO_ListingDates <- rbind(IPO_ListingDates, cbind(IssuerCompany, ListingDate, CurPrice))
}

names(IPO_ListingDates) <- c('IssuerCompany', 'ListingDate', 'CurPrice')
IPO_ListingDates$ListingDate<-as.Date(IPO_ListingDates$ListingDate,  "%B%d, %Y")
IPO_ListingDates$CurPrice<-as.numeric(IPO_ListingDates$CurPrice)

write.csv(IPO_ListingDates, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/IPO_ListingDates.csv")

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
dbWriteTable(mydb,"IPO_ListingDates",IPO_ListingDates,overwrite=T)
dbDisconnect(mydb)


# IPO Rating
IPO_Rating_Details = data.frame(stringsAsFactors = FALSE)
for (i in 1:20){
    ipo_details <- paste0('http://www.chittorgarh.com/ipo/ipo_ratings.asp?FormIPORating_Page=', i)
    ipo_table = readHTMLTable(ipo_details, as.data.frame = TRUE)
    ipo_data <- data.frame(ipo_table, stringsAsFactors = FALSE)
    IPO_Rating_Details <- rbind(IPO_Rating_Details, ipo_data)    
    Sys.sleep(time=1)
}

IPO_Rating_Details <- IPO_Rating_Details[IPO_Rating_Details[1] != 'Rating Scale >',]

IPO_Rating_Details <- cbind(IPO_Rating_Details, Subscribe=str_count(str_to_lower(IPO_Rating_Details[,4]), 'subscribe') + str_count(str_to_lower(IPO_Rating_Details[,4]), 'invest') + str_count(str_to_lower(IPO_Rating_Details[,4]), 'apply')-str_count(str_to_lower(IPO_Rating_Details[,4]), 'don[:print:]+apply')) 

IPO_Rating_Details <- cbind(IPO_Rating_Details, Avoid=str_count(str_to_lower(IPO_Rating_Details[,4]), 'avoid') + str_count(str_to_lower(IPO_Rating_Details[,4]), 'don[:print:]+apply')) 

IPO_Rating_Details <- cbind(IPO_Rating_Details, Neutral=str_count(str_to_lower(IPO_Rating_Details[,4]), 'neutral')) 

names(IPO_Rating_Details) <- c('IssuerCompany', 'OfficialRating', 'CapitalMarketRating', 'IPOAdvice', 'Subscribe', 'Avoid', 'Neutral')

IPO_Rating_Details$OfficialRating <- as.numeric(IPO_Rating_Details$OfficialRating)
IPO_Rating_Details$CapitalMarketRating <- as.numeric(IPO_Rating_Details$CapitalMarketRating)

IPO_Rating_Details <- IPO_Rating_Details[,c(1:3, 5:7)] 

write.csv(IPO_Rating_Details, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/IPO_Rating_Details.csv")

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
dbWriteTable(mydb,"IPO_Rating_Details",IPO_Rating_Details,overwrite=T)
dbDisconnect(mydb)

# Company  Sectors
# FirstExtract all Sectors
company_sectors = data.frame(stringsAsFactors = FALSE)

url <- 'http://bullseye.in.com/stocks/marketstats/indcomp.php?optex=BSE'
sector_urls <- url %>%
    read_html() %>%
    html_nodes(xpath = "/html/body/center[2]/div[1]/div[1]/div[7]/div[4]/div[1]/div[2]/ul")%>%
    html_children() %>%
    html_text() 

write.csv(sector_urls, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/sector_urls.csv")


# Next extract company in each sector

for(i in 2:length(sector_urls)){
    # Extract objective of IPO from each link
    url <- paste0('http://bullseye.in.com/stocks/marketstats/indcomp.php?optex=BSE&indcode=',curlEscape(sector_urls[i]))
    
    parse_text <- url %>%
        read_html() %>%
        xml_nodes(xpath = "//td//b") %>%
        xml_text() 
    if(length(parse_text)>0){
        parse_text <- as.data.frame(parse_text, stringsAsFactors = FALSE)
        company_sectors <- rbind(company_sectors, cbind(sector_urls[i], parse_text))
        Sys.sleep(time=1)
        message("Done Page No:", url, " Contains companies: ", nrow(parse_text))
    }
}

names(company_sectors) <- c('Sectors', 'IssuerCompany') 

write.csv(company_sectors, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/Company_Sectors.csv")

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
dbWriteTable(mydb,"Company_Sectors",company_sectors,overwrite=T)
dbDisconnect(mydb)

## NOTE : There is a manual clean-up in the company_sectors.csv since the issuername comes from 2 different sources and there is a name mismatch. I used excel to standardize the names across the 2 sources and reload back to MySQL and rewrite the csv as well. Alternative was to use a mapping table which I avoided for now. 



# Registrar
# 
# mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
# Registrars = dbGetQuery(conn = mydb, statement = "Select * from Registrars")
# dbDisconnect(mydb)
# 
# Registrar_IPO_List = data.frame()
# 
# write.csv(Registrars, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/Registrars.csv")
# 
# for (i in 1:nrow(Registrars[1])){
#     url <- paste0('http://www.chittorgarh.com/ipo/reports/ipo_list_by_issue_registrar.asp?a=', Registrars[i,1])
#     
#     Num_pages <- url %>%
#         read_html() %>% 
#         xml_nodes(xpath = "/html/body/div[3]/div/div[1]/div/div[2]/p[6]/text()[1]") %>%
#         xml_text() %>% 
#         str_extract_all("[0-9]+") %>%
#         as.integer()
#     
#     if(is.na(Num_pages)== FALSE){
#         Registrar_table = readHTMLTable(url, as.data.frame = TRUE)
#         registrar_data <- data.frame(Registrar_table, stringsAsFactors = FALSE)
#         Registrar_IPO_List <- rbind(Registrar_IPO_List, cbind(RegistrarName=Registrars[i, 2], registrar_data))    
#         Sys.sleep(time=1)
#     
#         if(ceiling(Num_pages/50)>=2){
#             for (j in 2:ceiling(Num_pages/50)){
#                 url <- paste0('http://www.chittorgarh.com/ipo/reports/ipo_list_by_issue_registrar.asp?a=', Registrars[i,1],'&FormIPO_Page=', j )
#                 
#                 Registrar_table = readHTMLTable(url, as.data.frame = TRUE)
#                 registrar_data <- data.frame(Registrar_table, stringsAsFactors = FALSE)
#                 Registrar_IPO_List <- rbind(Registrar_IPO_List, cbind(RegistrarName=Registrars[i, 2], registrar_data))    
#                 Sys.sleep(time=1)
#             }
#         }
#     }
#             
# }
# 
# names(Registrar_IPO_List) <- c('RegistrarName', 'IssuerCompany', 'IssueOpen', 'IssueClose', 'IssuePrice', 'IssueSize') 
# 
# Registrar_IPO_List <- Registrar_IPO_List[,c(1:2)] 
# 
# write.csv(Registrar_IPO_List, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/Registrar_IPO_List.csv")
# 
# mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
# dbWriteTable(mydb,"Registrar_IPO_List",Registrar_IPO_List,overwrite=T)
# dbDisconnect(mydb)


# Lead Manager

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
LeadManagers = dbGetQuery(conn = mydb, statement = "Select * from LeadManagers")
dbDisconnect(mydb)

LeadManager_IPO_List = data.frame()

write.csv(LeadManagers, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/LeadManagers.csv")


for (i in 1:nrow(LeadManagers[1])){
    url <- paste0('http://www.chittorgarh.com/ipo/reports/lead_manager_vs_listing_gain.asp?a=', LeadManagers[i,1])

    Num_pages <- url %>%
        read_html() %>% 
        xml_nodes(xpath = "/html/body/div[3]/div[1]/div[2]/p[4]/text()[1]") %>%
        xml_text() %>% 
        str_extract_all("[0-9]+") %>%
        as.integer()
    
    if(is.na(Num_pages)== FALSE){
        LeadManager_table = readHTMLTable(url, as.data.frame = TRUE)
        LeadManager_data <- data.frame(LeadManager_table, stringsAsFactors = FALSE)
        LeadManager_IPO_List <- rbind(LeadManager_IPO_List, cbind(LeadManagerName=LeadManagers[i, 2], LeadManager_data))    
        Sys.sleep(time=1)
        
        if(ceiling(Num_pages/50)>=2){
            for (j in 2:ceiling(Num_pages/50)){
                url <- paste0('http://www.chittorgarh.com/ipo/reports/lead_manager_vs_listing_gain.asp?a=', LeadManagers[i,1],'&FormIPO_Page=', j )
                LeadManager_table = readHTMLTable(url, as.data.frame = TRUE)
                LeadManager_data <- data.frame(LeadManager_table, stringsAsFactors = FALSE)
                LeadManager_IPO_List <- rbind(LeadManager_IPO_List, cbind(LeadManagerName=LeadManagers[i, 2], LeadManager_data))    
                Sys.sleep(time=1)
            }
        }
    }
}


names(LeadManager_IPO_List) <- c('LMName', 'IssuerCompany', 'IssueSize', 'OverSubscriptionTotal.', 'IssuePrice', 'ListingGains') 

LeadManager_IPO_List <- LeadManager_IPO_List[,c(1:2)]


#LeadManager_IPO_List<-cbind(LeadManager_IPO_List, Dummy=1)
#LeadManager_IPO_List %>% spread(LMName, Dummy)


write.csv(LeadManager_IPO_List, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/LeadManager_IPO_List.csv")

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
dbWriteTable(mydb,"LeadManager_IPO_List",LeadManager_IPO_List,overwrite=T)
dbDisconnect(mydb)


# IPO Objectives
# FirstExtract all IPo Links
IPo_URLs_for_Objective = data.frame(stringsAsFactors = FALSE)
for (i in 1:42){
    url <- paste0('http://www.chittorgarh.com/ipo/ipo_prospectus_list.asp?FormIPOProspectus_Page=', i)
    Objective_urls <- url %>%
        read_html() %>%
        html_nodes(xpath = "/html/body/div[3]/div/div[1]/div/div[3]/table/tbody//td[1]") %>%
        html_children()  %>%
        html_attr("href") 
    
    Objective_company <- url %>%
        read_html() %>%
        html_nodes(xpath = "/html/body/div[3]/div/div[1]/div/div[3]/table/tbody//td[1]") %>%
        html_children()  %>%
        html_text() 
    
    Objective_urls <- as.data.frame(Objective_urls, stringsAsFactors = FALSE)
    Objective_company <- as.data.frame(Objective_company, stringsAsFactors = FALSE)
    
    IPo_URLs_for_Objective <- rbind(IPo_URLs_for_Objective, cbind(Objective_company, Objective_urls))

#    message("Done Page No:", i, " Contains URLs: ", nrow(Objective_company))
    
    Sys.sleep(time=1)
}

write.csv(IPo_URLs_for_Objective, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/IPo_URLs_for_Objective.csv")

# Next parse objective from each URL
for(i in 1:nrow(IPo_URLs_for_Objective[2])){
    # Extract objective of IPO from each link
    url <- IPo_URLs_for_Objective[i,2]
    parse_text <- url %>%
        read_html() %>%
        xml_nodes(xpath = "/html/body/div[3]/div/div[1]/div/div[3]") %>%
        xml_text() 
    Sys.sleep(time=1)
    write(parse_text, file = paste0('D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/Objectives/', IPo_URLs_for_Objective[i,1], '.txt' ))
}

# # IPO reviews
# IPO_Review_Details = data.frame()
# for (i in 1:8){
#     ipo_details <- paste0('http://www.chittorgarh.com/ipo/ipo_review_list.asp?FormIPOReview_Page=', i)
#     ipo_table = readHTMLTable(ipo_details, as.data.frame = TRUE)
#     ipo_data <- data.frame(ipo_table, stringsAsFactors = FALSE)
#     IPO_Review_Details <- rbind(IPO_Review_Details, ipo_data)    
#     Sys.sleep(time=1)
# }
# write.csv(IPO_Review_Details, file = "D:/CUNY/Courses/Data Acquisition and Management/CUNY-DAM/FinalProject/IPO_Review_Details.csv")

