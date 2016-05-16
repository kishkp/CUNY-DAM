library(shiny)
library(RMySQL)
library(tidyr)
library(ggplot2)
library(reshape2)

# Load all the tables from MySQL

proj_user <- "sql5118904"
proj_pwd <- "wUQcwMzwUl"
proj_db <- "sql5118904"
proj_host <- "sql5.freemysqlhosting.net"

mydb = dbConnect(MySQL(), user=proj_user, password=proj_pwd, dbname=proj_db, host=proj_host)
#LeadManagers = dbGetQuery(conn = mydb, statement = "Select * from LeadManagers")
IPO_Issue_Details = dbGetQuery(conn = mydb, statement = "Select * from IPO_Issue_Details")
Company_Sectors = dbGetQuery(conn = mydb, statement = "Select * from Company_Sectors")
IPO_ListingDates = dbGetQuery(conn = mydb, statement = "Select * from IPO_ListingDates")
IPO_OverSub_ListingGains = dbGetQuery(conn = mydb, statement = "Select * from IPO_OverSub_ListingGains")
IPO_Rating_Details = dbGetQuery(conn = mydb, statement = "Select * from IPO_Rating_Details")
BSEData = dbGetQuery(conn = mydb, statement = "Select * from BSEData")
#LeadManager_IPO_List = dbGetQuery(conn = mydb, statement = "Select * from LeadManager_IPO_List")
# sqlStatement<- "SELECT a.IssuerCompany, a.IssuePrice, a.IssueType, a.IssueSize, b.Sector as IssuerSector, c.ListingDate, 
#         d.QIB, d.NII, d.RII, d.EMP, d.ListingGains, e.CapitalMarketRating as IPORating, e.Subscribe as Subscribe_Advice, 
#         e.Avoid as Avoid_Advice, e.Neutral as Neutral_Advice, 
#         f.Close as BSE_ClosePrice, f.DMA7, f.DMA15, f.DMA50, f.DMA100, 
#         f.DMA200, g.NewLMName as LMName FROM IPO_Issue_Details a 
#             left join Company_Sectors b on a.IssuerCompany = b.IssuerCompany
#             left join IPO_ListingDates c on a.IssuerCompany = c.IssuerCompany
#             left join IPO_OverSub_ListingGains d on a.IssuerCompany = d.IssuerCompany
#             left join IPO_Rating_Details e on a.IssuerCompany = e.IssuerCompany
#             left join BSEData f on c.ListingDate = f.TradeDate
#             left join LeadManager_IPO_List g on a.IssuerCompany = g.IssuerCompany
#         where c.ListingDate is not NULL
#         and d.ListingGains is not NULL"

sqlStatement<- "SELECT a.IssuerCompany, a.IssuePrice, a.IssueType, a.IssueSize, b.Sector as IssuerSector, c.ListingDate, d.QIB, d.NII, d.RII, d.EMP, d.ListingGains, e.CapitalMarketRating as IPORating, e.Subscribe as Subscribe_Advice, e.Avoid as Avoid_Advice, e.Neutral as Neutral_Advice, f.Close as BSE_ClosePrice, f.DMA7, f.DMA15, f.DMA50, f.DMA100, f.DMA200 FROM IPO_Issue_Details a 
            left join Company_Sectors b on a.IssuerCompany = b.IssuerCompany
            left join IPO_ListingDates c on a.IssuerCompany = c.IssuerCompany
            left join IPO_OverSub_ListingGains d on a.IssuerCompany = d.IssuerCompany
            left join IPO_Rating_Details e on a.IssuerCompany = e.IssuerCompany
            left join BSEData f on c.ListingDate = f.TradeDate
        where c.ListingDate is not NULL
        and d.ListingGains is not NULL"


AllData = dbGetQuery(conn = mydb, statement = sqlStatement)

dbDisconnect(mydb)

# Data Transformations

#1. Date Related transformations 

# Generate a WeekDay
AllData<- cbind(AllData, ListingWeekDay = weekdays(as.Date(AllData$ListingDate)))
AllData <- subset(AllData, AllData$ListingWeekDay != 'Saturday')

AllData$ListingWeekDay<-factor(AllData$ListingWeekDay)


# Generate Year, Month, Day
AllData<-separate(AllData, ListingDate, c("ListingYear", "ListingMonth", "ListingDay"), sep="-")

#2. Calculation of Trend - The trend is calculated as the comparision of the previous day DMA to the previous day closing price.

#Calculate Trend based on 7 DMA and store as another column
#Note: Trend for TODAY is Previous day Close > previous day 7DMA

AllData <- cbind(AllData, Trend = ifelse(lag(AllData$BSE_ClosePrice) - lag(AllData$DMA7)>0, "Positive", "Negative"))

#3. Successful or Unsuccessful IPO flag - Based on a listing gains cut-off, we calculate the success or failure of the IPO

#This is calculated later.

#4. SubscriptionRate - Based on the subscription percentages by QIB, NII, RII and EMP I create a total subscription rate for the IPO. The assumption is that higher the subscription, higher the listing gains.

AllData <- cbind(AllData, SubscriptionRate = rowSums(AllData[9:12], na.rm=TRUE))


#5. ReviewScore - For each IPO there are counts for the advice given by brokerages. The advice is either subscribe, avoid or neutral. I would like to generate a review score based on these columns. This will give me a value with which I can compare the reviews. 

AllData <- cbind(AllData, ReviewScore = AllData$Subscribe_Advice / (AllData$Subscribe_Advice + AllData$Avoid_Advice + AllData$Neutral_Advice)*100)


# We can now remove the original columns.
#AllData <- AllData[,-c(1,8:12, 15:23)]


shinyServer(function(input, output) {

    AllData_re <-   reactive({
        cbind(AllData, IPOresult = ifelse(AllData$ListingGains>=input$Profit_Cutoff, "Successful", "Unsuccessful"))
    })
    
    train.dataset <- reactive({
        x<-subset(AllData_re(), ListingYear  %in% c(2007:2014))
        train.dataset <- x[,-c(1,5:13,15:23)]
    })
    test.dataset <- reactive({
        y<-subset(AllData_re(), ListingYear  %in% c(2015:2016))
        test.dataset <- y[,-c(1,5:13,15:23)]
    })        
    
    
    output$text1 <- renderText({ 
        paste("Below is a data extract from the selected table:" )
    })
    
    output$table1 <- renderTable({ 
        selected_opt <- switch(input$ds,
                        "IPO_Issue_Details" = IPO_Issue_Details,
                        "Company_Sectors" = Company_Sectors,
                        "IPO_ListingDates" = IPO_ListingDates,
                        "IPO_OverSub_ListingGains" = IPO_OverSub_ListingGains,
                        "IPO_Rating_Details" = IPO_Rating_Details,
                        "BSEData" = BSEData
                        )
        head(selected_opt)
    })
    
    output$table2 <- renderTable({ 
        z<-AllData_re()
        table(z$IPOresult)
     })

    output$table3 <- renderTable({
        z<-AllData_re()
        head(z)
        
    })
    
    output$text2 <- renderText({ 
            paste("Below is the chart for selected variable:" )
    })
        
    output$plot1 <- renderPlot({
        AllData<-AllData_re()

        if(input$exploreVar=='IssuePrice') {
            ggplot(AllData, aes(x=IssuePrice, colour=IPOresult)) + geom_density()
        }
        else if(input$exploreVar=='IssueSize') {
            ggplot(AllData, aes(x=IssueSize, colour=IPOresult)) + geom_density()
        }
        
        else if(input$exploreVar=='SubscriptionRate') {
            ggplot(AllData, aes(x=SubscriptionRate, colour=IPOresult)) + geom_density()
        }
        else if(input$exploreVar=='ReviewScore') {
            ggplot(AllData, aes(x=ReviewScore, colour=IPOresult)) + geom_density()
        }
        
        else if(input$exploreVar=='IssuerSector') {
            newds<-melt(table(AllData$IssuerSector, AllData$IPOresult))
            names(newds) <- c("IssuerSector", "IPOresult", "Counts")
            
            ggplot(newds, aes(newds$IssuerSector, newds$Counts, fill = newds$IPOresult)) +                 geom_bar(stat = "identity")
        }
        else if(input$exploreVar=='IssueType') {
            newds<-melt(table(AllData$IssueType, AllData$IPOresult))
            names(newds) <- c("IssueType", "IPOresult", "Counts")
            
            ggplot(newds, aes(newds$IssueType, newds$Counts, fill = newds$IPOresult)) +                 geom_bar(stat = "identity")
        }
        
        else if(input$exploreVar=='ListingMonth') {
            newds<-melt(table(AllData$ListingMonth, AllData$IPOresult))
            names(newds) <- c("ListingMonth", "IPOresult", "Counts")
            
            ggplot(newds, aes(newds$ListingMonth, newds$Counts, fill = newds$IPOresult)) + 
                geom_bar(stat = "identity")
        }
        else if(input$exploreVar=='IPORating') {
            newds<-melt(table(AllData$IPORating, AllData$IPOresult))
            names(newds) <- c("IPORating", "IPOresult", "Counts")
            
            ggplot(newds, aes(newds$IPORating, newds$Counts, fill = newds$IPOresult)) + 
                geom_bar(stat = "identity")
        }
        else if(input$exploreVar=='ListingWeekDay') {
            newds<-melt(table(AllData$ListingWeekDay, AllData$IPOresult))
            names(newds) <- c("ListingWeekDay", "IPOresult", "Counts")
            
            ggplot(newds, aes(newds$ListingWeekDay, newds$Counts, fill = newds$IPOresult)) + 
                geom_bar(stat = "identity")
        }
        else if(input$exploreVar=='Trend') {
            newds<-melt(table(AllData$Trend, AllData$IPOresult))
            names(newds) <- c("Trend", "IPOresult", "Counts")
            
            ggplot(newds, aes(newds$Trend, newds$Counts, fill = newds$IPOresult)) + 
                geom_bar(stat = "identity")
        }
    })
    
    output$text3 <- renderText({ 
        model <- glm(IPOresult ~.,family=binomial(link='logit'),data=train.dataset())
        regtext <- as.character(model$coefficients[1])
        
        for(i in 2:length(model$coefficients)) {
            regtext <- 
                paste0(regtext, " + (", names(model$coefficients[i]), " * ", model$coefficients[i] , ") " )
        }
        paste(regtext)
    })

    output$table4 <- renderTable({
        model <- glm(IPOresult ~.,family=binomial(link='logit'),data=train.dataset())
        cbind(test.dataset(), Prediction=ifelse(predict(model, test.dataset(), type="response")>=0.5, "Success", "Failure"))        
    })
    
    output$table5 <- renderTable({
        model <- glm(IPOresult ~.,family=binomial(link='logit'),data=train.dataset())
        table(cbind(test.dataset(), Prediction=ifelse(predict(model, test.dataset(), type="response")>=0.5, "Success", "Failure"))[,9:10])        
    })

    output$table6 <- renderTable({
        model <- glm(IPOresult ~.,family=binomial(link='logit'),data=train.dataset())
        round(prop.table(table(cbind(test.dataset(), Prediction=ifelse(predict(model, test.dataset(), type="response")>=0.5, "Success", "Failure"))[,9:10]),2)*100,2)        

    })
    
    output$text4 <- renderText({ 
        model <- glm(IPOresult ~.,family=binomial(link='logit'),data=train.dataset())
        pred = cbind(test.dataset(), Prediction=ifelse(predict(model, test.dataset(), type="response")>=0.5, "Successful", "Unsuccessful"))        
        accuracy <- table(pred$Prediction, pred$IPOresult)
        predrate<- round(sum(diag(accuracy))/sum(accuracy)*100,2)
        paste("Predictive Rate: ", predrate, " %")
    })
        
#     output$text5 <- renderText({ 
#         model <- glm(IPOresult ~.,family=binomial(link='logit'),data=train.dataset())
#         pred = cbind(test.dataset(), Prediction=ifelse(predict(model, test.dataset(), type="response")>=0.5, "Successful", "Unsuccessful"))        
#         accuracy <- table(pred$Prediction, pred$IPOresult)
#         predrate<- round(sum(diag(accuracy))/sum(accuracy)*100,2)
#         
#         x<-confusionMatrix(data=pred$Prediction, pred$IPOresult)
#         x[2:4]
#     })
    
                
})





