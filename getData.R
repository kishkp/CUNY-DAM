
library(RCurl)
library(stringr)


# Read the data from github and skip the first 2 lines.  

URL_list <- read.csv("D:/CUNY/Courses/Data Acquisition and Management/Project 3/webpages_limited.csv", stringsAsFactors = FALSE)

skill_list <- read.csv("D:/CUNY/Courses/Data Acquisition and Management/Project 3/skills_limited.csv", stringsAsFactors = FALSE)

d <- data.frame(URL="", Skill="", Count=0, stringsAsFactors = FALSE)

for (eachURL in URL_list$WebpageURL){
    URL_raw<- getURL(eachURL)
    for (eachskill in skill_list$Skill) {
        
        d <- rbind(d, c(URL_list[URL_list$WebpageURL==eachURL,2], eachskill, str_count(URL_raw, eachskill)))        
        
    }
}
