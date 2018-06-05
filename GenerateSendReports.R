library(readr)
#Generate Reports for All Reviewers
committee <- read_csv("EasychairData/committee.csv",
                      locale = locale(encoding = "UTF8"))

Reviews <- read_csv("EasychairData/review.csv")

generateReport <- function(reviewerId){
    realReviewersFull<-committee[committee$`track #`==1 & (committee$`role`=="PC member" | committee$`role`=="senior PC member"),]
    realReviewersShort<-committee[committee$`track #`==2 & (committee$`role`=="PC member" | committee$`role`=="senior PC member"),]
    realReviewersAbstract<-committee[committee$`track #`==3 & (committee$`role`=="PC member" | committee$`role`=="senior PC member"),]

    reviewerFullnumber<-realReviewersFull[realReviewersFull$'person #'==reviewerId,]$`#`
    reviewerShortnumber<-realReviewersShort[realReviewersShort$'person #'==reviewerId,]$`#`
    reviewerAbstractnumber<-realReviewersAbstract[realReviewersAbstract$'person #'==reviewerId,]$`#`
    
    reviewsFull<-Reviews[Reviews$`member #`==reviewerFullnumber, ]
    reviewsShort<-Reviews[Reviews$`member #`==reviewerShortnumber, ]
    reviewsAbstract<-Reviews[Reviews$`member #`==reviewerAbstractnumber, ]
    
    reviewsOfReviewer<-rbind(reviewsFull,reviewsShort,reviewsAbstract)
    print("Creating")
    print(reviewerId)
    print(length(reviewsOfReviewer$'#'))
    if(length(reviewsOfReviewer$'#')>0){
        outf=paste("Reports/FeedbackReport_",reviewerId,".pdf",sep="")
        reviewerName=paste(realReviewers[realReviewers$`person #`==reviewerId,]$'first name',realReviewers[realReviewers$`person #`==reviewerId,]$'last name')
        reviewerName=enc2utf8(reviewerName)
        reviewerFirstName<-realReviewers[realReviewers$`person #`==reviewerId,]$'first name'
        rmarkdown::render("FeedbackReport.Rmd", output_file=outf, params = list(reviewerId = reviewerId, reviewerName = reviewerName, reviewerFirstName = reviewerFirstName))
    }
}

realReviewersFull<-committee[committee$`track #`==1 & (committee$`role`=="PC member" | committee$`role`=="senior PC member"),]
numberReviewsFull <- sapply(realReviewersFull$`#`, function(v) nrow(Reviews[Reviews$`member #`==v,]))


realReviewersShort<-committee[committee$`track #`==2 & (committee$`role`=="PC member" | committee$`role`=="senior PC member"),]
numberReviewsShort <- sapply(realReviewersShort$`#`, function(v) nrow(Reviews[Reviews$`member #`==v,]))

realReviewersAbstract<-committee[committee$`track #`==3 & (committee$`role`=="PC member" | committee$`role`=="senior PC member"),]
numberReviewsAbstract <- sapply(realReviewersAbstract$`#`, function(v) nrow(Reviews[Reviews$`member #`==v,]))

realReviewers<-rbind(realReviewersFull,realReviewersShort,realReviewersAbstract)
realReviewers<-aggregate(realReviewers, list(realReviewers$`person #`),last)

numberReviews <- sapply(realReviewers$`person #`, function(v) generateReport(v))

numberReviews <- sapply(realReviewers$`person #`, function(v) print(v))

#Generate test report
pN=1
reviewerName=paste(realReviewers[realReviewers$`person #`==pN,]$'first name',realReviewers[realReviewers$`person #`==pN,]$'last name')
reviewerName=enc2utf8(reviewerName)
print(reviewerName)
rmarkdown::render("/Users/xavierochoa/Documents/PythonNotebooks/LAK18Review/FeedbackReport.Rmd", clean=FALSE,params = list(reviewerId = pN, reviewerName = reviewerName, reviewerFirstName="Ryan"))


#email reports
mailReport <- function(reviewerId){
    
        outf=paste("/Users/xavierochoa/Documents/PythonNotebooks/LAK18Review/Reports/FeedbackReport_",reviewerId,".pdf",sep="")
        reviewerFirstName<-enc2native(realReviewers[realReviewers$`person #`==reviewerId,]$'first name')
        reviewerLastName<-enc2native(realReviewers[realReviewers$`person #`==reviewerId,]$'last name')
        reviewerEmail<-realReviewers[realReviewers$`person #`==reviewerId,]$'email'
        reviewerName=paste(reviewerFirstName,reviewerLastName)
        reviewerName=enc2native(reviewerName)
        mess<-paste("Dear ",reviewerFirstName,", \n\n",sep="")
        mess<-paste(mess,"Thanks for being part of the review process for LAK-18. ",sep="")
        mess<-paste(mess,"With your collaboration, we were able to provide at least 3 reviews per paper before the notification deadline.\n\n",sep="")
        mess<-paste(mess,"To help you improve as a reviewer for LAK, we have prepared a Feedback Report that you will find attached to this email. ",sep="")
        mess<-paste(mess,"This report contains some metrics and recommendations, as well as the review text of your co-reviewers and the metareview. ",sep="")
        mess<-paste(mess,"We hope that this information helps you reflect on your reviews and better understand the diverse and rich points of view of the Learning Analytics community.\n\n",sep="")
        mess<-paste(mess,"This report has been generated automatically and only we, the PC-chairs for LAK-18, have access to it. ",sep="")
        mess<-paste(mess,"If you have any question, comment or suggestion for future versions of this report, do not hesitate to reply to this email.\n\n ",sep="")
        mess<-paste(mess,"Thank you again for your collaboration,\n\n ",sep="")
        mess<-paste(mess,"With kind regards,\n\n ",sep="")
        mess<-paste(mess,"Simon, Rebecca, Agathe and Xavier\n ",sep="")
        mess<-paste(mess,"LAK-18 PC-chairs ",sep="")
        
        sender <- "xavier@cti.espol.edu.ec" # Replace with a valid address
        recipients <- c("xavier.ochoa@gmail.com") # Replace with one or more valid addresses
        #recipients <- c(reviwerEmail)
        print(reviewerEmail)
        email <- send.mail(from = sender,
                           to = recipients,
                           subject="LAK-18 Reviewer Feedback Report",
                           body = mess,
                           smtp = list(host.name = "smtp.gmail.com", port = 465, 
                                       user.name = "xavier.ochoa",            
                                       passwd = "jlgy4:40", ssl = TRUE),
                           attach.files = c(outf),
                           authenticate = TRUE,
                           send = FALSE)
        email$send()
        #gmail(to="xavier.ochoa@gmail.com", password="jlgy4:40", subject = "LAK-18 Reviewer Feedback Report", message = mess, from = "xavier@cti.espol.edu.ec", attachment = outf, username = "xavier.ochoa")
        
        #msg = mime() %>%
        #    from("xavier@cti.espol.edu.ec") %>%
        #    #to(reviewerEmail) %>%
        #    to("xavier.ochoa@gmail.com") %>%
        #    subject("LAK-18 Reviewer Feedback Report") %>%
        #    text_body(message) %>%
        #    attach_file(outf)
        
        #strwrap(as.character(msg))
        #print(paste("Sending email to",reviewerId))
        #send_message_fixed(msg)
        print(paste("Sent to",reviewerName, "with email", reviewerEmail,"the file", outf ))
  
}

#email to everyone
realReviewers<-committee[committee$`track #`==1 & committee$`role`=="PC member",]
numberReviews <- sapply(realReviewers$`#`, function(v) mailReport(v))


#email single report
mailReport(1)



