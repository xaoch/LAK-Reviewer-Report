library(readr)
library(lubridate)
library(pander)
panderOptions('round', 2)
panderOptions('keep.trailing.zeros', TRUE)

committee <- read_csv("~/Documents/PythonNotebooks/LAK17Review/committee.csv",
                      locale = locale(encoding = "UTF8"))


reviews <- read_csv("~/Documents/PythonNotebooks/LAK17Review/review.csv", 
                    col_types = cols(date = col_character(), 
                                     time = col_character()), locale = locale(encoding = "UTF8"))

submissions <- read_csv("~/Documents/PythonNotebooks/LAK17Review/submission.csv", 
                        locale = locale(encoding = "UTF-8"))

review_text <- read_csv("~/Documents/PythonNotebooks/LAK17Review/review_text.csv", 
                        locale = locale(encoding = "UTF-8"))

reviewerId<-reviewer$'#'

realReviewers<-committee[committee$`track #`==1 & committee$`role`=="PC member",]
numberReviews <- sapply(realReviewers$`#`, function(v) nrow(reviews[reviews$`member #`==v,]))
realReviewers$nofReviews<-numberReviews
datesReviews <- reviews[reviews$`member #` %in% realReviewers$`#`,c("member #", "date","time")]
datesReviews$DateTime<-ymd_hm(paste(datesReviews$date,datesReviews$time))
deadline<-ymd_hm("2016-11-24 12:00")
datesReviews$Minutes<-sapply(datesReviews$DateTime, function(d) (d-deadline)/ddays(1))

reviews$Length<-sapply(gregexpr("[[:alpha:]]+", reviews$text), function(x) sum(x > 0)-2)
review_text$Length<-sapply(gregexpr("[[:alpha:]]+", review_text$text), function(x) sum(x > 0))

reviews$TextLength<-sapply(reviews$'#', function(x) review_text[review_text$`review #`==x & review_text$'field #'==1,]$Length )
reviews$CommentText<-sapply(reviews$'#', function(x) review_text[review_text$`review #`==x & review_text$'field #'==1,]$text )

realReviews<-reviews[reviews$`member #` %in% realReviewers$`#`,]

submissions$accepted<-submissions$decision=="Accept as Full paper" | submissions$decision=="The paper is accepted as short"

findConcordance<-function(x){
    Rreviews<-reviews[reviews$`member #`==x,]
    Rreviews$accepted<-sapply(Rreviews$'submission #', function(x) submissions[submissions$'#'==x,]$accepted)
    Rreviews$Concordance<-(Rreviews$"total score">0 & Rreviews$accepted==TRUE) | (Rreviews$"total score"<0 & Rreviews$accepted==FALSE)
    sum(Rreviews$Concordance)
}

realReviewers$Concordance<-sapply(realReviewers$`#`, findConcordance)
realReviewers$RelativeConcordance<-realReviewers$Concordance/realReviewers$nofReviews

findDeviation<-function(x){
    subm<-realReviews[realReviews$"#"==x,]$"submission #"
    score<-realReviews[realReviews$"#"==x,]$"total score"
    subReviews<-realReviews[realReviews$"submission #"==subm,]
    averagescore<-mean(subReviews$"total score")
    score-averagescore
}

realReviews$Deviation<-sapply(realReviews$'#', findDeviation)

findMinutes<-function(x){
    subm<-datesReviews[datesReviews$"member #"==x,]
    mean(subm$Minutes)
}

realReviewers$Minutes<-sapply(realReviewers$'#', findMinutes)

reviewerDeviation<-function(x){
    subm<-realReviews[realReviews$"member #"==x,]
    mean(subm$Deviation)
}

realReviewers$Deviation<-sapply(realReviewers$'#', reviewerDeviation)

findLength<-function(x){
    subm<-realReviews[realReviews$"member #"==x,]
    mean(subm$TextLength)
}

realReviewers$TextLength<-sapply(realReviewers$'#', findLength)

write.csv(realReviewers, file = "2017data.csv")
