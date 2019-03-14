library(caret)
library(doMC)
registerDoMC(4)
data <- read.csv('C:/Sreeha/Desktop/Phishing Data.csv', header = F,colClasses = "factor")
names <- c("having_IP_Address", "URL_Length", "Shortening_Service", "having_At_Symbol",
           "double_slash_redirecting", "Prefix_Suffix", "having_Sub_Domain","SSLfinal_State",
           "Domain_registration_length", "Favicon", "port","HTTPS_token", "Request_URL", "URL_of_Anchor",
           "Links_in_tags","SFH", "Submitting_to_email", "Abnormal_URL", "Redirect","On_mouseover",
           "RightClick", "popUp_Window", "Iframe","age_of_domain", "DNSRecord", "web_traffic", "Page_Rank",
           "Google_Index", "Links_pointing_to_page", "Statistical_report", "Result")
names(data)<-names
str(data)
set.seed(1234)
train_in <- createDataPartition(y = data$Result,p = 0.75, list = FALSE)
training <- data[train_in,]
testing <- data[-train_in,]
fitControl <- trainControl(method = 'repeatedcv', repeats = 5,number = 5, verboseIter = T)

#SVM MODEL
print("Radial SVM and Prediction")
fitControl = trainControl(method = "repeatedcv", repeats = 5,number = 5, verboseIter = T)
rbfsvm.fit <- train(Result ~ .,  data = training, method = "svmRadial", trControl = fitControl,tuneLength = 5)
rbfsvm.predict <- predict(rbfsvm.fit, testing[,-31])
confusionMatrix(rbfsvm.predict, testing$Result)

#RANDOM FOREST MODEL
print("Random Forest and Prediction")
rf.fit <- train(Result ~ .,  data = training, method = "rf",importance = T, trControl = fitControl,tuneLength = 5)
plot(rf.fit)
rf.predict <- predict(rf.fit, testing[,-31])
