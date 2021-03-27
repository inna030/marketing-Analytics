library("xtable") # processing of regression output
library("knitr") # used for report compilation and table display
library("ggplot2") # very popular plotting library ggplot2
library("ggthemes") # themes for ggplot2
suppressMessages(library("mlogit")) # multinomial logit
library("caret")

RFMdata <- read.csv(file = "RFMData.csv",row.names=1)
kable(head(RFMdata,5),row.names = TRUE)
transp_dec<-rbind(
  colSums(data[seq(1, nrow(data), 4), ])/210,
  colSums(data[seq(2, nrow(data), 4), ])/210,
  colSums(data[seq(3, nrow(data), 4), ])/210,
  colSums(data[seq(4, nrow(data), 4), ])/210)
transp_dec<-transp_dec[,c(2:5)]
colnames(transp_dec) <- c('CHOICE SHARE','AVG. WAITING TTME', 'AVG. COST', 'AVG. TRAVEL TIME')
kable(transp_dec)
Household_Income <- data[seq(1, nrow(data), 4), 6]
summary(Household_Income)
require('mlogit')
mdata <- mlogit.data(data=data,
                     choice='MODE', # variable that contains choice
                     shape='long', # tells mlogit how data is structured (every row is alternative)
                     varying=3:5, # only select variables that describe the alternatives
                     alt.levels = c("plane", "train", "bus", "car"), # levels of the alternatives
                     id.var='TRAVELER') # consumer id
head(mdata,6)
set.seed(999)
model <- mlogit(MODE~TTME+INVC+INVT,data=mdata)
summary(model)
model.null <- mlogit(MODE~1,data=mdata)
lrtest(model,model.null)
kable(head(predict(model,mdata),1))
predicted_alternative <- apply(predict(model,mdata),1,which.max)
selected_alternative <- rep(1:4,210)[data$MODE>0]
confusionMatrix(predicted_alternative,selected_alternative)
model1 <- mlogit(MODE~TTME+INVC+INVT|HINC,data=mdata)
summary(model1)
lrtest(model1,model)
predicted_alternative <- apply(predict(model1,mdata),1,which.max)
selected_alternative <- rep(1:4,210)[data$MODE>0]
confusionMatrix(predicted_alternative,selected_alternative)
mdata.new <- mdata
mdata.new[seq(2,840,4),"INVT"] <- 0.9*mdata.new[seq(2,840,4),"INVT"]
predicted_alternative_new <- apply(predict(model1,mdata.new),1,which.max)

table(predicted_alternative)/210 # probability under original data
table(predicted_alternative_new)/210
(table(predicted_alternative_new) - table(predicted_alternative))/table(predicted_alternative)
model2 <- mlogit(MODE~TTME+INVC+INVT+TTME:HINC+INVC:HINC+INVT:HINC|HINC,data=mdata)
summary(model2)
lrtest(model2,model1)
















