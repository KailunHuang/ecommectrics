setwd("D:/study/econometrics1/Assignment/Assignment_2")

bil_data = read.csv("billionaires_clean2.csv")
# install.packages("devtools")
# devtools::install_github("acoppock/commarobust")
# library("commarobust")
library("sandwich")
library("AER")
library("survival")
library("rlang")

summary(bil_data)

numbil = bil_data$numbil0
natrent = bil_data$natrent
pop =  bil_data$pop
gdppc = bil_data$gdppc
roflaw = bil_data$roflaw

bil_data$natrent = bil_data$natrent/(10^10)

bil_data$pop = bil_data$pop/(10^7)

library("ggplot2")

#Q2

ggplot(data=bil_data, aes(x=natrent,y=numbil0))+
  geom_point(color = "black")+
  geom_smooth(method = "lm", formula = y ~ poly(x,1),se = FALSE)

ggplot(data=bil_data, aes(x=pop, y=numbil0))+
  geom_point(color = "black")+
  geom_smooth(method = "lm", se = FALSE)

ggplot(data=bil_data, aes(x=natrent,y=pop))+
  geom_point(color = "black")+
  geom_smooth(method = "lm", se = FALSE)

#Q3

ggplot(data=bil_data, aes(x=gdppc,y=numbil0))+
  geom_point(color = "black")+
  geom_smooth(method = "lm", se = FALSE)


ggplot(data=bil_data, aes(x=roflaw,y=numbil0))+
  geom_point(color = "black")+
  geom_smooth(method = "lm", se = FALSE)

ggplot(data=bil_data, aes(x=roflaw,y=gdppc))+
  geom_point(color = "black")+
  geom_smooth(method = "lm", se = FALSE)

#Q4

bil_data$d2005 <- as.numeric(bil_data$year == 2005, 1,0)
bil_data$d2006 <- as.numeric(bil_data$year == 2006, 1,0)
bil_data$d2007 <- as.numeric(bil_data$year == 2007, 1,0)
bil_data$d2008 <- as.numeric(bil_data$year == 2008, 1,0)
bil_data$d2009 <- as.numeric(bil_data$year == 2009, 1,0)
bil_data$d2010 <- as.numeric(bil_data$year == 2010, 1,0)
bil_data$d2011 <- as.numeric(bil_data$year == 2011, 1,0)
bil_data$d2012 <- as.numeric(bil_data$year == 2012, 1,0)
bil_data$d2013 <- as.numeric(bil_data$year == 2013, 1,0)


reg1 = lm(numbil0~natrent, data=bil_data)
reg2 = lm(numbil0~natrent+pop, data=bil_data)
reg3 = lm(numbil0~natrent+pop+gdppc, data=bil_data)
reg4 = lm(numbil0~natrent+pop+gdppc+roflaw, data=bil_data)
reg5 = lm(numbil0~natrent+pop+gdppc+roflaw+d2006+d2007+d2008+d2009+
            d2010+d2011+d2012+d2013,data=bil_data)

HC1 = sqrt(diag(vcovHC(reg1,"HC1")))
HC2 = sqrt(diag(vcovHC(reg2,"HC1")))
HC3 = sqrt(diag(vcovHC(reg3,"HC1")))
HC4 = sqrt(diag(vcovHC(reg4,"HC1")))
HC5 = sqrt(diag(vcovHC(reg5,"HC1")))

waldtest(reg1,vcov = vcovHC(reg1,"HC1"))
waldtest(reg2,vcov = vcovHC(reg2,"HC1"))
waldtest(reg3,vcov = vcovHC(reg3,"HC1"))
waldtest(reg4,vcov = vcovHC(reg4,"HC1"))
waldtest(reg5,vcov = vcovHC(reg5,"HC1"))


stargazer::stargazer(reg1,reg2,reg3,reg4,reg5,se = list(HC1,HC2,HC3,HC4,HC5),
                      out = "file.html",type="html")

#Q5

reg6 = lm(numbil0~log(pop)+natrent+gdppc+roflaw,data=bil_data)
reg7 = lm(log(numbil0)~pop+natrent+gdppc+roflaw,data=bil_data)
reg8 = lm(log(numbil0)~log(pop)+natrent+gdppc+roflaw,data=bil_data)

HC6 = sqrt(diag(vcovHC(reg6,"HC1")))
HC7 = sqrt(diag(vcovHC(reg7,"HC1")))
HC8 = sqrt(diag(vcovHC(reg8,"HC1")))

waldtest(reg6,vcov = vcovHC(reg6,"HC1"))
waldtest(reg7,vcov = vcovHC(reg7,"HC1"))
waldtest(reg8,vcov = vcovHC(reg8,"HC1"))

stargazer::stargazer(reg6,reg7,reg8,se=list(HC6,HC7,HC8),
                     out = "Q5_file.html",type="html")


#Q6

reg9 =lm(log(numbil0)~log(pop)*d2006+log(pop)*d2007+log(pop)*d2008+log(pop)*d2009
         +log(pop)*d2010+log(pop)*d2011+log(pop)*d2012+log(pop)*d2013+
           natrent+gdppc+roflaw,data = bil_data)
summary(reg9)

linearHypothesis(reg9,c("log(pop):d2006=0","log(pop):d2007=0","log(pop):d2008=0","log(pop):d2009=0",
                        "log(pop):d2010=0","log(pop):d2011=0","log(pop):d2012=0","log(pop):d2013=0"),
                        vcov = vcovHC(reg9, "HC1"))

#Q8
reg10 = lm(log(numbil0)~log(gdppc)*roflaw+log(pop)+natrent, data=bil_data)
summary(reg10)

#Q9
#log(numbil0) = -1.021+0.682*log(gdppc)+1.885*roflaw+0.459*log(pop)+0.023*natrent-0.422*log(gdppc)*roflaw
Ftest=linearHypothesis(reg10,c("log(gdppc)+0.1*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest
Ftest2=linearHypothesis(reg10,c("log(gdppc)+0.2*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest2
Ftest3=linearHypothesis(reg10,c("log(gdppc)+0.3*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest3
Ftest4=linearHypothesis(reg10,c("log(gdppc)+0.4*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest4
Ftest5=linearHypothesis(reg10,c("log(gdppc)+0.5*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest5
Ftest6=linearHypothesis(reg10,c("log(gdppc)+0.6*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest6
Ftest7=linearHypothesis(reg10,c("log(gdppc)+0.7*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest7
Ftest8=linearHypothesis(reg10,c("log(gdppc)+0.8*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest8
Ftest9=linearHypothesis(reg10,c("log(gdppc)+0.9*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest9
Ftest10=linearHypothesis(reg10,c("log(gdppc)+1*log(gdppc):roflaw=0"),vcov = vcovHC(reg10, "HC1"))
Ftest10
Fstat = Ftest[2,3]
