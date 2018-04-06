##Les Stanaland
##Natural Resources paper
##3.22.18

#read in the data
data <- read.csv("https://raw.githubusercontent.com/lss0612/Natural-Resources/master/fulldata.csv", header = TRUE)
View(data)

#preload script
preload<-function(x)
{
  x <- as.character(x)
  if (!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x,  repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

#panel linear model
preload("plm")

#panel data linear model, fixed effects

m1 <- plm(LRGDP~H1+H2+H3, data=data, model="within")
summary(m1)

m2 <- plm(LRGDP~H1+H2+H3+PolityIV, data=data, model="within")
summary(m2)

m3 <- plm(LRGDP~H1+H2+H3+PolityIV+ResourceCursed, data=data, model="within")
summary(m3)

m4 <- plm(LRGDP~H1+H2+H3+PolityIV+ResourceCursed+population, data=data, model="within")
summary(m4)

#output
library(stargazer)
stargazer(m1, m2, m3, m4, title="Regression Results - Fixed Effects")

#Dickey-Fuller test for ARMA
preload("tseries")
adf.test(data$H1) #result - stationary p=0.01
adf.test(data$H2) #result - root p=0.0869
adf.test(data$H3) #result - stationary p=0.01

#panel data linear model, fixed effects with IVs lagged
library(zoo)
H2lag <- lag(data$H2, k = -1)

m5 <- plm(LRGDP~H1+H2lag+H3+PolityIV, data=data, model="within")
summary(m5)

m6 <- plm(LRGDP~H1+H2lag+H3+PolityIV+ResourceCursed, data=data, model="within")
summary(m6)

m7 <- plm(LRGDP~H1+H2lag+H3+PolityIV+ResourceCursed+population, data=data, model="within")
summary(m7)

#output
library(stargazer)
stargazer(m5, m6, m7, title="Regression Results - Fixed Effects with 1 Year Lag for Hypothesis 2")

#robustness checks
#interaction between rent-seeking and resource cursed
RSRC <- data$H1 * data$ResourceCursed
m7 <- plm(LRGDP~H1+H2+H3+PolityIV+ResourceCursed+population+RSRC, data=data, model="within")
summary(m7)
stargazer(m7, title="Interaction between Rent-seeking and Resource Curse")

#alternate operationalisation of H1
oilprofits <- data$`oil profit`
m8 <- plm(LRGDP~oilprofits+H2lag+H3+PolityIV+ResourceCursed+population, data=data, model="within")
summary(m8)
stargazer(m8, title="Oil Rents as Alternate Predictor of Rent-seeking")
oilrents <- data$`oil rent`
m9 <- plm(LRGDP~oilrents+H2lag+H3+PolityIV+ResourceCursed+population, data=data, model="within")
summary(m9)


#attempts at visualisations
#pointplot
dev.off
g <- ggplot(data, aes(x = H1, y = LRGDP, group=country))
g + geom_point(varwidth=T, fill="plum") + 
  labs(title="Rent-seeking by Country",
       caption="Source: World Bank",
       x="Rent Seeking",
       y="Real per capita GDP")

levels(data$country) <- gsub("", "\n", levels(data$country))
g <- ggplot(data, aes(country, H1))
g + geom_point(aes(fill=factor(country))) + 
  labs(title="Happy Planet Index by Region, 2016",
       subtitle= "",
       caption="Source: HPI",
       x="HPI",
       y="Region")


