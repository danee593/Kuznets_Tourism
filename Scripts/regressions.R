
# fixed effects model and random effects model countries
require(plm)

fixed_TECK<- plm(CO2_log ~ Urban+Tourism_log+Tur_square+GDP_log+Renewable,data = CAK_logs,index = c("Country","Year"),model = "within")
random_TECK<- plm(CO2_log ~ Urban+Tourism_log+Tur_square+GDP_log+Renewable,data = CAK_logs,index = c("Country","Year"),model = "random")

#hausman test

phtest(fixed_TECK,random_TECK) #one model is inconsistent

#table of regression
require(stargazer)
stargazer(fixed_TECK,random_TECK)

#individual effects

ranef(random_TECK)
fixef(fixed_TECK)

#reg categories only fixed effects model
fixed_High<- CAK_logs_category %>%
  filter(Category=="High_Tourism")%>%
  plm(CO2_log ~ Urban+Tourism_log+Tur_square+GDP_log+Renewable,data = .,index = c("Country","Year"),model = "within")
fixed_Med<- CAK_logs_category %>%
  filter(Category=="Mid_Tourism")%>%
  plm(CO2_log ~ Urban+Tourism_log+Tur_square+GDP_log+Renewable,data = .,index = c("Country","Year"),model = "within")
fixed_Low<- CAK_logs_category %>%
  filter(Category=="Low_Tourism")%>%
  plm(CO2_log ~ Urban+Tourism_log+Tur_square+GDP_log+Renewable,data = .,index = c("Country","Year"),model = "within")
stargazer(fixed_High,fixed_Med,fixed_Low)


