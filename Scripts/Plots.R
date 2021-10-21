co2_renewable_tur_gdp<- CAK_logs %>%
  ggplot(aes(x=Renewable,y=CO2_log,size=Tourism_log,
             color=GDP_log))+
  geom_point(alpha=0.75)+
  theme_bw()
ggsave("Plots/co2_renewable.tiff",co2_renewable_tur_gdp,
       width = 12, height = 8, units = "in", dpi = 300)
