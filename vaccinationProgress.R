#!/usr/bin/Rscript

# Check recency of last update with minimal package load ------------------------------------------------------------

library("RMySQL")
library("readr")
library("dplyr")
library("lubridate")

# dbDisconnect(con)
tryCatch({
  con <- dbConnect(RMySQL::MySQL(), 
                   dbname = "vac_db", 
                   host = "example_host.de",
                   # host = "localhost", 
                   port = 3306,
                   user = "vac_user",
                   password = "password")
  dbGetQuery(con,'SET NAMES utf8mb4')
}, error = function(e) {
  message(e)
}, warning = function(w) {
  message(w)
})

# Read latest data from impfdashboard.de.
vacDelivery <-  read_delim("https://impfdashboard.de/static/data/germany_deliveries_timeseries_v2.tsv", delim = "\t")
vacData <-  read_delim("https://impfdashboard.de/static/data/germany_vaccinations_timeseries_v2.tsv", delim = "\t")

# Load last state of already used data and check if 
# the lates vaccination data is newer.
# Stop script if no new update to data is available.
tryCatch({old_data <- dbGetQuery(con, "SELECT * FROM vac_table")})

if (last(vacData$date) <= last(old_data$date)) {
  dbDisconnect(con)
  stop("The time is ", now(), "\n",
    "Last date from source: ", last(vacData$date), "\n", "Last date from database: ", last(old_data$date))
}


# Start posting -----------------------------------------------------------

setwd("/path/to/vaccinationBot/")

library("rtweet")
library("tidyverse")
library("scales")
library("dbx")

# Create Twitter token
consumer_key1=''
consumer_secret1=''
access_token1=''
access_secret1=''

token <- create_token(
  app = "VacBot",
  consumer_key1,
  consumer_secret1,
  access_token1,
  access_secret1)#

#Color values

#071E22
#1D7874
#679289
#F4C095
#EE2E31

# Reverse order
colVec <- c("#F4C095", "#679289","#1D7874","#071E22") 


# Prepare vaccination data for plotting
# Returns a data frame with vaccinations per date and vaccine
vacDataByImpfstoff <- 
  vacData %>% 
  select(date, dosen_biontech_kumulativ, dosen_moderna_kumulativ, dosen_astra_kumulativ, dosen_johnson_kumulativ) %>% 
  rename(comirnaty = dosen_biontech_kumulativ, 
         moderna = dosen_moderna_kumulativ, 
         astra = dosen_astra_kumulativ, 
         johnson = dosen_johnson_kumulativ) %>% 
  gather(impfstoff, geimpfte_dosen, -date) %>% 
  group_by(impfstoff) %>% 
  mutate(tages_dosen = if_else(is.na(geimpfte_dosen - lag(geimpfte_dosen)), geimpfte_dosen, geimpfte_dosen - lag(geimpfte_dosen)), 
         sieben_tages_schnitt = zoo::rollmean(tages_dosen, 7, 0, align = "right"))

# Prepare vaccination delivery data for plotting
vacDeliveryTotal <- 
  vacDelivery %>% 
  group_by(date, impfstoff) %>% 
  summarise(dosen = sum(dosen, na.rm = T)) %>% 
  mutate(source = "delivery")

# Check of if there are delivery forecasts for the current date
# If yes, append forecasts to delivery data
# Can be omitted if scheduled deliviries are not important

#delivery_forecast <- dbGetQuery(con, "SELECT * FROM impfstoff_forecast")

# delivery_forecast <- 
#   delivery_forecast %>% 
#   mutate(date = as.Date(delivery_forecast$date)) %>% 
#   filter(date > max(vacDeliveryTotal$date), date <= today()-1) 

# if (nrow(delivery_forecast)) {
#   vacDeliveryTotal <- 
#   vacDeliveryTotal %>% 
#     full_join(delivery_forecast %>% rename(dosen = forecast) %>% mutate(source = "forecast"))
# }

# Join vaccination and delivery data and set labels for plotting
vacDeliveryOutput <-
vacDeliveryTotal%>% 
  full_join(vacDataByImpfstoff) %>% 
  arrange(date) %>% 
  group_by(impfstoff) %>% 
  mutate(dosen = if_else(is.na(dosen), 0, dosen), 
         cumDosen = cumsum(dosen), 
         vorrat = if_else(is.na(geimpfte_dosen), cumDosen, if_else((cumDosen - geimpfte_dosen)<0, 0, cumDosen - geimpfte_dosen)), 
         source = if_else(date > min(vacDeliveryTotal$date[vacDeliveryTotal$source == "forecast"]), "Prognose", source),
         source = if_else((is.na(source)) | source =="delivery", "Lieferung", "Lieferung (geplant)"), 
         impfstoff = if_else(impfstoff == "comirnaty", "Comirnaty", if_else(impfstoff == "astra", "Astra", if_else(impfstoff == "moderna", "Moderna", "Johnson&Johnson"))))

# Plot data by vaccine
vacPlot1 <-
vacDeliveryOutput %>% 
  filter(date > today()-7*12) %>% 
  ggplot(aes(x=date))+
  geom_col(aes(y = vorrat, fill=impfstoff, alpha=source), alpha=0.5)#

# Plot overall data
temp_data <-
  vacDeliveryOutput %>% 
  group_by(date) %>% 
  summarise(tages_dosen = sum(tages_dosen, na.rm = T)) %>% 
  mutate(sieben_tages_schnitt = zoo::rollmean(x = tages_dosen, k=7,fill= 0, align="right")) %>% 
  filter(date > today()-7*12)

vacPlot1 +
  geom_line(data = temp_data, aes(y = tages_dosen*30, color="Tagesimpfungen"), size=1.5, alpha=1) + 
  geom_line(data = temp_data, aes(y = sieben_tages_schnitt*30, color="Sieben-Tages-Schnitt"), size=1, alpha=1, linetype="dashed") + 
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d. %b") +
  scale_color_manual(name = "Tagesimpfungen", values = c("Tagesimpfungen" = "#679289", "Sieben-Tages-Schnitt" = "#071E22") ) +
  scale_fill_manual(values = colVec) + #, 
  scale_y_continuous(sec.axis = sec_axis(~ . / 30,labels = unit_format(unit = "Mio", scale = 1e-6, decimal.mark = ","), name = "Impfungen / Tag"), 
                     labels = unit_format(unit = "Mio", scale = 1e-6, decimal.mark = ",")) +
  guides(fill=guide_legend(title="Impfstoffvorrat"), 
         color=guide_legend(title="Impfstoffverbrauch"),
         alpha="none") +
  theme_classic() +
  labs(x = "Datum", 
       y = "Impfstoffvorrat", 
       title = "Impflieferungen und laufender Verbrauch", 
       subtitle = paste("Stand:", last(vacDataByImpfstoff$date)), 
       caption = paste("Lieferungen werden wöchentlich aktualisiert. Letzte dokumentierte Lieferung: ", last(vacDelivery$date), "\nGeplante Lieferung basieren auf den Angaben des BMG (https://t1p.de/xi2u)")) +
  coord_cartesian(expand = 0, clip = "off") +
  theme(text = element_text(color = "#274E7D", size = 14), 
        legend.position = c(0.15,0.7), 
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10),
        axis.line = element_line(color = "#274E7D"), 
        axis.text = element_text(color = "#274E7D"), 
        axis.ticks = element_line(color = "#274E7D"), 
        axis.text.y.left = element_text( margin=margin(10,10,10,10, unit="pt")),
        axis.text.y.right = element_text( margin=margin(10,10,10,10, unit="pt")), 
        axis.text.x.bottom = element_text( margin=margin(10,10,10,10, unit="pt")), 
        plot.caption = element_text(margin = margin(10,10,10,10, unit = "pt"), size = 10),
        plot.title.position = "plot", 
        plot.caption.position = "plot",
        plot.title = element_text(margin = margin(10,10,5,10, unit = "pt"), hjust = 0.5),
        plot.subtitle = element_text(margin = margin(0,10,10,10, unit = "pt"), hjust = 0.5)) #+
  
if ("Lieferung (geplant)" %in% vacDeliveryOutput$source) {
  vacPlot <- vacPlot + 
    guides(alpha=guide_legend(title = "Datenquelle"))
}

ggsave("vaccinationDelivery.png", dpi = "retina", width = 10, height = 7)

vacDeliveryOutput %>% 
  {
    tryCatch(
      dbWriteTable(., conn = con, name = "vac_table", overwrite = TRUE, row.names=F),
      message(paste("Successfully stored data for in SQL database.")),
      error = function(e) message(e))
  }

tweetText <-
  paste0("💉-Update für den ", format.Date(last(vacData$date), "%d. %B."), 
        "\n\n",
        "Impfstatistiken:\n",
        "⏩ Impfungen: ", format(sum(vacData$dosen_erst_differenz_zum_vortag[length(vacData$dosen_erst_differenz_zum_vortag)] + vacData$dosen_zweit_differenz_zum_vortag[length(vacData$dosen_zweit_differenz_zum_vortag)]), big.mark = "."), "\n",
        "⏩ Erst: ", format(vacData$dosen_erst_differenz_zum_vortag[length(vacData$dosen_erst_differenz_zum_vortag)], big.mark = "."), "\n",
        "⏩ Zweit: ", format(vacData$dosen_zweit_differenz_zum_vortag[length(vacData$dosen_zweit_differenz_zum_vortag)], big.mark = "."),
        "\n\n",
        "Impfstoffvorräte (ca.):\n",
        "⏩ Astra: ",  format(last(vacDeliveryOutput$vorrat[vacDeliveryOutput$impfstoff=="Astra"])/1000000, decimal.mark = ",", digits = 3), " Mio.\n",
        "⏩ Comirnaty: ", format(last(vacDeliveryOutput$vorrat[vacDeliveryOutput$impfstoff=="Comirnaty"])/1000000, decimal.mark = ",", digits = 3), " Mio.\n",
        "⏩ Moderna: ", format(last(vacDeliveryOutput$vorrat[vacDeliveryOutput$impfstoff=="Moderna"])/1000000, decimal.mark = ",", digits = 3), " Mio.\n",
        "⏩ Johnson: ", format(last(vacDeliveryOutput$vorrat[vacDeliveryOutput$impfstoff=="Johnson&Johnson"])/1000000, decimal.mark = ",", digits = 3), " Mio.\n",
        "💯 Gesamtbestand: ", format(sum(vacDeliveryOutput$vorrat[vacDeliveryOutput$date == today()-1], na.rm = T)/1000000, decimal.mark = ",", digits = 3), " Mio.\n\n",
        "#impfen #impfstoff #rstats"
  )

tryCatch({
  post_tweet(tweetText, media = "vaccinationDelivery.png", token)
  lastTweet <- get_my_timeline(n=1)
}, error = function(e) {
  message(e)
})

dbDisconnect(con)

