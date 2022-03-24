################ECONOMIC POLICY DATA ANALYSIS################
#TO SAVE CHARTS YOU WILL NEED TO CHANGE THE FILE PATH FOR save_filepath = 

#download packages if needed
list.of.packages <- c('devtools', 'readabs', 'ggplot2', 'readr', 'dplyr', 'tidyr' , 'tidyverse', 'lubridate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load packages
lapply(c('devtools', 'readabs', 'ggplot2', 'readr', 'dplyr', 'tidyr' , 'tidyverse', 'lubridate', 'andytheme'), require, character.only = TRUE)

#install theme package from GitHub
install_github("andybridger/andytheme")

#####CHART 1#####
#DOWNLOAD YEARLY DFAT DATA#
#download csv from Github
urlfile="https://raw.githubusercontent.com/andybridger/uni/main/econpolicy/yearly_export_data.csv"
d_year_trade<-read_csv(url(urlfile))

str(d_year_trade)

#convert from wide to long form data
d_year_trade <- d_year_trade %>% gather(Export, value, 'LNG':'Other exports')

#Data is in A$000 turn into A$mn
d_year_trade <- d_year_trade %>%
  transform(value = value / 1000000)

#Make chart 1
chart1 <- ggplot(d_year_trade, aes(x=Year, y=value, fill = Export)) + 
  geom_bar(position="stack", stat="identity")+
  andy_y_continuous(limits = c(0, 150))+
  andy_style()+
  scale_fill_manual(values=c("#009E73", "#0072B2", "#56B4E9","#cc79a7"))+
  labs(title = "Australian exports to China have increased substantially, largely
because of resource exports",
       subtitle = "Annual Australian merchandise exports (A$bn)",
       caption = "Note: Merchandise exports exclude service exports. LNG = Liquified Natual Gas.
Source: DFAT")+
  theme(legend.position = "right",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 16))

#save chart1
finalise_plot(plot_name = chart1,
              save_filepath = "/Users/andrewbridger/Desktop/R/econpolicy/chart1.png",
              width_pixels = 640,
              height_pixels = 450)

#####CHARTS 2, 3, 4#####
#DOWNLOAD MONTHLY DFAT DATA#
#download csv from Github
urlfile="https://raw.githubusercontent.com/andybridger/uni/main/econpolicy/export_data.csv"
d_trade<-read_csv(url(urlfile))

#convert from wide to long form data
d_trade <- d_trade %>% gather(date, value, 'Jul-2018':'Dec-2021')
#convert date column to date format
d_trade$date <- my(d_trade$date)
d_trade$date
#check this worked
class(d_trade$date)

#convert NAs to 0
d_trade$value[is.na(d_trade$value)] = 0

#create data for the rest of world (excluding China)
d_test <- d_trade %>%
  filter(!Country=="China") %>% 
  group_by(date, `SITC 3 digit`) %>% 
  summarise(value = sum(value))

#add RoW as country and reorder columns to be the same as d_trade
d_test$Country <- NA
d_test$Country <- c("RoW")
d_test <- d_test[, c(4, 2, 1, 3)]

#bind two datasets together
d_trade <- rbind(d_test,d_trade)

#Data is in A$000 turn into A$mn
d_trade <- d_trade %>%
  transform(value = value / 1000)

#####CHART 2#####
d_china_ore <- d_trade %>%
  filter(Country=="China") %>%
  filter(`SITC.3.digit` == "281 Iron ore & concentrates" | `SITC.3.digit` == "281 Iron ores & concentrates") %>%
  filter(value > 0) %>%
  group_by(date)

#make the two iron ores the same
ironore_rename <- c("281 Iron ore & concentrates"="281 Iron ore & concentrates", 
                "281 Iron ores & concentrates"="281 Iron ore & concentrates")
d_china_ore$`SITC.3.digit` <- as.character(ironore_rename[d_china_ore$`SITC.3.digit`])

d_china_other <- d_trade %>%
  filter(Country=="China") %>%
  filter(!`SITC.3.digit` == "281 Iron ore & concentrates" & !`SITC.3.digit` == "281 Iron ores & concentrates") %>%
  group_by(date) %>% 
  summarise(value = sum(value))

#add RoW as country and reorder columns to be the same as d_trade
d_china_other$Country <- NA
d_china_other$Country <- c("China")
d_china_other$`SITC.3.digit` <- NA
d_china_other$`SITC.3.digit` <- c("Other exports")
d_china_other <- d_china_other[, c(3, 4, 1, 2)]

#bind two datasets together
d_china <- rbind(d_china_other,d_china_ore)
d_china <- d_china %>%
  transform(value = value / 1000)

#make chart 2
chart2 <- d_china %>% 
  ggplot(aes(x = date, y = value)) +
  geom_area(aes(fill = `SITC.3.digit`)) +
  stat_summary(fun = sum, geom = "line", size = 1) +
  scale_fill_manual(values=c("#0072B2","#cc79a7"))+
  andy_y_continuous()+
  andy_style()+
  labs(title="Australian exports to China rose and then fell largely due to price
fluctuations of iron ore",
       subtitle="Monthly Australian exports (A$mn) of iron ore and 'other exports' to China",
       caption="Note: Other exports includes all merchandise exports excluding iron ore.
Source: DFAT")+
  geom_label(aes(x=as.Date("2021-01-15"), y=17, label = "Total"),
           size = 4.5,
           lineheight = 1,
           label.size = NA,
           colour = "#000000",
           fill = NA)+
  geom_label(aes(x=as.Date("2021-03-01"), y=7.6, label = "Iron ore"),
             size = 4.5,
             lineheight = 1,
             label.size = NA,
             colour = "#FFFFFF",
             fill = NA)+
  geom_label(aes(x=as.Date("2020-01-01"), y=2.5, label = "Other exports"),
             size = 4.5,
             lineheight = 1,
             label.size = NA,
             colour = "#FFFFFF",
             fill = NA)
chart2

#save chart2
finalise_plot(plot_name = chart2,
              save_filepath = "/Users/andrewbridger/Desktop/R/econpolicy/chart2.png",
              width_pixels = 640,
              height_pixels = 450)


#####CHART 3#####
#change label name to exclude the SITC code
chart_names_1 <- c(
  "043 Barley" = "Barley",
  "263 Cotton" = "Cotton",
  "321 Coal" = "Coal",
  "283 Copper ores & concentrates" = "Copper ores")

#mark chart 3 - success in other markets
chart3 <- d_trade %>% 
  filter(Country %in% c("China", "RoW")) %>% 
  filter(`SITC.3.digit` %in% c("043 Barley", "263 Cotton", "321 Coal", "283 Copper ores & concentrates")) %>%
  ggplot(aes(x = date, y = value, group= Country, color = Country))+
  geom_line() +
  andy_y_continuous()+
  scale_colour_manual(values = c("#E69F00","#0072B2"))+
  facet_wrap(~fct_relevel(`SITC.3.digit`,"043 Barley", "263 Cotton", "321 Coal", "283 Copper ores & concentrates"), ncol = 2, scales = "free", labeller = as_labeller(chart_names_1)) +
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="043 Barley"), aes(xintercept=as.Date("2020-05-01")), colour="black", linetype="dashed") + 
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="263 Cotton"), aes(xintercept=as.Date("2020-10-01")), colour="black", linetype="dashed") +
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="321 Coal"), aes(xintercept=as.Date("2020-10-01")), colour="black", linetype="dashed") + 
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="283 Copper ores & concentrates"), aes(xintercept=as.Date("2020-11-01")), colour="black", linetype="dashed") +
  andy_style()+
  theme(strip.background =element_rect(fill="#0072B2"),
        strip.text = element_text(colour = 'white'),
        strip.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))+
  theme(legend.position = "right",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.size = unit(3, 'lines'))+
  labs(x="",y="",
       title="Australian barley, cotton, coal and copper ore exports have largely
found other markets",
       subtitle="Australian exports (A$mn) to China and Rest of World (RoW)",
       caption="Note: Dashed line indicates month trade restrictions were first reported.
Source: DFAT")

chart3

#save chart3
finalise_plot(plot_name = chart3,
              save_filepath = "/Users/andrewbridger/Desktop/R/econpolicy/chart3.png",
              width_pixels = 640,
              height_pixels = 450)

#####CHART 4#####
#change label name to exclude the SITC code
chart_names_2 <- c(
  "011 Beef, f.c.f." = "Beef",
  "112 Alcoholic beverages" = "Wine",
  "247 Wood, rough" = "Timber",
  "036 Crustaceans, f.c.f." = "Lobster")

#mark chart 4 - little success in other markets
chart4 <- d_trade %>% 
  filter(Country %in% c("China", "RoW")) %>% 
  filter(`SITC.3.digit` %in% c("011 Beef, f.c.f.", "112 Alcoholic beverages","247 Wood, rough", "036 Crustaceans, f.c.f.")) %>%
  ggplot(aes(x = date, y = value, group= Country, color = Country))+
  geom_line() +
  andy_y_continuous()+
  scale_colour_manual(values = c("#E69F00","#0072B2"))+
  facet_wrap(~fct_relevel(`SITC.3.digit`,"011 Beef, f.c.f.", "112 Alcoholic beverages","247 Wood, rough", "036 Crustaceans, f.c.f."), ncol = 2, scales = "free", labeller = as_labeller(chart_names_2)) +
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="011 Beef, f.c.f."), aes(xintercept=as.Date("2020-05-01")), colour="black", linetype="dashed") + 
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="112 Alcoholic beverages"), aes(xintercept=as.Date("2020-11-01")), colour="black", linetype="dashed") +
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="247 Wood, rough"), aes(xintercept=as.Date("2020-10-01")), colour="black", linetype="dashed") + 
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="036 Crustaceans, f.c.f."), aes(xintercept=as.Date("2020-11-01")), colour="black", linetype="dashed") +
  andy_style()+
  theme(strip.background =element_rect(fill="#0072B2"),
        strip.text = element_text(colour = 'white'),
        strip.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))+
  theme(legend.position = "right",
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.size = unit(3, 'lines'))+
  labs(x="",y="",
       title="Australian beef, wine, timber and lobster exports have had more
difficulty finding other markets",
       subtitle="Australian exports (A$mn) to China and Rest of World (RoW)",
       caption="Note: Dashed line indicates month trade restrictions were first reported.
Source: DFAT")

chart4

#save chart4
finalise_plot(plot_name = chart4,
              save_filepath = "/Users/andrewbridger/Desktop/R/econpolicy/chart4.png",
              width_pixels = 640,
              height_pixels = 450)

#####CHART 5#####
#change label name to exclude the SITC code
chart_names_3 <- c(
  "036 Crustaceans, f.c.f." = "Lobster",
  "112 Alcoholic beverages" = "Wine"
  )

chart5 <- d_trade %>% 
  filter(Country %in% c("Hong Kong (SAR of China)")) %>% 
  filter(`SITC.3.digit` %in% c("036 Crustaceans, f.c.f.", "112 Alcoholic beverages")) %>%
  ggplot(aes(x = date, y = value, group= Country, color = Country))+
  geom_line() +
  andy_y_continuous()+
  scale_colour_manual(values = c("#0072B2"))+
  facet_wrap(~fct_relevel(`SITC.3.digit`,"036 Crustaceans, f.c.f.", "112 Alcoholic beverages"), ncol = 2, scales = "free", labeller = as_labeller(chart_names_3)) +
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="036 Crustaceans, f.c.f."), aes(xintercept=as.Date("2020-11-01")), colour="black", linetype="dashed") + 
  geom_vline(data=filter(d_trade, `SITC.3.digit`=="112 Alcoholic beverages"), aes(xintercept=as.Date("2020-11-01")), colour="black", linetype="dashed") +
  andy_style()+
  theme(strip.background =element_rect(fill="#0072B2"),
        strip.text = element_text(colour = 'white'),
        strip.text.x = element_text(angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        plot.margin = margin(0.2,1,0.2,0.2, "cm"))+
  labs(x="",y="",
       title='Some Chinese demand for Australian wine and lobster is being met by
"grey-trade channels" through Hong Kong',
       subtitle="Australian exports (A$mn) to Hong Kong",
       caption="Note: Dashed line indicates month trade restrictions were first reported.
Source: DFAT")

chart5

#save chart5
finalise_plot(plot_name = chart5,
              save_filepath = "/Users/andrewbridger/Desktop/R/econpolicy/chart5.png",
              width_pixels = 640,
              height_pixels = 450)

