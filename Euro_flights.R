# Data Viz project: July 2022
# Data from: https://ansperformance.eu/data/

library(tidyverse)
library(readxl)
library(scales)
library(viridis)
library(patchwork)

Airport_Traffic <- read_excel("R_Proj/Data/Airport_Traffic.xlsx")

# Data manage
traffic$MONTH_NUM <- as.numeric(traffic$MONTH_NUM)
traffic$YEAR <- as.numeric(traffic$YEAR)

# Sum traffic by year and country
c_dat <- traffic %>% 
  filter(YEAR > 2017) %>% 
  group_by(YEAR,STATE_NAME) %>% 
  summarise(dept = sum(FLT_DEP_1),
            arrv = sum(FLT_ARR_1)) %>% 
  arrange(STATE_NAME,YEAR)

# Calculate mean traffic values to identify top-10 countries
p1 <- c_dat %>% 
  group_by(STATE_NAME) %>% 
  summarise(dept_m = mean(dept),
            arrv_m = mean(arrv)) %>% 
  arrange(dept_m)

# Top-10 countries
p2 <- c_dat %>% 
  filter(STATE_NAME == "France" | STATE_NAME == "Germany" | STATE_NAME == "Italy" | STATE_NAME == "Türkiye" | STATE_NAME == "Portugal" |
           STATE_NAME == "Norway" | STATE_NAME == "Spain" | STATE_NAME == "United Kingdom" | STATE_NAME == "Switzerland" | STATE_NAME == "Netherlands") 

## Plot time trends using stacked area plot
pl1 <- ggplot(p2, aes(x=YEAR, y= dept, fill = STATE_NAME)) +
  geom_area(color = "white") + xlab("") + ylab("Total Departures") + 
  ggtitle("Top-10 flight origins (2018-2022)") + 
  labs(fill = "Country",
       caption = "Data from Eurocontrol (https://ansperformance.eu/data/), \n 2022 Data covers January-May") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "grey") +
  geom_text(aes(x=2021, y=5000000, label = "Guess what \n happened here?"), hjust = 1.5, size = 4) +
  geom_curve(x=2021,y=4500000,xend=2020,yend=3000000,color="red",arrow = arrow()) +
  scale_y_continuous(labels = comma) +
  theme_pubr() + theme(legend.background = element_rect(size = 1, linetype = "solid", colour = "black"),
                       plot.title = element_text(size = 12, face = "bold"))


## Plot number of arrivals/departures using heatmaps
# Case 1: top-10 - UK
p3 <- traffic %>% 
  filter(STATE_NAME == "United Kingdom") %>% 
  filter(APT_NAME != "Newcastle") %>% 
  filter(YEAR != 2022) %>% 
  select(YEAR,MONTH_NUM,APT_NAME, FLT_DEP_1,FLT_ARR_1) %>% 
  group_by(YEAR,APT_NAME) %>% 
  summarise(dp = sum(FLT_DEP_1),
            ar = sum(FLT_ARR_1)) %>% 
  gather(typ,t,dp:ar,-YEAR,-APT_NAME) %>% 
  arrange(APT_NAME)

pl2 <- ggplot(p3, aes(x=typ,y=reorder(APT_NAME, +t), fill=t, group = factor(YEAR))) +
  geom_tile(color = "black") +
  geom_text(aes(label=t),color = "black", size=3) +
  scale_fill_gradientn(colours = rainbow(8)) + ylab("") + xlab("") +
  scale_x_discrete(labels = c("ar" = "Arrv.", "dp" = "Dept.")) +
  facet_grid(.~YEAR) + ggtitle("Airports traffic: United Kingdom") +
  theme_classic() + theme(legend.position = "none")

# Case 2: non-top 10: Poland
p4 <- traffic %>% 
  filter(STATE_NAME == "Poland") %>% 
  filter(APT_NAME != "Radom") %>% 
  filter(YEAR != 2022) %>% 
  select(YEAR,MONTH_NUM,APT_NAME, FLT_DEP_1,FLT_ARR_1) %>% 
  group_by(YEAR,APT_NAME) %>% 
  summarise(dp = sum(FLT_DEP_1),
            ar = sum(FLT_ARR_1)) %>% 
  gather(typ,t,dp:ar,-YEAR,-APT_NAME) %>% 
  arrange(APT_NAME)

pl3 <- ggplot(p4, aes(x=typ,y=reorder(APT_NAME, +t), fill=t, group = factor(YEAR))) +
  geom_tile(color = "black") +
  geom_text(aes(label=t),color = "white", size=3) +
  scale_fill_viridis_c() + ylab("") + xlab("") + ggtitle("Airports traffic: Poland") +
  scale_x_discrete(labels = c("ar" = "Arrv.", "dp" = "Dept.")) +
  facet_grid(.~YEAR) +
  theme_classic() + theme(legend.position = "none")

# Combine plots
pl <- pl1 / (pl2 | pl3)
pl + plot_annotation(title = "Flights across Europe: Time trends and more (2018-2022)",
                     theme = theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"))) 

# Save with high-end resolution
ggsave("~/Euro_fly.jpeg", width = 20, height = 10, units = "in", dpi = 300)


