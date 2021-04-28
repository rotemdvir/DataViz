## Public agencies fuel consumption
### April 2021

library(psych)  
library(foreign)
library(gplots)
library(MASS)
library(Hmisc)
library(ggthemes)
library(devtools)
library(gridExtra)
library(ggpubr)
library(tidyverse)
library(patchwork)

# Upload Data
library(readxl)
MyData <- read_excel("~/Dropbox/TAMU/New_Projects/Git_edits/DataViz/DataEdit.xlsx")

## Create cities data file
city.data <- MyData %>% 
  group_by(State,City,Population) %>%
  summarise(total_diesel = sum(Used_Diesel),
            total_gas = sum(Used_Gasoline),
            total_lpg = sum(Used_LPG),
            total_cng = sum(Used_CompNatGas),
            total_biodiesel = sum(Used_BioDiesel),
            total_other = sum(Used_Other))

describe(city.data$Population)

# 75-90th percentile of population size
## Dataset for identifying total consumption 
x <- city.data %>%
  filter(Population > 5000000) %>%
  mutate(all = total_diesel + total_gas + total_lpg + total_cng + total_biodiesel + total_other) %>%
  arrange(-all)

# Dataset for plotting top-10 in consumption 
## Top-10 for largest
city.data2 <- x %>%
  filter(all > 7000000) %>%
  relocate(City) %>%
  unite("CityState", City:State, sep = "-", remove = F)

# Prep plot
cols <- c("total_diesel" = "red", "total_gas" = "orange", "total_lpg" = "blue",
          "total_cng" = "yellow", "total_biodiesel" = "green", "total_other" = "grey")

# Reshape data to long format and plot stacked barplot
city.data3 <- city.data2 %>%
  gather(total, consump, total_diesel:total_other) %>%
  arrange(CityState)

# Stacked barplot
p_large <- ggplot(city.data3, aes(x=CityState, y=consump, fill = total)) +
  geom_col(width = 0.7) + xlab("") + ylab("") + 
  labs(title = "Large Cities (population more than 5 million)")  +
  scale_y_continuous(breaks = c(0, 20000000, 40000000, 60000000),
                     labels = c(0, "2000k", "4000k", "6000k")) + coord_flip() +
  theme_minimal()

p_large <- p_large + theme(legend.position = "top",
                           legend.title = element_text(),
                           legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
                   #        axis.text.x = element_text(vjust = 1),
                           axis.text.y = element_text(hjust = 0),
                            plot.title = element_text(color = "black", size = 10)) +
  scale_fill_manual(values = cols,
                    name = "Energy Products",
                    breaks = c("total_diesel", "total_gas", "total_lpg", "total_cng",
                               "total_biodiesel", "total_other"),
                    labels = c("Diesel", "Gasoline", "LPG", "Compressed Natural Gas", "Biodiesel", "Other"))


# 25th percentile of population size
## Dataset for identifying total consumption 
y <- city.data %>%
  filter(Population < 200000) %>%
  mutate(all = total_diesel + total_gas + total_lpg + total_cng + total_biodiesel + total_other) %>%
  arrange(-all)

## Top-10 for smallest in population
city.data2a <- y %>%
  filter(all > 700000) %>%
  relocate(City) %>%
  unite("CityState", City:State, sep = "-", remove = F)

# Reshape data to long format and plot stacked barplot
## plot for smaller cities (more diverse energy)
city.data3a <- city.data2a %>%
  gather(total, consump, total_diesel:total_other) %>%
  arrange(City)

p_small <- ggplot(city.data3a, aes(x=CityState, y=consump, fill = total)) +
  geom_col(width = 0.7) + xlab("") + ylab("Energy Consumption (in thousands)") + 
  labs(title = "Small Cities (population less than 200,000)") +
  scale_y_continuous(breaks = c(0, 500000, 1000000, 1500000),
                     labels = c(0, "500k", "1000k", "1500k")) + coord_flip() +
  theme_minimal()

p_small <- p_small + theme(legend.position = "top",
                           legend.title = element_text(),
                           legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
                   #        axis.text.x = element_text(vjust = 2.5, size = 7),
                           axis.text.y = element_text(hjust = 0),
                            plot.title = element_text(color = "black", size = 10)) +
  scale_fill_manual(values = cols,
                    name = "Energy Products",
                    breaks = c("total_diesel", "total_gas", "total_lpg", "total_cng",
                               "total_biodiesel", "total_other"),
                    labels = c("Diesel", "Gasoline", "LPG", "Compressed Natural Gas", "Biodiesel", "Other"))

## Combine plots (large and small population)
p_both <- ggarrange(p_large, p_small, nrow = 2, ncol = 1, common.legend = T, legend = "bottom")
p_both <- annotate_figure(p_both,
                          top = text_grob("Public transportation energy consumption (2019)",
                                          color = "black",
                                          face = "bold",
                                          size = 14))

###### Pie Charts for state data ########

## Upload state data
DataState <- read_excel("~/Dropbox/TAMU/New_Projects/Git_edits/DataViz/DataStateTotals.xlsx")

pie.data <- DataState %>%
  dplyr::select(State,Diesel,Gasoline,LPG,CNG,Biodiesel) %>%
  mutate(energy_all = Diesel+Gasoline+LPG+CNG+Biodiesel) %>%
  arrange(-energy_all)

state.longform <- pie.data %>%
  gather(fuel_type, quantity, Diesel:Biodiesel) %>%
  mutate(prop = round(quantity/energy_all,2)) %>%
#  arrange(-energy_all)
   arrange(State)

cols.chart <- c("green", "yellow", "red", "orange", "blue")

st.chart.data <- state.longform %>%
#  filter(State == "California") %>%
#  filter(State == "New York") %>%
#  filter(State == "Illinois") %>%
#  filter(State == "Texas") %>%
#  filter(State == "New Jersey") %>%
#  filter(State == "Washington") %>%
  mutate(prop2 = round(prop*100,1))

### Plot pie chart for each state
## edit labs(title) by state
ch.nj <- ggplot(st.chart.data, aes(x = "", y = prop, fill = factor(fuel_type))) +
  geom_bar(width = 2, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = ifelse(prop2 > 5, paste(prop2, "%"), "")), position = position_stack(vjust = 0.5)) +
  theme_classic()

ch.ny <- ch.ny + theme(legend.position = "bottom",
                 legend.title = element_text(),
                 legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank()) +
  scale_fill_manual(values = cols.chart,
                    name = "Energy Product") +
  labs(title = "New York",
       x = NULL,
       y = NULL)
  
## Combine all six states plots
ch.all <- ggarrange(ch.cal, ch.ny, ch.il, ch.tx, ch.nj, ch.wash,
                nrow = 2, ncol = 3, common.legend = T, legend = "none")
ch.all <- annotate_figure(ch.all,
                top = text_grob("Energy consumption data:\n Top-6 States",
                                color = "black",
                            #    face = "bold",
                                size = 14),
                bottom = text_grob("Plots do not display proportions smaller than 5% \n
                                   Data source: National Transit Database", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 8))


# Combine to full visualization
## Create viz title
pl.t <- text_grob("US Public Transportation System Energy Consumption (2019)",
                  color = "black",
                  face = "bold",
                  size = 18)
plot_0 <- as_ggplot(pl.t) + theme(plot.margin = margin(0,0,2,0, "cm"))


viz <- ggarrange(plot_0, NULL, p_both, ch.all,
                 nrow = 2, ncol = 2,heights = c(1,5))
viz

