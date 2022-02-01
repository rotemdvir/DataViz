# Plastics data 
# 2021 project (#TidyTuesday)

library(tidyverse)
library(MASS)
library(stringr)
library(viridis)
library(patchwork)
library(scales)

# Data uplink
plastics <- read.csv("~//Data/plastics.csv")

# Fig, 1: total amount by countries and continents
## Focus on countries with large amount of waste reported
d1<- plastics %>%
  group_by(country) %>%
  dplyr::select(parent_company, year, country, grand_total, num_events) %>%
  filter(grand_total > 500) %>%
  arrange(country, year)

d2 <- d1 %>%
  group_by(country) %>%
  summarise(total = sum(grand_total)) %>%
  filter(total > 10000) %>%
  arrange(country,total)

## Editing some of the names in the data
d2 <- d2 %>%
  mutate(total = ifelse(country == "Nigeria", total+total[country == "NIGERIA"], total))

d2 <- d2[!grepl("EMPTY",d2$country),]
d2 <- d2[!grepl("NIGERIA",d2$country),]
d2$country <- str_replace(d2$country, "United States of America", "USA")
d2$country <- str_replace(d2$country, "ECUADOR", "Ecuador")

## Join data on continents
d2 <- d2 %>%
  left_join(continents, by = "country")

d2 <- d2 %>%
  mutate(continent = ifelse(country == "Taiwan_ Republic of China (ROC)", "Asia", continent))

## Plot using ggplot
p1 <- d2 %>%
  ggplot(aes(reorder(country, +total),total)) +
  geom_bar(aes(fill = continent), stat = "identity") +
  geom_text(aes(label = total), hjust = -0.1, color = "black") +
  xlab("") + ylab("Total plastics collected") + ggtitle("Plastic clean-up by country and continent") +
  scale_x_discrete(labels = c("Taiwan_ Republic of China (ROC)" = "Taiwan")) +
  scale_fill_viridis(discrete = T) +
  theme_bw() + coord_flip() + theme(legend.position = "bottom",
                                    legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
                                    plot.title = element_text(color = "black", size = 14, face = "bold"))

# Figure 2: Stacked barplot for plastic types, top 6 firms
## Create sums of all types
d3 <- plastics %>%
  group_by(parent_company) %>%
  summarise(total = sum(grand_total, na.rm = T),
            hdpe_t = sum(hdpe, na.rm = T),
            ldpe_t = sum(ldpe, na.rm = T),
            pet_t = sum(pet, na.rm = T),
            pp_t = sum(pp, na.rm = T),
            ps_t = sum(ps, na.rm = T),
            pvc_t = sum(pvc, na.rm = T)) %>%
    mutate(other = total - (hdpe_t+ldpe_t+pet_t+ps_t+pp_t+pvc_t)) %>%
  arrange(-total)

## Filter 6 largest corporation with high waste numbers in data
d3a <- d3 %>%
  filter(parent_company == "Nestle" | parent_company == "Unilever" | parent_company == "Universal Robina Corporation" |
           parent_company == "Colgate-Palmolive" | parent_company == "The Coca-Cola Company" | parent_company == "Pepsico") %>%
  gather(type, amount, hdpe_t:other, -parent_company, -total) %>% arrange(parent_company)

## Classify type (soft drinks and consumer products)
d3a$drinks <- "no"
d3a$drinks[d3a$parent_company == "The Coca-Cola Company"] <- "yes"
d3a$drinks[d3a$parent_company == "Pepsico"] <- "yes"

## Labels for plot
f_lab <- c("Consumer Goods Giants","Soft Drink Giants")
names(f_lab) <- c("no","yes")

## Plot using ggplot
p2 <- ggplot(d3a, aes(x = parent_company, y = amount, fill = factor(type), group = drinks)) +
  geom_bar(position = "fill", stat = "identity") +
  xlab("") + ylab("Proportion of total plastic waste") + facet_grid(~drinks, labeller = labeller(drinks = f_lab)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_discrete(name = "Plastic Type",
                      labels = c("hdpe_t" = "High Density polyethylene", "ldpe_t" = "Low Density polyethylene", "other" = "Other",
                                  "pet_t" = "Polyester","pp_t" = "Polypropylene","ps_t" = "Polystyrene","pvc_t" = "PVC")) +
  scale_x_discrete(limits = c("Universal Robina Corporation","Unilever","Nestle","Colgate-Palmolive",
                              "The Coca-Cola Company","Pepsico")) +
  theme_bw() + coord_flip() + ggtitle("Plastic waste types: The (polluter) leader board") +
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.75, linetype = "solid", colour = "black"),
        plot.title = element_text(color = "black", size = 14, face = "bold"))
  

# Create viz by joining both plots with patchwork package
p <- p1 / p2
p + plot_annotation(title = "'Break Free from Plastic' cleaning campaigns (2019-2020)",
                    caption = "More info: breakfreefromplastic.org or #breakfreefromplastic",
                    theme = theme(plot.title = element_text(color = "blue", size = 20, face = "bold", hjust = 0.5)))

