

library(tidyverse)
library(readxl)
library(hrbrthemes)
library(patchwork)
library(nflplotR)


pats <- read_excel("~/Documents/R_Proj/Data/Pats_OLine.xlsx", sheet = "Sheet3")

pats <- pats %>% 
  mutate(p_rate = pressures/plays,
         ht_rate = hits/plays,
         tm = "NE")

p1 <- ggplot(pats, aes(x=factor(week), y=p_rate, group = outcome)) +
  geom_segment(aes(x=factor(week), xend=factor(week), y=0, yend=p_rate), color="grey") +
  geom_point(aes(color=outcome), size=4) + xlab("") + ylab("QB Pressure Rate") +
  scale_color_brewer(palette = "Set1") + labs(color = "Game result") +
  facet_wrap(vars(tm)) +
  theme_ipsum() + theme(legend.position = "bottom",
                        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
                        strip.text = element_nfl_wordmark(size=3, hjust = 0.5))
  
p2 <- ggplot(pats, aes(x=factor(week), y=ht_rate, group = outcome)) +
  geom_segment(aes(x=factor(week), xend=factor(week), y=0, yend=ht_rate), color="grey") +
  geom_point(aes(color=outcome), size=4) + xlab("Week") + ylab("QB Hits Rate") +
  scale_color_brewer(palette = "Set1") + labs(color = "Game result") +
  theme_ipsum() + theme(legend.position = "bottom",
                        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"))


pats2 <- pats %>% 
  group_by(outcome) %>% 
  summarise(pr = mean(pressures),
            ht = mean(hits),
            pt = mean(pts),
            yrd = mean(yards))

p3 <- pats2 %>%
  select(outcome,pt) %>% 
  ggplot(aes(x=outcome, y=pt)) +
  geom_bar(aes(fill=outcome), width = 0.75, stat = "identity", position = position_dodge(1)) +
  scale_fill_brewer(palette = "Set1") + xlab("") + ylab("Points/game") +
  theme_minimal() + theme(legend.position = "none") 

p4 <- pats2 %>%
  select(outcome,yrd) %>% 
  ggplot(aes(x=outcome, y=yrd)) +
  geom_bar(aes(fill=outcome), width = 0.75, stat = "identity", position = position_dodge(1)) +
  scale_fill_brewer(palette = "Set1") + xlab("") + ylab("Yards/game") +
  scale_y_continuous(position = "right") +
  theme_minimal() + theme(legend.position = "none") 

# add pic
pic <- "Documents/R_Proj/Data/pic.jpg"
pic2 <- ggdraw() +
  draw_image(pic, scale = 1)

## Full viz

#(p1 / p2 + plot_layout(guides='collect') & theme(legend.position = "bottom")) | (plot_spacer() / (p3 | p4))
(p1 / p2 + plot_layout(guides='collect') & theme(legend.position = "bottom")) | (p3 | p4)
viz <- (p1 / p2 + plot_layout(guides='collect') & theme(legend.position = "bottom")) | (pic2 / (p3 | p4))

viz +
  plot_annotation(title = "'Hold the line'",
                  subtitle = "The Pats O-Line protection of QB Mac Jones will determine 2022",
                  caption = "All data for 2021 season from pro-football-reference.com",
                  theme = theme(plot.title = element_text(size = 18, face = "bold", color = "darkblue"),
                                plot.subtitle = element_text(size=14, face = "italic", color = "#00AFBB")))

  
