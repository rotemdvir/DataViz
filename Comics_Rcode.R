
library(tidyverse)
library(ggplot2)
library(png)
library(jpeg)
library(ggpubr)
library(ggimage)
library(viridis)
library(hrbrthemes)
library(patchwork)

# Data upload
Comic <- read.csv("~/R_Proj/Data/ComicData.csv")

#### Figure 1: circular barplot - original Avengers with Lego pics ####
# Sub-sample by characters
comic <- Comic %>%
  filter(name == "Captain America " | name == "Iron Man " | name == "Thor " | name == "Hulk " |
           name == "Clinton Barton " | name == "Natalia Romanova ") 


# Link images to objects
p_path <- "~/Documents/R_Proj/Data/Pics/"

cap <- paste0(p_path, "cap_lego.png")
hawk <- paste0(p_path, "hawkeye_lego.png")
hulk <- paste0(p_path, "hulk_lego.png")
thor <- paste0(p_path, "thor_lego.png")
tony <- paste0(p_path, "tony_lego.png")
widow <- paste0(p_path, "widow_lego.png")

# Generate picture variable
comic$pic <- NA
comic$pic[comic$name == "Captain America "] <- cap
comic$pic[comic$name == "Clinton Barton "] <- hawk
comic$pic[comic$name == "Hulk "] <- hulk
comic$pic[comic$name == "Thor "] <- thor
comic$pic[comic$name == "Iron Man "] <- tony
comic$pic[comic$name == "Natalia Romanova "] <- widow

# Define aspect ratio 
asp_r <- 1.3

# Prep data for labels text
lab.d <- comic %>%
  filter(appearances > 1000) %>%
  mutate(id = c(1,2,3,4,5,6))

number_of_bar <- nrow(lab.d)
angle <-  90 - 360 * (lab.d$id-0.5) /number_of_bar
lab.d$hjust<-ifelse(angle < -90, 1, 0)
lab.d$a2 <- c(30,0,-30,30,0,-30)

### Generate Plot 
p.polar <-  comic %>%
  filter(appearances > 1000) %>%
  ggplot(aes(x = reorder(name, appearances), appearances, image = pic)) +
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  geom_image(size = 0.06, by = "width", asp = asp_r) + ylim(-100,3500) +
  ggtitle(label = "Original MCU Avengers in Comics", subtitle = "Count of appearances in bars") +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(color = "red", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "blue"),
        panel.grid = element_blank()) +
  geom_text(data=lab.d, aes(x=name, y=appearances - 600, label=appearances, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=3, angle= lab.d$a2, inherit.aes = FALSE ) 

#### Figure 2: Full data most appearances with images ####
# Subset data: most frequent, top 1 percentile in appearances
comic2 <- Comic %>%
  filter(publisher == "Marvel") %>%
  filter(appearances > 1800)

# Link images to objects
cap2 <- paste0(p_path, "cap2.jpg")
spidy <- paste0(p_path, "spidy.jpg")
wolverine <- paste0(p_path, "wolv.jpg")
tony <- paste0(p_path, "tony2.jpg")
thor <- paste0(p_path, "thor2.jpg")
grimm <- paste0(p_path, "ben.jpg")
reed <- paste0(p_path, "reed.jpg")
hulk2 <- paste0(p_path, "hulk2.jpg")
scott <- paste0(p_path, "scott.png")
jonny <- paste0(p_path, "jonny.jpg")
henry <- paste0(p_path, "mccoy.jpg")

# Generate picture variable
comic2$pic2 <- NA
comic2$pic2[comic2$name == "Captain America "] <- cap2
comic2$pic2[comic2$name == "Spider-Man "] <- spidy
comic2$pic2[comic2$name == "Wolverine "] <- wolverine
comic2$pic2[comic2$name == "Iron Man "] <- tony
comic2$pic2[comic2$name == "Thor "] <- thor
comic2$pic2[comic2$name == "Benjamin Grimm "] <- grimm
comic2$pic2[comic2$name == "Reed Richards "] <- reed
comic2$pic2[comic2$name == "Hulk "] <- hulk2
comic2$pic2[comic2$name == "Scott Summers "] <- scott
comic2$pic2[comic2$name == "Jonathan Storm "] <- jonny
comic2$pic2[comic2$name == "Henry McCoy "] <- henry

## Generate plot
p.img <- ggplot(comic2, aes(x = reorder(name,appearances), appearances, image = pic2)) +
  geom_point() +
  geom_segment(aes(x=name, xend=name,y=0,yend=appearances)) +
  geom_image(aes(y=-1), size = 0.05, by = "width", asp = asp_r) +
  geom_text(aes(label = year), vjust = -0.5) + 
  ggtitle(label = "Marvel Comics Data", subtitle = "Most appearances and year of first appearance") +
  xlab("") + ylab("Comic Appearances") +
  theme_classic() + coord_flip() + 
  theme(plot.title = element_text(color = "blue", size = 16, face = "bold"),
        plot.subtitle = element_text(color = "red"))


#### Figure 3: Top women character appearances ####
# Generate subset
lady <- Comic %>%
  filter(publisher == "Marvel") %>%
  filter(appearances > 850) %>%
  filter(sex == "Female Characters") 

# Create grouping variables labels
lady$spot <- "Comics Only"
lady$spot[lady$name == "Wanda Maximoff " | lady$name == "Janet van Dyne " | lady$name == "Natalia Romanova " |
            lady$name == "Carol Danvers "] <- "Arrived to MCU"

# Generate plot using bubble-plot 
p.lady <- ggplot(lady, aes(x = reorder(name,year), appearances, size = appearances, fill = spot)) +
  geom_point(alpha=0.5, shape=21) +
  geom_text(aes(label = year), vjust = -1.5, size = 2.5) +
  xlab("") + ylab("Comic Appearnaces") + ggtitle(label = "Women of Marvel", subtitle = "Most appearances and year of first appearance") +
  scale_fill_viridis(discrete=TRUE, option = "D") +
  theme_ipsum() +
  labs(fill = "", size = "Total") +
  coord_flip() 

p.lady <- p.lady + theme(legend.position = "bottom",
                         legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"),
                         plot.title = element_text(color = "purple", size = 20, face = "bold"),
                         plot.subtitle = element_text(color = "blue"),
                         axis.text.y = element_text(size = 7))

### Using patchwork to combine plots to viz
p.patch <- p.img | (p.lady / p.polar) + 
  plot_layout(heights = unit(c(3.75, 3), c('cm', 'null')))

p.patch

