# NFL games picks Dvir family 2022 season

library(tidyverse)
library(readxl)
library(estimatr)
library(hrbrthemes)
library(tidyquant)
library(mice)

# Upload weekly picks data
widedata <- read_excel("R_Proj/Data/NFL_Bets/nflpicks.xlsx", sheet = "overall")

# Data management - shift to long format
dat_long <- pivot_longer(widedata, 
                         cols = -name, 
                         names_to = c(".value", "wk"),
                         names_sep = "_") 

dat_long$wk <- as.numeric(dat_long$wk)

# Add variables: winning percentage; lagged winning percentrage
dat_long <- dat_long %>% 
  filter(name != "Dad_ATS") %>% 
  group_by(name) %>% 
  mutate(win_p = round((win/(win+loss)),2),
         p_wk = lag(win_p)) 

# Create dataset for aggregate picks
dat2 <- dat_long %>% 
  group_by(name) %>% 
  summarise(ws = sum(win),
            ls = sum(loss),
            perc = round(mean(win_p)*100,1)) %>% 
  gather(v1,val1, -name, -perc)

# Code variable values for plotting
dat2 <- dat2 %>% 
  mutate(val1 = ifelse(v1 == "ws", val1*(-1), val1)) 

dat2$name <- factor(dat2$name, levels = c('ido','Mom','daniel','Dad'))

# Figure 1: Stacked positive negative totals
ggplot(dat2, aes(x=name,y=val1, fill = v1)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(data = filter(dat2, v1 == "ws"), aes(y= 0, label = paste0(val1*(-1), " Correct")), 
            fontface="bold", hjust = 1.1, nudge_y = -0.5) +
  geom_text(data = filter(dat2, v1 == "ls"), aes(y=0, label = paste0(val1, " Wrong")), 
            fontface="bold", hjust = -0.2, color = "white", nudge_y = 0.5) +
  geom_text(data = filter(dat2, name == "Dad"), aes(y=120, label = "Win Perc."), fontface = "bold", size = 4.5, vjust = -2) +
  geom_label(aes(x=name, y=125, label = paste0(perc, "%")), fill = "yellow") +
  scale_fill_manual(values = c("maroon", "skyblue")) +
  scale_x_discrete(labels = c("ido" = "Ido", "daniel" = "Daniel")) +
  xlab("") + ylab("") + ggtitle("Full season record") +
  coord_flip() + theme_classic2() + theme(legend.position = "none",
                                          axis.text.x = element_blank(),
                                          axis.ticks = element_blank(),
                                          axis.text.y = element_text(size = 14, face = "bold"),
                                          plot.title = element_text(color = "blue", size = 20, face = "bold", hjust = 0.5))

# Create weekly winners and total correct picks, add winner variable for labels
dat_wk <- dat_long %>% 
  filter(name != "Dad_ATS") %>% 
  mutate(gm = win+loss,
         winner = case_when(wk == 3 | wk == 6 | wk == 18 ~ "Mom",
                            wk == 4 | wk == 7 | wk == 8 | wk == 9 | wk == 13 | wk == 16 ~ "Dad",
                            wk == 15 ~ "Ido",
                            wk == 5 | wk == 17 ~ "Daniel & Dad",
                            wk == 12 ~ "Ido & Dad",
                            wk == 10 ~ "Daniel & Mom",
                            wk == 14 ~ "Mom & Dad"))

# Figure 2: Weekly total correct (barplot grouped)
ggplot(dat_wk, aes(x=factor(wk), y=win, group = name)) +
  geom_bar(aes(fill = name), stat = "identity", position = position_dodge(), color = "black", width = 0.7) +
  geom_text(aes(label = win), color = "white", fontface = "bold", position = position_dodge(0.7), vjust = 1.5, size = 3) +
  geom_label(aes(x=factor(wk), y=-0.75, label = paste0("Wk Winner:\n", winner)),colour = "navyblue", fill = "yellow", size = 3, fontface = "bold") +
  scale_x_discrete(labels = paste0("Week ", dat_wk$wk, "\n Gms:", dat_wk$gm)) +
  scale_fill_manual(labels = c("Dad","Daniel","Ido","Mom"), values = c("#135531","#ff3000","#0066cc","#8866aa")) +
  xlab("") + ylab("# of correct picks") + ggtitle("Weekly picks and winners") +
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.background = element_rect(size = 1, linetype = "solid", colour = "navyblue"),
        plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5)) 


### Upload full season picks (including missing weeks)
widedata2 <- read_excel("/R_Proj/Data/NFL_Bets/nflpicks.xlsx", sheet = "impute")

# Data management - shift to long format
dat_long2 <- pivot_longer(widedata2, 
                         cols = -name, 
                         names_to = c(".value", "wk"),
                         names_sep = "_") 

# Create winning percentage variable
dat_long2 <- dat_long2 %>% 
  group_by(name) %>% 
  mutate(win_p = (win/(win+loss))) %>% 
  dplyr::select(-win,-loss)

dat_long2$wk <- as.numeric(dat_long2$wk)

## Imputation procedure
# Create 5 imputed datasets
tempDAT <- mice(dat_long2,m=5,maxit=50,meth='pmm',seed=500)

# Plot density of imputed datasets and their mean
densityplot(tempDAT)

# Validation procedure
# Split imputed data to 5 separate datasets
dat_t1 <- complete(tempDAT, 1)
dat_t2 <- complete(tempDAT, 2)
dat_t3 <- complete(tempDAT, 3)
dat_t4 <- complete(tempDAT, 4)
dat_t5 <- complete(tempDAT, 5)

# Merge 5 datasets and create an average variable
dat_t1 <- dat_t1 %>% mutate(w1=win_p) 
dat_t5 <- dat_t5 %>% mutate(w5=win_p) %>% 
  dplyr::select(-name,-wk,-win_p) 

dat_t1 <- cbind(dat_t1,dat_t2,dat_t3,dat_t4,dat_t5)
dat_t1 <- dat_t1 %>% 
  mutate(win_p2 = (w1+w2+w3+w4+w5)/5)

# Create full imputed dataset
dat_imp <- dat_t1 %>% 
  dplyr::select(-w1,-w2,-w3,-w4,-w5,-win_p)

# Add lagged winning percentage variable to imputed dataset  
dat_imp <- dat_imp %>% 
  group_by(name) %>% 
  mutate(win_lg = lag(win_p2))

# Complete missing values for lagged win p using full season averages
dat_imp %>% 
  group_by(name) %>% 
  summarise(a = mean(win_p2))

dat_imp$win_lg[is.na(dat_imp$win_lg) & dat_imp$name == "Dad"] <- 0.619
dat_imp$win_lg[is.na(dat_imp$win_lg) & dat_imp$name == "Dad-ATS"] <- 0.535
dat_imp$win_lg[is.na(dat_imp$win_lg) & dat_imp$name == "Daniel"] <- 0.555
dat_imp$win_lg[is.na(dat_imp$win_lg) & dat_imp$name == "Ido"] <- 0.49
dat_imp$win_lg[is.na(dat_imp$win_lg) & dat_imp$name == "Mom"] <- 0.542

# Create sub-set of season long mean to add to plot
dat_mn <- dat_imp %>% 
  filter(name != "Dad-ATS") %>% 
  group_by(name) %>% 
  summarise(mn = mean(win_p))

# Figure 3: Full season - weekly, 3-week moving avg. and season average (imputed data)
cols <- c("3-week Moving Avg."="red", "Full Season Avg."="#003300")
  
dat_imp %>% 
  filter(name != "Dad-ATS") %>% 
  ggplot(aes(x=wk, y=win_p, group = name)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "grey") +
  geom_ma(ma_fun = SMA, n = 3, linetype = 5, color = "red", size = 1) +
  geom_hline(data = dat_mn, aes(yintercept = mn, color = "#003300"), linetype = 5, size=0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_color_manual(name = "", values = cols) +
  xlab("Week") + ylab("Win percent") + 
  labs(title = "Our picks %", subtitle = "Weekly, a 3-week moving average & full season average (imputed data)") +
  facet_grid(~name) +
  theme_bw() + theme(legend.position = "bottom",
                     legend.background = element_rect(size = 1, linetype = "solid", colour = "black"),
                     strip.text.x = element_text(color = "darkblue"),
                     strip.background = element_rect(color="black", fill="yellow", size=1.5, linetype="solid"),
                     plot.title = element_text(color = "blue", size = 20, face = "bold"),
                     plot.subtitle = element_text(color = "blue", size = 14, face = "italic"))

### Predictions
# Run linear model with robust clustered SE
summary(m2 <- lm_robust(win_p2 ~ win_lg, data = dat_imp, clusters = name))

# Prediction dataset based on model estimates
pred.dat <- data.frame(win_lg = c(0.615,0.535,0.551,0.5,0.547))
cols2 <- c("Predicted value" = "blue", "Wk 18 actual" = "red")

pred.dat2 <- data.frame((predict(m2, newdata= pred.dat, interval = "confidence")))

# Add IDs and actual week 18 values
pred.dat2 <- pred.dat2 %>% 
  mutate(id = c("Dad","Dad-ATS","Daniel","Ido","Mom"),
         wk18 = c(0.625,0.438,0.562,0.562,0.688))

# Figure 4: Predicted versus actual week 18
ggplot(pred.dat2, aes(x=id, y=fit.fit)) +
  geom_pointrange(aes(ymin = fit.lwr, ymax = fit.upr), color = "red") +
  geom_label(aes(x=id, y=fit.fit, label = "Predicted wk 18", vjust = 0.5), 
             colour = "black", fill = "yellow", size = 3.5, fontface = "bold") +
  geom_point(aes(x=id, y=wk18), color = "blue", size = 2) +
  geom_text(aes(x=id, y=wk18, label = "Wk 18 \n Actual", vjust=-0.6), size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  xlab("") + ylab("Predicted Win %") + labs(title = "Linear predictions Wk 18:", subtitle = "Predicted interval & actual") +
  theme_classic() + coord_flip() +
  theme(plot.title = element_text(size = 20, face="bold", hjust = 0.5),
        plot.subtitle = element_text(size=16, face = "italic", hjust = 0.5),
        axis.text.y = element_text(size = 14, face = "bold"))

# Bootstrapping procedure - 500 bootstraps 
B=500
n =nrow(dat_imp)
newSamp <- with(dat_imp, dat_imp[!is.na(win_p2), ])

# Create 5 bootstrapped subsets, compute sub-sample of means
boot1 <- with(newSamp, matrix(sample(win_p2[name == "Dad"], size = n[1]*B, replace = T), B, n[1]))
mn_boot1 <- apply(boot1, 1, mean)

boot2 <- with(newSamp, matrix(sample(win_p2[name == "Dad-ATS"], size = n[1]*B, replace = T), B, n[1]))
mn_boot2 <- apply(boot2, 1, mean)

boot3 <- with(newSamp, matrix(sample(win_p2[name == "Ido"], size = n[1]*B, replace = T), B, n[1]))
mn_boot3 <- apply(boot3, 1, mean)

boot4 <- with(newSamp, matrix(sample(win_p2[name == "Daniel"], size = n[1]*B, replace = T), B, n[1]))
mn_boot4 <- apply(boot4, 1, mean)

boot5 <- with(newSamp, matrix(sample(win_p2[name == "Mom"], size = n[1]*B, replace = T), B, n[1]))
mn_boot5 <- apply(boot5, 1, mean)

# Merge mean distributions to one dataset
df <- data.frame(Dad=mn_boot1, Ido=mn_boot3, Daniel=mn_boot4, Mom=mn_boot5)
df_boot <- melt(df)

# Subset for each distribution mean for plot
dat_mn2 <- df_boot %>% 
  group_by(variable) %>% 
  summarise(xlab = mean(value),
            mn2 = round(mean(value),4)*100)

# Figure 5: Density plots of probability distributions
library(ggridges)

ggplot(df_boot, aes(x = value, y = variable, fill = variable)) +
  geom_density_ridges(scale = 0.95, alpha = 0.65) +
  theme_ridges() + 
  geom_label(data = dat_mn2, aes(x=xlab+0.002, y=variable, label = paste0("Mean \n", mn2, "%")),
             colour = "black", fill = "yellow", fontface = "bold", vjust = -0.45) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c("green","blue","red","purple")) +
  xlab("Predicted") + ylab("") + labs(title = "Season long winning %:",
                                      subtitle = "Predictions based on bootstrapped data") +
  theme(legend.position = "none",
        plot.title = element_text(size = 20, hjust = 0.5),
        plot.subtitle = element_text(size=16, face = "italic", hjust = 0.5, margin = margin(b=10)),
        axis.text.y = element_text(size = 14, face = "bold"))


### Picking ATS 
# Filter relevant data
dat_ats <- dat_long2 %>% 
  filter(name == "Dad-ATS")

# Create dataset splitting undedogs and favorites 
dogs <- data.frame(wk = c(3:10,12:18),
                   gm = c(16,16,16,14,14,15,13,14,16,15,13,16,16,15,16),
                   wins = c(7,12,10,7,9,12,9,4,3,6,7,12,9,5,7),
                   dog_pk = c(9,7,8,8,6,5,7,3,9,8,6,8,9,8,10),
                   dog_win = c(5,6,6,4,4,5,6,1,2,2,4,6,5,3,4))

# Organize data, add relevant variables
dogs <- dogs %>% 
  left_join(dat_ats, by = "wk") %>% 
  dplyr::select(-name) %>% 
  mutate(fav_pk = gm - dog_pk,
         fav_win = wins - dog_win,
         dog_p = dog_win / dog_pk,
         fav_p = fav_win / fav_pk)

# Prep dataset for bullet chart
dat_bullet <- dogs %>% 
  dplyr::select(wk,dog_pk,dog_win,fav_pk,fav_win)

dat_blt <- pivot_longer(dat_bullet, 
                         cols = -wk, 
                         names_to = c("type","act"),
                         names_sep = "_",
                        values_to = "val") 

dat_blt <- dat_blt %>% 
  mutate(wid = ifelse(act == "pk",0.8,0.5))

# Figure 6: Bullet chart for picking dogs and favorites
ggplot(dat_blt, aes(x=val, y=factor(wk), fill=act)) +
  geom_bar(aes(group = type), stat = "identity", color = "black", width = dat_blt$wid, position = position_dodge(1)) +
  geom_label(data = filter(dat_blt, type == "dog"), 
             aes(x=-0.2, y=factor(wk), label = "Dogs"), vjust = 1, colour = "black", fill = "yellow") +
  geom_label(data = filter(dat_blt, type == "fav"), 
             aes(x=-0.2, y=factor(wk), label = "Favs"), vjust = -0.1, colour = "navyblue", fill = "yellow") +
  geom_text(data = filter(dat_blt, type == "dog"),
            aes(label = val), hjust=-0.7, vjust=1.8, fontface = "bold") +
  geom_text(data = filter(dat_blt, type == "fav"),
            aes(label = val), hjust=-0.7, vjust=-0.8, fontface = "bold") +
  scale_fill_manual(values = c("skyblue", "grey"), labels = c("Total Games picked \n (Dogs / Favs)","Correct picks")) + 
  ylab("Week") + xlab("") + labs(fill = "Game Selection Splits") + ggtitle("Picking ATS") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.background = element_rect(size = 0.75, linetype = "solid", colour = "black"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

ggsave("~/Documents/R_Proj/Data/NFL_Bets/p4.jpeg", width = 15, height = 9, units = "in", dpi = 300, bg = "white")

# ATS season time trend
library(geomtextpath)

# Data prep
dat_ln <- dogs %>%
  dplyr::select(wk,win_p,dog_p,fav_p) %>% 
  gather(grp,val,win_p:fav_p, -wk) 

dat_ln$grp[dat_ln$grp == "dog_p"] <- "Dogs"
dat_ln$grp[dat_ln$grp == "fav_p"] <- "Favs"
dat_ln$grp[dat_ln$grp == "win_p"] <- "Full Season"

# Figure 7: Time trend of winning percentage by pick type
ggplot(dat_ln, aes(x=wk, y=val, color = grp)) +
  geom_line(size=2.5) +
  geom_textline(aes(label = grp), size=4, vjust=-0.7, hjust=0.015, fontface="bold") +
  geom_text(aes(x=15, y=0.97, label = "Full Season Averages"), size = 4.5, fontface = "bold", color = "black") +
  geom_label(aes(x=15, y=0.9, label = "Overall success: 53% \n Dogs success: 58.3% \n Favs success: 48.7%"),
             colour = "navyblue", fill = "yellow") +
  scale_y_continuous(labels=scales::percent) + ylab("Cover Percent") + xlab("Week") + ggtitle("Weekly ATS cover percentage: Dogs and Favorites") +
  theme_bw() + theme(legend.position = "none",
                     plot.title = element_text(size = 18, color = "darkblue", face = "bold",hjust = 0.5))


