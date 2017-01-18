library(tidyverse)
library(readxl)
library(ggplot2)
library(extrafont)
# Source Sans Pro google font downloaded from:
# https://fonts.google.com/specimen/Source+Sans+Pro?selection.family=Source+Sans+Pro
font_import("font", pattern = "[Rr]egular")

dat <- read_excel("staadata.xlsx")

dat %>% filter(!complete.cases(.)) %>% View

png("jobs.png", w = 640, h = 1600, family = "Source Sans Pro")
dat %>% group_by(Year) %>%
  rename(labor_force = `Civilian Labor Force Population`) %>%
  mutate(nat_ave = sum(Unemployed) / sum(labor_force)) %>%
  ungroup %>%
  mutate(state_rel_rate = Unemployed / labor_force - nat_ave,
         Year = as.numeric(Year),
         State = toupper(State)) %>% 
  filter(State != "DISTRICT OF COLUMBIA") %>%
  assign("dat_clean", ., inherits = TRUE) %>%
  ggplot(aes(x = Year, y = state_rel_rate)) +
  annotate("rect", xmin = 1976, xmax = 2015, ymin = 0, ymax = 0.1, 
           fill = "#F7E6E2", alpha = 0.9) +
  geom_line() +
  geom_point(data = filter(dat_clean, Year == 2015), 
             aes(color = state_rel_rate > 0)) + 
  geom_text(data = filter(dat_clean, Year == 2015),
            aes(label = paste0(ifelse(state_rel_rate > 0, "+", ""),
                              scales::percent(round(state_rel_rate, 3)))),
                nudge_y = 0.03, nudge_x = -3, size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("FALSE" = "#76b7b2", "TRUE" = "#e15759")) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  facet_wrap(~State, scales = "free_x", ncol = 5) +
  labs(x = "", y = "", title = "The State of U.S. Jobs",
       subtitle = paste("Percentage points below or above the national",
                        "unemployment rate, by state. Negative values represent",
                        "unemployment rates that were\nlower - or better,",
                        "from a jobs perspective - than the national rate.")) +
  theme_bw() +
  theme(strip.background = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        text = element_text(family = "Source Sans Pro"),
        plot.subtitle = element_text(color = "#999999", size = 10),
        plot.title = element_text(size = 28))
dev.off()
        
        