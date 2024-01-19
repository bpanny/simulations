library(rayshader)
library(ggplot2)
library(tidyverse)

gg = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), 
                  geom = "polygon",
                  n = 100,bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")
plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250)

library(viridis)
library(tidyverse)
measles = read_csv("https://tylermw.com/data/measles_country_2011_2019.csv")
melt_measles = reshape2::melt(measles, id.vars = c("Year", "Country", "Region", "ISO3"))
melt_measles$Month = melt_measles$variable
melt_measles$cases = melt_measles$value
melt_measles %>% 
  group_by(Year, Month) %>%
  summarize(totalcases = sum(cases,na.rm = TRUE)) %>% 
  mutate(totalcases = ifelse(Year == 2019 & !(Month %in% c("January","February","March")), NA, totalcases)) %>%
  ggplot() + 
  geom_tile(aes(x=Year, y=Month, fill=totalcases,color=totalcases),size=1,color="black") + 
  scale_x_continuous("Year", expand=c(0,0), breaks = seq(2011,2019,1)) +
  scale_y_discrete("Month", expand=c(0,0)) +
  scale_fill_viridis("Total\nCases") +
  ggtitle("Reported Worldwide Measles Cases") +
  labs(caption = "Data Source: WHO") +
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA)) -> 
  measles_gg

plot_gg(measles_gg, multicore = TRUE, width = 6, height = 5.5, scale = 300, 
        background = "#afceff",shadowcolor = "#3a4f70")