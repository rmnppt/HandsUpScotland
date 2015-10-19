### Explore
library(tidyr)
library(dplyr)
library(ggplot2)

theme_blog <- theme(
  text = element_text(size = 14),
  panel.grid = element_blank()
)

dat <- read.csv("Data/2014_Absolute_Cleaned.csv")
datM <- dat %>% gather(mode, count, Walk:Other)
levels(datM$mode) <- sub("...", "/", levels(datM$mode), fixed = T)

# modes by year and school type
modes <- datM %>% 
  group_by(School.Type, mode, Year) %>% 
  summarise(count = sum(count, na.rm = T)) %>%
  filter(!is.na(School.Type))

jpeg("Plots/Figure1.jpeg", 800, 500, quality = 100)
ggplot(modes, aes(x = Year, y = count)) + 
  geom_bar(aes(fill = mode), stat = "identity", position = "fill") +
  facet_grid(~ School.Type) +
  ylab("") +
  theme_blog
dev.off()

# # modes by school year type
# modes <- datM %>% 
#   group_by(Year.Group, mode) %>% 
#   summarise(count = sum(count, na.rm = T))
# 
# ggplot(modes, aes(x = Year.Group, y = count)) + 
#   geom_bar(aes(fill = mode), stat = "identity", position = "fill") +
#   ylab("") +
#   theme_blog
