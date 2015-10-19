### Explore
library(tidyr)
library(reshape2)
library(ggplot2)

theme_blog <- theme(
  text = element_text(size = 14)
)

dat <- read.csv("Data/2014_Cleaned.csv")
datM <- dat[,-13] %>% gather(mode, count, Walk:Other)
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

# trends in primary school 
primary <- datM %>%
  filter(School.Type == "Primary" & count > 0) %>%
  group_by(Local.Authority, Year, mode) %>%
  summarise(count = mean(count))

jpeg("Plots/Figure2.jpeg", 800, 500, quality = 100)
ggplot(primary, aes(x = Year, y = count)) +
  geom_line(aes(group = Local.Authority), alpha = 0.2) +
  # stat_summary(fun.y = mean, geom = "line", colour = "red", size = 1) +
  stat_smooth(method = "lm", colour = "red", size = 1) +
  facet_wrap(~ mode, nrow = 2, scales = "free") +
  theme_blog
dev.off()

# correlations
primaryCast <- primary %>% spread(., mode, count)
pairs(as.matrix(primaryCast[,3:10]))
