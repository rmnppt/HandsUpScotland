### Explore
library(tidyr)
library(dplyr)
library(ggplot2)

theme_blog <- theme(
  text = element_text(size = 14),
  panel.grid = element_blank()
)

dat <- read.csv("Data/2014_Proportions_Cleaned.csv")
datM <- dat %>% gather(mode, count, Walk:Other)
levels(datM$mode) <- sub("...", "/", levels(datM$mode), fixed = T)

# trends in primary school 
primary <- datM %>%
  filter(School.Type == "Primary" & count > 0) %>%
  group_by(Local.Authority, Year, mode) %>%
  summarise(count = mean(count))

# need to check if these trends are significant
models <- primary %>% 
  group_by(mode) %>%
  do(mod = lm(count ~ Year, .))

trends <- models %>% do(data.frame(
  interc = .$mod$coefficients[1],
  grad = .$mod$coefficients[2],
  pval = summary(.$mod)$coefficients[2,4]
))
trends <- cbind(mode = levels(primary$mode), trends)
sig <- which(trends$pval < 0.05)
trends[sig,]
trends$sig <- F
trends$sig[sig] <- T

# produce
jpeg("Plots/Figure2.jpeg", 620, 350, quality = 100, res = 70)
ggplot(primary, aes(x = Year, y = count)) +
  geom_line(aes(group = Local.Authority), alpha = 0.2, col = grey(0.3)) +
#   # stat_summary(fun.y = mean, geom = "line", colour = "red", size = 1) +
#   stat_smooth(method = "lm", colour = "red", size = 1) +
  geom_point(size = 1, alpha = 0.5) + 
  geom_abline(aes(intercept = interc, slope = grad, colour = sig), trends, size = I(2)) +
  facet_wrap(~ mode, nrow = 2, scales = "free") +
  theme_blog
dev.off()

# which schools are the outliers in scooter take-up
primary %>% 
  filter(Year == 2014 & mode == "Scooter/Skate") %>%
  ungroup %>%
  arrange(desc(count)) %>%
  top_n(10)


