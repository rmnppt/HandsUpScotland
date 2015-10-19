### Explore
library(readxl)
dat <- read_excel("Data/HUSS2014_NationalResults_EMBARGOED UNTIL 29.05.15.xlsx",
                  sheet = 15, col_names = T, skip = 2)

for(i in 5:13){
  # convert percentage variables to numeric
  dat[,i] <- as.numeric(dat[,i])
}
### i sometimes got an error here that was fixed by restarting R

# proportional data might be useful
write.csv(dat[1:12], "Data/2014_Proportions_Cleaned.csv", row.names = F)

# convert percentage variables to abs #
dat_abs <- dat
for(i in 1:nrow(dat)){
  dat_abs[i,5:12] <- dat[i,5:12] * dat[i,13]
}
write.csv(dat_abs[1:12], "Data/2014_Absolute_Cleaned.csv", row.names = F)
