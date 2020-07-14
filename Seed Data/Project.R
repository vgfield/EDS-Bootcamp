# Sara Schulkowski
# EDS Boot Camp
# Seed Data Project

library(readr)
library(dplyr)

# Read in seed data

seed_data<- read_csv("HWF_ALTEMP26_seed_data_1988_2018.csv")
avg_met_data<- read_csv("2dT_avgDailyTemps.csv")
max_met_data<- read_csv("2dT_MaxDailyTemps.csv")

# Create data frame for seed counts

total_seed<- as.data.frame(seed_data[,c(3,4,9,11,13,16,21,23,25,26,27,31)])

# Remove rows where censor column is 'Y'

total_seed<- subset(total_seed, CENSOR=='N')

# Replace NAs with 0

total_seed[is.na(total_seed)]<- 0

# Calculate individual seed biomass based on counts and seed weights (lbs)

# Yellow birch = 2.222 x 10^-6
# Sugar maple = 1.422 x 10^-4
# Red maple = 4.374 x 10^-5
# American beech = 6.25 x 10^-4
# Red spruce = 7.194 x 10^-6
# Hemlock = 5.347 x 10^-6
# Northern white ceder = 2.890 x 10^-6
# Balsam fir = 1.677 x 10^-5
# Paper birch = 7.246 x 10^-7

weights_per_hect<- c((2.222 * 10^-6), (1.422 * 10^-4), (4.374 * 10^-5), (6.25 * 10^-4), (7.194 * 10^-6), (5.347 * 10^-6), (2.890 * 10^-6), (1.677 * 10^-5), (7.246 * 10^-7))  * 0.453592 * 5714 # Converting lbs per seed to kg per hectare based on SA=0.07 m2

seed_weights<- data.frame(weights_per_hect, row.names=c("yebi", "suma", "rema", "ambe", "resp", "eahe", "nwce", "bafi", "pabi"))

total_seed$yebi_biomass<- total_seed$YEBI_TOTAL * seed_weights[1,1]

total_seed$suma_biomass<- total_seed$SUMA_TOTAL * seed_weights[2,1]

total_seed$rema_biomass<- total_seed$REMA_TOTAL * seed_weights[3,1]

total_seed$ambe_biomass<- total_seed$AMBE_TOTAL * seed_weights[4,1]

total_seed$resp_biomass<- total_seed$RESP_TOTAL * seed_weights[5,1]

total_seed$eahe_biomass<- total_seed$EAHE_TOTAL * seed_weights[6,1]

total_seed$nwce_biomass<- total_seed$NWCE_TOTAL * seed_weights[7,1]

total_seed$bafi_biomass<- total_seed$BAFI_TOTAL * seed_weights[8,1]

total_seed$pabi_biomass<- total_seed$PABI_TOTAL * seed_weights[9,1]

# Sum all tree seed counts daily

group_seed<- group_by(total_seed, YEAR, SEASON)

seed_sum<- summarize(group_seed, yebi=sum(yebi_biomass), suma=sum(suma_biomass), rema=sum(rema_biomass), ambe=sum(ambe_biomass), resp=sum(resp_biomass), eahe=sum(eahe_biomass), nwce=sum(nwce_biomass), bafi=sum(bafi_biomass), pabi=sum(pabi_biomass))

final_seed_sum<- as.data.frame(rowSums(seed_sum[,3:11]))

final_seed_sum$year<- seed_sum$YEAR

final_seed_sum$season<- seed_sum$SEASON

# Plot total seed data

fall_count<- data.frame(seq(1988,2017), final_seed_sum[c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58),1])

summer_count<- data.frame(seq(1989,2018), final_seed_sum[c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,60),1])

plot(fall_count[,1], fall_count[,2], xlab = "Year", ylab = "Seed Biomass (kg / ha)", main = "Total Seed Biomass for Newcomb Forest from 1988-2018", type = "b")
points(summer_count[,1], summer_count[,2], pch=16, type = "b", col="red")
legend("topleft", c("July-November", "November-July"), pch = c(1,16), col = c("black", "red"))

# Plot sugar maple time series

suma_agg<- aggregate(total_seed$suma_biomass ~ total_seed$YEAR, FUN="sum")

plot(suma_agg[,1], suma_agg[,2], type = "b", xlab = "Year", ylab = "Seed Biomass (kg / ha)", main = "Sugar Maple Seed Biomass from 1988-2018")

ambe_agg<- aggregate(total_seed$ambe_biomass ~ total_seed$YEAR, FUN="sum")

plot(ambe_agg[,1], ambe_agg[,2], type = "b", xlab = "Year", ylab = "Seed Biomass (kg / ha)", main = "American Beech Seed Biomass from 1988-2018")

# Temp data alalysis

total_seed$biomass_sum <- rowSums(total_seed[,13:21])
total_agg <- aggregate(total_seed$biomass_sum ~ total_seed$YEAR, FUN = "sum")
colnames(total_agg) <- c("year", "sum")

p.corr <- round(cor(-max_met_data$dT, total_agg$sum),3)

plot(-max_met_data$dT, total_agg$sum, xlab = "Annual Temperature Differential (degrees Fahrenheit)", ylab = "Total Annual Seed Biomass (kg / ha)", main = "Seed Biomass vs. Temperature Differentials \n for Maximum Temperatures between 1988-2018")
lines(lowess(-max_met_data$dT, total_agg$sum, f=3/4,iter=3),col=4)
text(-5,700, paste("r = ", p.corr))

p.corr1<- round(cor(-avg_met_data$dT, total_agg$sum),3)

plot(-avg_met_data$dT, total_agg$sum, xlab = "Annual Temperature Differential (degrees Fahrenheit)", ylab = "Total Annual Seed Biomass (kg / ha)", main = "Seed Biomass vs. Temperature Differentials \n for Average Temperatures between 1988-2018")
lines(lowess(-avg_met_data$dT, total_agg$sum, f=3/4,iter=3),col=4)
text(-2,700, paste("r = ", p.corr1))

# Plot standardized deviation of all annual seed production from the long-term mean

sd<- sd(total_agg$sum)

ltm <- mean(total_agg$sum)

total_agg$dev<- (total_agg$sum - ltm) / sd

plot(total_agg$year, total_agg$dev, xlab = "Year", ylab = "Total Biomass ASD", main = "SD of Total Annual Seed Biomass \n from Long-Term Mean (1988-2018)")

neg_dev<- subset(total_agg$dev, total_agg$dev<0)

sd_neg_dev<- -mean(neg_dev)

abline(h=sd_neg_dev, lty=2)

legend("topleft", lty=2, "Long-term mean")
