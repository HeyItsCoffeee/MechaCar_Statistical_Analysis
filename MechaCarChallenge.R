# Loading dplyr library
library("dplyr")

# Read csv into dataframe
mecha_car_df <- read.csv("MechaCar_mpg.csv", sep=",", check.names=F, stringsAsFactors=F)

# view few liens of dataframe
head(mecha_car_df)

# Perform linear regression to predict MPG
mecha_car.lm <- lm(formula = mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance+AWD+mpg, data=mecha_car_df)

summary(mecha_car.lm)

suspension_coil_df <- read.csv("Suspension_Coil.csv", sep=",", check.names=F, stringsAsFactors=F)
head(suspension_coil_df)

total_summary <- suspension_coil_df %>% summarise(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI,y=NULL), Standard_Deviation=sd(PSI))
head(total_summary)

lot_summary <- suspension_coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI,y=NULL), Standard_Deviation=sd(PSI))
head(lot_summary)

# T-Tests on Suspension Coils
?t.test()
t.test(suspension_coil_df$PSI, mu=1500)


# t-test on Single sample Lot1
t.test(subset(suspension_coil_df, Manufacturing_Lot=='Lot1')$PSI, mu = 1500)

# t-test on Single sample Lot2
t.test(subset(suspension_coil_df, Manufacturing_Lot=='Lot2')$PSI, mu = 1500)

# t-test on Single sample Lot3
t.test(subset(suspension_coil_df, Manufacturing_Lot=='Lot3')$PSI, mu = 1500)



