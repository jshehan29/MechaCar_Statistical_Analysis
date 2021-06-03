# Deliverable 1

# 3. Load the dplyr package
library(dplyr)

# 4. Import MechaCar_mpg data
mechacar_mpg <- read.csv('MechaCar_mpg.csv', check.names=F, stringsAsFactors=F)

# 5. Perform linear regression using all six variables from dataset
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + 
     AWD, data=mechacar_mpg)

# 6. Use summary() to determine the p-value and r-squared value for the model
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + 
             ground_clearance + AWD, data=mechacar_mpg))

# R-squared is literally square of r-value - use cheat sheet to determine
# correlation based on r-value

# H0 - slope of linear model is zero

# Ha - slope of linear model is non-zero

# If p-value is less than significance level (0.05), we reject null hypothesis
#    - There would be non-zero slope

# r-squared = 0.7149 -> r = 0.8455 --> good correlation
# p-value = 5.35e-11 --> non-zero slope

# Variables that have significant impact on mpg
# - Intercept - could mean significant features may need scaling/transforming to
# improve the predictive power of the model. Could also mean that there are 
# other variables that could explain mpg that are not included in the model.
# - vehicle_length
# - ground_clearance

# -------------------------------------------------
# Deliverable 2

# 2. Import Suspension_Coil.csv file as a table
suspension_coil <- read.csv('Suspension_Coil.csv',check.names=F,
                            stringsAsFactors=F)

# 3. Create total_summary dataframe using summarize() function
total_summary <- suspension_coil %>% summarize(Mean=mean(PSI), 
                                               Median=median(PSI), 
                                               Variance=var(PSI),
                                               SD=sd(PSI),
                                               .groups='keep')

# 4. Create a lot_summary dataframe that gives summary stats for each lot
lot_summary <- suspension_coil %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean=mean(PSI), Median=median(PSI), Variance = var(PSI), SD=sd(PSI),
            .groups = 'keep')


# -------------------------------------------------
# Deliverable 3

#Ho - there is no statistical difference between observed sample mean and 
# presumed population mean

#Ha - there is a statistical difference between observed sample mean and 
# presumed population mean

# Compare sample mean to population mean
t.test(suspension_coil$PSI, mu=1500)
# Gives p-value = 0.06028 -> above 0.05 significance level, so not sufficient 
# evidence to reject null hypothesis. We cannot say that there is a statistical
# difference between observed sample mean and presumed population mean

# Compare Lot 1 sample mean to population mean
t.test(subset(suspension_coil, Manufacturing_Lot=="Lot1")$PSI, mu=1500)
# Gives p-value = 1 -> above 0.05 significance level, so not sufficient
# evidence to reject null hypothesis.

# Compare Lot 2 sample mean to population mean
t.test(subset(suspension_coil, Manufacturing_Lot=="Lot2")$PSI, mu=1500)
# Gives p-value = 0.6072 -> above 0.05 significance level, so not sufficient
# evidence to reject null hypothesis

# Compare Lot 3 sample mean to population mean
t.test(subset(suspension_coil, Manufacturing_Lot=="Lot3")$PSI, mu=1500)
# Gives p-value = 0.04168 -> below 0.05 significance level, so there is
# sufficient evidence to reject null hypothesis





