

#boxplots and histograms (modules 4 and 5)

#avg
acme_jul <- acme %>%
  filter(MONTH %in% c(7),
         YEAR %in% c(2000, 2005, 2010, 2015, 2020),
         TAVG >= lower_bound, TAVG <= upper_bound,
         TMAX >= lower_bound, TMAX <= upper_bound,
         TMIN >= lower_bound, TMIN <= upper_bound
  )

ggplot(acme_jul, aes(x = factor(MONTH), y = TAVG, color = factor(YEAR))) +
  geom_boxplot() +
  labs(title = "Comparison of Average Daily Temperatures",
       x = "Year", y = "Temperature", color = "Year") +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank())

acme_julall <- acme %>%
  filter(MONTH %in% c(7),
         TAVG >= octlower_bound, TAVG <= octupper_bound,
         TMAX >= lower_bound, TMAX <= upper_bound,
         TMIN >= lower_bound, TMIN <= upper_bound)


describe(acme_julall$TAVG)
hist(acme_julall$TAVG, main="Daily Temperature Averages, 2000-2023", xlab="Temperature (F)")

#max
ggplot(acme_jul, aes(x = factor(MONTH), y = TMAX, color = factor(YEAR))) +
  geom_boxplot() +
  labs(title = "Comparison of Maximum Daily Temperatures",
       x = "Year", y = "Temperature", color = "Year") +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank())

acme_julall <- acme %>%
  filter(MONTH %in% c(7),
         TAVG >= octlower_bound, TAVG <= octupper_bound,
         TMAX >= lower_bound, TMAX <= upper_bound,
         TMIN >= lower_bound, TMIN <= upper_bound)


describe(acme_julall$TMAX)
hist(acme_julall$TMAX, main="Daily Temperature Maximums, 2000-2023", xlab="Temperature (F)")

#min
ggplot(acme_jul, aes(x = factor(MONTH), y = TMIN, color = factor(YEAR))) +
  geom_boxplot() +
  labs(title = "Comparison of Minimum Daily Temperatures",
       x = "Year", y = "Temperature", color = "Year") +
  theme(axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank())

acme_julall <- acme %>%
  filter(MONTH %in% c(7),
         TAVG >= octlower_bound, TAVG <= octupper_bound,
         TMAX >= lower_bound, TMAX <= upper_bound,
         TMIN >= lower_bound, TMIN <= upper_bound)

describe(acme_julall$TMIN)

hist(acme_julall$TMIN, main="Daily Temperature Minimums, 2000-2023", xlab="Temperature (F)")

#scatterplots/correlation (module 7)
acme_julyall <- acme %>%
  filter(MONTH %in% c(7),
         TAVG >= lower_bound, TAVG <= upper_bound,
         TMAX >= lower_bound, TMAX <= upper_bound,
         TMIN >= lower_bound, TMIN <= upper_bound)  # Example: Filtering for 2020 and 2021

write.csv(acme_julyall, "julyall.csv")

#avg
plot (acme_julyall$YEAR,acme_julyall$TAVG, 
      col = "blue",
      main = "Average Daily Temperatures by Year",
      xlab = "Year",
      ylab = "Temperature (F)",
      ylim = c(50,115))

abline(lm(acme_julyall$TAVG ~ acme_julyall$YEAR),col="red")

# Correlation for one pair of variables at a time
#x = education
# Gives r, hypothesis test, and confidence interval
cor.test(acme_julyall$YEAR,acme_julyall$TAVG)

# Linear regression model

avgreg <- lm (acme_julyall$YEAR ~ acme_julyall$TAVG)
summary (avgreg)

# Predict values based on regression equation

predict (avgreg)

#max
plot (acme_julyall$YEAR,acme_julyall$TMAX, 
      col = "blue",
      main = "Maximum Daily Temperatures by Year",
      xlab = "Year",
      ylab = "Temperature (F)",
      ylim = c(50,115))

abline(lm(acme_julyall$TMAX ~ acme_julyall$YEAR),col="red")

cor.test(acme_julyall$YEAR,acme_julyall$TMAX)
# Linear regression model
maxreg <- lm (acme_julyall$YEAR ~ acme_julyall$TMAX)
summary (maxreg)

#min
plot (acme_julyall$YEAR,acme_julyall$TMIN, 
      col = "blue",
      main = "Minimum Daily Temperatures by Year",
      xlab = "Year",
      ylab = "Temperature (F)",
      ylim = c(50,115))

abline(lm(acme_julyall$TMIN ~ acme_julyall$YEAR),col="red")

cor.test(acme_julyall$YEAR,acme_julyall$TMIN)
# Linear regression model
minreg <- lm (acme_julyall$YEAR ~ acme_julyall$TMIN)
summary (minreg)

#confidence intervals

#avg
stat.desc(acme_julall$TAVG,basic=F)
#max
stat.desc(acme_julall$TMAX,basic=F)
#min
stat.desc(acme_julall$TMIN,basic=F)

#one sample t-tests
t.test(acme_julyall$TAVG,mu = 82.5, alternative ="less")

#two group t-tests - looking at years 2000 and 2023 (first and last)

acme_23 <- acme %>%
  filter(MONTH %in% c(7),
         YEAR %in% c(2000, 2023),
         TAVG >= lower_bound, TAVG <= upper_bound,
         TMAX >= lower_bound, TMAX <= upper_bound,
         TMIN >= lower_bound, TMIN <= upper_bound)  # Example: Filtering for 2020 and 2021

t.test(TAVG ~ YEAR, data = acme_23)
t.test(TMAX ~ YEAR, data = acme_23)
t.test(TMIN ~ YEAR, data = acme_23)

#chi squared
acme_cat <- read.csv("julyall 1.csv") #already filtered, data is good

#min and avg
ttab_min <- table(acme_cat$TAVG_CAT, acme_cat$TMIN_CAT)
ttab_min

chi_min <- chisq.test(ttab_min)
chi_min

#max and avg
ttab_max <- table(acme_cat$TAVG_CAT, acme_cat$TMAX_CAT)
ttab_max

chi_max <- chisq.test(ttab_max)
chi_max

#max and min
ttab_both <- table(acme_cat$TMIN_CAT, acme_cat$TMAX_CAT)
ttab_both

chi_both <- chisq.test(ttab_both)
chi_both

#ANOVA
anova1 <- aov(TAVG ~ YEAR, data = acme_julyall)
anova1
summary(anova1)

anova2 <- aov(TMAX ~ YEAR, data = acme_julyall)
anova2
summary(anova2)

anova3 <- aov(TMIN ~ YEAR, data = acme_julyall)
anova3
summary(anova3)

#multiple regression
acme <- read.csv ("julyall 1.csv")

acme1 <- lm (TAVG ~ TMAX + TMIN + TMAX_CAT + TMIN_CAT + TAVG_CAT,
             data = acme)
acme1
anova(acme1)
coef(acme1)
summary(acme1)

#PCA
acmevar <- acme[, c(6,8,10)]

apc <- prcomp(acmevar,
              center = TRUE,  # Centers means to 0 (optional)
              scale = TRUE)  # Sets unit variance (helpful)
summary(apc)
plot(apc, main = "Scree Plot")
apc
acmeloadings <- predict(apc)
acmeloadings

#cluster analysis
ad <- dist(acmeloadings)
ac <- hclust(ad)
ac
# Plot dendrogram of clusters
plot(ac)
# Draw boxes around clusters
rect.hclust(ac, k = 2, border = "gray")
rect.hclust(ac, k = 3, border = "blue")
rect.hclust(ac, k = 4, border = "green4")
rect.hclust(ac, k = 5, border = "darkred")
ag4 <- cutree(ac, k = 2)

akm <- kmeans(acmeloadings, 2)
akm$cluster
akm
require(cluster)
clusplot(acmeloadings,  # data frame
         akm$cluster,  # cluster data
         color = TRUE,  # color
         shade = TRUE,  # Lines in clusters
         lines = 3,  # Lines connecting centroids
         labels = 2,main="Cluster Plot")  # Labels clusters and cases
