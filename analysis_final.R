install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("caret")
install.packages("FSA")
library(FSA)
library(caret)
library(ggpubr)
library(ggplot2)
library(dplyr)

#### Import Data and formatting ####

# import data
db <-read.csv("Database1&2_Ariel.csv")

# look at how many NAs in each column
colSums(!is.na(db))

# comfort was factor, turn it into numbers
db$Thermal.comfort <- as.numeric(as.character(db$Thermal.comfort))


#####  PMV classes analyses ######
PMV.class <- seq(0.1,1,0.1)

# thermal acceptability, acceptable is considered as satisfaction
db.t <- na.omit(db[ , c("PMV","Thermal.acceptability")]) 
db.ta <- db.t %>%
  filter(PMV <= 0.1 & PMV>= -0.1) %>%
  summarize(Sample.size = n(), Percent=100*sum(Thermal.acceptability)/n())
for (limit in seq(0.2, 1, 0.1)) {
  db_to_add <- db.t %>%
    filter(PMV <= limit & PMV>= -limit) %>%
    summarize(Sample.size = n(), Percent=100*sum(Thermal.acceptability)/n())
  db.ta <- full_join(db.ta, db_to_add)
}
db.ta$scale <- "Acceptable votes"
db.ta <- data.frame(db.ta,PMV.class)

# thermal sensation, -1 to 1 are considered as satisfaction
db.t <- na.omit(db[ , c("PMV","Thermal.sensation")]) 
db.ts <- db.t %>%
  filter(PMV <= 0.1 & PMV>= -0.1) %>%
  summarize(Sample.size = n(), 
            Percent=100*nrow(db.t %>% filter(PMV <= 0.1 & PMV>= -0.1) %>% filter(Thermal.sensation <=1 & Thermal.sensation >=-1))/n())
for (limit in seq(0.2, 1, 0.1)) {
  db_to_add <- db.t %>%
    filter(PMV <= limit & PMV>= -limit) %>%
    summarize(Sample.size = n(), 
              Percent=100*nrow(db.t %>% filter(PMV <= limit & PMV>= -limit) %>% filter(Thermal.sensation <=1 & Thermal.sensation >=-1))/n())
  db.ts <- full_join(db.ts, db_to_add)
}
db.ts$scale <- "|TSV| <= 1"
db.ts <- data.frame(db.ts,PMV.class)

# thermal comfort, 3.5 to 6 are considered as satisfaction
db.t <- na.omit(db[ , c("PMV","Thermal.comfort")]) 
db.tc <- db.t %>%
  filter(PMV <= 0.1 & PMV>= -0.1) %>%
  summarize(Sample.size = n(), 
            Percent=100*nrow(db.t %>% filter(PMV <= 0.1 & PMV>= -0.1) %>% filter(Thermal.comfort >=3.5))/n())
for (limit in seq(0.2, 1, 0.1)) {
  db_to_add <- db.t %>%
    filter(PMV <= limit & PMV>= -limit) %>%
    summarize(Sample.size = n(), 
              Percent=100*nrow(db.t %>% filter(PMV <= limit & PMV>= -limit) %>% filter(Thermal.comfort >=3.5))/n())
  db.tc <- full_join(db.tc, db_to_add)
}
db.tc$scale <- "Comfortable votes >= 3.5"
db.tc <- data.frame(db.tc,PMV.class)

# thermal preference, no change is considered as satisfaction
db.t <- na.omit(db[ , c("PMV","Thermal.preference")]) 
db.tp <- db.t %>%
  filter(PMV <= 0.1 & PMV>= -0.1) %>%
  summarize(Sample.size = n(), 
            Percent=100*nrow(db.t %>% filter(PMV <= 0.1 & PMV>= -0.1) %>% filter(Thermal.preference == "no change"))/n())
for (limit in seq(0.2, 1, 0.1)) {
  db_to_add <- db.t %>%
    filter(PMV <= limit & PMV>= -limit) %>%
    summarize(Sample.size = n(), 
              Percent=100*nrow(db.t %>% filter(PMV <= limit & PMV>= -limit) %>% filter(Thermal.preference == "no change"))/n())
  db.tp <- full_join(db.tp, db_to_add)
}
db.tp$scale <- "Prefer no change"
db.tp <- data.frame(db.tp,PMV.class)

# combine the results
Results <- rbind(db.ta,db.ts,db.tp,db.tc)
# keep 2 decimals for percent
Results$Percent <- round(Results$Percent,1)
write.csv(Results, "PMV_classes_results.csv")

#plot
p<- ggplot(Results, aes(x=PMV.class, y=Percent,col = as.factor(scale))) +
  geom_hline(aes(yintercept = 80),linetype="dashed") +
  geom_point(aes(size = Sample.size), alpha = 0.7) +
  geom_text(aes(label = Percent), hjust = 0, nudge_x = 0.01, size = 3.3, colour = "black") +
  annotate("text", x = 0.2, y = 50, label = "Class A") +
   annotate("text", x = 0.5, y = 50, label = "Class B") +
   annotate("text", x = 0.7, y = 50, label = "Class C") +
  theme_bw() +
  scale_x_continuous(breaks=seq(0.1,1,0.1), limits = c(0.1,1.04)) +
  scale_y_continuous(limits = c(50, 100)) +
  labs(x = "PMV range", 
       y="Percentage", 
       col = "Percentage of",
       size = "Sample size")+
  scale_colour_manual(values = c("lightsteelblue3","brown2","darkseagreen","skyblue"),
                      breaks = c("Acceptable votes",
                                 "Comfortable votes >= 3.5",
                                 "|TSV| <= 1",
                                 "Prefer no change")) +
  theme(text=element_text(size=16,  family="sans"),  # "sans" changes font to Arial
        axis.text.y=element_text(size=10, colour="black"), 
        axis.text.x=element_text(size=10, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'))
ggsave(filename = "PMV_classes.jpeg",plot=p, width = 8, height = 5)

#revise plot
pr1 <- ggplot(Results, aes(x=PMV.class, y=Percent,col = as.factor(scale))) +
  #geom_hline(aes(yintercept = 80),linetype="dashed") +
  geom_point(aes(size = Sample.size), alpha = 0.7) +
  geom_text(aes(label = Percent), hjust = 0, nudge_x = 0.02, size = 3.3, colour = "black") +
  annotate("text", x = 0.2, y = 50, label = "Class A") +
  annotate("text", x = 0.5, y = 50, label = "Class B") +
  annotate("text", x = 0.7, y = 50, label = "Class C") +
  annotate("text", x = 1.12, y = 79.8, label = "Comfortable votes >= 3.5", hjust = 0) +
  annotate("text", x = 1.12, y = 77.9, label = "|TSV| <= 1",  hjust = 0) +
  annotate("text", x = 1.12, y = 76.1, label = "Acceptable votes",  hjust = 0) +
  annotate("text", x = 1.12, y = 52.4, label = "Prefer no change",  hjust = 0) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0.1,1,0.1), limits = c(0.1,1.45)) +
  scale_y_continuous(limits = c(50, 100)) +
  labs(x = "PMV range", 
       y="Percentage", 
       col = "Percentage of",
       size = "Sample size")+
  scale_colour_manual(values = c("lightsteelblue3","brown2","darkseagreen","skyblue"),
                      breaks = c("Acceptable votes",
                                 "Comfortable votes >= 3.5",
                                 "|TSV| <= 1",
                                 "Prefer no change")) +
  theme(text=element_text(size=16,  family="sans"),  # "sans" changes font to Arial
        axis.text.x=element_text(size=10, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        # legend format
        legend.title = element_text(size=12),
        legend.position=c(0.8, 0.8),  
        legend.background = element_blank(),
        legend.key = element_blank()) +
   # turn off color legend
   guides(colour=FALSE)
ggsave(filename = "PMV_classes_2.jpeg",plot=pr1, width = 8, height = 5)


#### Actual Percent Dissatisfied vs. PMV ####

# calculate actual %unacceptability over PMV (bin=0.1) 
db.t <- na.omit(db[ , c("Thermal.acceptability", "PMV")]) 
db.ta <- db.t %>%
  group_by(PMV) %>%
  summarize(Sample.size = n(), APD=100-100*sum(Thermal.acceptability)/n()) # APD = actual percent disatisfied
db.ta$scale <- "Unacceptable votes"

# calculate actual %unsatisfaction over PMV (bin=0.1) 
db.t <- na.omit(db[ , c("Thermal.sensation", "PMV")]) 
db.t$unsat <- with(db.t, ifelse(Thermal.sensation >1 | Thermal.sensation < -1, 1, 0)) # mark 1 to be unsatisfied
db.ts <- db.t %>%
  group_by(PMV) %>%
  summarize(Sample.size = n(), 
            APD=100*sum(unsat)/n())
db.ts$scale <- "|TSV| > 1"

# calculate actual %uncomfort over PMV (bin=0.1) 
db.t <- na.omit(db[ , c("Thermal.comfort", "PMV")]) 
db.t$uncomf <- with(db.t, ifelse(Thermal.comfort < 3.5, 1, 0)) # mark 1 to be uncomfortable
db.tc <- db.t %>%
  group_by(PMV) %>%
  summarize(Sample.size = n(), 
            APD=100*sum(uncomf)/n())
db.tc$scale <- "Uncomfortable votes < 3.5"

# calculate actual preference  over PMV (bin=0.1) 
db.t <- na.omit(db[ , c("Thermal.preference", "PMV")]) 
db.t$unpref <- with(db.t, ifelse(Thermal.preference != "no change", 1, 0)) # mark 1 to be uncomfortable
db.tp <- db.t %>%
  group_by(PMV) %>%
  summarize(Sample.size = n(), 
            APD=100*sum(unpref)/n())
db.tp$scale <- "Prefer warmer or cooler"

# combine the actual percent dissatisfied
Results <- rbind(db.ta,db.ts,db.tp,db.tc)

# function of Predited percent dissatisfied
PPD <- function(x) {100-95*exp(-0.03353*x^4 - 0.2179*x^2)}

#plot
p<- ggplot(Results, aes(x=PMV, y=APD,col = as.factor(scale))) +
  geom_point(aes(size = Sample.size), alpha = 0.7) +
  stat_smooth(method='lm', formula = y~poly(x,2),se=FALSE,aes(weight = Sample.size)) +
  stat_function(fun = PPD, aes(colour = "Predicted dissatisfied"), linetype="dashed") +
  labs(y="Percentage", 
       col = "Percentage of",
       size = "Sample size") +
  scale_colour_manual(values = c("lightsteelblue3", "black","skyblue","brown2","darkseagreen"),
                      breaks = c("Unacceptable votes",
                                  "Uncomfortable votes < 3.5",
                                  "|TSV| > 1",
                                 "Prefer warmer or cooler",
                                  "Predicted dissatisfied")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks=seq(-3,3,1)) +
  theme_bw() +
  theme(text=element_text(size=16,  family="sans"),  # "sans" changes font to Arial
        axis.text.y=element_text(size=10, colour="black"), 
        axis.text.x=element_text(size=10, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'))
ggsave(filename = "Actual-PPD-quadratic.jpeg",plot=p, width = 8, height =5)

# revise plot
pr2 <- ggplot(Results, aes(x=PMV, y=APD,col = as.factor(scale))) +
  geom_point(aes(size = Sample.size), alpha = 0.7) +
  stat_smooth(method='lm', formula = y~poly(x,2),se=FALSE,aes(weight = Sample.size)) +
  stat_function(fun = PPD, xlim=c(-3,3), aes(colour = "Predicted dissatisfied"), linetype="dashed") +
  labs(y="Percentage",
       x ="PMV bin",
       col = "Percentage of",
       size = "Sample size") +
  annotate("text", x = 3.1, y = 100, label = "Predicted dissatisfied", hjust = 0) +
  annotate("text", x = 3.1, y = 92, label = "Prefer warmer or cooler", hjust = 0) +
  annotate("text", x = 3.1, y = 63, label = "|TSV| > 1",  hjust = 0) +
  annotate("text", x = 3.1, y = 57, label = "Unacceptable votes",  hjust = 0) +
  annotate("text", x = 3.1, y = 48, label = "Uncomfortable votes < 3.5",  hjust = 0) +
  scale_colour_manual(values = c("lightsteelblue3", "black","skyblue","brown2","darkseagreen"),
                      breaks = c("Unacceptable votes",
                                 "Uncomfortable votes < 3.5",
                                 "|TSV| > 1",
                                 "Prefer warmer or cooler",
                                 "Predicted dissatisfied")) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks=seq(-3,3,1), limits = c(-3,5.2)) +
  theme_bw() +
  theme(text=element_text(size=16,  family="sans"),  # "sans" changes font to Arial
        axis.text.y=element_text(size=10, colour="black"), 
        axis.text.x=element_text(size=10, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        # legend format
        legend.title = element_text(size=12),
        legend.position=c(0.8, 0.2),  
        legend.background = element_blank(),
        legend.key = element_blank()) +
  guides(color = FALSE)
ggsave(filename = "Actual-PPD-quadratic-2.jpeg",plot=pr2, width = 8, height =5)



######### Method 1 - bin air temperature #####

db.t <- na.omit(db[,c("Thermal.acceptability", "Air.temperature..C.","Building.type")]) 

# bin Air temprature to T.air, breaks are left open, right closed, e.g. (0.5, 1.5] = 1
db.t$T.air <- cut(db.t$Air.temperature..C., breaks = seq(0.5, 45.5, 1), labels = seq(1, 45, 1))
# turn T.air to numeric
db.t$T.air <- as.numeric(as.character(db.t$T.air))
# sample size for panels
count <- db.t %>%
  group_by(Building.type) %>%
  summarise(n=n())
# calculate percentage of acceptability
db.t <- db.t %>%
  group_by(Building.type, T.air) %>%
  summarize(Sample.size = n(), Percent.accept=100*sum(Thermal.acceptability)/n())
db.t$Sample.size <- as.numeric(db.t$Sample.size)
db.t$compliance <- with(db.t, ifelse(Percent.accept >= 80, ">=80%", "<80%"))
db.t <- subset(db.t, Sample.size > 25)

# calculate range, used for text in the graph
cdb.t <- db.t %>%
  filter(compliance == ">=80%") %>%
  summarise(Tmax = max(T.air),
            Tmin = min(T.air),
            range = Tmax - Tmin + 1)
#plot
p1<-ggplot(data = db.t, aes(x=T.air, y = Percent.accept)) +
  geom_point(aes(size=Sample.size, col = compliance)) +
  geom_text(data = cdb.t, aes(y = 75, x= Tmax+1, label = Tmax), size = 5) +
  geom_text(data = cdb.t, aes(y = 75, x=Tmin-1, label = Tmin), size = 5) +
  geom_text(data = count, aes(x= 25, y = 20, label = paste("n = ",n)), size = 5)+
  geom_text(data = cdb.t, aes(x= 25, y = 30, label = paste("range = ",range, "K")), size = 5)+
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))+
  theme_bw() +
  theme(text=element_text(size=16,  family="sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(fill="NA",colour = "black"))+  
  geom_hline(aes(yintercept = 80),linetype="dashed") +
  facet_grid(~Building.type)+
  labs(x="Air Temperature (˚C)", y = "Percentage of Acceptability", size = "Sample size", col = "Compliance") +
  ggtitle("Method 1: percentage of acceptability in temperature bins \n(keep only sample size > 25, bin size = 1˚C)")


##### Method 2 - neutral temperature range ####

# dataset to analyze
db.t <- na.omit(db[, c("Thermal.sensation", "Air.temperature..C.","Building.type")])
# put senior center to "others"
db.t$Building.type <- as.character(db.t$Building.type)
db.t$Building.type[db.t$Building.type == "Senior center"] <- "Others"

# calculate the neutral temperature, Griffiths constant = 0.4, T.neutral = T - TSV/G.
db.t$T.neutral <- db.t$Air.temperature..C. - db.t$Thermal.sensation/0.4

# numbers for the plot
cdb.t <- db.t %>%
  group_by(Building.type) %>%
  summarise(tenth = round(quantile(T.neutral,0.1),1),
            nintyth = round(quantile(T.neutral,0.9),1),
            median = round(median(T.neutral),1),
            range = nintyth - tenth,
            n=n())

p2<- ggplot(db.t, aes(y = T.neutral,x = 1)) +
  stat_summary(geom = "boxplot",
               fun.data = function(x) setNames(quantile(x, c(0, 0.1, 0.5, 0.9, 1)), c("ymin", "lower", "middle", "upper", "ymax")),
               position = position_dodge(width=0.9)) +
  scale_y_continuous(limits = c(10, 40)) +
  facet_grid(~Building.type)+
  geom_text(data = cdb.t, aes(y = median + 1, label = paste("median = ", median)),size=5) +
  geom_text(data = cdb.t, aes(y = 40, label = paste("range = ", range, "K")),size=5) +
  geom_text(data = cdb.t, aes(y = tenth - 1, label = paste("10th percentile = ",tenth)),size=5) +
  geom_text(data = cdb.t, aes(y = nintyth + 1, label = paste("90th percentile = ",nintyth)),size=5) +
  geom_text(data = cdb.t, aes(y = 38, label = paste("n = ", n)),size=5) +
  theme_bw() +
  labs( y = "Neutral Temperature (˚C)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text=element_text(size=16,  family="sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(fill="NA",colour = "black")) +
  ggtitle("Method 2: neutral temperature range (Griffiths constant = 0.4)")

### combine results of two methods ####
methods <- ggarrange(p1,p2, 
                    labels = c("(a)", "(b)"),
                    ncol = 1, nrow = 2)

ggsave(filename = "2methods.jpeg",plot=methods, width = 12, height = 8)


### performance measure of method 1 ####

# subset for method 1
db.t1 <- na.omit(db[, c("Thermal.acceptability", "Air.temperature..C.","Building.type")])
db.t1$T.air <- cut(db.t1$Air.temperature..C., breaks = seq(0.5, 45.5, 1), labels = seq(1, 45, 1))
# turn T.air to numeric
db.t1$T.air <- as.numeric(as.character(db.t1$T.air))

# calculate the difference between training and testing set
  # develop training and testing data sets
  intrain <- createDataPartition(y=db.t1$Air.temperature..C., p = 0.8, list=FALSE)
  training <- db.t1[intrain, ]
  testing <- db.t1[-intrain, ]
  # combine training and testing with labels
  training$set <- "Training"
  testing$set <- "Testing"
  db.t <- rbind(training, testing)
  # calculate percentage of acceptability
  db.t <- db.t %>%
    group_by(set, Building.type, T.air) %>%
    summarize(Sample.size = n(), Percent.accept=100*sum(Thermal.acceptability)/n())
  # calculate range
  err1 <- db.t %>%
    filter(Sample.size >25) %>%
    filter(Percent.accept >= 80) %>%
    group_by(set, Building.type) %>%
    summarise(Tmax = max(T.air),
              Tmin = min(T.air),
              range = Tmax - Tmin + 1) %>%
    group_by(Building.type) %>%
    summarise(diff_range = max(range)-min(range))

# run the test above 199 times more
for (i in 1:499) {
  # develop training and testing data sets
  intrain <- createDataPartition(y=db.t1$Air.temperature..C., p = 0.8, list=FALSE)
  training <- db.t1[intrain, ]
  testing <- db.t1[-intrain, ]
  # combine training and testing with labels
  training$set <- "Training"
  testing$set <- "Testing"
  db.t <- rbind(training, testing)
  # calculate percentage of acceptability
  db.t <- db.t %>%
    group_by(set, Building.type, T.air) %>%
    summarize(Sample.size = n(), Percent.accept=100*sum(Thermal.acceptability)/n())
  # calculate range
  err_add <- db.t %>%
    filter(Sample.size >25) %>%
    filter(Percent.accept >= 80) %>%
    group_by(set, Building.type) %>%
    summarise(Tmax = max(T.air),
              Tmin = min(T.air),
              range = Tmax - Tmin + 1) %>%
    group_by(Building.type) %>%
    summarise(diff_range = max(range)-min(range))
  err1 <- rbind(err1, err_add)
}
  
# calculate the mean difference between training and testing sets
err_mean1 <- err1 %>%
  group_by(Building.type) %>%
  summarise(error.mean = mean(diff_range))


#### performance measure of method 2 ####

# subset for method 2
# calculate difference between training and testing sets
db.t2 <- na.omit(db[, c("Thermal.sensation", "Air.temperature..C.","Building.type")])
# put senior center to "others"
db.t2$Building.type <- as.character(db.t2$Building.type)
db.t2$Building.type[db.t2$Building.type == "Senior center"] <- "Others"
# T neutral
db.t2$T.neutral <- db.t2$Air.temperature..C. - db.t2$Thermal.sensation/0.4

# calculate difference between training and testing sets
# develop training and testing data sets
intrain <- createDataPartition(y=db.t2$T.neutral, p = 0.8, list=FALSE)
training <- db.t2[intrain, ]
testing <- db.t2[-intrain, ]
# combine training and testing with labels
training$set <- "Training"
testing$set <- "Testing"
db.t <- rbind(training, testing)
# derive the range for training set
err2 <- db.t %>%
  group_by(set, Building.type) %>%
  summarise(range = quantile(T.neutral,0.9) - quantile(T.neutral,0.1)) %>%
  group_by(Building.type) %>%
  summarise(diff_range = max(range)-min(range))

# run the test above for 200 times
for (i in 1:499) {
  # develop training and testing data sets
  intrain <- createDataPartition(y=db.t2$T.neutral, p = 0.8, list=FALSE)
  training <- db.t2[intrain, ]
  testing <- db.t2[-intrain, ]
  # combine training and testing with labels
  training$set <- "Training"
  testing$set <- "Testing"
  db.t <- rbind(training, testing)
  # derive the range for training set
  err_add <- db.t %>%
    group_by(set, Building.type) %>%
    summarise(range = quantile(T.neutral,0.9) - quantile(T.neutral,0.1)) %>%
    group_by(Building.type) %>%
    summarise(diff_range = max(range)-min(range))
  err2 <- rbind(err2, err_add)
}

# calculate the mean difference between training and testing sets
err_mean2 <- err2 %>%
  group_by(Building.type) %>%
  summarise(error.mean = mean(diff_range))


#### test appropriate threshold ####
# use a proxy of "building" 
# assume the records with the same publication, city, building type, and cooling strategy are from the same building.

# method 1
db.t <- db[, c("Thermal.acceptability","Air.temperature..C.","Publication","City","Building.type","Cooling.strategy.building.level")]
db.t <- db.t[complete.cases(db.t$Thermal.acceptability), ]
db.t <- db.t[complete.cases(db.t$Air.temperature..C.), ]
db.t$T.air <- cut(db.t$Air.temperature..C., breaks = seq(0.5, 45.5, 1), labels = seq(1, 45, 1))
db.t$T.air <- as.numeric(as.character(db.t$T.air))
# calculate acceptable range for each "building"
building.sum <- db.t %>%
  group_by(Publication, City, Building.type, Cooling.strategy.building.level, T.air) %>%
  summarize(Sample.size = n(), Percent.accept=100*sum(Thermal.acceptability)/n()) %>%
  filter(Sample.size >25) %>%
  filter(Percent.accept >= 80) %>%
  group_by(Publication, City, Building.type, Cooling.strategy.building.level) %>%
  summarise(range = max(T.air) - min(T.air) +1)
building.sum %>%
  group_by(Building.type) %>%
  summarise(sum = n())
# Sorting x data
building.sum$range <- sort(building.sum$range)
# frequency of ranges
building.sum <- building.sum %>%
  group_by(range) %>%
  summarise(n = n())
# cumulative percentage of buildings
building.sum$n.cum <- cumsum(building.sum$n)/sum(building.sum$n)
# reverse cumulative. For example, 
# the third value in this case would be the sum of the third, fourth, fifth, …, last values in the original vector.
building.sum$n.rcum <- rcumsum(building.sum$n)/sum(building.sum$n)
# Fix manually the maximum value of y-axis
ymax1 <- 10
# plot
p3<-ggplot(data=building.sum,aes(x=range)) + 
  geom_col(aes(y=n),alpha=0.8) +
  geom_line(aes(x=range,y=n.rcum*ymax1), col="brown2", alpha = 0.9, lwd=1)+
  scale_y_continuous(name = 'Number of buildings',breaks = seq(0,10,2),
                     sec.axis = sec_axis(~./ymax1*100, 
                                         name = "Cumulative percentage of buildings (%)",
                                         breaks = seq(0,100,20))) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  geom_hline(aes(yintercept = 8),linetype="dashed") +
  geom_point(aes(x=1.9,y=8),size=5, color = "brown2") +
  xlab("Acceptable temperature range (K) by method 1") +
  theme_bw() +
  theme(text=element_text(size=16,  family="sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# method 2
db.t <- db[, c("Thermal.sensation","Air.temperature..C.","Publication","City","Building.type","Cooling.strategy.building.level")]
db.t$T.neutral <- db.t$Air.temperature..C. - db.t$Thermal.sensation/0.4
db.t <- db.t[complete.cases(db.t$T.neutral), ]
# calculate the comfort temeprature range for each "building"
building.sum <- db.t %>%
  group_by(Publication, City, Building.type, Cooling.strategy.building.level) %>%
  summarise(range = round(quantile(T.neutral,0.9) - quantile(T.neutral,0.1),0))
# remove the rows that range = 0 (the buidling had only 1 or 2 measurements)
building.sum <- building.sum[building.sum$range != 0, ] # 178 "buildings" remaining
# calculate for each widening range, how many buidlings (%) meets the range (cumulative)
# Sorting x data
building.sum$range <- sort(building.sum$range)
# frequency of ranges
building.sum <- building.sum %>%
  group_by(range) %>%
  summarise(n = n())
# cumulative percentage of buildings
building.sum$n.cum <- cumsum(building.sum$n)/sum(building.sum$n)
# reverse cumulative. For example, 
# the third value in this case would be the sum of the third, fourth, fifth, …, last values in the original vector.
building.sum$n.rcum <- rcumsum(building.sum$n)/sum(building.sum$n)
# Fix manually the maximum value of y-axis
ymax2 <- 50
# plot
p4<-ggplot(data=building.sum,aes(x=range)) + 
  geom_col(aes(y=n),alpha=0.8) +
  geom_line(aes(x=range,y=n.rcum*ymax2), col="brown2", alpha = 0.9, lwd=1)+
  scale_y_continuous(name = 'Number of buildings',breaks = seq(0,50,10),
                     sec.axis = sec_axis(~./ymax2*100, 
                                         name = "Cumulative percentage of buildings (%)",
                                         breaks = seq(0,100,20))) +
  scale_x_continuous(breaks = seq(0,14,2)) +
  geom_hline(aes(yintercept = 40),linetype="dashed") +
  geom_point(aes(x=5.9,y=40),size=5, color = "brown2") +
  xlab("Acceptable temperature range (K) by method 2") +
  theme_bw() +
  theme(text=element_text(size=16,  family="sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

threshold <- ggarrange(p3,p4, 
                     labels = c("(a)", "(b)"),
                     ncol = 2, nrow = 1)

ggsave(filename = "threshold.jpeg",plot=threshold, width = 12, height = 5)

#### method 2 compare to standards ####

# dataset to analyze
db.t <- db %>%
  filter(is.na(Thermal.sensation) == FALSE) %>%
  filter(is.na(Air.temperature..C.) == FALSE) %>%
  filter(Season == "Winter" | Season == "Summer") %>%
  filter(Building.type != "Others") %>%
  filter(Building.type != "Senior center")

# calculate the neutral temperature, Griffiths constant = 0.4, T.neutral = T - TSV/G.
db.t$T.neutral <- db.t$Air.temperature..C. - db.t$Thermal.sensation/0.4

cdb.t <- db.t %>%
  group_by(Building.type, Season) %>%
  summarise(tenth = round(quantile(T.neutral,0.1),1),
            nintyth = quantile(T.neutral,0.9),
            median = round(median(T.neutral),1),
            range = nintyth - tenth,
            n=n(),
            studies=length(unique(Publication)))

p<- ggplot(db.t, aes(y = T.neutral, x=Season,fill=Season)) +
  stat_summary(geom = "boxplot",
               fun.data = function(x) setNames(quantile(x, c(0, 0.1, 0.5, 0.9, 1)), c("ymin", "lower", "middle", "upper", "ymax"))) +
  scale_y_continuous(limits = c(10, 40)) +
  facet_grid(~Building.type)+
  scale_fill_manual(values=c("tan2","skyblue")) +
  geom_text(data = cdb.t, aes(y = median + 1, label = median),size=5) +
  geom_text(data = cdb.t, aes(y = 40, label = paste("range = ", range,"K")),size=5) +
  geom_text(data = cdb.t, aes(y = tenth - 1, label = tenth),size=5) +
  geom_text(data = cdb.t, aes(y = nintyth + 1, label = nintyth),size=5) +
  geom_text(data = cdb.t, aes(y = 38, label = paste("n records = ", n)),size=5) +
  geom_text(data = cdb.t, aes(y = 36, label = paste("n pubs = ", studies)),size=5) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        legend.position="none",
         text=element_text(size=16,  family="sans"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         panel.border = element_rect(fill="NA",colour = "black")) +
  labs( y = "Air Temperature (˚C)") +
  ggtitle("Acceptable air temperature (method 2 applied to the entire database)")
ggsave(filename = "method2_entire_database.jpeg",plot=p, width = 12, height =5)


#### Explore discrepency between standards and method 2 #####

# clo #

# dataset to analyze 
db.tt <- na.omit(db.t[, c("Clo","Building.type","Season")])

# count
count <- db.tt %>%
  group_by(Building.type, Season) %>%
  summarise(mean = round(mean(Clo),1),
            median = round(median(Clo),1),
            n = n())

# boxplot + violin T air in seasons with labels to show sample size and median
clo.plot<- ggplot(db.tt, aes(x=Season, y=Clo)) +
  geom_violin(aes(fill = Season)) +
  scale_fill_manual(values=c("tan2","skyblue")) +
  geom_boxplot(width=0.3, outlier.size = 0) +
  geom_text(data = count, aes(x = Season, y = 3, label = paste("n = ", n)),size=5) +
  geom_text(data = count, aes(x = Season, y = 2.8, label = paste("mean = ", mean)),size=5) +
  geom_text(data = count, aes(x = Season, y = 2.6, label = paste("median = ", median)),size=5) +
  stat_summary(fun.y=mean, geom="point", alpha=0.7, size=5, color="orchid3") + # show mean by diamond point
  facet_grid(~Building.type) +
  theme_bw() +
   theme(axis.title.x=element_blank(),
         legend.position="none",
         text=element_text(size=16,  family="sans"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         strip.background = element_blank(),
         panel.border = element_rect(fill="NA",colour = "black")) +
  labs(x = "Season", y = "Clo", fill = "Season") 



# Air speed #

# dataset to analyze 
db.tt <- na.omit(db.t[, c("Air.velocity..m.s.","Building.type","Season")])

# count
count <- db.tt %>%
  group_by(Building.type, Season) %>%
  summarise(mean = round(mean(Air.velocity..m.s.),2),
            median = round(median(Air.velocity..m.s.),2),
            n = n())

# boxplot + violin T air in seasons with labels to show sample size and median
va.plot <- ggplot(db.tt, aes(x=Season, y=Air.velocity..m.s.)) +
  geom_violin(aes(fill = Season)) +
  scale_fill_manual(values=c("tan2","skyblue")) +
  scale_y_continuous(limits = c(0,2), breaks = seq(0,2,1))+
  geom_boxplot(width=0.3, outlier.size = 0) +
  geom_text(data = count, aes(x = Season, y = 2, label = paste("n = ", n)),size=5) +
  geom_text(data = count, aes(x = Season, y = 1.85, label = paste("mean = ", mean)),size=5) +
  geom_text(data = count, aes(x = Season, y = 1.7, label = paste("median = ", median)),size=5) +
  stat_summary(fun.y=mean, geom="point", alpha=0.7, size=5, color="orchid3") + # show mean by diamond point
  facet_grid(~Building.type) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        text=element_text(size=16,  family="sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(fill="NA",colour = "black")) +
  labs(x = "Season", y = "Air velocity (m/s)", fill = "Season")

# combine two plots into one figure
clo.va <- ggarrange(clo.plot, va.plot, 
          labels = c("(a)", "(b)"),
          ncol = 1, nrow = 2)
ggsave(filename = "clo_Va.jpeg",plot=clo.va, width = 10, height =8)


##### Method 2 by regions ####

# dataset to analyze
db.t <- db %>%
  filter(is.na(Thermal.sensation) == FALSE) %>%
  filter(is.na(Air.temperature..C.) == FALSE) %>%
  filter(Season == "Winter" | Season == "Summer") %>%
  filter(Building.type != "Others") %>%
  filter(Building.type != "Senior center")

# classify the countries to European and non-European
db.t$Region <- with(db.t, ifelse(Country == "Denmark" |Country == "France"|Country == "Germany"|Country == "Greece"|Country == "Portugal"|Country == "Italy"|Country == "Slovakia"|Country == "Sweden"|Country == "United Kingdom"|Country == "UK","Europe",
                               ifelse(Country == "China" | Country == "India"|Country == "Indonesia"|Country == "Iran"|Country == "Japan"|Country == "Malaysia"|Country == "Pakistan"|Country == "Philippines"|Country == "Singapore"|Country == "South Korea"|Country == "Thailand", "Asia",
                                      ifelse(Country == "USA"|Country == "United States"|Country == "Canada","USA & Canada",
                                             ifelse(Country == "Australia","Australia", "Others")))))
db.t$Region <- as.factor(db.t$Region)



# focus on Asia and Europe
db.t <- db.t %>%
  filter(Region == "Europe" | Region == "Asia")


# calculate the neutral temperature, Griffiths constant = 0.4, T.neutral = T - TSV/G.
db.t$T.neutral <- db.t$Air.temperature..C. - db.t$Thermal.sensation/0.4

cdb.t <- db.t %>%
  group_by(Region, Building.type, Season) %>%
  summarise(tenth = round(quantile(T.neutral,0.1),1),
            nintyth = round(quantile(T.neutral,0.9),1),
            median = round(median(T.neutral),1),
            range = nintyth - tenth,
            n=n(),
            studies=length(unique(Publication)))

#plot
p<-ggplot(db.t, aes(y = T.neutral, x=Season,fill=Season)) +
  stat_summary(geom = "boxplot",
               fun.data = function(x) setNames(quantile(x, c(0, 0.1, 0.5, 0.9, 1)), c("ymin", "lower", "middle", "upper", "ymax"))) +
  scale_y_continuous(limits = c(10, 40)) +
  facet_grid(Region~Building.type)+
  scale_fill_manual(values=c("tan2","skyblue")) +
  geom_text(data = cdb.t, aes(y = median + 1, label = median),size=5) +
  geom_text(data = cdb.t, aes(y = 40, label = paste("range = ", range,"K")),size=5) +
  geom_text(data = cdb.t, aes(y = tenth - 1, label = tenth),size=5) +
  geom_text(data = cdb.t, aes(y = nintyth + 1, label = nintyth),size=5) +
  geom_text(data = cdb.t, aes(y = 38, label = paste("n records = ", n)),size=5) +
  geom_text(data = cdb.t, aes(y = 36, label = paste("n pubs = ", studies)),size=5) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        legend.position="none",
        text=element_text(size=16,  family="sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(fill="NA",colour = "black")) +
  labs( y = "Air Temperature (˚C)") +
  ggtitle("Acceptable air temperature (method 2 applied to Asia and Europe)")
ggsave(filename = "method2_region.jpeg",plot=p, width = 12, height =10)
