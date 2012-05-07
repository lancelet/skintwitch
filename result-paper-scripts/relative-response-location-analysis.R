library(ggplot2)
library(psych)

# Set working directory (hard coded)
setwd("/Users/jsm/Documents/dev/skintwitch/result-paper-scripts")

# Read CSV file containing tabulated relative location of the peak response
#  relative to the poke location for each trial.
csvdata = read.csv("../output/csv/response-locations-relative-to-pokes.csv")

# Multiply relative x and y coordinates by the size of the grid to obtain
#  positions in cm.
csvdata$RelX = csvdata$RelX * 70.0
csvdata$RelY = csvdata$RelY * 60.0

# Dump some descriptive statistics of the trials
print(describe.by(csvdata, csvdata$Site))

# Drop girthline, G1, G2, and G3 data (and re-order trials)
levels(csvdata$Site) <- c("T6", "T11", "T16", 
    "Control", "Girthline", "G1", "G2", "G3")
csvdata = subset(csvdata,
    (!csvdata$Site == "Girthline" &
     !csvdata$Site == "G1" &
     !csvdata$Site == "G2" &
     !csvdata$Site == "G3" &
     !csvdata$Site == "Control"))

# Create boxplots of X and Y data
relXPlot <- qplot(csvdata$Site, csvdata$RelX, data=csvdata, geom="boxplot",
    xlab="Trial Location / Control",
    ylab="X location relative to poke (cm)\n+ve is caudal") +
    theme_bw() + 
    coord_flip()
print(relXPlot)
relYPlot <- qplot(csvdata$Site, csvdata$RelY, data=csvdata, geom="boxplot",
    xlab="Trial Location / Control",
    ylab="Y location relative to poke (cm)\n+ve is ventral") +
    theme_bw()
print(relYPlot)

# One-way ANOVA, with Tukey HSD post-hoc test
print("Analysis of Variance: RelX")
anvx = aov(RelX ~ Site + Error(Horse/Site), data=csvdata)
print(summary(anvx))
print("Analysis of Variance: RelY")
anvy = aov(RelY ~ Site + Error(Horse/Site), data=csvdata)
print(summary(anvy))

# Test whether different categories have means different from zero
t6 = subset(csvdata, csvdata$Site == "T6")
t11 = subset(csvdata, csvdata$Site == "T11")
t16 = subset(csvdata, csvdata$Site == "T16")
print("T6 RelX")
print(t.test(t6$RelX, alternative=c("two.sided")))
print(describe(t6$RelX))
print("T6 RelY")
print(t.test(t6$RelY, alternative=c("two.sided")))
print(describe(t6$RelY))
print("T11 RelX")
print(t.test(t11$RelX, alternative=c("two.sided")))
print(describe(t11$RelX))
print("T11 RelY")
print(t.test(t11$RelY, alternative=c("two.sided")))
print(describe(t11$RelY))
print("T16 RelX")
print(t.test(t16$RelX, alternative=c("two.sided")))
print(describe(t16$RelX))
print("T16 RelY")
print(t.test(t16$RelY, alternative=c("two.sided")))
print(describe(t16$RelY))