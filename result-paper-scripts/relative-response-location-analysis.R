library(ggplot2)
library(psych)
library(ez)

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

# One-way ANOVA, multiple-comparisons
print("Analysis of Variance: RelX")
print(ezANOVA(data=csvdata, dv=.(RelX), wid=.(Horse), within=.(Site)))
print("Analysis of Variance: RelY")
print(ezANOVA(data=csvdata, dv=.(RelY), wid=.(Horse), within=.(Site)))

# No post-hoc testing; ANOVA not significant.