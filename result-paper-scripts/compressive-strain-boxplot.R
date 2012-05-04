library(RSvgDevice)
library(ggplot2)

# Set working directory (hard coded)
setwd("/Users/jsm/Documents/dev/skintwitch/result-paper-scripts")

# Read the CSV file containing minimum principal (compressive) strains
csvdata = read.csv("../output/csv/min-prin-strains.csv")

# Drop girthline data (and re-order the levels)
levels(csvdata$Trial) <- c("Control", "T6", "T11", "T16", "Girthline",
    "G1", "G2", "G3")
csvdata = subset(csvdata, 
    (!csvdata$Trial == "Girthline" &
     !csvdata$Trial == "G1" &
     !csvdata$Trial == "G2" &
     !csvdata$Trial == "G3"))

# ANOVA
# This is a one-way ANOVA, with a Tukey HSD post-hoc test
anv = aov(NStrain ~ Trial, data = csvdata)
print(summary(anv))
print(TukeyHSD(anv))
# In case the data is not Normal, we'll also do a Kruskal-Wallis ANOVA and a 
#  Wilcoxon rank sum test
print(kruskal.test(csvdata))
print(pairwise.wilcox.test(csvdata$NStrain, csvdata$Trial))

# Box plot
fontSize = 12
q <- qplot(csvdata$Trial, csvdata$NStrain, data = csvdata, geom="boxplot",
    xlab="Trial Location / Control",
    ylab="Normalized Maximum Compressive Strain\n(dimensionless)") + 
    theme_bw(base_size=fontSize) +
    geom_boxplot(outlier.shape=16, outlier.size=2) +
    ylim(-0.15001, 3) +
    opts(axis.title.x = theme_text(vjust=-0.9, size=fontSize)) +
    opts(axis.text.x = theme_text(angle=90, size=fontSize, hjust=1)) +
    opts(axis.title.y = theme_text(angle=90, hjust=0.5, vjust=0.35, size=fontSize)) +
    opts(panel.border = theme_rect(fill = NA, colour="black"))
print(q)

# Save the box plot as a PDF file
ggsave(plot=q, filename="compressive-strain-boxplot.pdf", width=5, height=5)
embedFonts(file="compressive-strain-boxplot.pdf")
