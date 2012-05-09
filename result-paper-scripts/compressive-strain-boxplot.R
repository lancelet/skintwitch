library(RSvgDevice)
library(ggplot2)
library(ez)

# Set working directory (hard coded)
setwd("/Users/jsm/Documents/dev/skintwitch/result-paper-scripts")

# Read the CSV file containing minimum principal (compressive) strains
csvdata = read.csv("../output/csv/min-prin-strains.csv")

# Drop girthline data (and re-order the levels)
levels(csvdata$Site) <- c("Control", "T6", "T11", "T16", "Girthline",
    "G1", "G2", "G3")
data <- subset(csvdata, 
    (!csvdata$Site == "Girthline" &
     !csvdata$Site == "G1" &
     !csvdata$Site == "G2" &
     !csvdata$Site == "G3"))

# ANOVA
# Here, we have a repeated-measures experiment.  Strain is the outcome
# (dependent) variable, which we think may depend upon Site.  Horse is a random
# variable (or subject), which may also affect the Strain.  For each horse at
# each site, we have multiple measures, and also possibly un-equal variances.
#  Strain = dependent variable
#  Horse  = subject identifier
#  Site   = within-subject variable
# ezANOVA performs:
#  - repeated measures ANOVA with Site as a within-Horse variable
#  - sphericity correction using Greenhouse-Geisser epsilon
# The sphericity correction and the Greenhouse-Geisser epsilon test for the
# problem of un-equal variance.
# After high statistical significance, we then go on to use a
# pairwise Wilcoxon Rank Sum test with Holm-Bonferroni probability adjustment 
# for multiple comparisons.
aov = ezANOVA(data=data, dv=.(Strain), wid=.(Horse), within=.(Site))
print(aov)
print(pairwise.wilcox.test(data$Strain, data$Site, "holm"))

# Box plot
fontSize = 12
q <- qplot(data$Site, data$Strain, data = data, geom="boxplot",
    xlab="Trial Location / Control",
    ylab="Maximum Compressive Strain\n(dimensionless)") + 
    theme_bw(base_size=fontSize) +
    geom_boxplot(outlier.shape=16, outlier.size=2) +
    ylim(-0.20, 0.0) +
    opts(axis.title.x = theme_text(vjust=-0.9, size=fontSize)) +
    opts(axis.text.x = theme_text(angle=90, size=fontSize, hjust=1)) +
    opts(axis.title.y = theme_text(angle=90, hjust=0.5, vjust=0.35, size=fontSize)) +
    opts(panel.border = theme_rect(fill = NA, colour="black"))
print(q)

# Save the box plot as a PDF file
ggsave(plot=q, filename="compressive-strain-boxplot.pdf", width=5, height=5)
embedFonts(file="compressive-strain-boxplot.pdf")
