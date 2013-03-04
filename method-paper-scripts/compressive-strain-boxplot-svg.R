library(tikzDevice)
library(ggplot2)

# Set working directory (hard coded)
setwd("/Users/jsm/Documents/dev/skintwitch/method-paper-scripts")

# Set TikZ options
#standAlone = FALSE
fontSize = 12
#tf = file.path(getwd(), "compressive-strain-boxplot.tex")
#tikz(tf, standAlone = standAlone, width = 4, height = 4, pointsize=fontSize)
#options(tikzLatex = "xelatex")

# Read the CSV file containing minimum principal (compressive) strains
csvdata = read.csv("../method-paper-output/min-prin-strains.csv")

# Re-order the trial names
csvdata$Trial <- ordered(csvdata$Trial, levels=c("Control", "G1", "G2", "G3", 
    "T6", "T11", "T16"))

# Drop girthline data
csvdata = subset(csvdata, !csvdata$Trial == "Girthline")

# ANOVA
# This is a one-way ANOVA, with a Tukey HSD post-hoc test
anv = aov(Strain ~ Trial, data = csvdata)
print(summary(anv))
print(TukeyHSD(anv))
# The data does not look very Normal (most groups are quite skewed), so we'll
#  also do a Kruskal-Wallis ANOVA and a Wilcoxon rank sum test
print(kruskal.test(csvdata))
print(pairwise.wilcox.test(csvdata$Strain, csvdata$Trial))

# Box plot
q <- qplot(csvdata$Trial, csvdata$Strain, data = csvdata, geom="boxplot",
    xlab="Trial Location / Control",
    ylab="Maximum Compressive Strain\n(dimensionless)") + 
    theme_bw(base_size=fontSize) +
    ylim(-0.15001, 0.00) +
    opts(axis.title.x = theme_text(vjust=-0.9, size=fontSize)) +
    opts(axis.text.x = theme_text(angle=90, size=fontSize, hjust=1)) +
    opts(axis.title.y = theme_text(angle=90, hjust=0.5, vjust=0.35, size=fontSize)) +
    opts(panel.border = theme_rect(fill = NA, colour="black"))
print(q)
dev.off()

# Show the box plot
#if (standAlone) {
#    tools::texi2dvi(tf, pdf=TRUE)
#    system(paste(getOption("pdfviewer"), 
#        file.path(getwd(), "compressive-strain-boxplot.pdf")))
#}
ggsave(q, file="compressive-strain-boxplot.svg")
