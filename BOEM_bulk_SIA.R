
# Script to characterize variation in fish isotopic niche sizes and positions (i.e., Standard Ellipse Areas) via bulk muscle stable carbon (d13C) and nitrogen (d15N) isotope data. 

# Code was modified from: 
# -- supporting resources associated with the article: Jackson et al. 2011. Comparing isotopic niche widths among and within communities: SIBER – Stable Isotope Bayesian Ellipses in R. Journal of Animal Ecology, 80, 595-602.
# ---- including, vignettes provided at: https://github.com/AndrewLJackson/SIBER
# -- supporting resources associated with the article: Swanson et al. 2015. A new probabilistic method for quantifying <i>n</i> -dimensional ecological niches and niche overlap. Ecology, 96, 318-324.
# ---- including, vignettes provided at:https://github.com/mlysy/nicheROVER


##############################

## Load necessary packages
require(ggplot2)
require(SIBER)
require(dplyr)
require(tidyr)
require(ggpubr)
require(nicheROVER)

## Set your working directory
#setwd("C:/Users/...") # If on a PC
#setwd("/Users/...") # If on a Mac
#setwd("~/Desktop/KI_fish/data")

setwd("~/Library/CloudStorage/OneDrive-UNC-Wilmington/Fish Team/BOEM/BOEM Data Analysis/Sams Sandbox/")

## Load the data
SIA <- read.csv("boem_bulk_CN_data_250618.csv")
metadata <- read.csv("boem_sia_metadata_250618.csv")

#checking for dupliates
duplicated(metadata$fish.id)

fish.bulk <- merge(SIA,metadata, by ="fish.id") # merge two datasets by "fish.id" column

str(fish.bulk) # ensure data in correct formats. 

# data that will be assigned unique colors or shapes generally need to be a factor.
fish.bulk$fish.id <- as.factor(fish.bulk$fish.id) 
fish.bulk$species <- as.factor(fish.bulk$species) 
fish.bulk$site <- as.factor(fish.bulk$site)

# columns with numbers and "NA" are often initially assigned as "chr = character" or "int = integer (i.e., no decimal points)" but should be "num = numeric" (allows decimal points)
fish.bulk$weight.g <- as.numeric(fish.bulk$weight.g)
fish.bulk$tl.mm <- as.numeric(fish.bulk$tl.mm)
fish.bulk$sl.mm <- as.numeric(fish.bulk$sl.mm)
fish.bulk$fl.mm <- as.numeric(fish.bulk$fl.mm)

# manually check the data set to ensure modifications were applied correctly

# remove non-target species
fish.bulk <- subset(fish.bulk, species != "Lizardfish" & species != "Pigfish") 

# fix species names
levels(fish.bulk$species)[2] <- "Atlantic Croaker"

#fish.bulk$dist_cat <- factor(fish.bulk$dist_cat, level = c("Very Low", "Medium", "Very High"))
#fish.bulk$species <- factor(fish.bulk$species, level = c("LU.FULV", "CE.UROD", "LU.BOHA","CE.ARGU","CA.MELA","AP.FURC"))

### Bulk SIA summary statistics  -----------------------------------------------------------

# Species
fish.bulk %>% 
  group_by(species) %>%
  summarize(n = n(), meanTL = mean(tl.mm, na.rm = TRUE)/10,
            minTL = min(tl.mm, na.rm = TRUE)/10,
            maxTL = max(tl.mm, na.rm = TRUE)/10)



##############################

### Plot SIA versus size (electronic supplementary material, figure S7) -----------------------------------------------------------

ggplot(fish.bulk, aes(weight.g/10, d13C)) +
  geom_point(aes(fill=species), pch = 21, col = "black", size = 3, alpha = 0.75) + 
  #scale_fill_manual(values = c("#2A0BD9", "#ABF8FF", "#A60021"))+
  geom_smooth(method="lm", colour="black", alpha=0.25)+
  #stat_regline_equation(label.y = 4.6, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = ceiling(max(fish.bulk$d13C)), aes(label = after_stat(rr.label)), size = 3.25) + 
  theme_test() +
  #theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) +
  scale_y_continuous(breaks=c(seq(floor(min(fish.bulk$d13C)),ceiling(max(fish.bulk$d13C)),2)),limits=c(floor(min(fish.bulk$d13C)),ceiling(max(fish.bulk$d13C)))) +
  theme(legend.position = 'top') + 
  xlab("Weight (g)") +
  ylab(expression({delta}^13*C~('permil'))) +
  theme(axis.text = element_text(colour="black")) +
  theme(text=element_text(size=12)) +
  theme(axis.ticks.length=unit(2.5,"mm")) +
  theme(panel.border = element_rect(color = "black")) +
  theme(strip.text = element_text(face = "italic")) +
  guides(fill = guide_legend(title = "Species", title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~species, scales = "free_x", nrow=1) 


Nvsl<-ggplot(fish.bulk, aes(sl.mm/10, d15N)) +
  geom_point(aes(fill=species), pch = 21, col = "black", size = 3, alpha = 0.75) + 
  #scale_fill_manual(values = c("#2A0BD9", "#ABF8FF", "#A60021"))+
  geom_smooth(method="lm", colour="black", alpha=0.25)+
  #stat_regline_equation(label.y = 4.6, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = ceiling(max(fish.bulk$d15N)), aes(label = after_stat(rr.label)), size = 3.25) + 
  theme_test() +
  #theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) +
  scale_y_continuous(breaks=c(seq(floor(min(fish.bulk$d15N)),ceiling(max(fish.bulk$d15N)),2)),limits=c(floor(min(fish.bulk$d15N)),ceiling(max(fish.bulk$d15N)))) +
  theme(legend.position = 'none') + 
  xlab("Standard Length (cm)") +
  ylab(expression({delta}^15*N~('permil'))) +
  theme(axis.text = element_text(colour="black")) +
  theme(text=element_text(size=12)) +
  theme(axis.ticks.length=unit(2.5,"mm")) +
  theme(panel.border = element_rect(color = "black")) +
  theme(strip.text = element_text(face = "italic")) +
  guides(fill = guide_legend(title = "Species", title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~species, scales = "free_x", nrow=1) 


Cvsl<-ggplot(fish.bulk, aes(sl.mm/10, d13C)) +
  geom_point(aes(fill=species), pch = 21, col = "black", size = 3, alpha = 0.75) + 
  #scale_fill_manual(values = c("#2A0BD9", "#ABF8FF", "#A60021"))+
  geom_smooth(method="lm", colour="black", alpha=0.25)+
  #stat_regline_equation(label.y = 4.6, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = ceiling(max(fish.bulk$d13C)), aes(label = after_stat(rr.label)), size = 3.25) + 
  theme_test() +
  theme(axis.text.x = element_blank()) +
  scale_y_continuous(breaks=c(seq(floor(min(fish.bulk$d13C)),ceiling(max(fish.bulk$d13C)),2)),limits=c(floor(min(fish.bulk$d13C)),ceiling(max(fish.bulk$d13C)))) +
  theme(legend.position = 'top') + 
  xlab(" ") +
  ylab(expression({delta}^13*C~('permil'))) +
  theme(axis.text = element_text(colour="black")) +
  theme(text=element_text(size=12)) +
  theme(axis.ticks.length=unit(2.5,"mm")) +
  theme(panel.border = element_rect(color = "black")) +
  theme(strip.text = element_text(face = "italic")) +
  guides(fill = guide_legend(title = "Species", title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~species, scales = "free_x", nrow=1) 

ggarrange(Cvsl,Nvsl, ncol=1, nrow=2)


### Reshape dataset for SIBER analyses -----------------------------------------------------------

# Note the following numeric values will replace letter names as required for the SIBER analyses

fish.bulk.SEA <- fish.bulk
fish.bulk.SEA <- droplevels(fish.bulk.SEA) # drop unused levels
#fish.bulk.SEA$species <- factor(fish.bulk.SEA$species, level = c("LU.FULV", "CE.UROD", "LU.BOHA","CE.ARGU","CA.MELA","AP.FURC"))
levels(fish.bulk.SEA$species) <- c("1", "2", "3","4","5","6","7","8") # [1] "Atlantic Bumper"  "Atlantic Croaker"  "Moonfish"  "Pinfish" "Spot"  "Striped Anchovy"  "Thread Herring"   "Weakfish" 

fish.bulk.SEA$species <- as.character(fish.bulk.SEA$species)

unique(fish.bulk.SEA$species)
data("demo.siber.data")


# Subset and reorganize data for d13C values, d15N values, and species
fish.bulk.SEA <- fish.bulk.SEA[,c(7,10,16)]
#fish.bulk.SEA <- fish.bulk.SEA[,c(2,7,11,12)]
names(fish.bulk.SEA)[1:3] <- c("iso1","iso2","group") #community is species
fish.bulk.SEA$community <- 1
#fish.bulk.SEA <- fish.bulk.SEA[,c(1,2,4,3)] # reorder columns
#fish.bulk.SEA <- fish.bulk.SEA[with(fish.bulk.SEA, order(community, group)),] # reorder rows within columns

# Create separate datasets for each species
fish.bulk.SEA.ATB <- subset(fish.bulk.SEA, group == "1")
fish.bulk.SEA.CRO <- subset(fish.bulk.SEA, group == "2")
fish.bulk.SEA.MOF <- subset(fish.bulk.SEA, group == "3")
fish.bulk.SEA.PIN <- subset(fish.bulk.SEA, group == "4")
fish.bulk.SEA.SPO <- subset(fish.bulk.SEA, group == "5")
fish.bulk.SEA.STA <- subset(fish.bulk.SEA, group == "6")
fish.bulk.SEA.THH <- subset(fish.bulk.SEA, group == "7")
fish.bulk.SEA.WEF <- subset(fish.bulk.SEA, group == "8")



### Quantify isotopic niche sizes using SIBER -----------------------------------------------------------

# create SIBER objects for each species (used for plotting + niche overlap analysis)
siber.ATB <- createSiberObject(fish.bulk.SEA.ATB)
siber.CRO <- createSiberObject(fish.bulk.SEA.CRO)
siber.MOF <- createSiberObject(fish.bulk.SEA.MOF)
siber.PIN <- createSiberObject(fish.bulk.SEA.PIN)
siber.SPO <- createSiberObject(fish.bulk.SEA.SPO)
siber.STA <- createSiberObject(fish.bulk.SEA.STA)
siber.THH <- createSiberObject(fish.bulk.SEA.THH)
siber.WEF <- createSiberObject(fish.bulk.SEA.WEF)


# Full SIBER model (used for all other analyses)
siber.full <- createSiberObject(fish.bulk.SEA)


# > Biplots w/ SEAc -----------------------------------------------------------

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.

#palette(c("#2A0BD9","#ABF8FF","#A60021"))

#library(scales)
#show_col(hue_pal()(8))

palette(c("#F8756D80","#CD960080","#7CAE0080","#00BE6780","#00BFC480","#00A9FF80","#C77CFF80","#FF61CC80"))

par(mfrow=c(1,1))
plotSiberObject(siber.full,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hulls.args,
                bty = "o",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'values (permil)'),
                ylab = expression({delta}^15*N~'values (permil)'),
                y.limits = c(5,17),
                x.limits = c(-27,-14))

points(fish.bulk.SEA$iso1, fish.bulk.SEA$iso2, bg=fish.bulk.SEA$group, pch = c(rep(21,8)), cex = 1.2, lwd=1, col = c(rep("black",0)))

plotGroupEllipses(siber.full, m = NULL, n = 100, p.interval = NULL, lty = 1, lwd = 5) # NULL = 40% of data


# Create d13C-d15N biplots for each species. Add sample size-correct maximum likelihood standard ellipse areas (SEAc).

# Set color palette
#palette(c("#2A0BD9","#ABF8FF","#A60021"))
dev.off()

par(mfrow=c(2,4),mar=c(5,5,1,1))


min(fish.bulk$d13C)
max(fish.bulk$d13C)

min(fish.bulk$d15N)
max(fish.bulk$d15N)

# Create lists of plotting arguments to be passed onwards to each of the three plotting functions.
#community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
#group.ellipses.args  <- list(n = 100, p.interval = NULL, lty = 1, lwd = 2)
#group.hull.args      <- list(lty = 3, col = "black")

# Species-specific biplots with maximum likelihood standard ellipses (SEAc), which account for c. 40% of the data. 


palette(c("#F8756D","#CD9600","#7CAE00","#00BE67","#00BFC4","#00A9FF","#C77CFF","#FF61CC"))


ATB <- {
  plotSiberObject(siber.ATB,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.ATB$iso1, fish.bulk.SEA.ATB$iso2, bg="#F8756D80", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.ATB, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(A)"), italic(" Atlantic Bumper"), " (n = 25)")), adj = c(0,1))
}

CRO <- {
  plotSiberObject(siber.CRO,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.CRO$iso1, fish.bulk.SEA.CRO$iso2, bg="#CD960080", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.CRO, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(B)"), italic(" Atlantic Croaker"), " (n = 87)")), adj = c(0,1))
}

MOF <- {
  plotSiberObject(siber.MOF,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.MOF$iso1, fish.bulk.SEA.MOF$iso2, bg="#7CAE0080", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.MOF, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(C)"), italic(" Moonfish"), " (n = 9)")), adj = c(0,1))
}


PIN <- {
  plotSiberObject(siber.PIN,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.PIN$iso1, fish.bulk.SEA.PIN$iso2, bg="#00BE6780", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.PIN, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(D)"), italic(" Pinfish"), " (n = 35)")), adj = c(0,1))
}


SPO <- {
  plotSiberObject(siber.SPO,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.SPO$iso1, fish.bulk.SEA.SPO$iso2, bg="#00BFC480", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.SPO, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(E)"), italic(" Spot"), " (n = 34)")), adj = c(0,1))
}


STA <- {
  plotSiberObject(siber.STA,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.STA$iso1, fish.bulk.SEA.STA$iso2, bg="#00A9FF80", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.STA, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(F)"), italic(" Striped Anchovy"), " (n = 9)")), adj = c(0,1))
}



THH <- {
  plotSiberObject(siber.THH,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.THH$iso1, fish.bulk.SEA.THH$iso2, bg="#C77CFF80", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.THH, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(G)"), italic(" Threadfin Herring"), " (n = 59)")), adj = c(0,1))
}


WEF <- {
  plotSiberObject(siber.WEF,
                  ax.pad = 2, 
                  hulls = F, community.hulls.args, 
                  ellipses = F, group.ellipses.args,
                  group.hulls = F, group.hull.args,
                  bty = "o",
                  iso.order = c(1,2),
                  xlab = expression({delta}^13*C~('permil')),
                  ylab = expression({delta}^15*N~('permil')),
                  y.limits = c(5,17),
                  x.limits = c(-27,-14))
  points(fish.bulk.SEA.WEF$iso1, fish.bulk.SEA.WEF$iso2, bg="#FF61CC80", pch = 21, cex = 1.5, lwd=1, col = c("black"))
  plotGroupEllipses(siber.WEF, m= NULL , n = 100, p.interval = NULL, lty = 1, lwd = 4)
  text(-27, 17, expression(paste(bold("(H)"), italic(" Weakfish"), " (n = 9)")), adj = c(0,1))
}




# Calculate summary statistics for each group: TA, SEA and SEAc
# TA: convex hull area
# SEA: maximum likelihood standard ellipse area
# SEAc: sample size-corrected maximum likelihood standard ellipse area

group.ML <- groupMetricsML(siber.full)


print(group.ML)


# > Fit Bayesian models to the data -----------------------------------------------------------

# The initial step for comparing compare isotopic niche width among groups is to fit Bayesian multivariate normal distributions to each group in the dataset. 

# set parameters defining how the sampling algorithm is to run
parms <- list()
parms$n.iter <- 2 * 10^4  # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3  # discard the first set of values
parms$n.thin <- 10  # thin the posterior by this many
parms$n.chains <- 2  # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.full, parms, priors)


# calculate the SEA on the posterior distribution of covariance matrix for each group (i.e, Bayesian SEA or SEA-B) (Supplementary Table 3).
SEA.B <- siberEllipses(ellipses.posterior)

# calculate some credible intervals 
cr.p <- c(0.95,0.99) # vector of quantiles

# call to hdrcde:hdr using lapply()
(SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p,2))

# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
(SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T))


# > Plot SEA-B data (electronic supplementary material, figure S5) -------------------------------------------------------


palette(c("#F8756D","#CD9600","#7CAE00","#00BE67","#00BFC4","#00A9FF","#C77CFF","#FF61CC"))

par(mfrow=c(1,1),mar=c(5,5,1,1))

siberDensityPlot(SEA.B, xticklabels = c("Atlantic Bumper", "Atlantic Croaker", "Moonfish", "Pinfish", "Spot", "Striped Anchovy", "Thread Herring", "Weakfish"), 
                 xlab = c("Species"),
                 ylab = expression("Standard Ellipse Area " ('permil' ^2) ),
                 bty = "o",
                 las = 1, 
                 probs = c(95, 75, 50),
                 ylim = c(0,20)
)


# add colored points for SEAc values calculated earlier
points(1:ncol(SEA.B), group.ML[3,], col=c("#F8756D","#CD9600","#7CAE00","#00BE67","#00BFC4","#00A9FF","#C77CFF","#FF61CC"), pch = c(rep(16,8)), lwd = 2)

# add vertical lines between species and species labels 
#abline(v = c(3.5,6.5,9.5,12.5,15.5), col="black", lwd=1, lty=2)

text(1, 20, expression(italic("n = 25")))
text(2, 20, expression(italic("n = 163")))
text(3, 20, expression(italic("n = 35")))
text(4, 20, expression(italic("n = 61")))
text(5, 20, expression(italic("n = 34")))
text(6, 20, expression(italic("n = 9")))
text(7, 20, expression(italic("n = 59")))
text(8, 20, expression(italic("n = 9")))




