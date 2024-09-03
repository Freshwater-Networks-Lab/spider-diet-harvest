Rawcommunity<-read.csv("community.csv")

#subset data set

subcommunity <- Rawcommunity[,-(1:7)]
head(subcommunity)                         # Before the change
rownames(subcommunity) <- subcommunity$Taxon  # Set the row names
head(subcommunity)                         # New row names before taxon column
subcommunity$Taxon <- NULL               # Remove the taxon column
head(subcommunity)                         # After the change

#transpose data

transcommunity <- t(subcommunity)

# Convert the transposed matrix back to a data frame
transcommunity <- as.data.frame(transcommunity)

# Add a column label to the first variable (sample)
transcommunity <- cbind(Sample = rownames(transcommunity), transcommunity)

# Reset the row names since they are now included as the "Species" column
rownames(transcommunity) <- NULL

# View the final transposed data
head(transcommunity)

# now to seperate the string into seperatre variables
#add in stringr package 
#install.packages("stringr")
library(stringr)
citation("stringr")
citation("dplyr")

transcommunity$Season <- str_sub(transcommunity$Sample, 1, 1)
transcommunity$Crop <- str_sub(transcommunity$Sample, 2, 2)
transcommunity$Repition <- str_sub(transcommunity$Sample, 4, 5)
transcommunity$Date <- str_sub(transcommunity$Sample, 7, 12)
head(transcommunity)

summary(transcommunity)

transcommunity$Sample<-as.factor(transcommunity$Sample)
transcommunity$Season<-as.factor(transcommunity$Season)
transcommunity$Crop<-as.factor(transcommunity$Crop)
transcommunity$Repition<-as.factor(transcommunity$Repition)
transcommunity$Date<-as.factor(transcommunity$Date)

summary(transcommunity)

working_data<- transcommunity
summary(working_data)

write.csv(working_data, "communityabundance.csv")

#####models#####
install.packages('mvabund')
library(mvabund)
citation("mvabund")

# Subset species abundance data
# 
species_collumns <- c("Pterostichus", "Harpalus", "Anchomenus_dorsalis", "Loricera_pilicornis", 
                     "Thalassophilus", "Bembidion", "Amara_sensu_stricto", "Amara", 
                     "Nebria_rufescens", "Platyderus_depressus", "Caribinae", "Caribidae", 
                     "Staphylinidae", "Phalacridae", "Curculionidae", "Coccinella_septempunctata",
                     "Chrysomelidae", "Phyllotreta", "Cryptophagidae", "Silvanidae", "Elateridae", 
                     "Coleoptera", "Braconidae", "Ichneumonidae_gelis", "Ichneumonidae", 
                     "Platygastridae", "Formicidae", "Lasius_flavus", "Hymenoptera", "Aphibidae",
                     "Cicadellidae", "Diptera", "Culicidae", "Tachinidae", "Syrphidae", 
                     "Brachycera", "Keroplatidae", "Muscidae", "Dolichopodidae", "Chironomidae",
                     "Stratiomyidae", "Linyphiidae", "Lycosidae", "Tetragnathidae", 
                     "Lithobiomorpha", "Lepidoptera", "Thripidae", "Collembola", "Lubricina", 
                     "Chrysopidae")

# Extract species abundance data
species_abundance <- working_data[,species_collumns]

# Convert species abundance data to an mvabund object
species_mvabund <- mvabund(species_abundance)

# View the structure of the mvabund object
summary(species_mvabund)

#create a multivariate model 
mlv1 <- manyglm(species_mvabund ~ Season + Date + Crop + Repition, data = working_data)

mlv2 <- manyglm(species_mvabund ~ Season, data = working_data)

mlv3 <- manyglm(species_mvabund ~ Date, data = working_data)

mlv4 <- manyglm(species_mvabund ~ Crop, data = working_data, family = "negative.binomial")

mlv5 <- manyglm(species_mvabund ~ Crop*Season, data = working_data)

#use this model 

mlv6 <- manyglm(species_mvabund ~ Crop*Season*Date, data = working_data, family = "negative.binomial")



# Summarry of models
summary(mlv1)
summary(mlv2)
summary(mlv3)

summary.manyglm(mlv4)

summary.manyglm(mlv5)
summary.manyglm(mlv6)

#predictions - work 
predict(mlv4, type = "response")

#assumptions for model 4
plot(mlv6) # minimal pattern, suggesting the quadratic mean-variance assumption is likely met for this family
#negative bionomial seems to fit the data fine

#anova of model
anova.manyglm(mlv2)
anova.manyglm(mlv4, p.uni = "adjusted") # provides results for seperate taxa
anova.manyglm(mlv6, p.uni = "adjusted")



#plots
plot(species_mvabund ~ working_data$Season, col = as.factor(working_data$Crop))
plot(species_mvabund ~ working_data$Crop, col = as.factor(working_data$Sample))

#####pivot longer####

#pivot longer function, potentially dplyr, to produce long form table, and then can pull out specific trends and plot in ggplot2
install.packages("tidyr")

library(tidyr)
citation("tidyr")



long_data <- working_data %>%
  pivot_longer(
    cols = -c(Sample, Season, Crop, Repition, Date),  # Exclude non-species columns
    names_to = "Species",
    values_to = "Abundance")

long_data$Species <- gsub("_", " ", long_data$Species)

# Print the plot
print(plot)


ggplot(aes(x=Crop, y = Abundance, fill = Crop), data =long_data) +
  geom_boxplot() +
  facet_wrap(~Species, scales = "free_y") +
  labs(x = "Crop", y = "Abundance") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") +
  theme_minimal() 

ggplot(aes(x=Crop, y = Abundance), data =long_data) +
  geom_violin() +
  facet_wrap(~Species, scales = "free_y") +
  labs(x = "Crop", y = "Abundance")


ggplot(aes(x=Season, y = Abundance), data =long_data) +
  geom_boxplot() +
  facet_wrap(~Species,  scales = "free_y") +
  labs(x = "Season", y = "Abundance")

ggplot(aes(x=Season, y = Abundance), data =long_data) +
  geom_violin() +
  facet_wrap(~Species,  scales = "free_y") +
  labs(x = "Season", y = "Abundance")


ggplot(aes(x=Date, y = Abundance), data =long_data) +
  geom_boxplot() +
  facet_wrap(~Species,scales = "free_y") +
  labs(x = "Date", y = "Abundance")

ggplot(aes(x=Date, y = Abundance), data =long_data) +
  geom_violin() +
  facet_wrap(~Species,scales = "free_y") +
  labs(x = "Date", y = "Abundance")



#####NMDS ian/fred####
# The lattice package may be useful for plotting. It doesn't need to be 
#   installed - just loaded into memory
library(lattice)


# The following will need installing before first use (with install.packages)
library(ggrepel)
library(vegan); library(ggplot2); library(dplyr)

#### Ordination with NMDS ####

# Run NMDS.
#   Abundance data are often strongly right-skewed: 'log1p' log-transforms 
#       the abundance data (adding 1 first to cope with zero values)
#   'distance = "bray"' uses the Bray-Curtis dissimilarities
#   'k = 2' creates a 2D scatter plot
#   'set.seed' to make this reproducible
set.seed(1)
mds.1 <- metaMDS(log1p(working_data[, 2:51]), distance = "bray",
                 k = 2, trymax = 100)
# Plot the relationship between actual distances and NMDS distances
stressplot(mds.1)

# What does the stress look like (>0.2 is bad, below 0.15 is acceptable)
mds.1$stress
#stress 0.1942247 =<0.2


# Plot 1: site scores using vegan's built in plotting functions. 
#         Each point = 1 sample
plot(mds.1, display = "sites")

ordihull(mds.1,                          # NMDS object
         groups = working_data$Season,       # Grouping variable
         label = TRUE,                   # Add in group labels
         draw = "polygon", alpha = 120,  # Draw semi-transparent polygons
         col = c("#66CD00", "#4876FF"))  # Set the colours for the groups


plot(mds.1, display = "sites")

ordihull(mds.1,                          # NMDS object
         groups = working_data$Crop,       # Grouping variable
         label = TRUE,                   # Add in group labels
         draw = "polygon", alpha = 120,  # Draw semi-transparent polygons
         col = c("#FFC1", "#FF4500"))  # Set the colours for the groups



######species names#####

#   Abbreviate the species' names e.g.
#   Save species names and scores 
spp <- data.frame(scores(mds.1, display = "species"))
head(spp)


# Create abbreviated names (first 5 characters)
spp$short.names <- substr(rownames(spp), start = 1, stop = 5)
spp$long.names <- rownames(spp)
spp$long.names <- gsub("_", " ", spp$long.names)

#######fred plots use these#######

crop_conv_hull_plot <- working_data.nmds %>%
  group_by(Crop) %>%  # Change `site` to whatever factor you want to group by
  slice(chull(NMDS1, NMDS2)) %>%
  ungroup()

#create plot function
plot <- ggplot(working_data.nmds, aes(x = NMDS1, y = NMDS2)) + 
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = Crop),
               alpha = 0.5, data = crop_conv_hull_plot) + 
  geom_text_repel(aes(x = NMDS1, y = NMDS2, label = long.names),
            size = 3, data = spp) +
  theme_bw() +  
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme(legend.text = element_text(size = 8), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 8, colour = "black"), 
        legend.position = "right", 
        panel.border = element_rect(colour = "black", fill = NA, linewidth =0.5), 
        legend.title = element_blank(), 
        legend.background = element_blank(), 
        title = element_text(size=10), legend.key.height = unit(2, "mm"))
plot

#plot by season
Season_conv_hull_plot <- working_data.nmds %>%
  group_by(Season) %>%  # Change to whatever factor you want to group by
  slice(chull(NMDS1, NMDS2)) %>%
  ungroup()

#create plot function

plot1 <- ggplot(working_data.nmds, aes(x = NMDS1, y = NMDS2)) + 
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = Season),
               alpha = 0.5, data = Season_conv_hull_plot) + 
  geom_text(aes(x = NMDS1, y = NMDS2, label = long.names),
            size = 3, data = spp) +
  theme_bw() +  
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme(legend.text = element_text(size = 8), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 8, colour = "black"), 
        legend.position = "right", 
        panel.border = element_rect(colour = "black", fill = NA, linewidth =0.5), 
        legend.title = element_blank(), 
        legend.background = element_blank(), 
        title = element_text(size=10), legend.key.height = unit(2, "mm"))

plot1

#plot by date
date_conv_hull_plot <- working_data.nmds %>%
  group_by(Date) %>%  # Change to whatever factor you want to group by
  slice(chull(NMDS1, NMDS2)) %>%
  ungroup()

#create plot function 

plot2 <- ggplot(working_data.nmds, aes(x = NMDS1, y = NMDS2)) + 
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = Date),
               alpha = 0.5, data = date_conv_hull_plot) + 
  geom_text(aes(x = NMDS1, y = NMDS2, label = long.names),
            size = 3, data = spp) +
  theme_bw() +  
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme(legend.text = element_text(size = 8), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 8, colour = "black"), 
        legend.position = "right", 
        panel.border = element_rect(colour = "black", fill = NA, linewidth =0.5), 
        legend.title = element_blank(), 
        legend.background = element_blank(), 
        title = element_text(size=10), legend.key.height = unit(2, "mm"))

plot2


##### species metric analysis####


# Add site scores to the data frame for diversity results (i.e. add these 
#    two columns to 'working data nmds'). The scores are called NMDS1 and NMDs2, and then need to add in abundance
working_data.nmds <- cbind(working_data, scores(mds.1, display = "sites"))
head(working_data.nmds)


# Calculate species richness using the 'specnumber' function. Only richness 
#    could be calculated if we had presence-absence data.
working_data.nmds$richness <- specnumber(working_data.nmds[, 2:51])
summary(working_data.nmds)


# 1. Total abundance is simply the row sum for the columns containing the 

working_data.nmds$Abundance <- rowSums(working_data.nmds[, 2:51])
summary(working_data.nmds)

# Point size can be use to display additional information and explore the
#  data more deeply e.g. total abundance
#size varying for abundance
ggplot(working_data.nmds, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(col = Crop, size = Abundance)) +
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = Crop),
               alpha = 0.5, data = crop_conv_hull_plot) +
  geom_text_repel(aes(x = NMDS1, y = NMDS2, label = long.names),
            size = 3, data = spp) + 
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme_bw() +
  theme(axis.text = element_text(size = 11))

ggplot(working_data.nmds, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(col = Season, size = Abundance)) +
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = Season),
               alpha = 0.5, data = Season_conv_hull_plot) +
  geom_text_repel(aes(x = NMDS1, y = NMDS2, label = long.names),
            size = 3, data = spp) + 
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme_bw() +
  theme(axis.text = element_text(size = 11))

ggplot(working_data.nmds, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(col = Date, size = Abundance)) +
  geom_polygon(aes(x = NMDS1, y = NMDS2, fill = Date),
               alpha = 0.5, data = date_conv_hull_plot) +
  geom_text_repel(aes(x = NMDS1, y = NMDS2, label = long.names),
            size = 3, data = spp) + 
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme_bw() +
  theme(axis.text = element_text(size = 11))


#  The plots below explore species richness and abundance
boxplot(working_data.nmds$richness ~ working_data.nmds$Crop, boxfill = "firebrick", 
        whisklty = 1, whisklwd = 2, whiskcol = "black", 
        staplelwd = 2, staplecol = "black", 
        xlab = "Crop", ylab = "Species richness", cex.lab = 1.5)

boxplot(working_data.nmds$richness ~ working_data.nmds$Season, boxfill = "firebrick", 
        whisklty = 1, whisklwd = 2, whiskcol = "black", 
        staplelwd = 2, staplecol = "black", 
        xlab = "Season", ylab = "Species richness", cex.lab = 1.5)

boxplot(working_data.nmds$richness ~ working_data.nmds$Date, boxfill = "firebrick", 
        whisklty = 1, whisklwd = 2, whiskcol = "black", 
        staplelwd = 2, staplecol = "black", 
        xlab = "Date", ylab = "Species richness", cex.lab = 1.5)

boxplot(working_data.nmds$Abundance ~ working_data.nmds$Crop, boxfill = "firebrick", 
        whisklty = 1, whisklwd = 2, whiskcol = "black", 
        staplelwd = 2, staplecol = "black", 
        xlab = "Crop", ylab = "Abundance", cex.lab = 1.5)

boxplot(working_data.nmds$Abundance ~ working_data.nmds$Season, boxfill = "firebrick", 
        whisklty = 1, whisklwd = 2, whiskcol = "black", 
        staplelwd = 2, staplecol = "black", 
        xlab = "Season", ylab = "Abundance", cex.lab = 1.5)

boxplot(working_data.nmds$Abundance ~ working_data.nmds$Date, boxfill = "firebrick", 
        whisklty = 1, whisklwd = 2, whiskcol = "black", 
        staplelwd = 2, staplecol = "black", 
        xlab = "Date", ylab = "Abundance", cex.lab = 1.5)
# Calculate the mean Abundance for each Date level
abunmeans <- tapply(working_data.nmds$Abundance, working_data.nmds$Date, mean)

# Add the means to the plot as red points
points(1:length(abunmeans), abunmeans, col = "white", pch = 19, cex = 1.5)

###models for species metrics




richmod<- glm(richness~ Crop*Date*Season, family = poisson, data = working_data.nmds)
richmod1<- glm(richness~ Crop*Date+Season*Crop+Season*Date, family = poisson, data = working_data.nmds)
richmod2<-  glm(richness~ Crop*Date+Season*Crop+Season+Date, family = poisson, data = working_data.nmds)
richmod3<-  glm(richness~ Crop , family = poisson (link = "identity"), data = working_data.nmds)
richmod4<-  glm(richness~ 1, family = poisson (link = "identity"), data = working_data.nmds)

AIC(richmod3, richmod4)
anova(richmod3, richmod4)
#models are not significantly different from one another. borderline though

summary.glm(richmod)
summary.lm(richmod3)
summary.glm(richmod3)
anova(richmod3)

plot(richmod3)



theta <- richmod3$deviance/richmod3$df.residual
theta # theta is very less than one, therefore data is not overdispersed

#check residuals 

devresid <- resid(richmod3, type = "deviance")
plot(devresid~richmod3$fitted.values)



devresid <- resid(richmod3, type = "deviance")
plot(devresid~richmod3$fitted.values)

#abundance - poisson model over dispersed theta = 10

abundmod <- glm(Abundance~ Crop*Date*Season, family = quasipoisson (link = "identity"), data = working_data.nmds)


summary.glm(abundmod)
summary.lm(abundmod)

anova(abundmod, test = "Chisq")
drop1(abundmod)

abundmod2<- glm(Abundance~ Crop*Date + Date*Season + Crop*Season, family = quasipoisson (link = "identity"), data = working_data.nmds)

anova(abundmod2)
drop1(abundmod2)
summary.lm(abundmod2)

abundmod3<- glm(Abundance~ Crop*Date + Date*Season , family = quasipoisson (link = "identity"), data = working_data.nmds)
anova(abundmod3)

abundmod4 <- glm(Abundance~ Crop*Date + Season , family = quasipoisson (link = "identity"), data = working_data.nmds)
anova(abundmod4)

abundmod5 <- glm(Abundance~ Crop + Date + Season , family = quasipoisson (link = "identity"), data = working_data.nmds)
anova(abundmod5)


abundmod6 <- glm(Abundance~ Crop + Date, family = quasipoisson (link = "identity"), data = working_data.nmds)
anova(abundmod6)

abundmod7<- glm(Abundance~ Crop , family = quasipoisson (link = "identity"), data = working_data.nmds)
anova(abundmod7)
summary.glm(abundmod7)
summary.lm(abundmod7)

plot(abundmod7) # no heteroscedacity, qq plot lots good, not covering cooks distance



