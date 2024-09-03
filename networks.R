# setwd("") # Set working directory for files

##### Null network generation #####

library("tidyverse")
library("econullnetr")
source("nullnet/Functions/generate_null_net_indiv.R") # This is a custom function for outputting simulated interactions from econullnetr

prey <- read.csv("nullnet/communityabundance.csv") # This dataset is an example to show how it should be laid out. If you replace these taxa with the ones you've identified, keeping the 'ENNRcode' format (i.e., S for Spring, W for Winter, O for OSR, etc.), this should work with your data. You may need to exclude SWR1T3 and WWR1T3 though as they appear to have no spiders associated with them

spiders <- read.csv("nullnet/spiderinteractions2.csv") # This is just an example list of prey that is exactly the same as the prey in the previous file (you should just copy and paste the column headers across) but with 0s for all but one column. It doesn't really matter which - I went with spiders, but you could go with linyphiids - just put 1s all the way down. This just means each spider has at least one interaction (this is factored into the null network generation)

colnames(spiders)[3:ncol(spiders)]
colnames(prey)[2:ncol(prey)]
prey



prey<-prey[,-c(2:13,18, 32, 43, 44, 46, 50)]
spiders<-spiders[,-c(3:14, 19, 33, 44, 45, 47, 51)]

colnames(prey)[2:ncol(prey)]
colnames(spiders)[3:ncol(spiders)]
######code for subsetting####
#collumn names not matching

unique(spiders$ENNRcode) == unique(prey$ENNRcode)
common_samples <- intersect(prey[, 1], spiders[, 1])

#Subset both datasets to these common samples
prey <- prey[prey[, 1] %in% common_samples, ]
spiders <- spiders[spiders[, 1] %in% common_samples, ]

#spiders_sub <- spiders[spiders$ENNRcode %in% prey$ENNRcode,]


#####remove any non prey taxa i.e. caribidae ####



#####modelling #####
null_sim <- generate_null_net_indiv(spiders[,2:ncol(spiders)], prey[,2:ncol(prey)], # This runs the null networks - you can reduce the 'sims' to play around and test it to get it to run quicker
                                    sims = 999, data.type = "names",
                                    summary.type = "none",
                                    r.samples = prey[,1],
                                    c.samples = spiders[,1])


null.rand <- null_sim$rand.data # This takes forward just the null network, irrespective of the observed data (which is the file we loaded above with mostly 0s)

null.rand$indiv <- rep(seq(1:nrow(spiders)), null_sim$n.iterations) 
ind.cons.avg <- aggregate(null.rand[,3:(ncol(null.rand)-1)], by = list(null.rand$Consumer, null.rand$indiv), mean) # This averages out the different simulations for each individual spider to make an average range of interactions
colnames(ind.cons.avg)[1:2] <- c("Consumer", "Individual")

ind.cons.avg.int <- ind.cons.avg[3:ncol(ind.cons.avg)]

ind.cons.avg <- ind.cons.avg[1] %>%
  mutate(Sowing = case_when(str_starts(Consumer, "W") ~ "Winter", str_starts(Consumer, "S") ~ "Spring", TRUE ~ NA), Crop = case_when(str_sub(Consumer, 2, 2) == "O" ~ "OSR", str_sub(Consumer, 2, 2) == "W" ~ "Wheat", TRUE ~ NA), Round = str_sub(Consumer, 4, 4), Transect = str_sub(Consumer, 6, 6)) %>%
  cbind(ind.cons.avg.int) # This formats the independent variables to make it easier to do analyses downstream

write.csv(ind.cons.avg, "ENNRSimOut.csv") # This saves the interactions to your working directory

##### Network plots #####

# This first network diagram will be almost unusable, but is just a simpler example

null.df <- ind.cons.avg %>%
  pivot_longer(cols = 6:ncol(.), values_to = "Weight", names_to = "Resource") %>%
  select(Consumer, Resource, Weight, Sowing, Crop, Round, Transect) %>%
  group_by(Consumer, Resource, Sowing, Crop, Round, Transect) %>%
  summarise(Weight = sum(Weight)) # There was an error related to duplicate edges below, which this addresses

cons.node.df <- data.frame(node = unique(c(levels(as.factor(null.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
res.node.df <- data.frame(node = unique(c(levels(as.factor(null.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
node.df <- rbind(cons.node.df, res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
node.df[node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(node.df$tr.height == 0)))
node.df[node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(node.df$tr.height == 0.5)))

#install igraph package
#install.packages("igraph")
library(igraph)



null.net <- graph_from_data_frame(d = null.df, vertices = node.df, directed = TRUE)
plot(null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(null.net, layout = as.matrix(node.df[, c("x.coord", "tr.height")]))

#install.packages("ggnetwork")
library(ggnetwork)

null.net2 <- fortify(null.net, layout = as.matrix(node.df[, c("x.coord", "tr.height")]))

null.net2$tr.height <- (null.net2$tr.height - 1) / 2
null.net2$y <- null.net2$y / 2 -0.006
null.net2$yend <- null.net2$yend / 2 -0.006

ggplot(null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

nullnetbip <- ggplot(null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(linewidth = (abs(Weight)))) +
  geom_point(data = node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Overall network")
nullnetbip

png(file = "Overall network.png", width = 20, height = 20, res = 150, units = "cm")
nullnetbip
dev.off()

#### spring wheat round 1 transect 1######
SWR1T1.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 1, Transect == 1) %>%
  droplevels()

SWR1T1.null.sub.df <- subset(SWR1T1.null.df, Weight > 0)
SWR1T1.null.sub.df$weight<- SWR1T1.null.sub.df$Weight

SWR1T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR1T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR1T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR1T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR1T1.node.df <- rbind(SWR1T1.cons.node.df, SWR1T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR1T1.node.df[SWR1T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR1T1.node.df$tr.height == 0)))
SWR1T1.node.df[SWR1T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR1T1.node.df$tr.height == 0.5)))

SWR1T1.null.net <- graph_from_data_frame(d = SWR1T1.null.sub.df, vertices = SWR1T1.node.df, directed = TRUE)
plot(SWR1T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe


plot(SWR1T1.null.net, layout = as.matrix(SWR1T1.node.df[, c("x.coord", "tr.height")]))

SWR1T1.null.net2 <- fortify(SWR1T1.null.net, layout = as.matrix(SWR1T1.node.df[, c("x.coord", "tr.height")]))

SWR1T1.null.net2$tr.height <- (SWR1T1.null.net2$tr.height - 1) / 2
SWR1T1.null.net2$y <- SWR1T1.null.net2$y / 2 -0.006
SWR1T1.null.net2$yend <- SWR1T1.null.net2$yend / 2 -0.006

ggplot(SWR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR1T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR1T1.nullnetbip <- ggplot(SWR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR1T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 1 transect 1 network")
SWR1T1.nullnetbip

png(file = "Spring Wheat Round 1 transect 1network.png", width = 20, height = 20, res = 150, units = "cm")
SWR1T1.nullnetbip
dev.off

SWR1T1.strength <- data.frame(strength(SWR1T1.null.net))
SWR1T1.strength$taxon <- rownames(SWR1T1.strength)
SWR1T1.strength<-SWR1T1.strength[(nrow(SWR1T1.cons.node.df)+1):nrow(SWR1T1.strength),]

#### spring wheat round 1 transect 2######
SWR1T2.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 1, Transect == 2) %>%
  droplevels()

SWR1T2.null.sub.df <- subset(SWR1T2.null.df, Weight > 0)
SWR1T2.null.sub.df$weight<- SWR1T2.null.sub.df$Weight

SWR1T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR1T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR1T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR1T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR1T2.node.df <- rbind(SWR1T2.cons.node.df, SWR1T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR1T2.node.df[SWR1T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR1T2.node.df$tr.height == 0)))
SWR1T2.node.df[SWR1T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR1T2.node.df$tr.height == 0.5)))

SWR1T2.null.net <- graph_from_data_frame(d = SWR1T2.null.sub.df, vertices = SWR1T2.node.df, directed = TRUE)
plot(SWR1T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR1T2.null.net, layout = as.matrix(SWR1T2.node.df[, c("x.coord", "tr.height")]))

SWR1T2.null.net2 <- fortify(SWR1T2.null.net, layout = as.matrix(SWR1T2.node.df[, c("x.coord", "tr.height")]))

SWR1T2.null.net2$tr.height <- (SWR1T2.null.net2$tr.height - 1) / 2
SWR1T2.null.net2$y <- SWR1T2.null.net2$y / 2 -0.006
SWR1T2.null.net2$yend <- SWR1T2.null.net2$yend / 2 -0.006

ggplot(SWR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR1T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR1T2.nullnetbip <- ggplot(SWR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR1T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 1 transect 2 network")
SWR1T2.nullnetbip

png(file = "Spring Wheat Round 1 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR1T2.nullnetbip
dev.off()

SWR1T2.strength <- data.frame(strength(SWR1T2.null.net))
SWR1T2.strength$taxon <- rownames(SWR1T2.strength)
SWR1T2.strength<-SWR1T2.strength[(nrow(SWR1T2.cons.node.df)+1):nrow(SWR1T2.strength),]


#### spring wheat round 1 transect 3 (no spiders)#####
SWR1T3.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 1, Transect == 3) %>%
  droplevels()

SWR1T3.null.sub.df <- subset(SWR1T3.null.df, Weight > 0)
SWR1T3.null.sub.df$weight<- SWR1T3.null.sub.df$Weight

SWR1T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR1T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR1T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR1T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR1T3.node.df <- rbind(SWR1T3.cons.node.df, SWR1T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR1T3.node.df[SWR1T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR1T3.node.df$tr.height == 0)))
SWR1T3.node.df[SWR1T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR1T3.node.df$tr.height == 0.5)))

SWR1T3.null.net <- graph_from_data_frame(d = SWR1T3.null.sub.df, vertices = SWR1T3.node.df, directed = TRUE)
plot(SWR1T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR1T3.null.net, layout = as.matrix(SWR1T3.node.df[, c("x.coord", "tr.height")]))

SWR1T3.null.net2 <- fortify(SWR1T3.null.net, layout = as.matrix(SWR1T3.node.df[, c("x.coord", "tr.height")]))

SWR1T3.null.net2$tr.height <- (SWR1T3.null.net2$tr.height - 1) / 2
SWR1T3.null.net2$y <- SWR1T3.null.net2$y / 2 -0.006
SWR1T3.null.net2$yend <- SWR1T3.null.net2$yend / 2 -0.006

ggplot(SWR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR1T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR1T3.nullnetbip <- ggplot(SWR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR1T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 1 transect 3 network")
SWR1T3.nullnetbip

png(file = "Spring Wheat Round 1 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR1T3.nullnetbip
dev.off()

#strength
SWR1T3.strength <- data.frame(strength(SWR1T3.null.net))
SWR1T3.strength$taxon <- rownames(SWR1T3.strength)
SWR1T3.strength<-SWR1T3.strength[(nrow(SWR1T3.cons.node.df)+1):nrow(SWR1T3.strength),]

#### spring wheat round 2 transect 1 #####

SWR2T1.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 2, Transect == 1) %>%
  droplevels()

SWR2T1.null.sub.df <- subset(SWR2T1.null.df, Weight > 0)
SWR2T1.null.sub.df$weight<- SWR2T1.null.sub.df$Weight

SWR2T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR2T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR2T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR2T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR2T1.node.df <- rbind(SWR2T1.cons.node.df, SWR2T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR2T1.node.df[SWR2T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR2T1.node.df$tr.height == 0)))
SWR2T1.node.df[SWR2T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR2T1.node.df$tr.height == 0.5)))

SWR2T1.null.net <- graph_from_data_frame(d = SWR2T1.null.sub.df, vertices = SWR2T1.node.df, directed = TRUE)
plot(SWR2T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR2T1.null.net, layout = as.matrix(SWR2T1.node.df[, c("x.coord", "tr.height")]))

SWR2T1.null.net2 <- fortify(SWR2T1.null.net, layout = as.matrix(SWR2T1.node.df[, c("x.coord", "tr.height")]))

SWR2T1.null.net2$tr.height <- (SWR2T1.null.net2$tr.height - 1) / 2
SWR2T1.null.net2$y <- SWR2T1.null.net2$y / 2 -0.006
SWR2T1.null.net2$yend <- SWR2T1.null.net2$yend / 2 -0.006

ggplot(SWR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR2T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR2T1.nullnetbip <- ggplot(SWR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR2T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 2 t1 network")
SWR2T1.nullnetbip

png(file = "Spring Wheat Round 2 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR2T1.nullnetbip
dev.off()

#strength

SWR2T1.strength <- data.frame(strength(SWR2T1.null.net))
SWR2T1.strength$taxon <- rownames(SWR2T1.strength)
SWR2T1.strength<-SWR2T1.strength[(nrow(SWR2T1.cons.node.df)+1):nrow(SWR2T1.strength),]

#### spring wheat round 2 transect 2####
SWR2T2.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 2, Transect == 2) %>%
  droplevels()

SWR2T2.null.sub.df <- subset(SWR2T2.null.df, Weight > 0)
SWR2T2.null.sub.df$weight<- SWR2T2.null.sub.df$Weight

SWR2T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR2T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR2T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR2T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR2T2.node.df <- rbind(SWR2T2.cons.node.df, SWR2T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR2T2.node.df[SWR2T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR2T2.node.df$tr.height == 0)))
SWR2T2.node.df[SWR2T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR2T2.node.df$tr.height == 0.5)))

SWR2T2.null.net <- graph_from_data_frame(d = SWR2T2.null.sub.df, vertices = SWR2T2.node.df, directed = TRUE)
plot(SWR2T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR2T2.null.net, layout = as.matrix(SWR2T2.node.df[, c("x.coord", "tr.height")]))

SWR2T2.null.net2 <- fortify(SWR2T2.null.net, layout = as.matrix(SWR2T2.node.df[, c("x.coord", "tr.height")]))

SWR2T2.null.net2$tr.height <- (SWR2T2.null.net2$tr.height - 1) / 2
SWR2T2.null.net2$y <- SWR2T2.null.net2$y / 2 -0.006
SWR2T2.null.net2$yend <- SWR2T2.null.net2$yend / 2 -0.006

ggplot(SWR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR2T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR2T2.nullnetbip <- ggplot(SWR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR2T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 2 transect 2 network")
SWR2T2.nullnetbip

png(file = "Spring Wheat Round 2 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR2T2.nullnetbip
dev.off()

#strength
SWR2T2.strength <- data.frame(strength(SWR2T2.null.net))
SWR2T2.strength$taxon <- rownames(SWR2T2.strength)
SWR2T2.strength<-SWR2T2.strength[(nrow(SWR2T2.cons.node.df)+1):nrow(SWR2T2.strength),]

#### spring wheat round 2 transect 3#####
SWR2T3.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 2, Transect == 3) %>%
  droplevels()

SWR2T3.null.sub.df <- subset(SWR2T3.null.df, Weight > 0)
SWR2T3.null.sub.df$weight<- SWR2T3.null.sub.df$Weight

SWR2T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR2T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR2T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR2T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR2T3.node.df <- rbind(SWR2T3.cons.node.df, SWR2T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR2T3.node.df[SWR2T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR2T3.node.df$tr.height == 0)))
SWR2T3.node.df[SWR2T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR2T3.node.df$tr.height == 0.5)))

SWR2T3.null.net <- graph_from_data_frame(d = SWR2T3.null.sub.df, vertices = SWR2T3.node.df, directed = TRUE)
plot(SWR2T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR2T3.null.net, layout = as.matrix(SWR2T3.node.df[, c("x.coord", "tr.height")]))

SWR2T3.null.net2 <- fortify(SWR2T3.null.net, layout = as.matrix(SWR2T3.node.df[, c("x.coord", "tr.height")]))

SWR2T3.null.net2$tr.height <- (SWR2T3.null.net2$tr.height - 1) / 2
SWR2T3.null.net2$y <- SWR2T3.null.net2$y / 2 -0.006
SWR2T3.null.net2$yend <- SWR2T3.null.net2$yend / 2 -0.006

ggplot(SWR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR2T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR2T3.nullnetbip <- ggplot(SWR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR2T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 2 transect 3 network")
SWR2T3.nullnetbip

png(file = "Spring Wheat Round 2 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR2T3.nullnetbip
dev.off()

#strength
SWR2T3.strength <- data.frame(strength(SWR2T3.null.net))
SWR2T3.strength$taxon <- rownames(SWR2T3.strength)
SWR2T3.strength<-SWR2T3.strength[(nrow(SWR2T3.cons.node.df)+1):nrow(SWR2T3.strength),]


#### Spring Wheat Round 3 transect 1 #####

SWR3T1.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 3, Transect == 1) %>%
  droplevels()

SWR3T1.null.sub.df <- subset(SWR3T1.null.df, Weight > 0)
SWR3T1.null.sub.df$weight<- SWR3T1.null.sub.df$Weight

SWR3T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR3T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR3T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR3T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR3T1.node.df <- rbind(SWR3T1.cons.node.df, SWR3T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR3T1.node.df[SWR3T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR3T1.node.df$tr.height == 0)))
SWR3T1.node.df[SWR3T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR3T1.node.df$tr.height == 0.5)))

SWR3T1.null.net <- graph_from_data_frame(d = SWR3T1.null.sub.df, vertices = SWR3T1.node.df, directed = TRUE)
plot(SWR3T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR3T1.null.net, layout = as.matrix(SWR3T1.node.df[, c("x.coord", "tr.height")]))

SWR3T1.null.net2 <- fortify(SWR3T1.null.net, layout = as.matrix(SWR3T1.node.df[, c("x.coord", "tr.height")]))

SWR3T1.null.net2$tr.height <- (SWR3T1.null.net2$tr.height - 1) / 2
SWR3T1.null.net2$y <- SWR3T1.null.net2$y / 2 -0.006
SWR3T1.null.net2$yend <- SWR3T1.null.net2$yend / 2 -0.006

ggplot(SWR3T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR3T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR3T1.nullnetbip <- ggplot(SWR3T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR3T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 3 transect 1 network")
SWR3T1.nullnetbip

png(file = "Spring Wheat Round 3 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR3T1.nullnetbip
dev.off()

#strength
SWR3T1.strength <- data.frame(strength(SWR3T1.null.net))
SWR3T1.strength$taxon <- rownames(SWR3T1.strength)
SWR3T1.strength<-SWR3T1.strength[(nrow(SWR3T1.cons.node.df)+1):nrow(SWR3T1.strength),]


#### Spring Wheat Round 3 transect 2 ####
SWR3T2.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 3, Transect == 2) %>%
  droplevels()

SWR3T2.null.sub.df <- subset(SWR3T2.null.df, Weight > 0)
SWR3T2.null.sub.df$weight<- SWR3T2.null.sub.df$Weight

SWR3T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR3T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR3T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR3T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR3T2.node.df <- rbind(SWR3T2.cons.node.df, SWR3T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR3T2.node.df[SWR3T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR3T2.node.df$tr.height == 0)))
SWR3T2.node.df[SWR3T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR3T2.node.df$tr.height == 0.5)))

SWR3T2.null.net <- graph_from_data_frame(d = SWR3T2.null.sub.df, vertices = SWR3T2.node.df, directed = TRUE)
plot(SWR3T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR3T2.null.net, layout = as.matrix(SWR3T2.node.df[, c("x.coord", "tr.height")]))

SWR3T2.null.net2 <- fortify(SWR3T2.null.net, layout = as.matrix(SWR3T2.node.df[, c("x.coord", "tr.height")]))

SWR3T2.null.net2$tr.height <- (SWR3T2.null.net2$tr.height - 1) / 2
SWR3T2.null.net2$y <- SWR3T2.null.net2$y / 2 -0.006
SWR3T2.null.net2$yend <- SWR3T2.null.net2$yend / 2 -0.006

ggplot(SWR3T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR3T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR3T2.nullnetbip <- ggplot(SWR3T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR3T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 3 transect 2 network")
SWR3T2.nullnetbip

png(file = "Spring Wheat Round 3 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR3T2.nullnetbip
dev.off()

#strength

SWR3T2.strength <- data.frame(strength(SWR3T2.null.net))
SWR3T2.strength$taxon <- rownames(SWR3T2.strength)
SWR3T2.strength<-SWR3T2.strength[(nrow(SWR3T2.cons.node.df)+1):nrow(SWR3T2.strength),]

#### Spring Wheat Round 3 transect 3 ####
SWR3T3.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "Wheat", Round == 3, Transect == 3) %>%
  droplevels()

SWR3T3.null.sub.df <- subset(SWR3T3.null.df, Weight > 0)
SWR3T3.null.sub.df$weight<- SWR3T3.null.sub.df$Weight

SWR3T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SWR3T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SWR3T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(SWR3T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SWR3T3.node.df <- rbind(SWR3T3.cons.node.df, SWR3T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SWR3T3.node.df[SWR3T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SWR3T3.node.df$tr.height == 0)))
SWR3T3.node.df[SWR3T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SWR3T3.node.df$tr.height == 0.5)))

SWR3T3.null.net <- graph_from_data_frame(d = SWR3T3.null.sub.df, vertices = SWR3T3.node.df, directed = TRUE)
plot(SWR3T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SWR3T3.null.net, layout = as.matrix(SWR3T3.node.df[, c("x.coord", "tr.height")]))

SWR3T3.null.net2 <- fortify(SWR3T3.null.net, layout = as.matrix(SWR3T3.node.df[, c("x.coord", "tr.height")]))

SWR3T3.null.net2$tr.height <- (SWR3T3.null.net2$tr.height - 1) / 2
SWR3T3.null.net2$y <- SWR3T3.null.net2$y / 2 -0.006
SWR3T3.null.net2$yend <- SWR3T3.null.net2$yend / 2 -0.006

ggplot(SWR3T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SWR3T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SWR3T3.nullnetbip <- ggplot(SWR3T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SWR3T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring wheat round 3 transect 3 network")
SWR3T3.nullnetbip

png(file = "Spring Wheat Round 3 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
SWR3T3.nullnetbip
dev.off()

#strength
SWR3T3.strength <- data.frame(strength(SWR3T3.null.net))
SWR3T3.strength$taxon <- rownames(SWR3T3.strength)
SWR3T3.strength<-SWR3T3.strength[(nrow(SWR3T3.cons.node.df)+1):nrow(SWR3T3.strength),]


#### Winter Wheat Round 1 Transect 1 #####
WWR1T1.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 1, Transect == 1) %>%
  droplevels()

WWR1T1.null.sub.df <- subset(WWR1T1.null.df, Weight > 0)
WWR1T1.null.sub.df$weight<- WWR1T1.null.sub.df$Weight

WWR1T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR1T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR1T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR1T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR1T1.node.df <- rbind(WWR1T1.cons.node.df, WWR1T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR1T1.node.df[WWR1T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR1T1.node.df$tr.height == 0)))
WWR1T1.node.df[WWR1T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR1T1.node.df$tr.height == 0.5)))

WWR1T1.null.net <- graph_from_data_frame(d = WWR1T1.null.sub.df, vertices = WWR1T1.node.df, directed = TRUE)
plot(WWR1T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR1T1.null.net, layout = as.matrix(WWR1T1.node.df[, c("x.coord", "tr.height")]))

WWR1T1.null.net2 <- fortify(WWR1T1.null.net, layout = as.matrix(WWR1T1.node.df[, c("x.coord", "tr.height")]))

WWR1T1.null.net2$tr.height <- (WWR1T1.null.net2$tr.height - 1) / 2
WWR1T1.null.net2$y <- WWR1T1.null.net2$y / 2 -0.006
WWR1T1.null.net2$yend <- WWR1T1.null.net2$yend / 2 -0.006

ggplot(WWR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR1T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR1T1.nullnetbip <- ggplot(WWR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR1T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 1 transect 1 network")
WWR1T1.nullnetbip

png(file = "Winter Wheat Round 1 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR1T1.nullnetbip
dev.off()

#strength
WWR1T1.strength <- data.frame(strength(WWR1T1.null.net))
WWR1T1.strength$taxon <- rownames(WWR1T1.strength)
WWR1T1.strength<-WWR1T1.strength[(nrow(WWR1T1.cons.node.df)+1):nrow(WWR1T1.strength),]

#### winter wheat round 1 transect 2####
WWR1T2.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 1, Transect == 2) %>%
  droplevels()

WWR1T2.null.sub.df <- subset(WWR1T2.null.df, Weight > 0)
WWR1T2.null.sub.df$weight<- WWR1T2.null.sub.df$Weight

WWR1T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR1T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR1T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR1T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR1T2.node.df <- rbind(WWR1T2.cons.node.df, WWR1T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR1T2.node.df[WWR1T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR1T2.node.df$tr.height == 0)))
WWR1T2.node.df[WWR1T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR1T2.node.df$tr.height == 0.5)))

WWR1T2.null.net <- graph_from_data_frame(d = WWR1T2.null.sub.df, vertices = WWR1T2.node.df, directed = TRUE)
plot(WWR1T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR1T2.null.net, layout = as.matrix(WWR1T2.node.df[, c("x.coord", "tr.height")]))

WWR1T2.null.net2 <- fortify(WWR1T2.null.net, layout = as.matrix(WWR1T2.node.df[, c("x.coord", "tr.height")]))

WWR1T2.null.net2$tr.height <- (WWR1T2.null.net2$tr.height - 1) / 2
WWR1T2.null.net2$y <- WWR1T2.null.net2$y / 2 -0.006
WWR1T2.null.net2$yend <- WWR1T2.null.net2$yend / 2 -0.006

ggplot(WWR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR1T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR1T2.nullnetbip <- ggplot(WWR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR1T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 1 transect 2 network")
WWR1T2.nullnetbip

png(file = "Winter Wheat Round 1 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR1T2.nullnetbip
dev.off()

#strength
WWR1T2.strength <- data.frame(strength(WWR1T2.null.net))
WWR1T2.strength$taxon <- rownames(WWR1T2.strength)
WWR1T2.strength<-WWR1T2.strength[(nrow(WWR1T2.cons.node.df)+1):nrow(WWR1T2.strength),]

#### Winter Wheat round 1 transect 3 (no spiders)####
WWR1T3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 1, Transect == 3) %>%
  droplevels()

WWR1T3.null.sub.df <- subset(WWR1T3.null.df, Weight > 0)


WWR1T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR1T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR1T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR1T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR1T3.node.df <- rbind(WWR1T3.cons.node.df, WWR1T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR1T3.node.df[WWR1T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR1T3.node.df$tr.height == 0)))
WWR1T3.node.df[WWR1T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR1T3.node.df$tr.height == 0.5)))

WWR1T3.null.net <- graph_from_data_frame(d = WWR1T3.null.sub.df, vertices = WWR1T3.node.df, directed = TRUE)
plot(WWR1T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR1T3.null.net, layout = as.matrix(WWR1T3.node.df[, c("x.coord", "tr.height")]))

WWR1T3.null.net2 <- fortify(WWR1T3.null.net, layout = as.matrix(WWR1T3.node.df[, c("x.coord", "tr.height")]))

WWR1T3.null.net2$tr.height <- (WWR1T3.null.net2$tr.height - 1) / 2
WWR1T3.null.net2$y <- WWR1T3.null.net2$y / 2 -0.006
WWR1T3.null.net2$yend <- WWR1T3.null.net2$yend / 2 -0.006

ggplot(WWR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR1T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR1T3.nullnetbip <- ggplot(WWR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR1T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 1 transect 2 network")
WWR1T3.nullnetbip

png(file = "Winter Wheat Round 1 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR1T3.nullnetbip
dev.off()




#### Winter Wheat Round 2 transect 1####
WWR2T1.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 2, Transect == 1) %>%
  droplevels()

WWR2T1.null.sub.df <- subset(WWR2T1.null.df, Weight > 0)
WWR2T1.null.sub.df$weight<- WWR2T1.null.sub.df$Weight

WWR2T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR2T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR2T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR2T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR2T1.node.df <- rbind(WWR2T1.cons.node.df, WWR2T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR2T1.node.df[WWR2T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR2T1.node.df$tr.height == 0)))
WWR2T1.node.df[WWR2T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR2T1.node.df$tr.height == 0.5)))

WWR2T1.null.net <- graph_from_data_frame(d = WWR2T1.null.sub.df, vertices = WWR2T1.node.df, directed = TRUE)
plot(WWR2T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR2T1.null.net, layout = as.matrix(WWR2T1.node.df[, c("x.coord", "tr.height")]))

WWR2T1.null.net2 <- fortify(WWR2T1.null.net, layout = as.matrix(WWR2T1.node.df[, c("x.coord", "tr.height")]))

WWR2T1.null.net2$tr.height <- (WWR2T1.null.net2$tr.height - 1) / 2
WWR2T1.null.net2$y <- WWR2T1.null.net2$y / 2 -0.006
WWR2T1.null.net2$yend <- WWR2T1.null.net2$yend / 2 -0.006

ggplot(WWR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR2T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR2T1.nullnetbip <- ggplot(WWR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR2T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 1 transect 2 network")
WWR2T1.nullnetbip

png(file = "Winter Wheat Round 2 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR2T1.nullnetbip
dev.off()

#strength
WWR2T1.strength <- data.frame(strength(WWR2T1.null.net))
WWR2T1.strength$taxon <- rownames(WWR2T1.strength)
WWR2T1.strength<-WWR2T1.strength[(nrow(WWR2T1.cons.node.df)+1):nrow(WWR2T1.strength),]

#### Winter Wheat Round 2 Transect 2 #####
WWR2T2.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 2, Transect == 2) %>%
  droplevels()

WWR2T2.null.sub.df <- subset(WWR2T2.null.df, Weight > 0)
WWR2T2.null.sub.df$weight<- WWR2T2.null.sub.df$Weight

WWR2T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR2T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR2T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR2T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR2T2.node.df <- rbind(WWR2T2.cons.node.df, WWR2T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR2T2.node.df[WWR2T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR2T2.node.df$tr.height == 0)))
WWR2T2.node.df[WWR2T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR2T2.node.df$tr.height == 0.5)))

WWR2T2.null.net <- graph_from_data_frame(d = WWR2T2.null.sub.df, vertices = WWR2T2.node.df, directed = TRUE)
plot(WWR2T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR2T2.null.net, layout = as.matrix(WWR2T2.node.df[, c("x.coord", "tr.height")]))

WWR2T2.null.net2 <- fortify(WWR2T2.null.net, layout = as.matrix(WWR2T2.node.df[, c("x.coord", "tr.height")]))

WWR2T2.null.net2$tr.height <- (WWR2T2.null.net2$tr.height - 1) / 2
WWR2T2.null.net2$y <- WWR2T2.null.net2$y / 2 -0.006
WWR2T2.null.net2$yend <- WWR2T2.null.net2$yend / 2 -0.006

ggplot(WWR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR2T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR2T2.nullnetbip <- ggplot(WWR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR2T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 2 transect 2 network")
WWR2T2.nullnetbip

png(file = "Winter Wheat Round 2 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR2T2.nullnetbip
dev.off()

#strength
WWR2T2.strength <- data.frame(strength(WWR2T2.null.net))
WWR2T2.strength$taxon <- rownames(WWR2T2.strength)
WWR2T2.strength<-WWR2T2.strength[(nrow(WWR2T2.cons.node.df)+1):nrow(WWR2T2.strength),]

#### Winter Wheat Round 2 Transect 3#####
WWR2T3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 2, Transect == 3) %>%
  droplevels()

WWR2T3.null.sub.df <- subset(WWR2T3.null.df, Weight > 0)
WWR2T3.null.sub.df$weight<- WWR2T3.null.sub.df$Weight

WWR2T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR2T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR2T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR2T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR2T3.node.df <- rbind(WWR2T3.cons.node.df, WWR2T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR2T3.node.df[WWR2T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR2T3.node.df$tr.height == 0)))
WWR2T3.node.df[WWR2T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR2T3.node.df$tr.height == 0.5)))

WWR2T3.null.net <- graph_from_data_frame(d = WWR2T3.null.sub.df, vertices = WWR2T3.node.df, directed = TRUE)
plot(WWR2T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR2T3.null.net, layout = as.matrix(WWR2T3.node.df[, c("x.coord", "tr.height")]))

WWR2T3.null.net2 <- fortify(WWR2T3.null.net, layout = as.matrix(WWR2T3.node.df[, c("x.coord", "tr.height")]))

WWR2T3.null.net2$tr.height <- (WWR2T3.null.net2$tr.height - 1) / 2
WWR2T3.null.net2$y <- WWR2T3.null.net2$y / 2 -0.006
WWR2T3.null.net2$yend <- WWR2T3.null.net2$yend / 2 -0.006

ggplot(WWR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR2T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR2T3.nullnetbip <- ggplot(WWR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR2T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 2 transect 3 network")
WWR2T3.nullnetbip

png(file = "Winter Wheat Round 2 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR2T3.nullnetbip
dev.off()


#strength
WWR2T3.strength <- data.frame(strength(WWR2T3.null.net))
WWR2T3.strength$taxon <- rownames(WWR2T3.strength)
WWR2T3.strength<-WWR2T3.strength[(nrow(WWR2T3.cons.node.df)+1):nrow(WWR2T3.strength),]

#### Winter Wheat Round 3 Transect 1####
WWR3T1.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 3, Transect == 1) %>%
  droplevels()

WWR3T1.null.sub.df <- subset(WWR3T1.null.df, Weight > 0)
WWR3T1.null.sub.df$weight<- WWR3T1.null.sub.df$Weight

WWR3T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR3T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR3T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR3T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR3T1.node.df <- rbind(WWR3T1.cons.node.df, WWR3T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR3T1.node.df[WWR3T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR3T1.node.df$tr.height == 0)))
WWR3T1.node.df[WWR3T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR3T1.node.df$tr.height == 0.5)))

WWR3T1.null.net <- graph_from_data_frame(d = WWR3T1.null.sub.df, vertices = WWR3T1.node.df, directed = TRUE)
plot(WWR3T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR3T1.null.net, layout = as.matrix(WWR3T1.node.df[, c("x.coord", "tr.height")]))

WWR3T1.null.net2 <- fortify(WWR3T1.null.net, layout = as.matrix(WWR3T1.node.df[, c("x.coord", "tr.height")]))

WWR3T1.null.net2$tr.height <- (WWR3T1.null.net2$tr.height - 1) / 2
WWR3T1.null.net2$y <- WWR3T1.null.net2$y / 2 -0.006
WWR3T1.null.net2$yend <- WWR3T1.null.net2$yend / 2 -0.006

ggplot(WWR3T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR3T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR3T1.nullnetbip <- ggplot(WWR3T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR3T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 3 transect 1 network")
WWR3T1.nullnetbip

png(file = "Winter Wheat Round 3 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR3T1.nullnetbip
dev.off()

#strength
WWR3T1.strength <- data.frame(strength(WWR3T1.null.net))
WWR3T1.strength$taxon <- rownames(WWR3T1.strength)
WWR3T1.strength<-WWR3T1.strength[(nrow(WWR3T1.cons.node.df)+1):nrow(WWR3T1.strength),]

#### Winter Wheat Round 3 Transect 2####
WWR3T2.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 3, Transect == 2) %>%
  droplevels()

WWR3T2.null.sub.df <- subset(WWR3T2.null.df, Weight > 0)
WWR3T2.null.sub.df$weight<- WWR3T2.null.sub.df$Weight

WWR3T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR3T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR3T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR3T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR3T2.node.df <- rbind(WWR3T2.cons.node.df, WWR3T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR3T2.node.df[WWR3T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR3T2.node.df$tr.height == 0)))
WWR3T2.node.df[WWR3T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR3T2.node.df$tr.height == 0.5)))

WWR3T2.null.net <- graph_from_data_frame(d = WWR3T2.null.sub.df, vertices = WWR3T2.node.df, directed = TRUE)
plot(WWR3T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR3T2.null.net, layout = as.matrix(WWR3T2.node.df[, c("x.coord", "tr.height")]))

WWR3T2.null.net2 <- fortify(WWR3T2.null.net, layout = as.matrix(WWR3T2.node.df[, c("x.coord", "tr.height")]))

WWR3T2.null.net2$tr.height <- (WWR3T2.null.net2$tr.height - 1) / 2
WWR3T2.null.net2$y <- WWR3T2.null.net2$y / 2 -0.006
WWR3T2.null.net2$yend <- WWR3T2.null.net2$yend / 2 -0.006

ggplot(WWR3T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR3T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR3T2.nullnetbip <- ggplot(WWR3T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR3T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 3 transect 2 network")
WWR3T2.nullnetbip

png(file = "Winter Wheat Round 3 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR3T2.nullnetbip
dev.off()

#strength
WWR3T2.strength <- data.frame(strength(WWR3T2.null.net))
WWR3T2.strength$taxon <- rownames(WWR3T2.strength)
WWR3T2.strength<-WWR3T2.strength[(nrow(WWR3T2.cons.node.df)+1):nrow(WWR3T2.strength),]

#### Winter Wheat Round 3 Transect 3 ####
WWR3T3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 3, Transect == 3) %>%
  droplevels()

WWR3T3.null.sub.df <- subset(WWR3T3.null.df, Weight > 0)
WWR3T3.null.sub.df$weight<- WWR3T3.null.sub.df$Weight


WWR3T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WWR3T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WWR3T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(WWR3T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WWR3T3.node.df <- rbind(WWR3T3.cons.node.df, WWR3T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WWR3T3.node.df[WWR3T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WWR3T3.node.df$tr.height == 0)))
WWR3T3.node.df[WWR3T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WWR3T3.node.df$tr.height == 0.5)))

WWR3T3.null.net <- graph_from_data_frame(d = WWR3T3.null.sub.df, vertices = WWR3T3.node.df, directed = TRUE)
plot(WWR3T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WWR3T3.null.net, layout = as.matrix(WWR3T3.node.df[, c("x.coord", "tr.height")]))

WWR3T3.null.net2 <- fortify(WWR3T3.null.net, layout = as.matrix(WWR3T3.node.df[, c("x.coord", "tr.height")]))

WWR3T3.null.net2$tr.height <- (WWR3T3.null.net2$tr.height - 1) / 2
WWR3T3.null.net2$y <- WWR3T3.null.net2$y / 2 -0.006
WWR3T3.null.net2$yend <- WWR3T3.null.net2$yend / 2 -0.006

ggplot(WWR3T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WWR3T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WWR3T3.nullnetbip <- ggplot(WWR3T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WWR3T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter wheat round 3 transect 3 network")
WWR3T3.nullnetbip

png(file = "Winter Wheat Round 3 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
WWR3T3.nullnetbip
dev.off()

#strength
WWR3T3.strength <- data.frame(strength(WWR3T3.null.net))
WWR3T3.strength$taxon <- rownames(WWR3T3.strength)
WWR3T3.strength<-WWR3T3.strength[(nrow(WWR3T3.cons.node.df)+1):nrow(WWR3T3.strength),]



#### Winter OSR Round 1 Transect 1 ####
WOR1T1.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 1, Transect == 1) %>%
  droplevels()

WOR1T1.null.sub.df <- subset(WOR1T1.null.df, Weight > 0)
WOR1T1.null.sub.df$weight<- WOR1T1.null.sub.df$Weight

WOR1T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR1T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR1T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR1T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR1T1.node.df <- rbind(WOR1T1.cons.node.df, WOR1T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR1T1.node.df[WOR1T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR1T1.node.df$tr.height == 0)))
WOR1T1.node.df[WOR1T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR1T1.node.df$tr.height == 0.5)))

WOR1T1.null.net <- graph_from_data_frame(d = WOR1T1.null.sub.df, vertices = WOR1T1.node.df, directed = TRUE)
plot(WOR1T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR1T1.null.net, layout = as.matrix(WOR1T1.node.df[, c("x.coord", "tr.height")]))

WOR1T1.null.net2 <- fortify(WOR1T1.null.net, layout = as.matrix(WOR1T1.node.df[, c("x.coord", "tr.height")]))

WOR1T1.null.net2$tr.height <- (WOR1T1.null.net2$tr.height - 1) / 2
WOR1T1.null.net2$y <- WOR1T1.null.net2$y / 2 -0.006
WOR1T1.null.net2$yend <- WOR1T1.null.net2$yend / 2 -0.006

ggplot(WOR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR1T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR1T1.nullnetbip <- ggplot(WOR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR1T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 1 transect 1 network")
WOR1T1.nullnetbip

png(file = "Winter OSR Round 1 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR1T1.nullnetbip
dev.off()

#strength
WOR1T1.strength <- data.frame(strength(WOR1T1.null.net))
WOR1T1.strength$taxon <- rownames(WOR1T1.strength)
WOR1T1.strength<-WOR1T1.strength[(nrow(WOR1T1.cons.node.df)+1):nrow(WOR1T1.strength),]

#### Winter OSR Round 1 Transect 2 ####
WOR1T2.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 1, Transect == 2) %>%
  droplevels()

WOR1T2.null.sub.df <- subset(WOR1T2.null.df, Weight > 0)
WOR1T2.null.sub.df$weight<- WOR1T2.null.sub.df$Weight

WOR1T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR1T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR1T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR1T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR1T2.node.df <- rbind(WOR1T2.cons.node.df, WOR1T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR1T2.node.df[WOR1T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR1T2.node.df$tr.height == 0)))
WOR1T2.node.df[WOR1T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR1T2.node.df$tr.height == 0.5)))

WOR1T2.null.net <- graph_from_data_frame(d = WOR1T2.null.sub.df, vertices = WOR1T2.node.df, directed = TRUE)
plot(WOR1T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR1T2.null.net, layout = as.matrix(WOR1T2.node.df[, c("x.coord", "tr.height")]))

WOR1T2.null.net2 <- fortify(WOR1T2.null.net, layout = as.matrix(WOR1T2.node.df[, c("x.coord", "tr.height")]))

WOR1T2.null.net2$tr.height <- (WOR1T2.null.net2$tr.height - 1) / 2
WOR1T2.null.net2$y <- WOR1T2.null.net2$y / 2 -0.006
WOR1T2.null.net2$yend <- WOR1T2.null.net2$yend / 2 -0.006

ggplot(WOR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR1T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR1T2.nullnetbip <- ggplot(WOR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR1T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 1 transect 2 network")
WOR1T2.nullnetbip

png(file = "Winter OSR Round 1 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR1T2.nullnetbip
dev.off()

#strength
WOR1T2.strength <- data.frame(strength(WOR1T2.null.net))
WOR1T2.strength$taxon <- rownames(WOR1T2.strength)
WOR1T2.strength<-WOR1T2.strength[(nrow(WOR1T2.cons.node.df)+1):nrow(WOR1T2.strength),]

#### Winter OSR Round 1 Transect 3 ####
WOR1T3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 1, Transect == 3) %>%
  droplevels()

WOR1T3.null.sub.df <- subset(WOR1T3.null.df, Weight > 0)
WOR1T3.null.sub.df$weight<- WOR1T3.null.sub.df$Weight

WOR1T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR1T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR1T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR1T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR1T3.node.df <- rbind(WOR1T3.cons.node.df, WOR1T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR1T3.node.df[WOR1T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR1T3.node.df$tr.height == 0)))
WOR1T3.node.df[WOR1T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR1T3.node.df$tr.height == 0.5)))

WOR1T3.null.net <- graph_from_data_frame(d = WOR1T3.null.sub.df, vertices = WOR1T3.node.df, directed = TRUE)
plot(WOR1T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR1T3.null.net, layout = as.matrix(WOR1T3.node.df[, c("x.coord", "tr.height")]))

WOR1T3.null.net2 <- fortify(WOR1T3.null.net, layout = as.matrix(WOR1T3.node.df[, c("x.coord", "tr.height")]))

WOR1T3.null.net2$tr.height <- (WOR1T3.null.net2$tr.height - 1) / 2
WOR1T3.null.net2$y <- WOR1T3.null.net2$y / 2 -0.006
WOR1T3.null.net2$yend <- WOR1T3.null.net2$yend / 2 -0.006

ggplot(WOR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR1T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR1T3.nullnetbip <- ggplot(WOR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR1T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 1 transect 3 network")
WOR1T3.nullnetbip

png(file = "Winter OSR Round 1 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR1T3.nullnetbip
dev.off()

#strength
WOR1T3.strength <- data.frame(strength(WOR1T3.null.net))
WOR1T3.strength$taxon <- rownames(WOR1T3.strength)
WOR1T3.strength<-WOR1T3.strength[(nrow(WOR1T3.cons.node.df)+1):nrow(WOR1T3.strength),]

#### Winter OSR Round 2 Transect 1####
WOR2T1.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 2, Transect == 1) %>%
  droplevels()

WOR2T1.null.sub.df <- subset(WOR2T1.null.df, Weight > 0)
WOR2T1.null.sub.df$weight<- WOR2T1.null.sub.df$Weight

WOR2T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR2T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR2T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR2T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR2T1.node.df <- rbind(WOR2T1.cons.node.df, WOR2T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR2T1.node.df[WOR2T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR2T1.node.df$tr.height == 0)))
WOR2T1.node.df[WOR2T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR2T1.node.df$tr.height == 0.5)))

WOR2T1.null.net <- graph_from_data_frame(d = WOR2T1.null.sub.df, vertices = WOR2T1.node.df, directed = TRUE)
plot(WOR2T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR2T1.null.net, layout = as.matrix(WOR2T1.node.df[, c("x.coord", "tr.height")]))

WOR2T1.null.net2 <- fortify(WOR2T1.null.net, layout = as.matrix(WOR2T1.node.df[, c("x.coord", "tr.height")]))

WOR2T1.null.net2$tr.height <- (WOR2T1.null.net2$tr.height - 1) / 2
WOR2T1.null.net2$y <- WOR2T1.null.net2$y / 2 -0.006
WOR2T1.null.net2$yend <- WOR2T1.null.net2$yend / 2 -0.006

ggplot(WOR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR2T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR2T1.nullnetbip <- ggplot(WOR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR2T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 2 transect 1 network")
WOR2T1.nullnetbip

png(file = "Winter OSR Round 2 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR2T1.nullnetbip
dev.off()

WOR2T1.strength <- data.frame(strength(WOR2T1.null.net))
WOR2T1.strength$taxon <- rownames(WOR2T1.strength)
WOR2T1.strength<-WOR2T1.strength[(nrow(WOR2T1.cons.node.df)+1):nrow(WOR2T1.strength),]

#### Winter OSR Round 2 Transect 2 ####
WOR2T2.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 2, Transect == 2) %>%
  droplevels()

WOR2T2.null.sub.df <- subset(WOR2T2.null.df, Weight > 0)
WOR2T2.null.sub.df$weight<- WOR2T2.null.sub.df$Weight

WOR2T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR2T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR2T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR2T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR2T2.node.df <- rbind(WOR2T2.cons.node.df, WOR2T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR2T2.node.df[WOR2T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR2T2.node.df$tr.height == 0)))
WOR2T2.node.df[WOR2T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR2T2.node.df$tr.height == 0.5)))

WOR2T2.null.net <- graph_from_data_frame(d = WOR2T2.null.sub.df, vertices = WOR2T2.node.df, directed = TRUE)
plot(WOR2T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR2T2.null.net, layout = as.matrix(WOR2T2.node.df[, c("x.coord", "tr.height")]))

WOR2T2.null.net2 <- fortify(WOR2T2.null.net, layout = as.matrix(WOR2T2.node.df[, c("x.coord", "tr.height")]))

WOR2T2.null.net2$tr.height <- (WOR2T2.null.net2$tr.height - 1) / 2
WOR2T2.null.net2$y <- WOR2T2.null.net2$y / 2 -0.006
WOR2T2.null.net2$yend <- WOR2T2.null.net2$yend / 2 -0.006

ggplot(WOR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR2T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR2T2.nullnetbip <- ggplot(WOR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR2T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 2 transect 2 network")
WOR2T2.nullnetbip

png(file = "Winter OSR Round 2 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR2T2.nullnetbip
dev.off()

#strength
WOR2T2.strength <- data.frame(strength(WOR2T2.null.net))
WOR2T2.strength$taxon <- rownames(WOR2T2.strength)
WOR2T2.strength<-WOR2T2.strength[(nrow(WOR2T2.cons.node.df)+1):nrow(WOR2T2.strength),]

#### Winter OSR Round 2 Transect 3 ####
WOR2T3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 2, Transect == 3) %>%
  droplevels()

WOR2T3.null.sub.df <- subset(WOR2T3.null.df, Weight > 0)
WOR2T3.null.sub.df$weight<- WOR2T3.null.sub.df$Weight

WOR2T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR2T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR2T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR2T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR2T3.node.df <- rbind(WOR2T3.cons.node.df, WOR2T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR2T3.node.df[WOR2T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR2T3.node.df$tr.height == 0)))
WOR2T3.node.df[WOR2T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR2T3.node.df$tr.height == 0.5)))

WOR2T3.null.net <- graph_from_data_frame(d = WOR2T3.null.sub.df, vertices = WOR2T3.node.df, directed = TRUE)
plot(WOR2T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR2T3.null.net, layout = as.matrix(WOR2T3.node.df[, c("x.coord", "tr.height")]))

WOR2T3.null.net2 <- fortify(WOR2T3.null.net, layout = as.matrix(WOR2T3.node.df[, c("x.coord", "tr.height")]))

WOR2T3.null.net2$tr.height <- (WOR2T3.null.net2$tr.height - 1) / 2
WOR2T3.null.net2$y <- WOR2T3.null.net2$y / 2 -0.006
WOR2T3.null.net2$yend <- WOR2T3.null.net2$yend / 2 -0.006

ggplot(WOR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR2T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR2T3.nullnetbip <- ggplot(WOR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR2T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 2 transect 3 network")
WOR2T3.nullnetbip

png(file = "Winter OSR Round 2 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR2T3.nullnetbip
dev.off()

#strength
WOR2T3.strength <- data.frame(strength(WOR2T3.null.net))
WOR2T3.strength$taxon <- rownames(WOR2T3.strength)
WOR2T3.strength<-WOR2T3.strength[(nrow(WOR2T3.cons.node.df)+1):nrow(WOR2T3.strength),]

#### Winter OSR Round 3 Transect 1 no spiders####
WOR3T1.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 3, Transect == 1) %>%
  droplevels()

WOR3T1.null.sub.df <- subset(WOR3T1.null.df, Weight > 0)
WOR3T1.null.sub.df$weight<- WOR3T1.null.sub.df$Weight

WOR3T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR3T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR3T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR3T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR3T1.node.df <- rbind(WOR3T1.cons.node.df, WOR3T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR3T1.node.df[WOR3T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR3T1.node.df$tr.height == 0)))
WOR3T1.node.df[WOR3T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR3T1.node.df$tr.height == 0.5)))

WOR3T1.null.net <- graph_from_data_frame(d = WOR3T1.null.sub.df, vertices = WOR3T1.node.df, directed = TRUE)
plot(WOR3T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR3T1.null.net, layout = as.matrix(WOR3T1.node.df[, c("x.coord", "tr.height")]))

WOR3T1.null.net2 <- fortify(WOR3T1.null.net, layout = as.matrix(WOR3T1.node.df[, c("x.coord", "tr.height")]))

WOR3T1.null.net2$tr.height <- (WOR3T1.null.net2$tr.height - 1) / 2
WOR3T1.null.net2$y <- WOR3T1.null.net2$y / 2 -0.006
WOR3T1.null.net2$yend <- WOR3T1.null.net2$yend / 2 -0.006

ggplot(WOR3T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR3T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR3T1.nullnetbip <- ggplot(WOR3T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR3T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 3 transect 1 network")
WOR3T1.nullnetbip

png(file = "Winter OSR Round 3 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR3T1.nullnetbip
dev.off()


#### Winter OSR Round 3 Transect 2 no spiders####
WOR3T2.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 3, Transect == 2) %>%
  droplevels()

WOR3T2.null.sub.df <- subset(WOR3T2.null.df, Weight > 0)


WOR3T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR3T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR3T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR3T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR3T2.node.df <- rbind(WOR3T2.cons.node.df, WOR3T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR3T2.node.df[WOR3T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR3T2.node.df$tr.height == 0)))
WOR3T2.node.df[WOR3T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR3T2.node.df$tr.height == 0.5)))

WOR3T2.null.net <- graph_from_data_frame(d = WOR3T2.null.sub.df, vertices = WOR3T2.node.df, directed = TRUE)
plot(WOR3T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR3T2.null.net, layout = as.matrix(WOR3T2.node.df[, c("x.coord", "tr.height")]))

WOR3T2.null.net2 <- fortify(WOR3T2.null.net, layout = as.matrix(WOR3T2.node.df[, c("x.coord", "tr.height")]))

WOR3T2.null.net2$tr.height <- (WOR3T2.null.net2$tr.height - 1) / 2
WOR3T2.null.net2$y <- WOR3T2.null.net2$y / 2 -0.006
WOR3T2.null.net2$yend <- WOR3T2.null.net2$yend / 2 -0.006

ggplot(WOR3T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR3T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR3T2.nullnetbip <- ggplot(WOR3T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR3T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 3 transect 2 network")
WOR3T2.nullnetbip

png(file = "Winter OSR Round 3 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR3T2.nullnetbip
dev.off()


#### Winter OSR Round 3 Transect 3 no spiders####
WOR3T3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "OSR", Round == 3, Transect == 3) %>%
  droplevels()

WOR3T3.null.sub.df <- subset(WOR3T3.null.df, Weight > 0)


WOR3T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(WOR3T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
WOR3T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(WOR3T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
WOR3T3.node.df <- rbind(WOR3T3.cons.node.df, WOR3T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
WOR3T3.node.df[WOR3T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(WOR3T3.node.df$tr.height == 0)))
WOR3T3.node.df[WOR3T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(WOR3T3.node.df$tr.height == 0.5)))

WOR3T3.null.net <- graph_from_data_frame(d = WOR3T3.null.sub.df, vertices = WOR3T3.node.df, directed = TRUE)
plot(WOR3T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(WOR3T3.null.net, layout = as.matrix(WOR3T3.node.df[, c("x.coord", "tr.height")]))

WOR3T3.null.net2 <- fortify(WOR3T3.null.net, layout = as.matrix(WOR3T3.node.df[, c("x.coord", "tr.height")]))

WOR3T3.null.net2$tr.height <- (WOR3T3.null.net2$tr.height - 1) / 2
WOR3T3.null.net2$y <- WOR3T3.null.net2$y / 2 -0.006
WOR3T3.null.net2$yend <- WOR3T3.null.net2$yend / 2 -0.006

ggplot(WOR3T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = WOR3T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

WOR3T3.nullnetbip <- ggplot(WOR3T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = WOR3T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Winter OSR round 3 transect 3 network")
WOR3T3.nullnetbip

png(file = "Winter OSR Round 3 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
WOR3T3.nullnetbip
dev.off()



#### Spring OSR Round 1 Transect 1 ####
SOR1T1.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "OSR", Round == 1, Transect == 1) %>%
  droplevels()

SOR1T1.null.sub.df <- subset(SOR1T1.null.df, Weight > 0)
SOR1T1.null.sub.df$weight<- SOR1T1.null.sub.df$Weight

SOR1T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR1T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR1T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR1T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR1T1.node.df <- rbind(SOR1T1.cons.node.df, SOR1T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR1T1.node.df[SOR1T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR1T1.node.df$tr.height == 0)))
SOR1T1.node.df[SOR1T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR1T1.node.df$tr.height == 0.5)))

SOR1T1.null.net <- graph_from_data_frame(d = SOR1T1.null.sub.df, vertices = SOR1T1.node.df, directed = TRUE)
plot(SOR1T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR1T1.null.net, layout = as.matrix(SOR1T1.node.df[, c("x.coord", "tr.height")]))

SOR1T1.null.net2 <- fortify(SOR1T1.null.net, layout = as.matrix(SOR1T1.node.df[, c("x.coord", "tr.height")]))

SOR1T1.null.net2$tr.height <- (SOR1T1.null.net2$tr.height - 1) / 2
SOR1T1.null.net2$y <- SOR1T1.null.net2$y / 2 -0.006
SOR1T1.null.net2$yend <- SOR1T1.null.net2$yend / 2 -0.006

ggplot(SOR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR1T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR1T1.nullnetbip <- ggplot(SOR1T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR1T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 1 transect 1 network")
SOR1T1.nullnetbip

png(file = "Spring OSR Round 1 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR1T1.nullnetbip
dev.off()
#strength
SOR1T1.strength <- data.frame(strength(SOR1T1.null.net))
SOR1T1.strength$taxon <- rownames(SOR1T1.strength)
SOR1T1.strength<-SOR1T1.strength[(nrow(SOR1T1.cons.node.df)+1):nrow(SOR1T1.strength),]

#### Spring OSR Round 1 Transect 2 ####
SOR1T2.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "OSR", Round == 1, Transect == 2) %>%
  droplevels()

SOR1T2.null.sub.df <- subset(SOR1T2.null.df, Weight > 0)
SOR1T2.null.sub.df$weight<- SOR1T2.null.sub.df$Weight

SOR1T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR1T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR1T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR1T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR1T2.node.df <- rbind(SOR1T2.cons.node.df, SOR1T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR1T2.node.df[SOR1T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR1T2.node.df$tr.height == 0)))
SOR1T2.node.df[SOR1T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR1T2.node.df$tr.height == 0.5)))

SOR1T2.null.net <- graph_from_data_frame(d = SOR1T2.null.sub.df, vertices = SOR1T2.node.df, directed = TRUE)
plot(SOR1T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR1T2.null.net, layout = as.matrix(SOR1T2.node.df[, c("x.coord", "tr.height")]))

SOR1T2.null.net2 <- fortify(SOR1T2.null.net, layout = as.matrix(SOR1T2.node.df[, c("x.coord", "tr.height")]))

SOR1T2.null.net2$tr.height <- (SOR1T2.null.net2$tr.height - 1) / 2
SOR1T2.null.net2$y <- SOR1T2.null.net2$y / 2 -0.006
SOR1T2.null.net2$yend <- SOR1T2.null.net2$yend / 2 -0.006

ggplot(SOR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR1T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR1T2.nullnetbip <- ggplot(SOR1T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR1T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 1 transect 2 network")
SOR1T2.nullnetbip

png(file = "Spring OSR Round 1 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR1T2.nullnetbip
dev.off()

#strength
SOR1T2.strength <- data.frame(strength(SOR1T2.null.net))
SOR1T2.strength$taxon <- rownames(SOR1T2.strength)
SOR1T2.strength<-SOR1T2.strength[(nrow(SOR1T2.cons.node.df)+1):nrow(SOR1T2.strength),]

#### Spring OSR Round 1 Transect 3 ####
SOR1T3.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "OSR", Round == 1, Transect == 3) %>%
  droplevels()

SOR1T3.null.sub.df <- subset(SOR1T3.null.df, Weight > 0)
SOR1T3.null.sub.df$weight<- SOR1T3.null.sub.df$Weight

SOR1T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR1T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR1T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR1T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR1T3.node.df <- rbind(SOR1T3.cons.node.df, SOR1T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR1T3.node.df[SOR1T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR1T3.node.df$tr.height == 0)))
SOR1T3.node.df[SOR1T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR1T3.node.df$tr.height == 0.5)))

SOR1T3.null.net <- graph_from_data_frame(d = SOR1T3.null.sub.df, vertices = SOR1T3.node.df, directed = TRUE)
plot(SOR1T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR1T3.null.net, layout = as.matrix(SOR1T3.node.df[, c("x.coord", "tr.height")]))

SOR1T3.null.net2 <- fortify(SOR1T3.null.net, layout = as.matrix(SOR1T3.node.df[, c("x.coord", "tr.height")]))

SOR1T3.null.net2$tr.height <- (SOR1T3.null.net2$tr.height - 1) / 2
SOR1T3.null.net2$y <- SOR1T3.null.net2$y / 2 -0.006
SOR1T3.null.net2$yend <- SOR1T3.null.net2$yend / 2 -0.006

ggplot(SOR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR1T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR1T3.nullnetbip <- ggplot(SOR1T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR1T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 1 transect 3 network")
SOR1T3.nullnetbip

png(file = "Spring OSR Round 1 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR1T3.nullnetbip
dev.off()

#strength
SOR1T3.strength <- data.frame(strength(SOR1T3.null.net))
SOR1T3.strength$taxon <- rownames(SOR1T3.strength)
SOR1T3.strength<-SOR1T3.strength[(nrow(SOR1T3.cons.node.df)+1):nrow(SOR1T3.strength),]

#### Spring OSR Round 2 Transect 1####
SOR2T1.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "OSR", Round == 2, Transect == 1) %>%
  droplevels()

SOR2T1.null.sub.df <- subset(SOR2T1.null.df, Weight > 0)
SOR2T1.null.sub.df$weight<- SOR2T1.null.sub.df$Weight

SOR2T1.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR2T1.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR2T1.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR2T1.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR2T1.node.df <- rbind(SOR2T1.cons.node.df, SOR2T1.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR2T1.node.df[SOR2T1.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR2T1.node.df$tr.height == 0)))
SOR2T1.node.df[SOR2T1.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR2T1.node.df$tr.height == 0.5)))

SOR2T1.null.net <- graph_from_data_frame(d = SOR2T1.null.sub.df, vertices = SOR2T1.node.df, directed = TRUE)
plot(SOR2T1.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR2T1.null.net, layout = as.matrix(SOR2T1.node.df[, c("x.coord", "tr.height")]))

SOR2T1.null.net2 <- fortify(SOR2T1.null.net, layout = as.matrix(SOR2T1.node.df[, c("x.coord", "tr.height")]))

SOR2T1.null.net2$tr.height <- (SOR2T1.null.net2$tr.height - 1) / 2
SOR2T1.null.net2$y <- SOR2T1.null.net2$y / 2 -0.006
SOR2T1.null.net2$yend <- SOR2T1.null.net2$yend / 2 -0.006

ggplot(SOR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR2T1.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR2T1.nullnetbip <- ggplot(SOR2T1.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR2T1.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 2 transect 1 network")
SOR2T1.nullnetbip

png(file = "Spring OSR Round 2 transect 1 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR2T1.nullnetbip
dev.off()

#strength
SOR2T1.strength <- data.frame(strength(SOR2T1.null.net))
SOR2T1.strength$taxon <- rownames(SOR2T1.strength)
SOR2T1.strength<-SOR2T1.strength[(nrow(SOR2T1.cons.node.df)+1):nrow(SOR2T1.strength),]

#### Spring OSR Round 2 Transect 2 ####
SOR2T2.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "OSR", Round == 2, Transect == 2) %>%
  droplevels()

SOR2T2.null.sub.df <- subset(SOR2T2.null.df, Weight > 0)
SOR2T2.null.sub.df$weight<- SOR2T2.null.sub.df$Weight

SOR2T2.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR2T2.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR2T2.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR2T2.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR2T2.node.df <- rbind(SOR2T2.cons.node.df, SOR2T2.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR2T2.node.df[SOR2T2.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR2T2.node.df$tr.height == 0)))
SOR2T2.node.df[SOR2T2.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR2T2.node.df$tr.height == 0.5)))

SOR2T2.null.net <- graph_from_data_frame(d = SOR2T2.null.sub.df, vertices = SOR2T2.node.df, directed = TRUE)
plot(SOR2T2.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR2T2.null.net, layout = as.matrix(SOR2T2.node.df[, c("x.coord", "tr.height")]))

SOR2T2.null.net2 <- fortify(SOR2T2.null.net, layout = as.matrix(SOR2T2.node.df[, c("x.coord", "tr.height")]))

SOR2T2.null.net2$tr.height <- (SOR2T2.null.net2$tr.height - 1) / 2
SOR2T2.null.net2$y <- SOR2T2.null.net2$y / 2 -0.006
SOR2T2.null.net2$yend <- SOR2T2.null.net2$yend / 2 -0.006

ggplot(SOR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR2T2.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR2T2.nullnetbip <- ggplot(SOR2T2.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR2T2.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 2 transect 2 network")
SOR2T2.nullnetbip

png(file = "Spring OSR Round 2 transect 2 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR2T2.nullnetbip
dev.off()

#strength
SOR2T2.strength <- data.frame(strength(SOR2T2.null.net))
SOR2T2.strength$taxon <- rownames(SOR2T2.strength)
SOR2T2.strength<-SOR2T2.strength[(nrow(SOR2T2.cons.node.df)+1):nrow(SOR2T2.strength),]

#### SPring OSR Round 2 Transect 3 ####
SOR2T3.null.df <- null.df %>%
  filter(Sowing == "Spring", Crop == "OSR", Round == 2, Transect == 3) %>%
  droplevels()

SOR2T3.null.sub.df <- subset(SOR2T3.null.df, Weight > 0)
SOR2T3.null.sub.df$weight<- SOR2T3.null.sub.df$Weight

SOR2T3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR2T3.null.sub.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR2T3.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR2T3.null.sub.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR2T3.node.df <- rbind(SOR2T3.cons.node.df, SOR2T3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR2T3.node.df[SOR2T3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR2T3.node.df$tr.height == 0)))
SOR2T3.node.df[SOR2T3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR2T3.node.df$tr.height == 0.5)))

SOR2T3.null.net <- graph_from_data_frame(d = SOR2T3.null.sub.df, vertices = SOR2T3.node.df, directed = TRUE)
plot(SOR2T3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR2T3.null.net, layout = as.matrix(SOR2T3.node.df[, c("x.coord", "tr.height")]))

SOR2T3.null.net2 <- fortify(SOR2T3.null.net, layout = as.matrix(SOR2T3.node.df[, c("x.coord", "tr.height")]))

SOR2T3.null.net2$tr.height <- (SOR2T3.null.net2$tr.height - 1) / 2
SOR2T3.null.net2$y <- SOR2T3.null.net2$y / 2 -0.006
SOR2T3.null.net2$yend <- SOR2T3.null.net2$yend / 2 -0.006

ggplot(SOR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR2T3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR2T3.nullnetbip <- ggplot(SOR2T3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR2T3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 2 transect 3 network")
SOR2T3.nullnetbip

png(file = "Spring OSR Round 2 transect 3 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR2T3.nullnetbip
dev.off()

#strength
SOR2T3.strength <- data.frame(strength(SOR2T3.null.net))
SOR2T3.strength$taxon <- rownames(SOR2T3.strength)
SOR2T3.strength<-SOR2T3.strength[(nrow(SOR2T3.cons.node.df)+1):nrow(SOR2T3.strength),]
##### Spring OSR Round 3 Network no spiders#####

SOR3.null.df <- null.df %>%
  filter(Sowing == "Winter", Crop == "Wheat", Round == 3) %>%
  droplevels()

SOR3.cons.node.df <- data.frame(node = unique(c(levels(as.factor(SOR3.null.df$Consumer))))) %>%
  mutate(tr.height = 0.5, level = "Higher", x.coord = rep(1))
SOR3.res.node.df <- data.frame(node = unique(c(levels(as.factor(SOR3.null.df$Resource))))) %>%
  mutate(tr.height = 0, level = "Lower", x.coord = rep(1))
SOR3.node.df <- rbind(SOR3.cons.node.df, SOR3.res.node.df) # These lines create a dataframe that contains some basic information about the nodes in the networks - what level they are on and which coordinates they should be plotted at
SOR3.node.df[SOR3.node.df$tr.height == 0, ]$x.coord <- 
  seq(0, 1, length.out = (sum(SOR3.node.df$tr.height == 0)))
SOR3.node.df[SOR3.node.df$tr.height == 0.5, ]$x.coord <- 
  seq(.2, .8, length.out = (sum(SOR3.node.df$tr.height == 0.5)))

SOR3.null.net <- graph_from_data_frame(d = SOR3.null.df, vertices = SOR3.node.df, directed = TRUE)
plot(SOR3.null.net) # This is step one in plotting the network - it will look a bit weird at the moment because it's all the interactions in one network, and based on very few prey groups from my example dataframe

plot(SOR3.null.net, layout = as.matrix(SOR3.node.df[, c("x.coord", "tr.height")]))

SOR3.null.net2 <- fortify(SOR3.null.net, layout = as.matrix(SOR3.node.df[, c("x.coord", "tr.height")]))

SOR3.null.net2$tr.height <- (SOR3.null.net2$tr.height - 1) / 2
SOR3.null.net2$y <- SOR3.null.net2$y / 2 -0.006
SOR3.null.net2$yend <- SOR3.null.net2$yend / 2 -0.006

ggplot(SOR3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_point(data = SOR3.node.df, aes(x = x.coord, y = tr.height),
             inherit.aes = FALSE, size = 5) +
  theme_blank()

nodepal <- c("darkgreen", "#FDE725FF")

SOR3.nullnetbip <- ggplot(SOR3.null.net2, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.4, colour = "gray60", aes(size = (abs(Weight)))) +
  geom_point(data = SOR3.node.df, aes(x = x.coord, y = tr.height, fill = level), shape = 21,
             inherit.aes = FALSE, alpha = 0.8, size = 4) +
  theme_blank() + coord_fixed() +
  scale_fill_manual(guide = 'none', values = nodepal)+
  theme(legend.key.size = unit(0.5, 'cm'), legend.text = element_text(size=10), plot.title = element_text(size=16, face = "bold", hjust=0.5), legend.title = element_text(size=12, face = "bold"), legend.key.width = unit(0.4, "cm")) +
  scale_size(guide = "none") + ggtitle("Spring OSR round 3 network")
SOR3.nullnetbip

png(file = "Spring OSR Round 3 network.png", width = 20, height = 20, res = 150, units = "cm")
SOR3.nullnetbip
dev.off()

##### Network merged plot #####
install.packages("egg")
library(egg)

ggarrange(WOR1.nullnetbip, SOR1.nullnetbip, WOR2.nullnetbip, SOR2.nullnetbip, WOR3.nullnetbip, SOR3.nullnetbip, ncol = 2, nrow = 3)

png(file = "OSR networks.png", width = 40, height = 60, res = 150, units = "cm")
ggarrange(WOR1.nullnetbip, SOR1.nullnetbip, WOR2.nullnetbip, SOR2.nullnetbip, WOR3.nullnetbip, SOR3.nullnetbip, ncol = 2, nrow = 3)
dev.off()

ggarrange(WWR1.nullnetbip, SWR1.nullnetbip, WWR2.nullnetbip, SWR2.nullnetbip, WWR3.nullnetbip, SWR3.nullnetbip, ncol = 2, nrow = 3)

png(file = "Wheat networks.png", width = 40, height = 60, res = 150, units = "cm")
ggarrange(WWR1.nullnetbip, SWR1.nullnetbip, WWR2.nullnetbip, SWR2.nullnetbip, WWR3.nullnetbip, SWR3.nullnetbip, ncol = 2, nrow = 3)
dev.off()

#####analysis of degree#####

networks <- list(SOR1T1.null.net, SOR1T2.null.net, SOR1T3.null.net, SOR2T1.null.net, SOR2T2.null.net, SOR2T3.null.net, SWR1T1.null.net, SWR1T2.null.net, SWR2T1.null.net, SWR2T2.null.net, SWR2T3.null.net, SWR3T1.null.net, SWR3T2.null.net, SWR3T3.null.net, WWR1T1.null.net, WWR1T2.null.net, WWR2T1.null.net, WWR2T2.null.net, WWR2T3.null.net, WWR3T1.null.net, WWR3T2.null.net, WWR3T3.null.net, WOR1T1.null.net, WOR1T2.null.net, WOR1T3.null.net, WOR2T1.null.net, WOR2T2.null.net, WOR2T3.null.net)

names(networks) <- c("SOR1T1", "SOR1T2", "SOR1T3", "SOR2T1", "SOR2T2", "SOR2T3", "SWR1T1", "SWR1T2", "SWR2T1", "SWR2T2", "SWR2T3", "SWR3T1", "SWR3T2", "SWR3T3", "WWR1T1", "WWR1T2", "WWR2T1", "WWR2T2", "WWR2T3", "WWR3T1", "WWR3T2", "WWR3T3", "WOR1T1", "WOR1T2", "WOR1T3", "WOR2T1", "WOR2T2", "WOR2T3")

### Degree 

degree_extract <- function(net){ 
  degree.d <- degree(net)
  spid.d <- degree.d[1]
  return(spid.d)
}



res_dframe <- data.frame(network_ID = names(networks), degree = rep(0,length(networks)))

res_dframe$degree <- sapply(networks, degree_extract)

summary(res_dframe)

library(stringr)

res_dframe$Season <- str_sub(res_dframe$network_ID, 1, 1)
res_dframe$Crop <- str_sub(res_dframe$network_ID, 2, 2)
res_dframe$Round <- str_sub(res_dframe$network_ID, 4, 4)
res_dframe$Transect<- str_sub(res_dframe$network_ID, 6, 6)



res_dframe$Season <- as.factor(res_dframe$Season)
res_dframe$Crop <- as.factor(res_dframe$Crop)
res_dframe$Round <- as.factor(res_dframe$Round)
res_dframe$Transect <- as.factor(res_dframe$Transect)

summary(res_dframe)

degreemod<- glm(degree~ Crop*Round*Season, family = poisson (link = log), data = res_dframe)

#check assumptions real quick
theta <- degreemod$deviance/degreemod$df.residual
theta # theta is 0.5664147, therefore data not overdispersed

#check residuals 

devresid <- resid(degreemod, type = "deviance")
plot(devresid~degreemod$fitted.values)

plot(devresid~res_dframe$Crop)
plot(devresid~res_dframe$Season)
plot(devresid~res_dframe$Round) #only one with a pattern really present

AIC(degreemod)
#log = 143.4652
#identity = 143.4652
#sqrt = "   "

drop1(degreemod, test = "Chi") # remove interaction

summary.glm(degreemod)

degreemod2<- glm(degree~ Crop*Round + Crop*Season + Season*Round, family = poisson (link = log), data = res_dframe)
summary.glm(degreemod2)

drop1(degreemod2, test = "Chi") # remove crop:season interaction

degreemod3<- glm(degree~ Crop*Round + Season*Round, family = poisson (link = log), data = res_dframe)

summary.glm(degreemod3) #remove crop:round interaction 
drop1(degreemod3)

degreemod4<- glm(degree~Round*Season + Crop, family = poisson (link = log), data = res_dframe)

drop1(degreemod4)

degreemod5<- glm(degree~Round+Season + Crop, family = poisson (link = log), data = res_dframe)

drop1(degreemod5) #remove crop

degreemod6<- glm(degree~Season+Round,  family = poisson (link = log), data = res_dframe)
drop1(degreemod6)#remove round

degreemod7<- glm(degree ~Season,  family = poisson (link = "identity"), data = res_dframe)
degreemod8 <- glm(degree ~1,  family = poisson (link = log), data = res_dframe)

anova(degreemod7, degreemod8)
#no significance
summary.lm (degreemod7)
#r-squared value = 0.37 i.e. 37% of deviance explained
summary.lm(degreemod8)

anova(degreemod7, degreemod8)
#these models are not significantly different from one another 

#### analysis of strength ####
#do same as above, but with the strength data sets, so higher proportion, higher selection of prey. can use as proxy for bio-control

strength.df <- list(SOR1T1.strength, SOR1T2.strength, SOR1T3.strength, 
                 SOR2T1.strength, SOR2T2.strength, SOR2T3.strength, 
                 SWR1T1.strength, SWR1T2.strength, SWR2T1.strength, 
                 SWR2T2.strength, SWR2T3.strength, SWR3T1.strength, 
                 SWR3T2.strength, SWR3T3.strength, WWR1T1.strength, 
                 WWR1T2.strength, WWR2T1.strength, WWR2T2.strength, 
                 WWR2T3.strength, WWR3T1.strength, WWR3T2.strength, 
                 WWR3T3.strength, WOR1T1.strength, WOR1T2.strength, 
                 WOR1T3.strength, WOR2T1.strength, WOR2T2.strength, 
                 WOR2T3.strength)

                             

names(strength.df) <- c("SOR1T1", "SOR1T2", "SOR1T3", "SOR2T1", "SOR2T2", "SOR2T3", "SWR1T1", "SWR1T2", "SWR2T1", "SWR2T2", "SWR2T3", "SWR3T1", "SWR3T2", "SWR3T3", "WWR1T1", "WWR1T2", "WWR2T1", "WWR2T2", "WWR2T3", "WWR3T1", "WWR3T2", "WWR3T3", "WOR1T1", "WOR1T2", "WOR1T3", "WOR2T1", "WOR2T2", "WOR2T3")




# List of all your strength data frames
strength.df <- list(SOR1T1.strength, SOR1T2.strength, SOR1T3.strength, 
                    SOR2T1.strength, SOR2T2.strength, SOR2T3.strength, 
                    SWR1T1.strength, SWR1T2.strength, SWR2T1.strength, 
                    SWR2T2.strength, SWR2T3.strength, SWR3T1.strength, 
                    SWR3T2.strength, SWR3T3.strength, WWR1T1.strength, 
                    WWR1T2.strength, WWR2T1.strength, WWR2T2.strength, 
                    WWR2T3.strength, WWR3T1.strength, WWR3T2.strength, 
                    WWR3T3.strength, WOR1T1.strength, WOR1T2.strength, 
                    WOR1T3.strength, WOR2T1.strength, WOR2T2.strength, 
                    WOR2T3.strength)

# Assigning names to the list
names(strength.df) <- c("SOR1T1", "SOR1T2", "SOR1T3", "SOR2T1", "SOR2T2", "SOR2T3", 
                        "SWR1T1", "SWR1T2", "SWR2T1", "SWR2T2", "SWR2T3", "SWR3T1", 
                        "SWR3T2", "SWR3T3", "WWR1T1", "WWR1T2", "WWR2T1", "WWR2T2", 
                        "WWR2T3", "WWR3T1", "WWR3T2", "WWR3T3", "WOR1T1", "WOR1T2", 
                        "WOR1T3", "WOR2T1", "WOR2T2", "WOR2T3")

# Ensure all data frames have consistent column names
strength.df <- lapply(strength.df, function(df) {
  colnames(df) <- c("strength", "taxon")
  return(df)
})


# Combining all data frames into one
combined_df <- do.call(rbind, lapply(names(strength.df), function(name) {
  df <- strength.df[[name]]
  df$network_ID <- name # Add network ID as a new column
  df
}))

# Resetting the row names (optional)
row.names(combined_df) <- NULL

# Preview the combined data frame
summary(combined_df)
combined_df$taxon<-as.factor(combined_df$taxon)


plot(combined_df$strength~combined_df$taxon, data = combined_df) #crappy preliminary plot

####do some plots highlighting specific factors e.g crop

combined_df$Season <- str_sub(combined_df$network_ID, 1, 1)
combined_df$Crop <- str_sub(combined_df$network_ID, 2, 2)
combined_df$Round<- str_sub(combined_df$network_ID, 4, 4)
combined_df$Transect<- str_sub(combined_df$network_ID, 6, 6)



combined_df$Season <- as.factor(combined_df$Season)
combined_df$Crop <- as.factor(combined_df$Crop)
combined_df$Round <- as.factor(combined_df$Round)
combined_df$Transect <- as.factor(combined_df$Transect)

summary(combined_df)

strenmod<-glm(strength~Crop*taxon + Season*taxon + Round*taxon, family = Gamma, data = combined_df)
strenmod2<-glm(strength~ Crop*Round*Season + Round*taxon + Crop*taxon + Season*taxon, family = Gamma (link = "log"), data = combined_df)
summary.lm(strenmod2)

anova(strenmod, test = "Chisq")
anova(strenmod2, test = "Chisq")

#r-squared = 0.6956 i.e. 69.5%
drop1(strenmod2)
summary.lm(strenmod)




###plots
library(ggplot2)

# model predictions
combined_df$predicted_strength <- predict(strenmod, type = "response")

# Plot predicted strength by crop and taxon
ggplot(combined_df, aes(x = taxon, y = predicted_strength, color = Crop)) +
  geom_point(alpha = 0.4, size = 2.8) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log"))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1, fill = "white", aes(color = Crop), stroke = 1.5) +
  labs(title = "",
       x = "",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), axis.title.x = element_text(angle = 90), axis.text.y = element_text(angle = 90) )
)

#box plot for crop
ggplot(combined_df, aes(x = Crop, y = strength)) +
  geom_boxplot(aes(x = Crop, y = strength, fill = Crop), outlier.colour = NA, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", aes(color = Crop), stroke = 1.5) +
  theme_minimal() +
  labs(title = "",
       x = "Crop",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme(axis.text = element_text( size =15), axis.title = element_text(size =18), legend.text = element_text(size = 15), legend.title = element_text(size = 18))




# Plot predicted strength by season and taxon
ggplot(combined_df, aes(x = taxon, y = predicted_strength, color = Season)) +
  geom_point(alpha = 0.4, size = 2.8) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log"))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1, fill = "white", aes(color = Season), stroke = 1.5) +
  labs(title = "",
       x = "",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), axis.title.x = element_text(angle = 90), axis.text.y = element_text(angle = 90) )
)


#box plot for season
ggplot(combined_df, aes(x = Season, y = strength)) +
  geom_boxplot(aes(x = Season, y = strength, fill = Season), outlier.colour = NA, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", aes(color = Season), stroke = 1.5) +
  theme_minimal() +
  labs(title = "",
       x = "Season",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") +
  theme(axis.text = element_text( size =15), axis.title = element_text(size =18), legend.text = element_text(size = 15), legend.title = element_text(size = 18))

  

# Plot predicted strength by date and taxon
ggplot(combined_df, aes(x = taxon, y = predicted_strength, color = Round)) +
  geom_point(alpha = 0.4, size = 2.8) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log"))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white", aes(color = Round), stroke = 1.5) +
  labs(title = "",
       x = "",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "inferno") + 
  scale_fill_viridis_d(option = "inferno") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 11), axis.title.x = element_text(angle = 90), axis.text.y = element_text(angle = 90) )
)

#box plot for round
ggplot(combined_df, aes(x = Round, y = strength)) +
  geom_boxplot(aes(x = Round, y = strength, fill = Round), outlier.colour = NA, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", aes(color = Round), stroke = 1.5) +
  theme_minimal() +
  labs(title = "",
       x = "Sampling Round",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") +
  theme(axis.text = element_text( size =15), axis.title = element_text(size =18), legend.text = element_text(size = 15), legend.title = element_text(size = 18))


#####pest vs beneficial####

#subset data to just beneficial and pest groups

summary(combined_df)
combined_df
library(dplyr)

pest.df <- combined_df %>%
  filter(!(taxon %in% c("Staphylinidae", "Phalacridae", "Cryptophagidae", "Coleoptera", "Elateridae", "Formicidae", "Lasiusflavus", "Hymenoptera", "Culicidae" , "Diptera" , "Brachycera" ,  "Dolichopodidae" , "Chironomidae" ,  "Stratiomyidae" )))

summary(pest.df)

# create list of pests
pest_taxa <- c("Curculionidae", "Phyllotreta", "Aphibidae", "Lepidoptera", "Silvanidae", "Keroplatidae", "Thripidae", "Collembola", "Chrysopidae", "Chrysomelidae")  

# add status collumn, where if pest_taxa then = pest, if not beneficial
pest.df$status <- ifelse(pest.df$taxon %in% pest_taxa, "Pest", "Beneficial")

pest.df$status<-as.factor(pest.df$status)

summary(pest.df)

pest.df$status <- relevel(pest.df$status, ref = "Pest")

##model

pestmod<-glm(strength~Crop*Season*status*Round, family = Gamma (link = "log"), data = pest.df)
summary.lm(pestmod)
anova(pestmod, test = "Chisq")

drop1(pestmod)

pestmod2<-glm(strength~Crop*status + Season*status + status*Round, family = Gamma (link = "log"), data = pest.df)
anova(pestmod2, test = "Chisq")

pestmod3<-glm(strength~Crop*status + Season*status, family = Gamma (link = "log"), data = pest.df)
anova(pestmod3, test = "Chisq")

pestmod4<-glm(strength~Crop*status, family = Gamma (link = "log"), data = pest.df)
pestmod4rl<-glm(strength~Crop*status, family = Gamma (link = "log"), data = pest.df)
anova(pestmod4)
summary.glm(pestmod4rl)
#r-squared = 9.6% thus the model is only explaining minimal variance in the strength of consumption
plot(pestmod4) #assumptions all good

# Plot predicted strength by status and crop
ggplot(pest.df, aes(x = status, y = predicted_strength, color = Crop)) +
  geom_point(alpha = 0.4, size = 2.8) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log"))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white", aes(color = Crop), stroke = 1.5) +
  labs(title = "",
       x = "Status",
       y = "Weighted Degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 15) )

ggplot(pest.df, aes(x = status, y = predicted_strength, color = Round)) +
  geom_point(alpha = 0.4, size = 2.8) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log"))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white", aes(color = Round), stroke = 1.5) +
  labs(title = "",
       x = "Status",
       y = "Predicted Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))

ggplot(pest.df, aes(x = status, y = predicted_strength, color = Season)) +
  geom_point(alpha = 0.4, size = 2.8) +
  geom_smooth(method = "glm", method.args = list(family = Gamma(link = "log"))) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white", aes(color = Season), stroke = 1.5) +
  labs(title = "",
       x = "Status",
       y = "Predicted Strength") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11), axis.text.y = element_text(angle = 90) )


#box plot for round
ggplot(pest.df, aes(x = status, y = strength)) +
  geom_boxplot(aes(x = status, y = strength, fill = status), outlier.colour = NA, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", aes(color = status), stroke = 1.5) +
  theme_minimal() +
  labs(title = "",
       x = "Status",
       y = "Weighted degree") +
  scale_colour_viridis_d(option = "turbo") + 
  scale_fill_viridis_d(option = "turbo") +
  theme(axis.text = element_text( size =15), axis.title = element_text(size =18), legend.text = element_text(size = 15), legend.title = element_text(size = 18))


citation("igraph")
