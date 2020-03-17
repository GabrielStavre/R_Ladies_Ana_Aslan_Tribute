#########################################################
## R-Ladies Bucharest challenge - Tribute to Ana Aslan ##
#########################################################

# Set working directory
#setwd("~/Documents/Plots/RLadies_20200307/GitHub version")


# Housekeeping
rm(list = ls())


# Define libraries
library(ggplot2)
library(grid)
library(dplyr)
library(patchwork)
library(jpeg)
library(magick)
library(gridExtra)
library(ggthemes)
library(readxl)


# Load the official logo of R-Ladies Bucharest (RLB)
RLB <- image_read("R-ladies_Bucharest.png")

# Create grob to add to plot - a title layer
title_layer <- grobTree(rectGrob(gp=gpar(fill="white")),
                 textGrob("\n Ana Aslan, inventor of the Gerovital \n", x=0, hjust=0, 
                          gp=gpar(fontsize=25,col="black")),
                 rasterGrob(RLB, x=1, hjust=1,
                            width = 0.15,
                            height = 0.65))


# 1st plot - the formula
#########################################################


# Create data for geom_segment
d1=data.frame(x=c(2.5,0,0,2.5,5,5), 
             y=c(0,2.5,5,7.5,5,2.5), 
             vx=c(-2.5,0,2.5,2.5,0,-2.5), 
             vy=c(2.5,2.5,2.5,-2.5,-2.5,-2.5))

d2=data.frame(x=c(0.3,2.5,4.5),
              y=c(2.5,7,2.5),
              vx=c(0,2.25,-1.9),
              vy=c(2.5,-2.25,-1.9))

d3=data.frame(x=c(0,5,7.5,7.25,7.75),
              y=c(2.5,5,7.5,7.5,7.5),
              vx=c(-2.5,2.5,2.5,0,0),
              vy=c(-2.5,2.5,-2.5,2.5,2.5))

# Combine data
d <- bind_rows(d1,d2,d3)

# Build first part of the formula
T1 <- ggplot() + 
  geom_segment(data=d, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=2, color="forestgreen") + 
  xlim(-5,25) +
  ylim(-5,25) +
  geom_point(data=d, mapping=aes(x=x, y=y), size=1, shape=21, color="forestgreen", fill = "forestgreen")

# Create data for geom_segment
b <- data.frame(x=c(10,12.5,15,17.5,17.5,17.5,20), 
              y=c(5,7.5,5,7.5,10,7.5,5), 
              vx=c(2.5,2.5,2.5,0,2.5,2.5,2.5), 
              vy=c(2.5,-2.5,2.5,2.5,2.5,-2.5,2.5))

# Build second part of the formula
T2 <- T1 +
  geom_segment(data=b, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=2, color="red") +
  geom_point(data=b, mapping=aes(x=x, y=y), size=1, shape=21, color="red", fill = "red") +
  theme_void()

# Final plot
G1 <- T2 +
  annotate(geom="text", x=-2.5, y=-1, label="paste(H [2], \"N\")", parse = TRUE,color="forestgreen",size = 7) +
  annotate(geom="text", x=7.5, y=11, label="O",color="forestgreen",size = 7) +
  annotate(geom="text", x=7.5, y=5.5, label="C",color="forestgreen",size = 7) +
  annotate(geom="text", x=10, y=4, label="O",color="forestgreen",size = 7) +
  annotate(geom="text", x=12.5, y=8.5, label="paste(CH [2], \"\")", parse = TRUE,color="red",size = 7) +
  annotate(geom="text", x=15, y=4, label="paste(CH [2], \"\")", parse = TRUE,color="red",size = 7) +
  annotate(geom="text", x=18.5, y=8.2, label="N",color="red",size = 7) +
  annotate(geom="text", x=22, y=12.5, label=as.character(expression(paste(C[2],H[5]))), parse = TRUE, color="red", size = 7) +
  annotate(geom="text", x=24.5, y=7.5, label=as.character(expression(paste(C[2],H[5]))), parse = TRUE, color="red", size = 7) +
  annotate(geom="text", x=10, y=20, label=as.character(expression(paste("Procaine (",C[13],H[20],N[2],O[2],")"))), 
           parse = TRUE, color="black",size = 10) +
  theme(panel.background = element_rect(fill = "blanchedalmond"))
  
G1


# 2nd plot - the Lady
#########################################################

# Load image with Ana Aslan
img <- image_read("ana_aslan.jpg")

# Define points to set the relative space for further positioning
position = data.frame(x = c(1,2),
                      y = c(1,2))

# Make plot
G2 <- ggplot(data = position,aes(x = x, y = y)) +
  geom_point(color = "blanchedalmond") +
  annotation_custom(rasterGrob(image_blur(img), 
                               width = 0.75,
                               height = 0.5)) +
  theme_void() +
  annotate(geom = "text",x=1.5,y=1.10,
           label="To grow old in a beautiful and 
           dignified way is at the same time 
           a science and an art.",
           size = 5, fontface = 'italic', hjust = 0.5) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        axis.line=element_blank(),
        panel.background = element_rect(fill = "blanchedalmond"))

G2


# 3rd plot - timeline with most important events
#########################################################

# Load events
events <- read_excel("milestones.xlsx")
events$value <- 5
events$y_end <- c(0.3,-0.3,0.3,-0.3,0.3,-0.3,0.3,-0.3)

# Make plot
G3 <- ggplot(data = events, aes(x = date, y = value)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "blanchedalmond")) +
  geom_point(size = 3, shape = 1, stroke = 3) +
  xlim(1890,1990) +
  annotate("point", x = 1900, y = 5.5, color = "blanchedalmond") +
  annotate("point", x = 1900, y = 4.5, color = "blanchedalmond") +
  geom_hline(yintercept=5, linetype="dashed", 
             color = "black", size=0.5) +
  annotate("text", x = events$date[1], y = 4.95, label = events$date[1]) +
  annotate("text", x = events$date[2], y = 5.05, label = events$date[2]) +
  annotate("text", x = events$date[3], y = 4.95, label = events$date[3]) +
  annotate("text", x = events$date[4], y = 5.05, label = events$date[4]) +
  annotate("text", x = events$date[5], y = 4.95, label = events$date[5]) +
  annotate("text", x = events$date[6], y = 5.05, label = events$date[6]) +
  annotate("text", x = events$date[7], y = 4.95, label = events$date[7]) +
  annotate("text", x = events$date[8], y = 5.05, label = events$date[8]) +
  geom_segment(data = events, mapping = aes(x = date, y = value, xend = date, yend = value + y_end),
               size = 1, color = "black") +
  annotate("text", x = events$date[1], y = 5.4, label = "Born in Braila, \n Romania") +
  annotate("text", x = events$date[2], y = 4.6, label = "Procaine was first \n synthesized") +
  annotate("text", x = events$date[3], y = 5.4, label = "Graduates from \n medical school") +
  annotate("text", x = events$date[4], y = 4.6, label = "Experiments on the \n effects of procaine \n against aging") +
  annotate("text", x = events$date[5], y = 5.4, label = "Introduction of her drug \n called Gerovital H3 \n in Italy") +
  annotate("text", x = events$date[6], y = 4.6, label = "First Gerovital H3 \n cream made in \n Farmec Labs, Romania") +
  annotate("text", x = events$date[7], y = 5.4, label = "She invented Aslavital, \n aimed to delay \n the skin aging process") +
  annotate("text", x = events$date[8], y = 4.6, label = "Dies age 91")
  
G3

# Combine individual plots into single graph
# OBS: layout_matrix is used to set relative importance of each plot and order
grid.arrange(title_layer, G1, G2, G3, ncol=2, nrow=3,
             layout_matrix = rbind(
               c(1,1,1),
               c(2,2,3),
               c(2,2,3),
               c(4,4,4),
               c(4,4,4)))


# OBS ( !!! ): export/ save plot as 1080x1080
