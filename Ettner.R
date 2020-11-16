# load the data -----------------------------------------------------------

# Get and print current working directory.
print(getwd())

# Set current working directory.
setwd("/web/com")

# Get and print current working directory.
print(getwd())



data <- read.csv(file = "file:///C:/Users/Desktop/tex.csv")

install.packages("ggplot2")
install.packages("multcompView")
install.packages("plyr")
install.packages("gridExtra")
install.packages("dplyr")
#also load required packages
library(ggplot2)
library(multcompView)
library(plyr)
library(gridExtra)
library(dplyr)
# preview the data ---------------------------------------------------------


tob <- ggplot(data, aes(x=line, y=tob, fill = rep))+ #x= the groups you want on your x-axis, y=the variable you want to plot, fill=the differnt groups, here replicates 
  geom_boxplot(alpha=0.7) + #put here all "aestetic parameters, alpha =transparency
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "time [days]", limit = c(15,30)) + #name=s.a, limit= the range of your y-axis
  ggtitle("Time of bolting") + #title
  theme_bw() + #the backgroundcolour off your plot ()= transparent, theme= font, size and position of the labeling
  theme(plot.title = element_text(hjust= 0.5, face = "bold"), #hjust = position of the title, 0.5=middle, face=special font format
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + #your colours
  labs(fill = "Replicate") #the labeling of your legend
tob #just for visualizing it in the right bottom panel, use pdf() + dev.off() to save your figures

rdb <- ggplot(data, aes(x= line, y=rdb, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "diameter [mm]", limit = c(20,90)) +
  ggtitle("Rosette diameter") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
rdb

leaves <- ggplot(data, aes(x= line, y=leaves, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "number", limit = c(5,17)) +
  ggtitle("Number of leaves") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
leaves



height28 <- ggplot(data, aes(x= line, y=height28, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "height [mm]", limit = c(200,1200)) +
  ggtitle("height after 28 Days") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
height28


rd28 <- ggplot(data, aes(x= line, y=rd28, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "diameter [mm]", limit = c(0,120)) +
  ggtitle("Rosette diameter") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
rd28

pI42 <- ggplot(data, aes(x= line, y=pI42, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "number", limit = c(0,10)) +
  ggtitle("Primary inflorescence") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
pI42


#sI42 <- ggplot(data, aes(x= line, y=sI42, fill = rep))+
#  geom_boxplot(alpha=0.7) + 
#  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
#  scale_y_continuous(name= "number", limit = c(0,12)) +
#  ggtitle("Secondary inflorescence") +
#  theme_bw() +
#  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
#        text = element_text(size = 12),
#        axis.title.y = element_text(face = "bold" ),
#        axis.title.x = element_blank(),
#        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
#        legend.position ="none")+
#  scale_fill_brewer(palette = "Accent") + 
#  labs(fill = "Replicate")
#sI42

#Silique <- ggplot(data, aes(x= line, y=data$siliques, fill = rep))+
#  geom_boxplot(alpha=0.7) + 
#  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
#  scale_y_continuous(name= "number", limit = c(0,40)) +
#  ggtitle("Number of siliques") +
#  theme_bw() +
#  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
#        text = element_text(size = 12),
#        axis.title.y = element_text(face = "bold" ),
#        axis.title.x = element_blank(),
#        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
#        legend.position ="none")+
#  scale_fill_brewer(palette = "Accent") + 
#  labs(fill = "Replicate")
#Silique


rd42 <- ggplot(data, aes(x= line, y=rd42, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "diameter [mm]", limit = c(0,120)) +
  ggtitle("Rosette diameter") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
rd42


height42 <- ggplot(data, aes(x= line, y=height42, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "height [mm]", limit = c(200,6000)) +
  ggtitle("height after 42 Days") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
height42




rd56 <- ggplot(data, aes(x= line, y=rd56, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "diameter [mm]", limit = c(0,120)) +
  ggtitle("Rosette diameter") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
rd56


height56 <- ggplot(data, aes(x= line, y=height56, fill = rep))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("Col_0", "3xNXF", "4xNXF", "6xNXF_2.1", "6xNXF_2.2", "6xNXF_2.3", "6xNXF_2.5", "6xNXF_2.6", "6xNXF_2.7", "7xNXF")) + #name = which labeling on the x scale, limits= the order you want
  scale_y_continuous(name= "height [mm]", limit = c(200,6000)) +
  ggtitle("Final height") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face = "bold" ),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 11, angle = 45, hjust = 1),
        legend.position ="none")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate")
height56






pdf(file = "all data.pdf")
grid.arrange(tob,	leaves,	rdb,	rd28,	height28,	pI,	height42,	rd42,	height56,	rd56, ncol=2)
dev.off()

jpeg(file = "all data.jpeg", quality = 100, width = 750, height = 750)
grid.arrange(tob,	leaves,	rdb,	rd28,	height28,	pI,	height42,	rd42,	height56,	rd56, ncol=2)
dev.off()


#pdf(file = "all data_vegetative.pdf")
#grid.arrange(tob, height56, leaves, rd28, ncol=2)
#dev.off()

#jpeg(file = "all data_vegetative.jpeg", quality = 100, width = 750, height = 750)
#grid.arrange(tob, height56, leaves, rd28, ncol=2)
#dev.off()

#pdf(file = "all data_generative.pdf")
#grid.arrange(pI42, sI42, Silique, eS, ncol=2)
#dev.off()

#jpeg(file = "all data_generative.jpeg", quality = 100, width = 750, height = 750)
#grid.arrange(pI42, sI42, Silique, eS, ncol=2)
#dev.off()


# analyze the data --------------------------------------------------------
#after visual observation of the data, I choosed the second replicate and the spt16_WT3 and spt16_PM2 for further analyses
#pooling between the replicates would create a huge range, where single differences would be lost
#the different lines should also not be pooled, the best rescue constructs are choosen. 



rd_data <- data[data$rep=="B", ]
col_0 <- rd_data[rd_data$line=="col_0",]
NXF3x <- rd_data[rd_data$line=="3xNXF",]
NXF4x <- rd_data[rd_data$line=="4xNXF",]
NXF6x2.1 <- rd_data[rd_data$line=="NXF6x2.1",]
NXF6x2.2 <- rd_data[rd_data$line=="NXF6x2.2",]
NXF6x2.3 <- rd_data[rd_data$line=="NXF6x2.3",]
NXF6x2.5 <- rd_data[rd_data$line=="NXF6x2.5",]
NXF6x2.6 <- rd_data[rd_data$line=="NXF6x2.6",]
NXF6x2.7 <- rd_data[rd_data$line=="NXF6x2.7",]
NXF7x <- rd_data[rd_data$line=="NXF7x",]

new_data <- rbind(col_0, NXF3x, NXF4x, NXF6x2.1, NXF6x2.2, NXF6x2.3,  NXF6x2.5,   NXF6x2.6, NXF6x2.7,  NXF7x)



#to measure the overall differneces for more than two unpaired, parametric groups with equal variances, a ANOVA will be performed
#for my data following parameters will be interesting to analyse further (I just compared them visual)
#rdb, leaves, rd28, eS 

ANOVA <- aov(new_data$rdb ~ new_data$line)
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data$line", conf.level = 0.95)
t_Tukey <- as.data.frame(TUKEY[1:1])
print(t_Tukey)
write.csv(t_Tukey, file = "Tukey_rdb.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )

# I need to group the treatments that are not different each other together.
generate_label_df <- function(HSD, flev){
  
  # Extract labels and factor levels from Tukey post-hoc 
  test.levels <- HSD[[flev]][,4]
  print(test.levels)
  test.labels <- multcompLetters(test.levels, reversed = TRUE)['Letters']
  #I need to put the labels in the same order as in the boxplot 
  print(test.labels)
  plot.labels <- names(test.labels[['Letters']])
  print(plot.labels)
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data, flev, function (x) max(fivenum(x$rdb)) + 10)
  print(boxplot.df)
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = test.labels[['Letters']],
                            stringsAsFactors = FALSE)
  print(plot.levels)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  print(labels.df)
  return(labels.df)
}

#Draw a basic boxplot
t_rdb <- ggplot(new_data, aes(x= line, y=rdb))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("col_0", "spt16", "spt16_WT3", "spt16_PM2")) + 
  scale_y_continuous(name= "Diameter [mm]", limit = c(20,120)) +
  ggtitle("Rosette diameter at bolting") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data$line'), aes(x = plot.labels, y = V1, label = labels))
t_rdb



#--> copy and paste line 167 - 217 and chance eS for leaves

ANOVA <- aov(new_data$leaves ~ new_data$line)
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data$line", conf.level = 0.95)
t_Tukey <- as.data.frame(TUKEY[1:1])
print(t_Tukey)
write.csv(t_Tukey, file = "Tukey_leaves.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )




# I need to group the treatments that are not different each other together.
generate_label_df <- function(HSD, flev){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  print(Tukey.levels)
  Tukey.labels <- multcompLetters(Tukey.levels, reversed = TRUE)['Letters']
  print(Tukey.labels)
  plot.labels <- names(Tukey.labels[['Letters']])
  print(plot.labels)
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data, flev, function (x) max(fivenum(x$leaves)) + 1)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}


#Draw a basic boxplot
t_leaves <- ggplot(new_data, aes(x= line, y=leaves))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("col_0", "spt16", "spt16_WT3", "spt16_PM2")) + 
  scale_y_continuous(name= "Number", limit = c(5,18)) +
  ggtitle("Number of leaves at bolting") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data$line'), aes(x = plot.labels, y = V1, label = labels))
t_leaves

#rd28 <- just use find and replace (ctrl + shift + j) and excange eS through rd 28 (should be 6 replacements) 

ANOVA <- aov(new_data$rd28 ~ new_data$line)
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data$line", conf.level = 0.95)
t_Tukey <- as.data.frame(TUKEY[1:1])
print(t_Tukey)
write.csv(t_Tukey, file = "Tukey_rd28.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )




# I need to group the treatments that are not different each other together.
generate_label_df <- function(HSD, flev){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  print(Tukey.levels)
  Tukey.labels <- multcompLetters(Tukey.levels, reversed = TRUE)['Letters']
  print(Tukey.labels)
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data, flev, function (x) max(fivenum(x$rd28)) + 10)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}


#Draw a basic boxplot
t_rd28 <- ggplot(new_data, aes(x= line, y=rd28))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("col_0", "spt16", "spt16_WT3", "spt16_PM2")) + 
  scale_y_continuous(name= "Diameter [mm]", limit = c(20,120)) +
  ggtitle("Rosette diameter at DAS28") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data$line'), aes(x = plot.labels, y = V1, label = labels)) +
  geom_point(alpha = 0.5)
t_rd28



#eS


ANOVA <- aov(new_data$eS ~ new_data$line)
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data$line", conf.level = 0.95)
ordert <- as.data.frame(c("spt16_WT3-col_0", "spt16_PM2-col_0", "spt16-col_0","spt16_PM2-spt16", "spt16_WT3-spt16", "spt16_WT3-spt16_PM2"))
t_Tukey <- as.data.frame(TUKEY[1:1], levels = c("spt16_WT3-col_0", "spt16_PM2-col_0", "spt16-col_0","spt16_PM2-spt16", "spt16_WT3-spt16", "spt16_WT3-spt16_PM2"))
TUKEY_1 <- t_Tukey[match(ordert, rownames(t_Tukey)),]
t_Tukey
write.csv(t_Tukey, file = "Tukey_eS.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )
match?
  df <- data.frame(name=letters[1:4], value=c(rep(TRUE, 2), rep(FALSE, 2)))
target <- c("b", "c", "a", "d")
df[match(target, df$name),]


# I need to group the treatments that are not different each other together.
generate_label_df <- function(HSD, flev){
  
  # Extract labels and factor levels from Tukey post-hoc 
  t_Tukey.levels <- HSD[[flev]][,4]
  print(t_Tukey.levels)
  t_Tukey.labels <- multcompLetters(t_Tukey.levels, reversed = TRUE)['Letters']
  print(t_Tukey.labels)
  plot.labels <- names(t_Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data, flev, function (x) max(fivenum(x$eS)) + 10)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = t_Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}


#Draw a basic boxplot
t_eS <- ggplot(new_data, aes(x= line, y=eS))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("col_0", "spt16", "spt16_WT3", "spt16_PM2")) + 
  scale_y_continuous(name= "fully developed siliques in %", limit = c(0,110)) +
  ggtitle("fully developed siliques") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data$line'), aes(x = plot.labels, y = V1, label = labels))
t_eS



rd_data <- data[data$rep=="A", ]
Col_0 <- rd_data[rd_data$line=="Col_0",]
spt16 <- rd_data[rd_data$line == "spt16",]
spt16_WT3 <- rd_data[rd_data$line=="spt16_WT3",]
spt16_PM2 <- rd_data[rd_data$line=="spt16_PM2",]
new_data <- rbind(Col_0, spt16, spt16_WT3, spt16_PM2)


ANOVA <- aov(new_data$height56 ~ new_data$line)
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data$line", conf.level = 0.95)
t_Tukey <- as.data.frame(TUKEY[1:1])
write.csv(t_Tukey, file = "Tukey_eS.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )




# I need to group the treatments that are not different each other together.
generate_label_df <- function(HSD, flev){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data, flev, function (x) max(fivenum(x$height56)) + 30)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}


#Draw a basic boxplot
t_height <- ggplot(new_data, aes(x= line, y=height56))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("col_0", "spt16", "spt16_WT3", "spt16_PM2")) + 
  scale_y_continuous(name= "height at day 56 [mm]", limit = c(200,520)) +
  ggtitle("final height") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data$line'), aes(x = plot.labels, y = V1, label = labels))
t_height



ANOVA <- aov(new_data$seeds ~ new_data$line)
summary(ANOVA) # I habe a significant result, but I dont know between which groups the effect is significant -> TUKEY

TUKEY <- TukeyHSD(x=ANOVA, "new_data$line", conf.level = 0.95)
t_Tukey <- as.data.frame(TUKEY[1:1])
write.csv(t_Tukey, file = "Tukey_seeds.csv") #An Excel file will be created, containing the Tukey results
# Tuckey test representation :
plot(TUKEY , las=1 , col="brown" )




# I need to group the treatments that are not different each other together.
generate_label_df <- function(HSD, flev){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- HSD[[flev]][,4]
  Tukey.labels <- multcompLetters(Tukey.levels, reversed = T)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  
  # Get highest quantile for Tukey's 5 number summary and add a bit of space to buffer between    
  # upper quantile and label placement
  boxplot.df <- ddply(new_data, flev, function (x) max(fivenum(x$seeds)) + 10)
  
  # Create a data frame out of the factor levels and Tukey's homogenous group letters
  plot.levels <- data.frame(plot.labels, labels = Tukey.labels[['Letters']],
                            stringsAsFactors = FALSE)
  
  # Merge it with the labels
  labels.df <- merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = flev, sort = FALSE)
  
  return(labels.df)
}


#Draw a basic boxplot
t_seeds <- ggplot(new_data, aes(x= line, y=seeds))+
  geom_boxplot(alpha=0.7) + 
  scale_x_discrete(limits = c("col_0", "spt16", "spt16_WT3", "spt16_PM2")) + 
  scale_y_continuous(name= "seeds per silique", limit = c(0,120)) +
  ggtitle("seed set") +
  theme_bw() +
  theme(plot.title = element_text(hjust= 0.5, face = "bold"),
        text = element_text(size = 12),
        axis.title.y = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size = 9, face = "bold.italic"),
        legend.position ="bottom")+
  scale_fill_brewer(palette = "Accent") + 
  labs(fill = "Replicate") +
  geom_text(data = generate_label_df(TUKEY, 'new_data$line'), aes(x = plot.labels, y = V1, label = labels))
t_seeds

#create your output file

pdf(file="Boxplots.pdf")
grid.arrange(t_rdb, t_rd28, t_height, t_eS, ncol=2)
dev.off()

pdf(file = "Boxplots_rdb_leaves_height.pdf", width = 3.5)
grid.arrange(t_rdb, t_leaves, t_height, ncol=1)
dev.off()

pdf(file = "Boxplots_eS_seeds.pdf", height = 4, width = 8)
grid.arrange(t_eS, t_seeds, ncol=2)
dev.off()

# Analyse of the seeds ----------------------------------------------------


seeds <- read.csv(file="file:///C:/Users/hanna/OneDrive/Uni/Master Biology/Focussubject Botany - Biochemistry, Genetics, Development/Internship/Results/seeds P. SPT16.csv")

ANOVA <- aov(seeds$Count ~ seeds$X)
summary(ANOVA)
TUKEY <- TukeyHSD(x=ANOVA, "seeds$X", conf.level = 0.95)

ggplot(seeds, aes(x=X, y=Count))