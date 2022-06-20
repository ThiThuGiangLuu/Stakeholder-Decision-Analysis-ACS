library(cowplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra)
library(tidyverse)
library(egg)

#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("gganimate")
#install.packages("ggpubr")
#install.packages("egg")


#1.Plotting NPV for each stakeholder in Intervention 1####
# Plotting NPV using box plot####  
# Create data frame (not just vector) for plot. The data frame contains
# values of different (groups) for plotting 
#The data frame might need to be grouped to be able to use ggplot 
mc_result<-read.csv("MCResults/y.csv",header = TRUE,sep=",")

# Convert data into thousand unit
npv1<-mc_result[,c(2:14)]/1000

npv1_data_frame <- data.frame(NPV1= c(
  npv1$NPV_Intervention1,npv1$NPV_Hydromet_Intervention1,
  npv1$NPV_DARD_Intervention1,npv1$NPV_ASC_Intervention1,
  npv1$NPV_SMSP_Intervention1, npv1$NPV_SS_Intervention1,
  npv1$NPV_FS_Intervention1,npv1$NPV_PPS_Intervention1,
  npv1$NPV_WU_NGO_Intervention1,npv1$NPV_Rice_Intervention1,
  npv1$NPV_AH_Intervention1,npv1$NPV_Fish_Intervention1,
  npv1$NPV_Public_Intervention1),
  Label = c(rep("Provincial People's Committee", 10000), rep("Provincial Hydromet Station", 10000),
            rep("Provicial Agricultural Department", 10000),rep("District Agricultural Service Centre", 10000),
            rep("SMS service providers", 10000), rep("Seed Suppliers", 10000),
            rep("Fertilizer Suppliers", 10000),rep("Plant Protection Suppliers", 10000),
            rep("Women's Union/NGO", 10000), rep("Rice farmers", 10000),
            rep("Animal Husbandry Farmers", 10000),rep("Fish Farmers", 10000),
            rep("Public", 10000)))
# box_plot geom####   
npv1plot<-ggplot(data = npv1_data_frame, aes(x=NPV1, y= Label))+ 
  geom_boxplot()+theme_bw()+ 
  ggtitle("a) Weather station-SMS-Gender")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  xlab("Net Present Value (thousand USD)")+
  geom_vline(xintercept = 0, col="dark green")+
  ylab("Stakeholder group")+
  scale_x_continuous(breaks = seq(from=-2500, to=7500, by=2500), position="top")

#ggsave("Figures/NPV_Stake_holder_1.tiff", dpi=500, width = 6, height = 4)  

#2.Plotting NPV for each stakeholder in Intervention 2####
mc_result<-read.csv("MCResults/y.csv",header = TRUE,sep=",")
# Convert data into thousand unit
npv2<-mc_result[,c(17:29)]/1000

npv2_data_frame <- data.frame(NPV2= c(
  npv2$NPV_Intervention2,npv2$NPV_Hydromet_Intervention2,
  npv2$NPV_DARD_Intervention2,npv2$NPV_ASC_Intervention2,
  npv2$NPV_SMSP_Intervention2, npv2$NPV_SS_Intervention2,
  npv2$NPV_FS_Intervention2,npv2$NPV_PPS_Intervention2,
  npv2$NPV_WU_NGO_Intervention2,npv2$NPV_Rice_Intervention2,
  npv2$NPV_AH_Intervention2,npv2$NPV_Fish_Intervention2,
  npv2$NPV_Public_Intervention2),
  Label = c(rep("Provincial People's Committee", 10000), rep("Provincial Hydromet Station", 10000),
            rep("Provicial Agricultural Department", 10000),rep("District Agricultural Service Centre", 10000),
            rep("SMS service providers", 10000), rep("Seed Suppliers", 10000),
            rep("Fertilizer Suppliers", 10000),rep("Plant Protection Suppliers", 10000),
            rep("Women's Union/NGO", 10000), rep("Rice farmers", 10000),
            rep("Animal Husbandry Farmers", 10000),rep("Fish Farmers", 10000),
            rep("Public", 10000)))

# box_plot geom####   
npv2plot<-ggplot(data = npv2_data_frame, aes(x=NPV2, y= Label))+ 
  geom_boxplot()+theme_bw()+
  ggtitle("b) SMS-Gender")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  xlab("Net Present Value (thousand USD)")+
  geom_vline(xintercept = 0, col="dark green")+
  ylab("")+
  scale_x_continuous(breaks = seq(from=-2500, to=7500, by=2500), position="top")

#ggsave("Figures/NPV_Stake_holder_2.tiff", dpi=500, width = 6, height = 4)  

#3.Plotting NPV for each stakeholder in Intervention 3####
mc_result<-read.csv("MCResults/y.csv",header = TRUE,sep=",")
# Convert data into thousand unit
npv3<-mc_result[,c(32:45)]/1000

npv3_data_frame <- data.frame(NPV3= c(
  npv3$NPV_Intervention3,npv3$NPV_Hydromet_Intervention3,
  npv3$NPV_DARD_Intervention3,npv3$NPV_ASC_Intervention3,
  npv3$NPV_SMSP_Intervention3, npv3$NPV_SS_Intervention3,
  npv3$NPV_FS_Intervention3,npv3$NPV_PPS_Intervention3,
  npv3$NPV_Village_Leader_Intervention3, npv3$NPV_WU_NGO_Intervention3,
  npv3$NPV_Rice_Intervention3,npv3$NPV_AH_Intervention3,
  npv3$NPV_Fish_Intervention3,npv3$NPV_Public_Intervention3),
  Label = c(rep("Provincial People's Committee", 10000), rep("Provincial Hydromet Station", 10000),
            rep("Provicial Agricultural Department", 10000),rep("District Agricultural Service Centre", 10000),
            rep("SMS service providers", 10000), rep("Seed Suppliers", 10000),
            rep("Fertilizer Suppliers", 10000),rep("Plant Protection Suppliers", 10000),
            rep("Village Leaders", 10000), rep("Women's Union/NGO", 10000), 
            rep("Rice farmers", 10000),rep("Animal Husbandry Farmers", 10000),
            rep("Fish Farmers", 10000),rep("Public", 10000)))

# box_plot geom####   
npv3plot<-ggplot(data = npv3_data_frame, aes(x=NPV3, y= Label))+ 
  geom_boxplot()+theme_bw()+
  ggtitle("c) SMS-Loudspeaker")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  xlab("Net Present Value (thousand USD)")+
  geom_vline(xintercept = 0, col="dark green")+
  ylab("Stakeholder group")+
  scale_x_continuous(breaks = seq(from=-2500, to=7500, by=2500))

#ggsave("Figures/NPV_Stake_holder_3.tiff", dpi=500, width = 6, height = 4)  


#4.Plotting NPV for each stakeholder in Intervention 4####
mc_result<-read.csv("MCResults/y.csv",header = TRUE,sep=",")
# Convert data into thousand unit
npv4<-mc_result[,c(48:61)]/1000

npv4_data_frame <- data.frame(NPV4= c(
  npv4$NPV_Intervention4,npv4$NPV_Hydromet_Intervention4,
  npv4$NPV_DARD_Intervention4,npv4$NPV_ASC_Intervention4,
  npv4$NPV_SMSP_Intervention4, npv4$NPV_SS_Intervention4,
  npv4$NPV_FS_Intervention4,npv4$NPV_PPS_Intervention4,
  npv4$NPV_Village_Leader_Intervention4, npv4$NPV_WU_NGO_Intervention4,
  npv4$NPV_Rice_Intervention4,npv4$NPV_AH_Intervention4,
  npv4$NPV_Fish_Intervention4,npv4$NPV_Public_Intervention4),
  Label = c(rep("Provincial People's Committee", 10000), rep("Provincial Hydromet Station", 10000),
            rep("Provicial Agricultural Department", 10000),rep("District Agricultural Service Centre", 10000),
            rep("SMS service providers", 10000), rep("Seed Suppliers", 10000),
            rep("Fertilizer Suppliers", 10000),rep("Plant Protection Suppliers", 10000),
            rep("Village Leaders", 10000), rep("Women's Union/NGO", 10000), 
            rep("Rice farmers", 10000),rep("Animal Husbandry Farmers", 10000),
            rep("Fish Farmers", 10000),rep("Public", 10000)))

# box_plot geom####   
npv4plot<-ggplot(data = npv4_data_frame, aes(x=NPV4, y= Label))+ 
  geom_boxplot()+theme_bw()+ 
  ggtitle("d) Paper-Loudspeaker")+
  theme(plot.title = element_text(size = 12, face = "bold"))+
  xlab("Net Present Value (thousand USD)")+
  geom_vline(xintercept = 0, col="dark green")+
  ylab("")+
  scale_x_continuous(breaks = seq(from=-2500, to=7500, by=2500))

#ggsave("Figures/NPV_Stake_holder_4.tiff", dpi=500, width = 6, height = 4)  

#Combine Figures####
#https://aosmith.rbind.io/2019/05/13/small-multiples-plot/

NPV_Stake_holder<-cowplot::plot_grid(
                     npv1plot + 
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank() ), 
                     npv2plot+theme(axis.text.y=element_text(hjust=0.4)),
                     npv3plot +
                     theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           axis.title.y = element_blank() ),
                     npv4plot+theme(axis.text.y=element_text(hjust=0.4)),
                     nrow = 2,
                     align = "v")

ggsave("Figures/FIG2.TIF.NPV_Stake_holder.Print_with_color.tiff", dpi = 500, 
       width = 11,
       height = 6)





