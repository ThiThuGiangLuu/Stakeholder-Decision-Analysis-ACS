#install.packages("ggrepel")
#install.packages("viridis")
#install.packages("magrittr")
library(magrittr)
library(dplyr)
library(viridis)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(cowplot)
#Call data for stakeholders' expertise, willingness and potential contribution 
#in the decision analysis#

stakeholder<-read.csv("stakeholder.csv")
#1. Plot stakeholders' experience, availability and expertise in decision analysis ####

expertplot<-ggplot(data = stakeholder, aes(x=Experience, y=Availability, label =stakeholders, color=Expertise))+ 
  geom_point(aes(shape=Gender))+
  xlab("Relevant Experience")+
 #label names of stakeholder and expand space to show full names
  scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0,5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0,1))+
  scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                     breaks = seq(0,5, by = 1), 
                     limits = c(0, 5),
                     expand = c(0,1))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))+
# Create line to categorize stakeholders
  geom_hline(yintercept=2.5, color="white", size=2)+
  geom_vline(xintercept=2.5, color="white", size=2)+
# Show all names of overlapped values
# https://ggrepel.slowkow.com/articles/examples.html
  geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=3)+
  annotate("text", label = "Potential core experts",
    x = 4.5, y = 3.2, size = 4, colour = "black")+
  annotate("text", label = "Potential resource stakeholders",
           x = 4.3, y = 0.25, size = 4, colour = "black")
 ggsave("Figures/FIG3.TIF.Expert.Print_with_color.tiff", dpi=400, height=7, width = 8)
 
 #2. Stakeholder power and interest analysis in 2019 (with lines)####
 stakeholder<-read.csv("stakeholder.csv")
 Stakeholder_analysis_2019<-ggplot(data = stakeholder, aes(x=Interest_2019, y=Influence_2019, 
                                label=stakeholders, color=Attitude_2019, size=Relevance_2019))+ 
   geom_point(alpha=0.5)+
   xlab("Perceived Interest (2019)")+
   ylab("Perceived Influence (2019)")+
   labs (color="Attitude (2019)", size="Relevance (2019)")+
   #define the scale size as area to ensure the consistence of size in 2019 and 2020: scale_size_area()
   
   #label names of stakeholder and expand space to show full names
   scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(-0.5, 5.5))+
   scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(-0.5, 5.5))+
                      
   # Show the size from 0 to 5, not only from 2-5 although we do not have 0 and 1 in the data table
   scale_size(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5))+
   # Writing texts outside plot area: https://www.tutorialspoint.com/how-to-write-text-outside-plot-using-ggplot2-in-r
   
   coord_cartesian(clip = "off")+
   
   theme(plot.margin = unit(c(1,1,1,1), "cm"))+
   scale_color_manual(breaks = c("Negative", "No information", "Positive"),
                      values=c("red", "blue", "dark green"))+
   # Create line to categorize stakeholders
   geom_hline(yintercept=2.5, color="white", size=2)+
   geom_vline(xintercept=2.5, color="white", size=2)+
   # Show all names of overlapped values
   # https://ggrepel.slowkow.com/articles/examples.html
   geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=2.5)+
   annotate("text", label = "1. High Interest, High Influence",
            x = 4.0, y = 5.5, size = 3.8, colour = "black")+
   annotate("text", label = "3. High Interest, Low Influence",
            x = 4.0, y = -0.5, size = 3.8, colour = "black")+
   annotate("text", label = "2. Low Interest, High Influence",
          x = 1.0, y = 5.5, size = 3.8, colour = "black")+
   annotate("text", label = "4. Low Interest, Low Influence",
            x = 1.0, y = -0.5, size = 3.8, colour = "black")
 
 ggsave("Figures/FIG5.Stakeholder_analysis_2019.Print_with_color.tiff", dpi=400, height=7, width = 8)
 
 
 #3. Stakeholder power and interest analysis in 2020 (with lines)####
 stakeholder<-read.csv("stakeholder.csv")
 Stakeholder_analysis_2020<-ggplot(data = stakeholder, aes(x=Interest_2020, y=Influence_2020, 
                                                           label=stakeholders,
                                                           color=Attitude_2020, size=Relevance_2020))+ 
   geom_point(alpha=0.5)+
   xlab("Perceived Interest (2020)")+
   ylab("Perceived Influence (2020)")+
   #editing legend: https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
   labs (color="Attitude (2020)", size="Relevance (2020)")+
   #define the scale size as area to ensure the consistence of size in 2019 and 2020: scale_size_area()
   
   
   #label names of stakeholder and expand space to show full names
   
   
   scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(-0.5, 5.5))+
   scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(-0.5, 5.5))+
   
   scale_size(labels = paste(seq(0, 5, by = 1)), 
              breaks = seq(0,5, by = 1), 
              limits = c(0, 5))+
   coord_cartesian(clip = "off")+
   
   theme(plot.margin = unit(c(1,1,1,1), "cm"))+
   scale_color_manual(breaks = c("Negative", "No information", "Positive", "Neutral"),
                      values=c("red", "blue", "dark green", "thistle4"))+
   # Create line to categorize stakeholders
   geom_hline(yintercept=2.5, color="white", size=2)+
   geom_vline(xintercept=2.5, color="white", size=2)+
   # Show all names of overlapped values
   # https://ggrepel.slowkow.com/articles/examples.html
   geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=2.5)+
   
   annotate("text", label = "1. High Interest, High Influence",
            x = 4.0, y = 5.5, size = 3.8, colour = "black")+
     #https://www.tutorialspoint.com/how-to-write-text-outside-plot-using-ggplot2-in-r
  #https://statisticsglobe.com/annotate-text-outside-of-ggplot2-plot-in-r
   # 
   annotate("text", label = "3. High Interest, Low Influence",
            x = 4.0, y = -0.5, size = 3.8, colour = "black")+ 
   annotate("text", label = "2. Low Interest, High Influence",
            x = 1.0, y = 5.5, size = 3.8, colour = "black")+
   annotate("text", label = "4. Low Interest, Low Influence",
            x = 1.0, y = -0.5, size = 3.8, colour = "black")
 
 ggsave("Figures/FIG6.Stakeholder_analysis_2020.Print_with_color.tiff", dpi=400, height=7, width = 8)
 
 #Warning####
 # Number of removing rows corresponds to the number of rows with NA values
  # 1: Removed 4 rows containing missing values (geom_point). 
  # 2: Removed 4 rows containing missing values (geom_text_repel)
 #Bobus####
 #4. Stakeholder Relevance and Influence analysis in 2020####
 stakeholder<-read.csv("stakeholder.csv")
 
 stakeholder_m<- stakeholder %>%
   mutate(annotation = ifelse(Interest_2020 <2, "yes", "no"))
 
 Stakeholder_analysis_RI_2020<-ggplot(data = stakeholder_m, aes(x=Relevance_2020, y=Influence_2020,
                                                  size = Interest_2020, color = Attitude_2020 ))+
   geom_point(alpha=0.5) +
   #scale_color_viridis(discrete=TRUE) +
             
   xlab("Perceived Relevance (2020)")+
   ylab("Perceived Influence (2020)")+
   scale_color_manual(breaks = c("Negative", "No information", "Positive", "Neutral"),
                      values=c("darkorange4", "blue", "dark green", "thistle4"))+
   geom_text_repel(data=stakeholder_m %>% filter(annotation=="yes"), aes(label=stakeholders), 
                   box.padding = 0.3, max.overlaps = Inf, size=2.5, color ="red" )+
   geom_text_repel(data=stakeholder_m %>% filter(annotation=="no"), aes(label=stakeholders), 
                   box.padding = 0.6, max.overlaps = Inf,size=2.5, color ="bisque4" )+
  
   #label names of stakeholder and expand space to show full names
   scale_x_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5),
                      expand = c(0,0.5))+
   scale_y_continuous(labels = paste(seq(0, 5, by = 1)), 
                      breaks = seq(0,5, by = 1), 
                      limits = c(0, 5),
                      expand = c(0,0.5))+
   scale_size(range=c(1,7))+
   theme(plot.margin = unit(c(1,1,1,1), "cm"))
  ggsave("Figures/Stakeholder_analysis_RI_2020.jpeg", dpi=400, height=7, width = 8)
 
   
  # Show all names of overlapped values
   # https://ggrepel.slowkow.com/articles/examples.html
   # Advanced visualization with labling
   # https://www.yan-holtz.com/PDF/Ggplot2_advancedTP_correction.html
   #geom_text_repel(box.padding = 0.3, max.overlaps = Inf, size=2.5)

   
 

 
 
 
 
 # Combine stakeholder analysis for both 2019 and 2020
 
 SA_2019_2020 <- plot_grid(Stakeholder_analysis_2019, Stakeholder_analysis_2020,
                         labels = c('a)', 'b)'))
                          
 
 ggsave("Figures/FIG7.TIF.SA_2019_2020.Print_with_color.tiff", dpi = 500, 
        width = 15,
        height = 6)

#https://stackoverflow.com/questions/15624656/label-points-in-geom-point
#https://intellipaat.com/community/16343/how-to-put-labels-over-geombar-for-each-bar-in-r-with-ggplot2

 # 5. Stakeholder power and interest analysis in 2019 without names####
 
 Stakeholder_analysis_2019_no_name<-ggplot(data = stakeholder, aes(x=Interest_2019, y=Influence_2019, 
                                                           label=stakeholders,
                                                           color=Attitude_2019, size=Relevance_2019))+ 
   geom_point()+
   xlab("Perceived Interest (2019)")+
   ylab("Perceived Influence (2019)")+
   scale_size_area()+
   theme(plot.margin = unit(c(1,1,1,1), "cm"))+
   scale_color_manual(breaks = c("Negative", "No information", "Positive"),
                      values=c("red", "blue", "dark green"))+
   # Create line to categorize stakeholders
   geom_hline(yintercept=2.5, color="white", size=2)+
   geom_vline(xintercept=2.5, color="white", size=2)+
   # Show all names of overlapped values
   annotate("text", label = "Engage/Consult/Seek support",
            x = 4.0, y = 5.0, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Organize collectively",
            x = 4.0, y = 0.5, size = 4.0, colour = "grey48")+
   annotate("text", label = "Keep satisfied/Increase interest",
            x = 1.0, y = 5.0, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Increase interest/Dialogue",
            x = 1.0, y = 0.5, size = 4.0, colour = "grey48")
 Stakeholder_analysis_2019_no_name
 
 ggsave("Figures/stakeholder_analysis_2019_no_name.jpeg", dpi=400, height=7, width = 8)

# 6. Stakeholder power and interest analysis in 2020 without names####
 
 Stakeholder_analysis_2020_no_name<-ggplot(data = stakeholder, aes(x=Interest_2020, y=Influence_2020, 
                                                          
                                                           color=Attitude_2020, size=Relevance_2020))+ 
   geom_point()+
   xlab("Perceived Interest (2020)")+
   ylab("Perceived Influence (2020)")+
   theme(plot.margin = unit(c(1,1,1,1), "cm"))+
   scale_color_manual(breaks = c("Negative", "No information", "Positive", "Neutral"),
                      values=c("red", "blue", "dark green", "thistle4"))+
   # Create line to categorize stakeholders
   geom_hline(yintercept=2.5, color="white", size=2)+
   geom_vline(xintercept=2.5, color="white", size=2)+
   annotate("text", label = "Engage/Consult/Seek support",
            x = 4.0, y = 5, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Organize collectively",
            x = 4.0, y = 0.5, size = 4.0, colour = "grey48")+
   annotate("text", label = "Keep satisfied/Increase interest",
            x = 1.0, y = 5, size = 4.0, colour = "grey48")+
   annotate("text", label = "Inform/Increase interest/Dialogue",
            x = 1.0, y = 0.5, size = 4.0, colour = "grey48")
 ggsave("Figures/stakeholder_analysis_2020.jpeg", dpi=400, height=7, width = 8)
 
 # Combine stakeholder analysis for both 2019 and 2020
 
 SA_2019_2020_no_name <- plot_grid(Stakeholder_analysis_2019_no_name, Stakeholder_analysis_2020_no_name,
                           labels = c('a)', 'b)'))
 

 
 
 
 
 
 
 
 
 
 
 
 