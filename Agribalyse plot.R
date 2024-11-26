#This code will create a vilion graph for Agribalyse data 

#1.0. Clear the environment
rm(list=ls())

#2.0. Set WD
getwd()

#3.0. Load the libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

#4.0. Loading the database 
Agribalyse = read_xlsx ("Agribalyse_3.1_steps_modified.xlsx")
Foodnames  = read_xlsx ("Selected food names.xlsx")

#5.0 Calculating the farm contribution 
Agribalyse$FGClimatechange = (Agribalyse$`Climate change Agriculture`/Agribalyse$`Climate change Total`)*100
Farmcontribution = data.frame(Agribalyse$`LCI Name`, Agribalyse$`Food Category`, Agribalyse$FGClimatechange ,Agribalyse$`Colour group`, Agribalyse$`Climate change Total`)
Level_order = c('Vegetables','Fruits','Nuts, legumes and oilseeds', 'Fish and seafood products','Milk and dairy products','Eggs','Animal oils and fats','Meat and meat products','Flours and pie crusts','Breakfast cereals and cookies','Pasta, rice and cereals','Bread, cakes and pastries','Vegetable oils and fats','Culinary aids','Mixed dishes','Baby foods','Ice creams and sorbets','Sugar based confectionery','Dairy based confectionery','Non-alcoholic drinks','Alcoholic drink')

#5.1. Calculating filtered data 
Filter_high = Farmcontribution$Agribalyse..LCI.Name. %in% c(Foodnames$High)
Filter_low = Farmcontribution$Agribalyse..LCI.Name. %in% c(Foodnames$Low)

#6.0. Plotting the graph - Total 
#6.1. plotting grid
A = 
  ggplot(Farmcontribution, aes(x = factor(Agribalyse..Food.Category.,levels = Level_order))) +
  
#6.2. Add the points
  geom_point(aes(y = Agribalyse..Climate.change.Total., color = Agribalyse..Colour.group.),size = 2.0, alpha = 0.4) + 
  geom_point (data = Farmcontribution[Filter_high,],
              aes(x = Agribalyse..Food.Category.,y = Agribalyse..Climate.change.Total.), 
              color = "red", 
              size = 2.5, 
              shape = 17)+
  geom_point (data = Farmcontribution[Filter_low,],
              aes(x = Agribalyse..Food.Category.,y = Agribalyse..Climate.change.Total.), 
              color = "black", 
              size = 2.5, 
              shape = 18)+
  
#6.3. Add box plot
  geom_boxplot(aes(y = Agribalyse..Climate.change.Total.),width = 0.5, fill = "white", outlier.color = "black", alpha = 0)+
  
#6.4. Changing the themes
  coord_flip()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x = "Food groups ", y = "Total GHG emissions (kg-CO2e/kg)", color = "")+ 
  theme(panel.background = element_blank())+ 
  theme(axis.line = element_line(color='black'))+ 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 9))+
  theme(axis.text.y = element_text(size = 9)) + 
  theme(axis.title.y = element_text(size = 9))+
  theme(axis.title.x = element_text(size = 9))+ 
  theme(axis.ticks = element_blank()) + 
  theme(strip.text = element_text(size = 7.5), strip.background = element_blank(),strip.placement = "bottom")


#7.0. Ploting the graph - % contribution
#7.1. plotting the data frame
B = 
ggplot(Farmcontribution, aes(x = factor(Agribalyse..Food.Category.,levels = Level_order))) +
  
#7.2. Add the points
geom_point(aes(y = Agribalyse.FGClimatechange, color = Agribalyse..Colour.group.),size = 2.0, alpha = 0.4) + 
  geom_point (data = Farmcontribution[Filter_high,],
              aes(x = Agribalyse..Food.Category.,y = Agribalyse.FGClimatechange ), 
              color = "red", 
              size = 2.5, 
              shape = 17)+
    geom_point (data = Farmcontribution[Filter_low,],
              aes(x = Agribalyse..Food.Category.,y = Agribalyse.FGClimatechange), 
              color = "black", 
              size = 2.5, 
              shape = 18)+
  
#7.3. Add box plot
geom_boxplot(aes(y = Agribalyse.FGClimatechange),width = 0.5, fill = "white", outlier.color = "black", alpha = 0)+
  
#7.4. Changing the themes
coord_flip()+ 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
labs(x = "Food groups ", y = "Agricultural production GHG emissions relative to total GHG emissions (%)", color = "")+ 
theme(panel.background = element_blank())+ 
theme(axis.line = element_line(color='black'))+ 
theme(legend.position = "none") +
theme(axis.text.x = element_text(size = 9))+
theme(axis.text.y = element_text(size = 9)) + 
theme(axis.title.y = element_text(size = 9))+
theme(axis.title.x = element_text(size = 9))+
theme(axis.ticks = element_blank()) +
theme(strip.text = element_text(size = 7.5), strip.background = element_blank(),strip.placement = "bottom")

#8.0 Arranging the plots in the grid 
plot_grid(A,B, ncol = 1, nrow = 2)






