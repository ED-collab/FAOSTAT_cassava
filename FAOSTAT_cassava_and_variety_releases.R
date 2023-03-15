# Packages ####
library(tidyverse)
library(countrycode)
library(ggpubr)
library(ggrepel)
library(cowplot)

# Data prep ####
#Copy the steps from the PLotly visualization file, but with a few tweaks
#Grab the data manually from FAOSTAT
#The name will vary depending on the date of download
FAO <- read.csv("data/FAOSTAT_data_en_1-5-2023.csv", header = TRUE)

#Subset data to remove the FAO-specific flags and columns we don't need
FAOF <-
  subset(FAO,
         select = c('Area', 'Area.Code..ISO3.', 'Element', 'Year', 'Value'))
FAOwide <- spread(FAOF, Element, Value) #long to wide
colnames(FAOwide)[1] <- "Country"
colnames(FAOwide)[4] <- "Area"

FAOwide$Yield <-
  as.numeric(as.character(FAOwide$Yield)) / 10000 #Convert from hg/ha to T/ha

FAOwide$continent <-
  countrycode(sourcevar = FAOwide[, "Area.Code..ISO3."],
              origin = "iso3c",
              destination = "continent")

FAOwide$region <-
  countrycode(sourcevar = FAOwide[, "Area.Code..ISO3."],
              origin = "iso3c",
              destination = "region")

#We now have both continent and region label columns in our dataset
#Let's replace NAs with 0
FAOwide$Area <- FAOwide$Area %>%  replace_na(0)
FAOwide$Production <- FAOwide$Production %>%  replace_na(0)
FAOwide$Yield <- FAOwide$Yield %>%  replace_na(0)

#This introduces some weird cases in which there is 0 under Area and Yield but some small number like 1 or 2 under Production.
#My guess is that this arises when the area is less than 1 ha - FAOstat rounds it to a 0, introducing NAs in calculation.
#We don't really care about such small numbers since we're taking a global approach, so we'll leave them as 0 for now.

unique(FAOwide$Country) #Check country names
apply(FAOwide, 2, function(x)
  any(is.na(x))) #Check for the presence of NAs by column

#No more NAs in our data columns, but there is some in the continent and country
#FAO likes to report both 'China' and 'China, mainland'. Let's remove that redundancy.
FAOwide <- FAOwide %>% filter(Country != "China")

#China, mainland lacks both 'continent' and 'region' entries - we can fix that.
FAOwide <- FAOwide %>%
  mutate(continent = ifelse(Country == 'China, mainland', 'Asia', continent)) %>% mutate(region =
                                                                                           ifelse(Country == 'China, mainland', 'East Asia & Pacific', region))

#The entry for Sudan lacks both continent and region.
FAOwide <- FAOwide %>%
  mutate(continent = ifelse(Country == 'Sudan (former)', 'Africa', continent)) %>% mutate(region =
                                                                                            ifelse(Country == 'Sudan (former)', 'Sub-Saharan Africa', region))

#Reunion has missing region, but the accents have messed up the name, so let's use the ISO code
FAOwide <- FAOwide %>%
  mutate(region = ifelse(Area.Code..ISO3. == 'REU', 'Sub-Saharan Africa', region))

#The messed up names for Réunion and Côte d'Ivoire are due to translation of the special characters. We'll just substitute the appropriate accents in.
FAOwide <- FAOwide %>%
  mutate(Country = ifelse(Area.Code..ISO3. == 'REU', 'Réunion', Country)) %>%
  mutate(Country = ifelse(Area.Code..ISO3. == 'CIV', "Côte d'Ivoire", Country))

#Because we are using Lao People's Democratic Republic a lot, let's shorten the name to Lao PDR
FAOwide<- FAOwide %>% mutate(Country=recode(Country, "Lao People's Democratic Republic" = "Lao PDR"))

#Let's fix Viet Nam to Vietnam too
FAOwide<- FAOwide %>% mutate(Country=recode(Country, "Viet Nam" = "Vietnam"))

# Viz ####
#Now we can make visualizations.Let's set the manual scale ground rules for consistency

myColors<- (values=c("Myanmar"="gray5",
                     "Lao PDR"="gray20",
                     "Cambodia"="gray35",
                     "Vietnam"="gray50",
                     "Thailand"="gray65"))

myShapes<- (values=c("Myanmar"=19,
                     "Lao PDR"=18,
                     "Cambodia"=15,
                     "Vietnam"=16,
                     "Thailand"=17))

myLines<- (values=c("Myanmar"="dotted",
                    "Lao PDR"="dotted",
                    "Cambodia"="longdash",
                    "Vietnam"="dashed",
                    "Thailand"="solid"))


## Area graphs of all GMS ####
GMS <-
  FAOwide %>% filter(Area.Code..ISO3. %in% c('KHM', 'VNM', 'THA', 'MMR', 'LAO'))
GMS$Country <-
  factor(
    GMS$Country,
    levels = c(
      "Myanmar",
      "Lao PDR",
      "Cambodia",
      "Vietnam",
      "Thailand"
    )
  ) #manually reorder smallest to biggest

CTV <- GMS %>% filter(Area.Code..ISO3. %in% c('KHM', 'VNM', 'THA'))

ggplot(data = GMS, aes(x = Year, y = Area/1000000, group = Country)) +
  geom_area(aes(fill = Country)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Area (M ha)") +
  theme_pubr() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = myColors, limits=force)

### Facets ####
#New strip/facet labels
Axis_labs0 <- c(`Area` = "Area (M ha)",
               `Production` = "Production (M t)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labeller0 <- function(variable,value){
  return(Axis_labs0[value])
}

GMSL %>% filter(element %in% c('Area', 'Production')) %>% 
ggplot(aes(x = Year, y = value/1000000, group = Country)) +
  geom_area(aes(fill=Country), size=1, alpha=0.7) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labeller0)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right")+
  scale_fill_manual(values = myColors, limits=force)

## Line graphs ####
#If we want to make line graphs of the three measures, in my opinion the easiest method is by pivoting back to long format and making use of the faceting function of ggplot2.
#First we use pivot_longer to create a grouped column storing all our 3 numeric variables 'element', and put the corresponding values in a column named 'value'
GMSL <- GMS %>% pivot_longer(
  cols = c(Area, Yield, Production),
  names_to = "element",
  values_to = "value"
)

CTVL <- CTV %>% pivot_longer(
  cols = c(Area, Yield, Production),
  names_to = "element",
  values_to = "value"
)
CTVL$Country <-
  factor(
    CTVL$Country,
    levels = c(
      "Myanmar",
      "Lao PDR",
      "Cambodia",
      "Thailand",
      "Vietnam"
    )
  ) 

#New strip/facet labels
Axis_labs <- c(`Area` = "Area (ha)",
               `Production` = "Production (t)",
               `Yield` = "Yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labeller <- function(variable,value){
  return(Axis_labs[value])
}

ggplot(GMSL , aes(x = Year, y = value, group = Country)) +
  geom_line(aes(linetype = Country, colour=Country), size=1, alpha=0.7) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labeller)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force)+
  scale_linetype_manual(values = myLines, limits=force)

### With only CTV ####
ggplot(CTVL, aes(x = Year, y = value, group = Country)) +
  geom_line(aes(linetype = Country, colour=Country), size=1, alpha=0.85) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labeller)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right")+
  scale_color_manual(values = myColors, limits=force)+
  scale_linetype_manual(values = myLines, limits=force)

### CTV B&W Yield only ####
Axis_labsy <- c(`Yield` = "Yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labellery <- function(variable,value){
  return(Axis_labsy[value])
}


CTVL %>% filter(element=='Yield') %>% ggplot(aes(x = Year, y = value, group = Country)) +
  geom_line(aes(linetype = Country, colour=Country), size=1, alpha=0.85) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellery)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force)+
  scale_linetype_manual(values = myLines, limits=force)

### CTV B&W points and smooth ####

CTVL %>% filter(element=='Yield') %>% ggplot(aes(x = Year, y = value, group = Country)) +
  geom_point(aes(shape = Country, colour=Country), size=1, alpha=0.85) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellery)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force)+
  scale_shape_manual(values = myShapes, limits=force)+
  geom_smooth(method=loess, aes(group=Country, linetype=Country, colour=Country), se=F)+
  scale_linetype_manual(values = myLines, limits=force)

## Releases over time - scatter ####
RelImran <- read.csv("D:/Dropbox/Business case analysis/TH_VN_var_releases.csv", header = TRUE)

ggplot(RelImran, aes(x=Year, y=Yield, color=Country)) + 
  geom_point(size=2) +
  theme_pubr() +
  theme(legend.title = element_blank())

ggplot(RelImran, aes(x=Year, y=Starch.content, color=Country)) + 
  geom_point(size=2) +
  theme_pubr() +
  theme(legend.title = element_blank())

RElL <- RelImran %>% pivot_longer(
  cols = c(Yield, Starch.content, Starch.yield),
  names_to = "element",
  values_to = "value"
) 



#New strip/facet labels
Axis_labsR <- c(`Starch.content` = "Starch content (%)",
               `Starch.Yield` = "Starch Yield (t ha⁻¹)",
               `Yield` = "Yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labellerR <- function(variable,value){
  return(Axis_labsR[value])
}

RElL %>% filter(!is.na(Year)) %>% ggplot(aes(x = Year, y = value, group = Country, color=Country, shape = Country)) +
facet_grid(rows = "element",
           scales = "free",
           switch = 'y',
           labeller = as_labeller(Axis_labellerR))+
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE) +
  theme_pubr() +
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force) +
  scale_shape_manual(values = myShapes, limits=force)



### With only yield and starch ####
#New strip/facet labels
Axis_labsRs <- c(`Starch.content` = "Starch content (%)",
                `Yield` = "Yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labellerRs <- function(variable,value){
  return(Axis_labsRs[value])
}



RElL %>% filter(element %in% c('Yield', 'Starch.content') & !is.na(Year)) %>%
  ggplot(aes(x = Year, y = value, group = Country, color=Country, shape = Country)) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellerRs))+
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE) +
  theme_pubr() +
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right")+
  scale_color_manual(values = myColors, limits=force)


### Yield, Starch Yield, Starch Content ####

#New strip/facet labels
Axis_labsRs <- c(`Starch.content` = "Starch content (%)",
                 `Starch.yield` = "Starch yield (t ha⁻¹)",
                 `Yield` = "Root yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labellerRs <- function(variable,value){
  return(Axis_labsRs[value])
}

RElL %>% filter(element %in% c('Yield', 'Starch.content', 'Starch.yield') & !is.na(Year)) %>%
  ggplot(aes(x = Year, y = value, group = Country, color=Country, shape = Country)) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellerRs))+
  geom_point(size=2) +
  geom_smooth(method=lm, se=FALSE) +
  theme_pubr() +
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right")+
  scale_color_manual(values = myColors, limits=force)+
  stat_cor()+
  stat_regline_equation(label.x = 1988)

## Release yield & starch content added to FAOSTAT yield ####
Axis_labellery <- function(variable,value){
  return(Axis_labsy[value])
}

panel1<-CTVL %>% filter(element=='Yield') %>% ggplot(aes(x = Year, y = value, group = Country)) +
  geom_line(aes(linetype = Country, colour=Country), size=1) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellery)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force)+
  scale_linetype_manual(values = myLines, limits=force)+
  scale_x_continuous(limits = c(1961,2021), labels = NULL, breaks = NULL) +
  scale_y_continuous(breaks=seq(0,30,5)) + 
  labs(x = NULL)



panel1b <-CTVL %>% filter(element=='Yield') %>% ggplot(aes(x = Year, y = value, group = Country)) +
  geom_point(aes(shape = Country, colour=Country), size=1) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellery)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force)+
  scale_shape_manual(values = myShapes, limits=force)+
  geom_smooth(method=loess, aes(group=Country, linetype=Country, colour=Country), se=F)+
  scale_linetype_manual(values = myLines, limits=force)+
  scale_x_continuous(limits = c(1961,2021), labels = NULL, breaks = NULL) +
  scale_y_continuous(breaks=seq(0,30,5)) + 
  labs(x = NULL)


Axis_labsRs <- c(`Starch.content` = "Starch content (%)",
                 `Yield` = "Yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labellerRs <- function(variable,value){
  return(Axis_labsRs[value])
}

panel2<-RElL %>% filter(element %in% c('Yield', 'Starch.content') & !is.na(Year)) %>%
  ggplot(aes(x = Year, y = value, group = Country, color=Country, shape = Country)) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellerRs))+
  geom_point(size=2) +
  geom_smooth(method=lm, aes(group=Country, linetype=Country, colour=Country), se=FALSE, show.legend = FALSE) +
  theme_pubr() +
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right")+
  scale_color_manual(values = myColors, limits=force)+
  scale_x_continuous(limits = c(1961,2021))+
  scale_shape_manual(values = myShapes, limits=force)


plot_grid(panel1, panel2,  rel_heights = c(2,3), nrow = 2)

plot_grid(panel1b, panel2,  rel_heights = c(2,3), nrow = 2)

## Release yield, starch yield, starch content, FAOSTAT yield ####

Axis_labsy <- c(`Yield` = "Root yield (t ha⁻¹)")
Axis_labellery <- function(variable,value){
  return(Axis_labsy[value])
}

panel1b <-CTVL %>% filter(element=='Yield') %>% ggplot(aes(x = Year, y = value, group = Country)) +
  geom_point(aes(shape = Country, colour=Country), size=1.5) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellery)) + #Facet the grid by the new group column we created containing our 3 numeric variables
  scale_y_continuous(labels = scales::comma) + #Format y scale using thousands separators
  theme_pubr() + #From ggpubr, with sensible publication defaults
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = myColors, limits=force)+
  scale_shape_manual(values = myShapes, limits=force)+
  geom_smooth(method=loess, aes(group=Country, linetype=Country, colour=Country), se=F)+
  scale_linetype_manual(values = myLines, limits=force)+
  scale_x_continuous(limits = c(1961,2018), labels = NULL, breaks = NULL) +
  scale_y_continuous(breaks=seq(0,30,5)) + 
  labs(x = NULL)

#Now panel 2

Axis_labsRs <- c(`Starch.content` = "Starch content (%)",
                 `Starch.yield` = "Starch yield (t ha⁻¹)",
                 `Yield` = "Root yield (t ha⁻¹)")

#function to apply the labels to the plot - we will call it in the 'labeller'
Axis_labellerRs <- function(variable,value){
  return(Axis_labsRs[value])
}

panel2<-RElL %>% filter(element %in% c('Yield', 'Starch.content', 'Starch.yield') & !is.na(Year)) %>%
  ggplot(aes(x = Year, y = value, group = Country, color=Country, shape = Country)) +
  facet_grid(rows = "element",
             scales = "free",
             switch = 'y',
             labeller = as_labeller(Axis_labellerRs))+
  geom_point(size=2) +
  geom_smooth(method=lm, aes(group=Country, linetype=Country, colour=Country), se=FALSE, show.legend = TRUE) +
  theme_pubr() +
  theme(axis.title.y = element_blank(), 
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size=11),
        legend.title = element_blank(),
        legend.position = "right")+
  scale_color_manual(values = myColors, limits=force)+
  scale_x_continuous(limits = c(1961,2021))+
  scale_shape_manual(values = myShapes, limits=force)+
  stat_cor()+
  stat_regline_equation(label.x = 1985)


plot_grid(panel1b, panel2,  rel_heights = c(1,3), nrow = 2)

## Over time with paths ####
#We can also plot the progression over time, using paths
library(tidyquant)

GMS %>% 
  ggplot(aes(x=Area, y=Production, group=Country)) + 
  geom_point(aes(colour=Country), size=1)+
  geom_path(arrow = arrow(type="closed", length = unit(0.3, "cm")), aes(colour=Country)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic()+
  labs(title="Annual data (1961-2020)",x="Area (ha)", y="Production (t)", colour="Country")
# geom_ma(ma_fun = SMA, n=3)

### Using 10 yr avgs ####
#Specifying 10 year increments - each point is a 10-year average

GMS %>% group_by(Country, Year=ceiling(Year/10)*10) %>% 
  summarize(Year=paste(first(Year)-10, first(Year), sep='-'),
            Area=mean(Area), Production=mean(Production), Yield=mean(Yield)) %>% 
  ggplot(aes(x=Area, y=Production, group=Country)) + 
  geom_point(aes(colour=Country), size=1)+
  geom_path(arrow = arrow(type="closed", length = unit(0.3, "cm")), aes(colour=Country), size=1, alpha=.6) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic()+
  labs(title="10 year averages (1961-2020)", x="Area (ha)", y="Production (t)", colour="Country")
# geom_ma(ma_fun = SMA, n=3)
ggsave("outputs/10_yr_avgs.png", width = 180, height = 100, units='mm', dpi=300)

### 5 yr avgs #### 
#Let's try automatically slicing 5 year increment averages to smooth things out
GMS %>% group_by(Country, Year=ceiling(Year/5)*5) %>% 
  summarize(Year=paste(first(Year)-5, first(Year), sep='-'),
            Area=mean(Area), Production=mean(Production), Yield=mean(Yield)) %>% 
  ggplot(aes(x=Area, y=Production, group=Country)) + 
  geom_point(aes(colour=Country))+
  geom_path(arrow = arrow(length = unit(0.3, "cm")), aes(colour=Country), size=2, alpha=.6) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic()+
  labs(title="5 year averages (1961-2020)",x="Area (ha)", y="Production (t)", colour="Country")

### Animate ####
#We can animate the paths using gganimate -this will give us a .gif that can be inserted into powerpoints, etc.
library(gganimate)
GMS %>% 
  ggplot(aes(x=Area, y=Production, group=Country)) + 
  geom_point(aes(colour=Country), size=1)+
  geom_path(arrow = arrow(type="closed", length = unit(0.3, "cm")), aes(colour=Country), size=1, alpha=.6) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic()+
  labs(title="Cassava area changes, 1961-2020",x="Area (ha)", y="Production (t)", colour="Country")+
  transition_reveal(Year)

#Feeling nasty. Why don't we do it for the whole planet and see where we end up? This time we'll divide into 5-year increments by region.
FAOwide %>% filter(region != "Middle East & North Africa") %>% group_by(region, Year=ceiling(Year/5)*5) %>% 
  summarize(Year=paste(first(Year)-5, first(Year), sep='-'),
            Area=mean(Area), Production=mean(Production), Yield=mean(Yield)) %>% 
  ggplot(aes(x=Area, y=Production, group=region)) + 
  geom_point(aes(colour=region, size=Yield), alpha=0.7)+
  scale_size_continuous(range = c(1,8)) +
  geom_path(arrow = arrow(type="closed", length = unit(0.4, "cm")), aes(colour=region), size=1.1, alpha=.8) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic() +
  theme() +
  labs(title="5 year averages (1961-2020)",x="Area (ha)", y="Production (t)", colour="Region") +
  geom_text(data = subset(FAOwide, Year=="1960-1965" | Year=="2015-2020"), aes(label=Year))

## Vary line widths ####
#What if we want to vary the line (geom_path) width instead?
GMS5<-FAOwide %>% filter(region != "Middle East & North Africa") %>% group_by(region, Year=ceiling(Year/5)*5) %>% 
  summarize(Year=paste(first(Year)-5, first(Year), sep='-'),
            Area=mean(Area), Production=mean(Production), Yield=mean(Yield))
  GMS5 %>% ggplot(aes(x=Area, y=Production, group=region)) +
  scale_size_continuous(range = c(1,6)) +
  geom_path(aes(colour=region, size=Yield),  lineend = "round") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic() +
  theme() +
  labs(title="5 year averages",x="Area (ha)", y="Production (t)", colour="Region") +
  geom_text_repel(data = subset(GMS5, Year=="1960-1965" | Year=="2015-2020"), aes(label=Year), size=3)

## 3 dimensions ####
#We can even go into 3 dimensions by using Plotly and adding Yield
library(plotly)
plot_ly(GMS, x = ~Area, y = ~Production, z = ~Yield) %>%
  add_paths(color = ~Country)

## Treemap ####
#This type of data can also be used for making a hierarchical treemap
# library
library(treemap)
 
# treemap for Area
treemap(FAOwide,
            index=c("region","Country"),
            vSize="Area",
            type="index"
            )

# Again for production
treemap(FAOwide,
            index=c("region","Country"),
            vSize="Production",
            type="index",
        border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
    border.lwds=c(3,2)                         # Width of colors
            )

#Our data is already mostly nested - we have world, regions, and countries. This structure plays nice with certain visualizations like treemaps or circle packing.
#Note that we are adding data columns to the FAOwide dataset we created. Since we're not changing the columns we created previously, this shouldn't cause us any problems if we want to go back and rerun earlier code.

#Add a column giving the top level 'World'
FAOwide$World<-"World"

## Circle packing ####
# Circlepacker package
library(circlepackeR)         
#devtools::install_github("jeromefroe/circlepackeR") # If needed

# Change the format. This uses the data.tree library. This library needs a column that looks like root/group/subgroup/..., so let's create that.
library(data.tree)
FAOwide$pathString <- paste("World", FAOwide$continent, FAOwide$region, FAOwide$Country, sep = "/")
Area_ <- as.Node(FAOwide)

# Make the plot
circlepackeR(Area_, size = "Area")

# You can customize the minimum and maximum values of the color range.
p <- circlepackeR(Area_, size = "Area", color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)")

p
# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/circular_packing_circlepackeR2.html"))
```