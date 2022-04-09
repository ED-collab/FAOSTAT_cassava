######### Making animated and interactive graphs from FAOSTAT data ############

### Load in packages ####
library(tidyverse)
library(ggplot2)
library(plotly)
library(gapminder)
library(countrycode)

### Data download and processing ####
#As far as I can tell, FAO still stubbornly refuses to provide an API to allow us to pull data directly.
#This is only a minor inconvenience since downloading data from the site in standard format is pretty easy and could be automated.
#For this example I will just be using a manually-downloaded version of the data generated in .csv  by going to the [https://www.fao.org/faostat/en/#data/QCL](FAOSTAT website)

#Select all countries -> to make a later step easier, click the gear and choose ISO3 for country identifiers
#Area harvested, yield, production/quantity
#Under items, select cassava
#Years - all available (goes back to 1961)
#Click 'download data'

#Read in the downloaded .csv FAOSTAT data generated
#The name will vary depending on the date of your download
FAO <- read.csv("data/FAOSTAT_data_4-8-2022.csv", header = TRUE)
head(FAO)

#Subset data to remove the FAO-specific flags and columns we don't need
FAOF <-
  subset(FAO,
         select = c('Area', 'Area.Code..ISO3.', 'Element', 'Year', 'Value'))
FAOF

#How many individual 'countries' are in this dataset?
FAOF %>%
  summarise(Unique_Elements = n_distinct(Area))
#So 104 countries have reported growing cassava (as of 2022) - impressive

#Go from long to wide with tidyr; change column name of 'Area' to 'Country'
#Notice we use Element and Value in the spread argument
FAOwide <- spread(FAOF, Element, Value)
colnames(FAOwide)[1] <- "Country"
colnames(FAOwide)[4] <- "Area"

#Yield is expressed by FAO as hectograms per hectare (100g/ha), so must always convert to T/ha
FAOwide$Yield <- as.numeric(as.character(FAOwide$Yield)) / 10000

#Let's check it out
head(FAOwide)
#That looks fine - we have our countries, ISO3 code, year, and the three variables we were interested in.
#But we want one more thing - continent/region. This will help us to color code our points for easy reference later.
#This is why we got the ISO3 code - a standard tag we can use for this purpose.
#We will use the [countrycode](https://cran.r-project.org/web/packages/countrycode/countrycode.pdf) package to do this.
#Let's add the World Bank 'continent' and 'region' designations

FAOwide$continent <-
  countrycode(sourcevar = FAOwide[, "Area.Code..ISO3."],
              origin = "iso3c",
              destination = "continent")

FAOwide$region <-
  countrycode(sourcevar = FAOwide[, "Area.Code..ISO3."],
              origin = "iso3c",
              destination = "region")

#We now have both continent and region label columns in our dataset
#Let's also drop rows containing NA for Area - they are countries that didn't report any cassava for some periods

FAOwide <- drop_na(FAOwide, Area)
#That dropped over 100 rows. We should now have a complete dataset in wide format
#Unfortunately, there are often a few quirks related to the ISO matching (the warning messages above should have tipped us off about the unmatched ISOs), so do a quick check.

unique(FAOwide$Country) #Check country names
apply(FAOwide, 2, function(x) any(is.na(x))) #Check for the presence of NAs by column

#So looks like we have a bit of cleaning to do.

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




############### Plotly + Gapminder version... so easy #################
#Using the Plotly library requires learning some new syntax if you're used to ggplot, but it's pretty straightforward.
#Plotly plays friendly with pipes, which simplifies things a lot.
#The Gapminder package, based on the famous Hans Rosling style, makes these styles of animations very simple.
#There are some other handy features on by default, including autoscaling of the plot size in browser.

#Basic plot
interactive_area_production <- FAOwide %>%
  plot_ly(
    x = ~ Production,
    y = ~ Area,
    size = ~ Yield,
    color = ~ region,
    frame = ~ Year,
    text = ~ Country,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  )

#With pipes we can customize the basic plot.
#Adding elasticity to animation (and turn off redraw which looks janky), and log transform the y axis.
#Manually specifying axis labels, title, and legend title.

interactive_area_production <-
  interactive_area_production %>% layout(yaxis = list(type = "log"), xaxis = list(type = "log")) %>%
  animation_opts(500, easing = "elastic", redraw = FALSE) %>% #the 500 calibrates time between steps in the animation: less=shorter
  layout(
    title = 'Cassava production 1961-2020',
    plot_bgcolor = "rgba(0, 0, 0, 0)", #This means transparent
    xaxis = list(title = 'Production (tons), log scaled'),
    yaxis = list(title = 'Area (hectares), log scaled'),
    legend = list(x = 0, y = 1, title = list(text = '<b> Region </b>'), bgcolor = 'rgba(0,0,0,0)'),
    margin = list(b = 200, t = 40, pad = 4)
  ) %>%
  layout(
    annotations =
      list(
        x = 1,
        y = -0.275,
        text = "Source: FAOSTAT (2022). Visualization: Erik Delaquis, Alliance of Bioversity International and CIAT",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        xanchor = 'right',
        yanchor = 'auto',
        xshift = 0,
        yshift = 0,
        font = list(size = 10, color = "black")
      )
  )
interactive_area_production

## Custom function for checking html widget file size ####
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb, " MB")
}

#Use the new function to check file size
widget_file_size(interactive_area_production)
#File size is 5.561 MB

#Save the widget as html
htmlwidgets::saveWidget(interactive_area_production, "outputs/FAO_inter.html")

# Partial bundle function reduces size of html by only keeping the dependencies we need for our particular animation
interactive_area_production_reduced <- partial_bundle(interactive_area_production)
widget_file_size(interactive_area_production_reduced)
#File size is 2.11 MB

#Save the reduced version under a modified name
htmlwidgets::saveWidget(interactive_area_production_reduced, "outputs/FAO_inter_reduce.html")


####### If you don't want Plotly and prefer a GGplot solution #########################
#This version also works perfectly fine, but is quite a bit more fiddly, especially with labels, titles, and Tooltips.

library(gganimate)
library(gifski)
library(png)

#### gganimate code for getting an animated GIF ####
p <-
  ggplot(
    FAOwide,
    aes(
      Yield / 10000,
      Area / 1000000,
      size = Production / 1000000,
      color = continent,
      frame = Year
    )
  ) +
  labs(x = "Yield",
       y = "Area",
       size = "Production",
       color = 'Continent') +
  geom_point() +
  scale_color_brewer(type = 'div', palette = 'Spectral') +
  # gganimate code
  ggtitle("Year: {frame_time}") +
  transition_time(Year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

# animate - this takes a few moments
animate(p, width = 450, height = 400)
p
# save as a GIF
anim_save("outputs/animated.gif")

#### Interactive HTML version with slider for website purposes ####
p2 <-
  ggplot(
    FAOwide,
    aes(
      Yield,
      Area ,
      size = Production,
      color = continent,
      frame = Year,
      label = Country
    )
  ) +
  labs(x = "Yield (T/Ha)",
       y = "Area (Ha)",
       size = "Production (Tons)",
       color = 'Continent') +
  geom_point() +
  scale_color_brewer(type = 'div', palette = 'Spectral')

p2

#Converting units to handier ones
p3 <-
  ggplot(
    FAOwide,
    aes(
      size = Production / 1000000,
      color = continent,
      label = Country,
      alpha = 0.5,
      frame = Year,
      Yield,
      Area / 1000000
    )
  ) +
  labs(x = "Yield (T/Ha)", y = "Area (M Ha)", size = "Production (M T)") +
  geom_point() +
  scale_color_brewer(type = 'div',
                     palette = 'Spectral',
                     na.translate = F)
#Turn off scientific notation
options(scipen = 999)

# Running p2/p3 on their own will map all our data values for every country at every year on a single plot. maybe not that useful.
p3


# Generate the interactive visual and a HTML output with a call to plotly
ggp <- ggplotly(p3, height = 600, width = 900) %>%
  animation_opts(frame = 180,
                 easing = "linear",
                 redraw = FALSE,) %>%
  layout(
    annotations =
      list(
        x = 1,
        y = -0.4,
        text = "Source: FAOSTAT (2022). Visualization: Erik Delaquis, Alliance of Bioversity International and CIAT",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        xanchor = 'right',
        yanchor = 'auto',
        xshift = 0,
        yshift = 0,
        font = list(size = 10, color = "black")
      )
  ) %>%
  layout(margin = list(b = 200, t = 40, pad = 4)) %>%
  layout(legend = list(x = 0.8, y = 0.9)) %>%
  layout(title = list(text = "Global cassava trends", font = list(size =
                                                                    18, color = "black")))
ggp


#Save html widget
htmlwidgets::saveWidget(ggp, "outputs/FAOcassava.html")

#Check file size
widget_file_size(ggp)


#### Once again, with region ####

p4 <-
  ggplot(
    FAOwide,
    aes(
      size = Production / 1000000,
      color = region,
      label = Country,
      frame = Year,
      Yield,
      Area / 1000000
    )
  ) +
  labs(x = "Yield (T/Ha)", y = "Area (M Ha)", size = "Production (M T)") +
  geom_point(aes(alpha = 0.5)) +
  scale_color_brewer(type = 'div', palette = 'Spectral')


ggp2 <- ggplotly(p4, height = 600, width = 900) %>%
  animation_opts(frame = 180,
                 easing = "linear",
                 redraw = FALSE,) %>%
  layout(
    annotations =
      list(
        x = 1,
        y = -0.4,
        text = "Source: FAOSTAT (2022). Visualization: Erik Delaquis, Alliance of Bioversity International and CIAT",
        showarrow = F,
        xref = 'paper',
        yref = 'paper',
        xanchor = 'right',
        yanchor = 'auto',
        xshift = 0,
        yshift = 0,
        font = list(size = 10, color = "black")
      )
  ) %>%
  layout(margin = list(b = 200, t = 40, pad = 4)) %>%
  layout(legend = list(x = 0.8, y = 0.9)) %>%
  layout(title = list(text = "Global cassava trends", font = list(size =
                                                                    18, color = "black")))
ggp2

htmlwidgets::saveWidget(ggp2, "outputs/FAOcassava.html")

ham2 <- partial_bundle(ggp2)
htmlwidgets::saveWidget(ham2, "outputs/FAOpartial.html")
