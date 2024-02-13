#Setup ####
#set packages needed
pkgs <- c("here", "tidyverse", "hrbrthemes", "lubridate", "cowplot", "sf",
          "sfhotspot", "plotly")
#install/update any packages needed
install.packages(pkgs, repos = 'http://cran.rstudio.com/')
#load packages
lapply(pkgs, require, character.only = TRUE) #where

#Data Import ####
wings <- read.csv("F23_wings.csv")
rep <- read.csv("F23_repeat_wings.csv")
mig <- read.csv("migration - f23.csv")

rep1 <- rep %>% 
  filter(If.you.found.multiple.birds.of.the.same.species..in.the.same.location..and.having.the.same.disposition..please.enter.the.number.of.birds.here > 1)

rep2 <- rbind(rep, rep1)
rep2 <- merge(rep2, wings, by.x = "ParentGlobalID", by.y = "GlobalID") 
rep2 <- rep2 %>% 
  mutate(Date = as_date(Date...Time, format = "%m/%d/%Y %I:%M:%S %p"))
mig <- mig %>% 
  mutate(Date = as_date(Date, format = "%m/%d/%Y"))
rep3 <- merge(rep2, mig,  by = "Date")

w1 <- rep2 %>% 
  filter(Disposition != "") %>% 
  mutate(Count = 1) %>% 
  group_by(Disposition) %>% 
  summarize(Count = sum(Count)) %>% 
  arrange(Count) %>% 
  mutate(Disposition=factor(Disposition, levels=Disposition)) 

p <- ggplot(w1, mapping = aes(Count, Disposition)) +
      geom_bar(stat = "identity", fill = "plum4") +
      labs(x = "\nCount", y = "",
        title = "Number of Birds Found by Disposition",
        subtitle = "The status of birds when found by volunteers.",
        caption = "Last updated 12-6-2023") +
      theme(plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic", color = "grey55"))

p <- ggplotly(p, tooltip = "Count")
library(htmlwidgets)
saveWidget(p, "wings_F23_1.html", selfcontained = F, libdir = "lib")

w2 <- rep2 %>% 
  filter(Disposition != "") %>% 
  mutate(Count = 1) %>% 
  mutate(Bird.Species..if.known. = ifelse(Bird.Species..if.known. == "Gray catbird", "Gray Catbird", 
                                          ifelse(Bird.Species..if.known. == "Northern Flicker ", "Northern Flicker", 
                                                 ifelse(Bird.Species..if.known. == "Northern Waterthrush ", "Northern Waterthrush", Bird.Species..if.known.)))) %>% 
  group_by(Bird.Species..if.known.) %>% 
  summarize(Count = sum(Count)) %>%  
  arrange(Count) %>% 
  mutate(Bird.Species..if.known.=factor(Bird.Species..if.known., levels=Bird.Species..if.known.)) 

p <- ggplot(w2, mapping = aes(Count, Bird.Species..if.known.)) +
  geom_bar(stat = "identity", fill = "lightsteelblue4") +
  labs(x = "\nCount", y = "Species\n",
       title = "Number of Birds Found by Species",
       caption = "Last updated 10-26-2023") +
  theme(plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic", color = "grey55"))

p <- ggplotly(p, tooltip = c("Species", "Count"))
saveWidget(p, "wings_F23_2.html", selfcontained = F, libdir = "lib")

w3 <- rep3 %>% 
  mutate(Count = ifelse(Disposition != "", 1, 0)) %>% 
  group_by(Date, Wind.Speed, Dew, Temp, Migrate, Altitude) %>% 
  summarize(Count = sum(Count))

p1 <- ggplot(w3) +
  geom_line(mapping = aes(Date, Count),
            color = "grey") +
  geom_point(mapping = aes(Date, Count),
             stat = "identity", fill = "#69b3a2", 
             color = "black", shape = 21, size = 4) +
  labs(x = "\nDate", y = "Count\n",
       title = "Number of Birds Found by Date",
       caption = "Last updated 10-26-2023") +
  #theme_ipsum() +
  theme(plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic", color = "grey55"))
p2 <- ggplot(mig) +
  #geom_line(mapping = aes(Date, m),
  #color = "grey") +
  geom_bar(mapping = aes(Date, Migrate),
           stat = "identity", fill = "#69b3a2") +
  #geom_smooth(mapping = aes(x = Date, y = Migrate),
  #se = FALSE, color = "black") +
  labs(x = "\nDate", y = "",
       title = "Nightly Migration",
       #subtitle = "*Unknown birds include individuals taken to the WI Humane Society that were identified upon admission",
       caption = "Last updated 10-26-2023") +
  scale_y_continuous(breaks = c(0, 1000000, 2000000, 3000000),
                     labels = c("0", "1M", "2M", "3M")) +
  #theme_ipsum() +
  theme(plot.subtitle = element_text(size = 10, face = "italic"),
        plot.caption = element_text(size = 7, face = "italic", color = "grey55"))

#ggplot(w3) +
#geom_line(mapping = aes(Date, Temp),
#color = "grey") +
#geom_line(mapping = aes(Date, Dew),
#color = "grey") +
#geom_line(mapping = aes(Date, Wind.Speed),
#color = "grey") +
#geom_line(mapping = aes(Date, Count),
#color = "grey") +
#labs(x = "\nDate", y = "Count\n",
#title = "Number of Birds Found by Date") +
#theme_ipsum() +
#theme(plot.subtitle = element_text(size = 10, face = "italic"),
#plot.caption = element_text(size = 7, face = "italic", color = "grey55"))

m1 <- lm(Count ~ Migrate, data = w3)
m2 <- lm(Count ~ Migrate + Altitude, data = w3)

plot_grid(p1, p2)

#need to jitter points based on location number for locs that do not have a lat long
#then run this and export hotspots as shapefile
p <- rep3 %>% 
      select(ObjectID.x, Provide.a.description.of.the.location.if.you.did.not.use.the.web.map..use.photo.above.to.identify.a.general.area., x.x,y.x) %>% 
      filter(x.x == 0,
         Provide.a.description.of.the.location.if.you.did.not.use.the.web.map..use.photo.above.to.identify.a.general.area.!= "",
         Provide.a.description.of.the.location.if.you.did.not.use.the.web.map..use.photo.above.to.identify.a.general.area.!= "NML")
gen <- read.csv("wings_gen.csv")
p1 <- p %>% 
      mutate(CID = parse_number(Provide.a.description.of.the.location.if.you.did.not.use.the.web.map..use.photo.above.to.identify.a.general.area.)) %>% 
      arrange(CID) %>% 
      mutate(OID = 1:nrow(p)) %>% 
      select(ObjectID.x, CID, OID) %>% 
      left_join(., gen, by = "OID") %>% 
      rename(y.x = "Lat", x.x = "Long") %>% 
      select(ObjectID.x, x.x, y.x, CID.x, OID) 
rep4 <- rep3 %>% 
          mutate(OID = ifelse(x.x == 0, 1:nrow(p1), NA)) %>% 
          left_join(., p1, by = "OID") %>% 
          mutate(x.x.x = ifelse(x.x.x == 0, x.x.y, x.x.x),
                 y.x.x = ifelse(y.x.x == 0, y.x.y, y.x.x)) %>% 
          filter(x.x.x != 0)
      

points <- st_as_sf(x = rep4,
                   coords = c("x.x.x", "y.x.x"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
points_utm <- st_transform(points, 32616)
points_hotspots <- hotspot_gistar(points_utm, 5)
points_hotspots %>% 
  filter(gistar>0, pvalue<0.05) %>% 
  ggplot(aes(color = kde, fill = kde)) +
  geom_sf() +
  scale_colour_distiller(aesthetics = c("color", "fill"), direction = 1) +
  labs(title = "Density of bird collisions") +
  theme_void()
st_write(points_utm, "F23_wings.shp", delete_layer = TRUE)
st_write(points_hotspots, "F23_wings_hotspots.shp", delete_layer = TRUE)
write.csv(rep4, "F23_alldata.csv")
