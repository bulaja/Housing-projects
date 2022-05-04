setwd("C://Users/dejan.bulaja/Desktop/R Programming/Belgrade Housing Project/")

library(plotly)
library(ggplot2)
library(leaflet)
a <- read.csv("Data/Final with coordinates.csv")

# Distribucija broja soba
ggplotly(ggplot(aes(x=broj_soba), data=a) + 
           geom_histogram(fill='#0099FF',  bins=20, stat="count") + 
           ggtitle('Distribucija broja soba') + theme_tq())


# Prosjecna cijena po opstini
a %>% 
  group_by(opstina) %>% 
  filter (opstina != "") %>%
  summarize(across(cena, mean)) %>%
  mutate(cena = round(cena)) %>%
  arrange(desc(cena)) %>%
  mutate(opstina=factor(opstina,
                        levels = c("Savski Venac", "Stari Grad", "Vracar", "Novi Beograd",
                                   "Vozdovac","Zvezdara", "Cukarica", "Zemun", "Palilula",
                                   "Rakovica", "Surcin", "Obrenovac","Grocka", 
                                   "Mladenovac", "Lazarevac", "Barajevo", "Sopot"))) %>%
  plot_ly(
    x = ~opstina,
    y = ~cena,
    name = "Prosjecna cijena po opstini",
    type = "bar"
  ) %>%
  layout(
    title = "Prosjecna cijena po opstini",
    xaxis = list(title = "Opstina"),
    yaxis = list(title = "Cijena",
                 tickformat = "\u20ac",
                 ticksuffix = "\u20ac")
  )

# Prosjecna cijena po stanju stana
a %>%
  mutate(stanje = factor(stanje, 
                         levels = c("U izgradnji","Novo","Luksuz",
                                    "Renovirano","Uobicajeno",
                                    "Potrebno renoviranje", "Staro"))) %>%
  group_by(stanje) %>% 
  summarize(across(cena, mean)) %>% 
  plot_ly(
    x = ~stanje,
    y = ~cena,
    name = "Prosjecna cijena po stanju",
    type = "bar"
  ) %>% 
  layout(
    title = "Prosjecna cijena po stanju stana",
    xaxis = list(title = "Stanje"),
    yaxis = list(title = "Cijena")
  )

# Odnos Povrsine i cijene

ggplotly(a%>%  
           ggplot(aes(x=povrsina, y=cena)) + geom_point(col="black") +
           geom_smooth(se=F) +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix="")) +
           labs(title= "Povrsina / Cijena",
                x= "Povrsina",
                y= "Cijena") + theme_tq())

# Odnos Povrsine i cijene / m2

ggplotly(a %>% 
           ggplot(aes(x=povrsina, y=cena_po_m2)) + geom_point(col="black") +
           geom_smooth(se=F) +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix="")) +
           labs(title= "Povrsina / Cijena/m2",
                x= "Povrsina",
                y= "Cijena/m2") + theme_tq())


# Cena po m2 po opstini

a %>% 
  group_by(opstina) %>% 
  summarise(across(cena_po_m2, mean)) %>%
  mutate(cena_po_m2=round(cena_po_m2)) %>%
  mutate(opstina= factor(opstina, levels= c("Savski Venac", "Stari Grad", "Vracar",
                                            "Novi Beograd","Vozdovac",
                                            "Zvezdara","Cukarica","Palilula", "Zemun", "Surcin",
                                            "Rakovica", "Grocka", "Obrenovac", "Sopot",
                                            "Mladenovac", "Lazarevac", "Barajevo"))) %>%
  plot_ly(
    x = ~opstina,
    y = ~cena_po_m2,
    name = "Average price per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average price/m2 per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average price",
                            tickformat = "\u20ac",
                            ticksuffix = "\u20ac"))

# Average size per neighborhood
a %>%
  group_by(opstina) %>%
  summarise(across(povrsina,mean)) %>%
  mutate(povrsina = round(povrsina)) %>%
  mutate(opstina= factor(opstina, levels= c("Savski Venac", "Stari Grad", "Vracar",
                                            "Novi Beograd","Vozdovac",
                                            "Zvezdara","Cukarica","Palilula", "Zemun", "Surcin",
                                            "Rakovica", "Grocka", "Obrenovac", "Sopot",
                                            "Mladenovac", "Lazarevac", "Barajevo"))) %>%
  plot_ly(
    x = ~opstina,
    y = ~povrsina,
    name = "Average size per neighborhood",
    type = "bar"
  ) %>% layout(title = "Average size per neighborhood",
               xaxis = list(title = "Neighborhood"),
               yaxis = list(title = "Average size in m2"))

# Price / Number of Rooms
ggplotly(a %>% 
           ggplot(aes(broj_soba,cena)) +
           geom_boxplot(aes(group=broj_soba), fill="#0099FF") +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ",")) +
           labs(title = "Price / Number of Rooms",
                x = "Number of rooms",
                y = "Price"))

b <- read.csv("Data/Final with coordinates.csv") %>%
  filter(opstina=="Grocka")
leaflet(data=b) %>%
  addTiles() %>%
  addMarkers(jitter(b$Y, factor = 75), 
             jitter(b$X, factor = 75),
             clusterOptions = markerClusterOptions(),
             label = paste(b$cena_po_m2, b$ulica, b$opstina, sep=", "))

a %>% select(opstina, mesto, ulica) %>% filter(opstina=="Grocka")
