library(DataExplorer)
library(magrittr)
library(plotly)
library(tidyquant)
library(scales)

# Ucitavanje podataka
source('01_functions/load_data.R')

df %>% plot_intro()
df %>% plot_missing()
df %>% plot_histogram()
df %>% plot_bar()




# prosjecna cijena kvadrata po mjestu
df %>%  group_by(opstina,mesto) %>% 
  summarise(prosek = median(cena_po_m2)) %>% 
  arrange(desc(prosek)) %>% print(n = 50)

# broj nedostajucih vrijednosti
df %>%
  map_df(~ sum(is.na(.))) %>%
  gather() %>%
  arrange(desc(value)) %>% print(n = Inf)


# broj jedinstvenih vrijednosti u svakoj koloni
df %>%
  map_df(~ unique(.) %>%
           length()) %>% 
  gather() %>% print(n=Inf)

# Distribucija broja soba
plot_brojsoba <- ggplotly(ggplot(aes(x=broj_soba), data=df) + 
           geom_histogram(fill='#0099FF',  bins=20, stat="count") + 
           ggtitle('Distribucija broja soba') + theme_tq())


# Prosjecna cijena po opstini
plot_cena_opstina <- df %>% 
  group_by(opstina) %>% 
  filter (opstina != "") %>%
  summarize(across(cena, mean)) %>%
  mutate(cena = round(cena)) %>%
  arrange(desc(cena)) %>%
  mutate(opstina=factor(opstina,
                        levels = c("Savski Venac", "Stari Grad", "Vracar", "Novi Beograd",
                                   "Vozdovac","Zvezdara", "Cukarica", "Zemun", "Palilula",
                                   "Rakovica","Obrenovac", "Surcin","Grocka", 
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
plot_cena_stanje <- df %>%
  mutate(stanje = factor(stanje, 
                         levels = c("U izgradnji","Novo","Luksuz",
                                    "Renovirano","Uobicajeno",
                                    "Potrebno renoviranje", "Staro"))) %>%
  group_by(stanje) %>% 
  summarize(across(cena, mean)) %>% 
  mutate(cena = round(cena)) %>%
  plot_ly(
    x = ~stanje,
    y = ~cena,
    name = "Prosjecna cijena po stanju",
    type = "bar"
  ) %>% 
  layout(
    title = "Prosjecna cijena po stanju stana",
    xaxis = list(title = "Stanje"),
    yaxis = list(title = "Cijena",
                 tickformat = "\u20ac",
                 ticksuffix = "\u20ac")
  )

# Odnos Povrsine i cijene

plot_povrsina_cena <- ggplotly(df%>%  
           ggplot(aes(x=povrsina, y=cena)) + geom_point(col="black") +
           geom_smooth(se=F) +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix="")) +
           labs(title= "Povrsina / Cijena",
                x= "Povrsina",
                y= "Cijena") + theme_tq())

# Odnos Povrsine i cijene/m2

plot_povrsina_cenam2 <- ggplotly(df %>% 
           ggplot(aes(x=povrsina, y=cena_po_m2)) + geom_point(col="black") +
           geom_smooth(se=F) +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix="")) +
           labs(title= "Povrsina / Cijena/m2",
                x= "Povrsina",
                y= "Cijena/m2") + theme_tq())


# Cijena po m2 po opstini

plot_cenam2_opstina <- df %>% 
  group_by(opstina) %>% 
  summarise(across(cena_po_m2, mean)) %>%
  mutate(cena_po_m2=round(cena_po_m2)) %>%
  mutate(opstina= factor(opstina, levels= c("Savski Venac", "Stari Grad", "Vracar",
                                            "Novi Beograd","Vozdovac",
                                            "Zvezdara","Cukarica","Palilula", "Zemun",
                                            "Rakovica","Surcin", "Grocka", "Obrenovac", "Sopot",
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
plot_kvadratura_opstina <- df %>%
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
plot_cena_brojsoba <- ggplotly(df %>% 
           ggplot(aes(broj_soba,cena)) +
           geom_boxplot(aes(group=broj_soba), fill="#0099FF") +
           scale_y_continuous(labels = dollar_format(suffix="\u20ac", prefix ="", big.mark = ",")) +
           labs(title = "Price / Number of Rooms",
                x = "Number of rooms",
                y = "Price"))


# Save Image ----
save.image('03_env/eda_data.RData')
