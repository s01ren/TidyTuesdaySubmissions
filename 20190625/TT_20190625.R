# ------------------------------------------------------------------------
#
# TIDY TUESDAY 2019-06-25
#
# ------------------------------------------------------------------------


# load packages -----------------------------------------------------------
library(sp)
library(tidyverse)
library(viridis)


# mapping table -----------------------------------------------------------
mapping_city_district <- tribble(
  ~city, ~district,
  'aachen', 'Städteregion Aachen',
  'ansbach', 'Ansbach (Kreisfreie Stadt)',
  'babenhausen', 'Darmstadt',
  'bad pyrmont', 'Hameln-Pyrmont',
  'bamberg', 'Bamberg (Kreisfreie Stadt)',
  'baumholder', 'Birkenfeld',
  'bensheim', 'Darmstadt',
  'berlin', 'Berlin',
  'bierenbachtal', 'Oberbergischer Kreis',
  'biesenthal', 'Barnim',
  'bitburg', 'Eifelkreis Bitburg-Prüm',
  'bocholt', 'Borken',
  'bochum', 'Bochum',
  'bremen', 'Bremen',
  'buchholz', 'Heidekreis',
  'chemnitz', 'Chemnitz',
  'cologne', 'Köln',
  'darmstadt', 'Darmstadt',
  'dresden', 'Dresden',
  'elbingen', 'Westerwaldkreis',
  'emlichheim', 'Grafschaft Bentheim',
  'emmelshausen', 'Rhein-Hunsrück-Kreis',
  'erfurt', 'Erfurt',
  'erlangen', 'Erlangen',
  'frankfurt', 'Frankfurt (Oder)',
  'frankfurt am main', 'Frankfurt am Main',
  'freiburg', 'Freiburg im Breisgau',
  'fulda', 'Fulda',
  'gelsenkirchen', 'Gelsenkirchen',
  'grafenhausen', 'Waldshut',
  'hamburg', 'Hamburg',
  'hanau', 'Main-Kinzig-Kreis',
  'hannover', 'Region Hannover',
  'haus', 'Freyung-Grafenau',
  'heidelberg', 'Heidelberg',
  'heilbronn', 'Heilbronn',
  'kaiserlautern', 'Kaiserslautern (Kreisfreie Stadt)',
  'kassel', 'Kassel',
  'kelsterbach', 'Groß-Gerau',
  'kirchzell', 'Miltenberg',
  'lampertheim', 'Bergstraße',
  'langenleiten', 'Rhön-Grabfeld',
  'magdeburg', 'Magdeburg',
  'mainz', 'Mainz',
  'mannheim', 'Mannheim',
  'maugenhard', 'Lörrach',
  'miesau', 'Kaiserslautern',
  'mittenwald', 'Garmisch-Partenkirchen',
  'muenster', 'Münster',
  'munich', 'München (Kreisfreie Stadt)',
  'neckarsulm', 'Heilbronn',
  'neumarkt', 'Neumarkt in der Oberpfalz',
  'neuseddin', 'Potsdam-Mittelmark',
  'neuruppin', 'Ostprignitz-Ruppin',
  'neuss', 'Bergstraße',
  'nurenburg', 'Nürnberg',
  'obernheim', 'Zollernalbkreis',
  'osnabruck', 'Osnabrück (Kreisfreie Stadt)',
  'ottersberg', 'Verden',
  'ramstein', 'Kaiserslautern',
  'ransbach-baumbach', 'Westerwaldkreis',
  'regensburg', 'Regensburg',
  'schafhausen', 'Alzey-Worms',
  'schwalmtal', 'Vogelsbergkreis',
  'schweinfurt', 'Schweinfurt',
  'schwetzingen', 'Rhein-Neckar-Kreis',
  'sembach', 'Kaiserslautern',
  'senftenberg', 'Oberspreewald-Lausitz',
  'siegen', 'Siegen-Wittgenstein',
  'staufen', 'Breisgau-Hochschwarzwald',
  'stuttgart', 'Stuttgart',
  'trier', 'Trier',
  'waldorf', 'Ahrweiler',
  'weiden', 'Weiden in der Oberpfalz',
  'weissenburg', 'Weißenburg-Gunzenhausen',
  'werder', 'Potsdam-Mittelmark',
  'wildflecken', 'Bad Kissingen',
  'zehdenick', 'Oberhavel',
  'zirndorf', 'Fürth (Kreisfreie Stadt)'
)

# import map of German Laender --------------------------------------------
shape_de_level_1 <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_DEU_1_sp.rds", encoding = "utf-8"))
tmp1 <- data.frame(id = rownames(shape_de_level_1@data), shape_de_level_1@data)
tmp1$id <- as.character(tmp1$id)
tmp2 <- fortify(shape_de_level_1)
LAENDER <- 
  tmp1 %>% 
  left_join(tmp2, by = "id")
rm(tmp1, tmp2)


# import map of German districts ------------------------------------------
shape_de_level_2 <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_DEU_2_sp.rds", encoding = "utf-8"))
tmp1 <- data.frame(id = rownames(shape_de_level_2@data), shape_de_level_2@data)
tmp1$id <- as.character(tmp1$id)
tmp2 <- fortify(shape_de_level_2)
DISTRICTS <- 
  tmp1 %>% 
  left_join(tmp2, by = "id") %>% 
  mutate(
    NAME_2 = str_replace(NAME_2, "M�nchen", "München") %>% 
      str_replace("F�rth", "Fürth") %>% 
      str_replace("Osnabr�ck", "Osnabrück")
  )
rm(tmp1, tmp2)


# import UFO sightings ----------------------------------------------------
ufo_raw <- readr::read_csv(
  file = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv"
)

UFO_DISTR <- ufo_raw %>% 
  filter(country == "de") %>% 
  #filter(encounter_length <= 86400/2) %>% 
  mutate(encounter_length = case_when(encounter_length > 10000 ~ 10000, TRUE ~ encounter_length)) %>% 
  rowwise() %>% 
  mutate(city = trimws(str_split(city_area, "\\(", n = 2)[[1]][1])) %>% 
  left_join(mapping_city_district, by = "city") %>% 
  select(district, encounter_length) %>% 
  ungroup() %>% 
  group_by(district) %>% 
  summarise(encounter_length = sum(encounter_length, na.rm = TRUE))
UFO_DISTR


# join plot data ----------------------------------------------------------
PLOT_DISTR_UFO <- DISTRICTS %>% 
  left_join(UFO_DISTR, by = c("NAME_2" = "district")) %>% 
  mutate(encounter_length = replace_na(encounter_length, 0))

# plot map ----------------------------------------------------------------
ggplot(data = PLOT_DISTR_UFO, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = encounter_length), col = "white") + 
  geom_polygon(data = LAENDER, aes(x = long, y = lat, group = group), col = "black", fill = NA) + 
  # STYLING -----------------------------------------------------------------
  theme_classic() + 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    legend.position = "bottom",
    aspect.ratio = mapasp(shape_de_level_1)
  ) + 
  labs(
    title = "UFO sightings in Germany by encounter length",
    caption = "TidyTuesday 2019-06-25"
  ) + 
  #scale_fill_gradient(low = "#bdc3c7", high = "#3CD070", name = "Encounter length")
  scale_fill_viridis(
    option = "magma", 
    direction = -1,
    name = "Encounter length in seconds",
    # here we use guide_colourbar because it is still a continuous scale
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = F,
      title.position = 'top',
      # some shifting around
      title.hjust = 0.5,
      label.hjust = 0.5
    ))
ggsave("./ufo_germany.png")








