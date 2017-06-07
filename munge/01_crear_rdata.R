
library(ProjectTemplate)
reload.project()

library(forcats)


df.cpj.17 <- readxl::read_excel("data/cpj-database.xls", 
                                sheet = 1, skip = 1) %>% 
  as_tibble() %>% 
  mutate_if(is.factor, tolower) %>% 
  mutate_all(str_trim) %>% 
  separate(col = Date, into = c("day_month", "year"), 
           sep = ",", remove = F) %>% 
  mutate(year = parse_number(year)) %>% 
  mutate(country_killed = fct_recode(`Country Killed`, 
                                     israel = "israel and the occupied palestinian territory"),
         year = ifelse(year < 92, year + 2000, year)) %>% 
  rownames_to_column() 
df.cpj.17

names(df.cpj.17) <- tolower(names(df.cpj.17))

df.cpj.17 %>% data.frame() %>% head
df.cpj.17 %>% dim
str(df.cpj.17)



which(is.na(df.cpj.17$date))




# 1. Motive table
tab.mot <- df.cpj.17[1:( which(is.na(df.cpj.17$date))[1] -1), ]
tab.mot  %>% dim # 1244

# 2. Dates
aux <- tab.mot %>% 
  filter(is.na(year)) %>%
  filter(`day_month` != "unknown") %>%
  mutate(year = parse_number(`day_month`)) %>% 
  mutate(year = parse_number( ifelse(year < 29, 
                                     str_sub(`day_month`, -4, -1), 
                                     year)) ) 
aux %>% data.frame()

tab.motive <- tab.mot %>% 
  filter(!(rowname %in% aux$rowname)) %>% 
  bind_rows(aux) %>% 
  mutate(quinquenio = cut(year, breaks = seq(1990, 2020, by = 5), 
                          include.lowest = T),
         cuatrienio = cut(year, breaks = seq(1992, 2020, by = 4), 
                          include.lowest = T)) %>% 
  ungroup %>% 
  mutate(cuatrienio = fct_collapse(cuatrienio, 
                                   `(2012,2017] ` = c("(2012,2016]", "(2016,2020]"))) %>% 
  rename(impunity = `impunity (for murder)`,
         type_death = `type of death`,
         source_fire = `source of fire`) %>% 
  mutate(impunity = fct_explicit_na(factor(impunity), "na"),
         source_fire_c = fct_lump( factor(source_fire), n = 8)) %>% 
  mutate(impunity = fct_recode(impunity, 
                               `sin impunidad` = 'no',
                               parcial = 'partial',
                               `impunidad` = 'yes', 
                               `sin info` = 'na'), 
         source_fire_c = fct_recode( str_replace_na(source_fire_c), 
                                     `grupo criminal` = "criminal group",
                                     `oficiales gobierno` = "government officials",
                                     locales = "local residents",
                                     militares = "military officials",
                                     `violencia callejera` = "mob violence",
                                     `grupo paramilitar`  = "paramilitary group",
                                     `grupo político`  = "political group",
                                     desconocido  = "unknown fire",
                                     desconocido = "NA",
                                     otro  = "Other"),
         type_death = fct_recode( str_replace_na(type_death), 
                                 asesinato = "murder",
                                 `fuego cruzado` = "crossfire/combat-related",
                                 `misión peligrosa` = "dangerous assignment",
                                 desconocido = "unknown",
                                 desconocido = "NA")) 

tab.motive %>% dim # 1244
tab.motive %>% head
tab.motive$quinquenio %>%  unique
tab.motive$cuatrienio %>%  unique
tab.motive$source_fire %>% unique
tab.motive$type_death %>% unique
tab.motive$impunity %>% unique


apply(is.na(tab.motive), 2, sum)
filter(tab.motive, is.na(quinquenio))

length(tab.motive$name)
n_distinct(tab.motive$name)



cache("df.cpj.17")
cache("tab.motive")
