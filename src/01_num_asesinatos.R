
library(ProjectTemplate)
reload.project()

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))



# Función para mapas
world <- map_data(map = "world")
world %>% head
ggmap_freq <- function(sub){
  tt.gg <- world %>% 
    as_tibble() %>% 
    mutate(region = tolower(region)) %>% 
    left_join( sub, 
               by = c("region" = "country_killed"))
  ggplot(tt.gg, aes(x = long, y = lat, 
                    group = group, fill = freq))+ 
    geom_polygon() + 
    theme_bw() + 
    coord_fixed() + 
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = .5),
          legend.position = "bottom") + 
    xlab(NULL) +
    ylab(NULL)
}




# ......................................... #
# Motivos Global

tt <- tab.motive %>% 
  # filter(`country killed` == "mexico") %>% 
  dplyr::select(rowname, name, 
                type_death, source_fire_c) %>% 
  gather(var.lab, var.value, -rowname, -name) %>% 
  group_by(var.lab, var.value) %>% 
  summarise(n = n_distinct(name)) %>% 
  ungroup 


tt %>% 
  filter(var.lab == "type_death") %>% 
  mutate(prop = round( 100*n/sum(n)) ) %>% 
  ggplot(aes(x = fct_reorder(var.value, n), 
             y = prop)) + 
  geom_bar(stat = "identity", 
           aes(fill = var.value), alpha = .5) + 
  geom_label(aes(label = paste0(var.value, ": ", prop, "%"),
                 y = 0,
                 color = var.value), 
             hjust = 0, size  = 5) +
  coord_flip() + 
  ylab("%") + xlab(NULL) + 
  scale_fill_manual(values = wes_palette( "GrandBudapest" )) + 
  scale_color_manual(values = wes_palette("BottleRocket")) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 15), 
        legend.position = "none") + 
  ggtitle("Asesinato es el principal tipo de muerte",
          subtitle = "Proporción de asesinatos por tipo de muerte")
ggsave("graphs/03_prop_typedeath.png", width = 4, height = 3)


pal <- wes_palette(15, name = "BottleRocket", type = "continuous")
tt %>% 
  filter(var.lab == "source_fire_c") %>% 
  mutate(prop = round( 100*n/sum(n)) ) %>% 
  ggplot(aes(x = fct_reorder(var.value, n), 
             y = prop)) + 
  geom_bar(stat = "identity", 
           aes(fill = var.value), alpha = .5) + 
  geom_label(aes(label = paste0(var.value, ": ", prop, "%"),
                 y = 0,
                 color = var.value), 
             hjust = 0, size  = 5) +
  coord_flip() + 
  ylab("%") + xlab(NULL) + 
  scale_fill_manual(values = pal ) + 
  scale_color_manual(values = pal ) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 15), 
        legend.position = "none") + 
  ggtitle("Grupos políticos y militares como principal\nfuente de fuego",
          subtitle = "Proporción de asesinatos por fuente de fuego")
ggsave("graphs/03_prop_sourcefire.png", width = 5, height = 7)




# ......................................... #
# Motivos México

tt <- tab.motive %>% 
  filter(`country killed` == "mexico") %>%
  dplyr::select(rowname, name, 
                type_death, source_fire_c) %>% 
  gather(var.lab, var.value, -rowname, -name) %>% 
  group_by(var.lab, var.value) %>% 
  summarise(n = n_distinct(name)) %>% 
  ungroup 

pal <- wes_palette(15, name = "BottleRocket", type = "continuous")
tt %>% 
  filter(var.lab == "source_fire_c") %>% 
  mutate(prop = round( 100*n/sum(n)) ) %>% 
  ggplot(aes(x = fct_reorder(var.value, n), 
             y = prop)) + 
  geom_bar(stat = "identity", 
           aes(fill = var.value), alpha = .5) + 
  geom_label(aes(label = paste0(var.value, ": ", prop, "%"),
                 y = 0,
                 color = var.value), 
             hjust = 0, size  = 5) +
  coord_flip() + 
  ylab("%") + xlab(NULL) + 
  scale_fill_manual(values = pal ) + 
  scale_color_manual(values = pal ) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 15), 
        legend.position = "none") + 
  ggtitle("Grupos criminales y fuente desconocida como\n principal fuente de fuego",
          subtitle = "Proporción de asesinatos por fuente de fuego en México")
ggsave("graphs/03_prop_sourcefire_mex.png", width = 5, height = 5)






# ......................................... #
# Muertes global por año
tab.motive %>%
  group_by(year) %>% 
  # tally %>% 
  summarise(n = n_distinct(name)) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_bar(stat = "identity", aes(alpha = n,
                                  fill = n)) + 
  geom_smooth(se = F, color = "#800000", 
              method = "loess", size = 2) + 
  scale_x_continuous(breaks = seq(1993, 2017, by = 2) ) + 
  scale_fill_continuous(low = "#c0e7e3", high = "#416863") + 
  scale_alpha_continuous(range = c(.6, 1)) +
  ylab("número de asesinatos") + 
  xlab("año") +
  ggtitle( paste("Hasta mayo de 2017 se han registrado", 
                 n_distinct(tab.motive$name), 
                 "\nmuertes con motivo conocido en el mundo"),
          "Número de muertes global por año.") +
  theme(legend.position = "none")
ggsave("graphs/01_ww_trend.png", width = 5.5, height = 4)



# ......................................... #
# Muertes mexico por año
tab.motive %>%
  filter(country_killed == "mexico") %>% 
  group_by(year) %>% 
  # tally %>% 
  summarise(n = n_distinct(name)) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_bar(stat = "identity", aes(alpha = n,
                                  fill = n)) + 
  geom_smooth(se = F, color = "#800000", 
              method = "loess", size = 2) + 
  scale_x_continuous(breaks = seq(1993, 2017, by = 2) ) + 
  scale_fill_continuous(low = "#c0e7e3", high = "#416863") + 
  scale_alpha_continuous(range = c(.4, .8)) +
  ylab("número de asesinatos") + 
  xlab("año") +
  ggtitle(paste("En México hay un total de", 
                n_distinct( filter(tab.motive, country_killed == "mexico")$name), 
                "muertes, pero\nen 2017 se observa un incremento importante"),
          "Número de muertes en México por año.") +
  theme(legend.position = "none")
ggsave("graphs/01_mex_trend.png", width = 5, height = 4)





# ............................................................................ #
# Frecuencia por país total periodo
tt <- tab.motive %>% 
  group_by(country_killed) %>% 
  # tally %>% 
  summarise(n = n_distinct(name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character)
tt$country_killed %>% n_distinct()

tt %>% 
  rename(freq = n) %>% 
  ggmap_freq(.) + 
  scale_fill_continuous(low = "#cdece8", high = "#4e7c77",
                        na.value = "grey95") +
  guides(fill = guide_legend(title = "# asesinatos")) + 
  ggtitle("Iraq y Syria mayor número de asesinatos 1992-2017")
ggsave("graphs/mapas_eda/mapa_tot.png", width = 7,height = 6)

tab.top15 <- arrange(tt, desc(n)) %>% .[1:15,]

pal <- wes_palette(15, name = "BottleRocket", type = "continuous")
tab.motive %>% 
  group_by(country_killed) %>% 
  # tally %>% 
  summarise(n = n_distinct(name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character) %>% 
  arrange(desc(n)) %>% 
  .[1:15,] %>% 
  rownames_to_column("rank") %>% 
  ggplot(aes(x = fct_reorder(rank, n), 
             y = n)) + 
  geom_bar(stat = "identity", 
           aes(fill = country_killed), alpha = .5) + 
  geom_label(aes(label = paste0(country_killed, ": ", n),
                 y = 0,
                 color = country_killed), 
             hjust = 0, size  = 5) +
  coord_flip() + 
  ylab("Número") + xlab(NULL) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme(axis.text.y = element_text(size = 15), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 15), 
        legend.position = "none") + 
  ggtitle("México ocupa el número 9 de 98 países",
          subtitle = "Número de asesinatos por país")
ggsave("graphs/02_top15_pais.png", width = 6, height = 7)




# ............................................................................ #

# Frecuencia por país por cuatrienio
tab.ggmapy <- tab.motive %>% 
  group_by(country_killed, cuatrienio) %>% 
  # tally %>% 
  summarise(n = n_distinct(name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character) %>% 
  ungroup() %>% 
  complete(cuatrienio, nesting(country_killed),
           fill = list(n = 0)) %>% 
  mutate(ng = cut(n, breaks = c(0, 1, 5, 10, 20, 50, 100), include.lowest = T, right = F)) %>% 
  mutate_at(.cols = "country_killed", .funs = as.character)
tab.ggmapy$country_killed %>% n_distinct()
tab.ggmapy$ng %>% table

colPalette <- colorRampPalette(colors = c("#ffe5e5",
                                          "#B30000"))(n_distinct(tab.ggmapy$ng))
ggmap_year <- function(tt){
  tt %>% 
    rename(freq = ng) %>% 
    ggmap_freq(.) + 
    guides(fill = guide_legend(title = "# asesinatos")) +
    scale_fill_manual(values = colPalette, drop = F,
                      na.value = "gray90",
                      name = "# nasesinatos")+
    ggtitle(unique(tt$cuatrienio))
}

gg.tib <- tab.ggmapy %>% 
  filter(!is.na(cuatrienio)) %>% 
  group_by(cuatrienio) %>% 
  do(ggmap = ggmap_year(.))
sapply(1:nrow(gg.tib), function(num){
  ggsave(filename = paste0("graphs/mapas_eda/mapa_cuatri_", num, ".png"),
         plot = gg.tib$ggmap[[num]], width = 7,height = 6)
  "fin"
})



# ............................................................................ #

# Frecuencia por país por año
tab.ggmapy <- tab.motive %>% 
  group_by(country_killed, year) %>% 
  # tally %>% 
  summarise(n = n_distinct(name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character) %>% 
  ungroup() %>% 
  complete(year, nesting(country_killed),
           fill = list(n = 0)) %>% 
  mutate(ng = cut(n, breaks = c(0, 1, 5, 10, 20, 35), include.lowest = T, right = F)) %>% 
  mutate_at(.cols = "country_killed", .funs = as.character)
tab.ggmapy$country_killed %>% n_distinct()
tab.ggmapy$ng %>% table

colPalette <- colorRampPalette(colors = c("#b2b2ff",
                                          "#33337f"))(n_distinct(tab.ggmapy$ng))
ggmap_year <- function(tt){
  tt %>% 
    rename(freq = ng) %>% 
    ggmap_freq(.) + 
    guides(fill = guide_legend(title = "# asesinatos")) +
    scale_fill_manual(values = colPalette, drop = F,
                      na.value = "gray90",
                      name = "# nasesinatos")+
    ggtitle(unique(tt$year)) +
    theme(title = element_text(size = 13))
}

gg.tib <- tab.ggmapy %>% 
  group_by(year) %>% 
  do(ggmap = ggmap_year(.))
sapply(1:nrow(gg.tib), function(num){
  ggsave(filename = paste0("graphs/mapas_eda/mapa_year_", num, ".png"),
         plot = gg.tib$ggmap[[num]], width = 7,height = 6)
  "fin"
})


