library(ProjectTemplate)
reload.project()

library(FactoMineR)
library(RColorBrewer)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

head(tab.motive)
tab.motive %>% head
tab.motive %>% names



# ......................................... #

# Función de CA
ggCA <- function(ca.fit, var.size = 5, col.size = 3){
  mca1_vars_df <- data.frame(ca.fit$col$coord) %>% 
    rownames_to_column("columna")
  mca1_obs_df <- data.frame(ca.fit$row$coord) %>% 
    rownames_to_column("renglon") %>% 
    mutate(col.color = ifelse(renglon == "mexico", "1.mex", "2.no mex"))
  
  pal <- wes_palette(4, name = "GrandBudapest")
  
  ggplot(data = mca1_obs_df, 
         aes(x = Dim.1, y = Dim.2)) + 
    geom_point(colour = "gray50", alpha = 0.7) + 
    # geom_density2d(colour = "gray80") +
    geom_text(aes(label = renglon,
                  color= col.color,
                  size= col.color),
              nudge_x = .05, nudge_y = .05, 
              alpha = .7) + 
    scale_size_manual(values = c(6, 4.5)) +
    geom_point(data = mca1_vars_df, 
              size = 3,
              aes(x = Dim.1, y = Dim.2, 
                  colour = columna)) +
    geom_text(data = mca1_vars_df, 
              fontface = "bold", 
              size = var.size,
              aes(x = Dim.1, y = Dim.2, 
                  label = columna, 
                  colour = columna)) +
    scale_color_manual(values = c("gray20", "gray50", pal))
}


# ......................................... #

# impunidad vs type death
tt <- tab.motive %>% 
  group_by(type_death, impunity) %>% 
  tally %>% 
  complete(nesting(type_death), impunity, fill = list(n = 0) ) %>% 
  spread(impunity, n) %>% 
  data.frame()

row.names(tt) <- tt$type_death
CA(tt[, -1], graph = F)


  
  

# ......................................... #
# Total
tab.motive %>% 
  group_by(impunity) %>% 
  summarise(n = n_distinct(name)) %>% 
  ggplot(aes( x = fct_reorder(impunity, n) , y = n)) +
  geom_bar(stat = "identity", alpha = .7) +
  coord_flip() + 
  ylab("número de asesinatos") + 
  xlab("impunidad") +
  ggtitle("La mayoría de los asesinatos quedan impunes")


tab.motive %>% 
  # filter(year < 2017) %>% 
  filter(country_killed == "mexico") %>% 
  group_by(impunity, year) %>% 
  summarise(n = n_distinct(name)) %>% 
  ggplot(aes( x= year, y = n, color = impunity))+
  geom_line(alpha = .5)+
  geom_smooth(se = F, 
              method = "loess", size = 1) + 
  scale_x_continuous(breaks = seq(1992, 2017, by = 2) ) 




# ......................................... #

# Asociación de país total 1992 a 2017
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(impunity, country_killed_c) %>% 
  summarise(n = n_distinct(name))
tt$country_killed_c %>% table
tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(impunity, n, fill = 0) %>% 
  data.frame(check.names = F)
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit = ca.fit, var.size = 7, col.size = 4) +
  ggtitle("México está asociado a la impunidad total-parcial", 
          "Asociación de impunidad por país desde 1992.") + 
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave(filename = "graphs/impunity/imp_ca_cuatri_total.png", width = 7,height = 6)



# ......................................... #

# Asociación de país total a partir de 2006
tt <- tab.motive %>% 
  filter(year >= 2006) %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(impunity, country_killed_c) %>% 
  summarise(n = n_distinct(name))
tt$country_killed_c %>% table

tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(impunity, n, fill = 0) %>% 
  data.frame(check.names = F)
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit = ca.fit, var.size = 7, col.size = 4) +
  ggtitle("México está asociado a la impunidad total-parcial", 
          "Asociación de impunidad por país desde 2006") + 
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave(filename = "graphs/impunity/imp_ca_cuatri_total.png", width = 7,height = 6)





# ......................................... #

# Asociación de pais por cuatrienio
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  filter(year < 2017) %>% 
  group_by(impunity, country_killed_c, cuatrienio) %>% 
  summarise(n = n_distinct(name))

tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(impunity, n, fill = 0) %>% 
  data.frame(check.names = F)

ggCA_year <- function(sub){
  # sub <- tab %>% filter(cuatrienio == "[1992,1996]")
  sub <- sub %>% data.frame(check.names = F)
  row.names(sub) <- sub$country_killed_c
  tab.ca <- sub[, c(-1,-2)]
  ca.fit <- CA(tab.ca, graph = F)
  # summary(ca.fit, nb.dec = 2, ncp = 2)
  ggCA(ca.fit) +
    ggtitle(paste("Impunidad por País\n", 
                  unique(sub$cuatrienio))) +
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank() )
}

ggca.tib <- tab %>% 
  group_by(cuatrienio) %>% 
  do(ggca = ggCA_year(.))
sapply(1:nrow(ggca.tib), function(num){
  ggsave(filename = paste0("graphs/impunity/imp_ca_cuatri_", num, ".png"),
         plot = ggca.tib$ggca[[num]], width = 7,height = 6)
  "fin"
})
