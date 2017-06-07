library(ProjectTemplate)
reload.project()

library(FactoMineR)
library(RColorBrewer)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

head(tab.motive)
names(tab.motive)

tab.motive %>% head
tab.motive %>% names


# ......................................... #

# Función de CA
ggCA <- function(ca.fit, var.size = 5, col.size = 3, country = F){
  mca1_vars_df <- data.frame(ca.fit$col$coord) %>% 
    rownames_to_column("columna")
  mca1_obs_df <- data.frame(ca.fit$row$coord) %>% 
    rownames_to_column("renglon") 
  
  if(country == T){
    mca1_obs_df <- mca1_obs_df %>% 
      mutate(col.color = ifelse(renglon == "mexico", "1.mex", "2.no mex"))
  }
  
  # pal.man <- c(brewer.pal(7, "Set2"), brewer.pal(3, "Set1")[1:2])
  pal.man <- c(wes_palette(7, name = "GrandBudapest", type = "continuous"), 
               wes_palette(2, name = "GrandBudapest", type = "continuous"))

  
  gg.1 <- ggplot(data = mca1_obs_df, 
         aes(x = Dim.1, y = Dim.2)) + 
    geom_point(colour = "gray50", alpha = 0.7) + 
    geom_text(data = mca1_vars_df, 
              fontface = "bold", size = var.size,
              aes(x = Dim.1, y = Dim.2, 
                  label = columna, 
                  colour = columna)) +
    geom_point(data = mca1_vars_df, 
              size = 3,
              aes(x = Dim.1, y = Dim.2, 
                  colour = columna))
    
  if(country == T){
    gg.f <- gg.1 + 
      geom_text(aes(label = renglon,
                  color= col.color,
                  size= col.color),
              # size = col.size, 
              nudge_x = .05, nudge_y = .05, 
              # color = "gray50", 
              alpha = .7) +
    scale_size_manual(values = c(6, 4))  +
    scale_color_manual(values = c("gray20", "gray50", pal.man))
  }
  if(country == F){
    gg.f <- gg.1 + 
      geom_text(aes(label = renglon),
                size = col.size,
                nudge_x = .05, nudge_y = .05, 
                color = "gray40",
                alpha = .7)+
      scale_color_manual(values = c(pal.man, "gray20", "gray50"))
  }
  gg.f
}


# ......................................... #

# impunidad vs source_fire_c
tt <- tab.motive %>% 
  group_by(source_fire_c, impunity) %>% 
  tally %>% 
  complete(nesting(source_fire_c), impunity, fill = list(n = 0) ) %>% 
  spread(impunity, n) %>% 
  data.frame(check.names = F)

row.names(tt) <- tt$source_fire_c
ca.fit <- CA(tt[, -1], graph = F)
ggCA(ca.fit, col.size = 4) + 
  ggtitle("Impunidad asociada a grupos políticos,\ncriminales y paramilitares",
          "Asociación entre impunidad y fuente de fuego") + 
  theme(legend.position  = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave("graphs/sourcefire/source_impunity_tot.png", width = 6, height = 5)




# impunidad vs source_fire_c en MEXICO
tt <- tab.motive %>% 
  filter(`country killed` == "mexico") %>% 
  group_by(source_fire_c, impunity) %>% 
  tally %>% 
  complete(nesting(source_fire_c), impunity, fill = list(n = 0) ) %>% 
  spread(impunity, n) %>% 
  data.frame(check.names = F)

row.names(tt) <- tt$source_fire_c
ca.fit <- CA(tt[, -1], graph = F)
ggCA(ca.fit, col.size = 4) + 
  ggtitle("Impunidad asociada a criminales y se\npierde información en grupos militares y de gobierno",
          "Asociación entre impunidad y fuente de fuego en México") + 
  theme(legend.position  = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave("graphs/sourcefire/source_impunity_mex.png", width = 6, height = 5)







# ......................................... #
# type of death vs source_fire_c
tt <- tab.motive %>% 
  group_by(source_fire_c, type_death) %>% 
  tally %>% 
  complete(nesting(source_fire_c), type_death, fill = list(n = 0) ) %>% 
  spread(type_death, n, fill = 0) %>% 
  data.frame(check.names = F)

row.names(tt) <- tt$source_fire_c
ca.fit <- CA(tt[, -1], graph = F)
ggCA(ca.fit, var.size = 6, col.size = 5) + 
  ggtitle("Asesinato asociado a grupos criminales,\nlocales y oficiales de gobierno",
          "Asociación entre tipo de muerte y fuente de fuego") + 
  theme(legend.position  = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave("graphs/sourcefire/source_typefire_tot.png", width = 6, height = 5)




# ......................................... #

# Asociación de país total 1992 a 2017
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(source_fire_c, country_killed_c) %>% 
  summarise(n = n_distinct(name))

tt$country_killed_c %>% table

tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(source_fire_c, n, fill = 0) %>% 
  data.frame(check.names = F)
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit, var.size = 5, country = T) +
  ggtitle("México asociado a paises como Filipinas y Brazil",
          "Fuente de Fuego por País") + 
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave(filename = "graphs/sourcefire/source_ca_country_total.png", 
       width = 7,height = 6)






# ......................................... #

# Asociación de pais cada 10 años
tt <- tab.motive %>% 
  filter(!is.na(year)) %>%
  mutate(periodo = cut(year, breaks = c(1992, 2005, 2017),
                       include.lowest = T,
                       dig.lab = 5)) %>%
  mutate(country_killed_c = fct_lump(country_killed, n = 15)) %>% 
  # filter(year < 2017) %>% 
  group_by(source_fire_c, country_killed_c, periodo) %>% 
  summarise(n = n_distinct(name))
tt

tab <- tt %>% 
  filter(country_killed_c != "Other") %>%
  spread(source_fire_c, n, fill = 0) %>% 
  data.frame(check.names = F)
apply(tab[, -1:-2], 2, sum)


ggCA_year <- function(sub){
  # sub <- tab %>% filter(periodo == "(2004,2017]")
  print(unique(sub$periodo))
  sub <- sub %>% data.frame(check.names = F)
  row.names(sub) <- sub$country_killed_c
  tab.ca <- sub[, c(-1,-2)]
  ca.fit <- CA(tab.ca, graph = F)
  
  ggCA(ca.fit, var.size = 5, country = T) +
    ggtitle("",
            paste("Asosciación de fuente de fuego y paises ",
                  unique(sub$periodo))) +
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank() )
 }

ggca.tib <- tab %>%
  group_by(periodo) %>%
  do(ggca = ggCA_year(.))
ggca.tib$ggca

sapply(1:nrow(ggca.tib), function(num){
  ggsave(filename = paste0("graphs/sourcefire/source_ca_periodo_", num, ".png"),
         plot = ggca.tib$ggca[[num]], width = 7,height = 6)
  "fin"
})




