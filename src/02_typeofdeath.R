library(ProjectTemplate)
reload.project()

library(FactoMineR)
library(RColorBrewer)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

library(FactoMineR)
library(RColorBrewer)

tab.motive %>% head
tab.motive %>% names




# ......................................... #

# Función de CA
ggCA <- function(ca.fit, var.size = 5, col.size = 3){
  mca1_vars_df <- data.frame(ca.fit$col$coord) %>% 
    rownames_to_column("columna")
  mca1_obs_df <- data.frame(ca.fit$row$coord) %>% 
    rownames_to_column("renglon") %>% 
    mutate(col.color = ifelse(renglon == "mexico", "si", "no"))
  
  pal.man <- wes_palette(4, name = "GrandBudapest", type = "continuous")
  
  ggplot(data = mca1_obs_df, 
         aes(x = Dim.1, y = Dim.2)) + 
    geom_point(colour = "gray50", alpha = 0.7) + 
    geom_text(aes(label = renglon,
                  color= col.color,
                  size= col.color),
              # size = col.size, 
              nudge_x = .05, nudge_y = .05, 
              # color = "gray50", 
              alpha = .7) + 
    scale_size_manual(values = c(4, 6))+
    geom_text(data = mca1_vars_df, 
              fontface = "bold", size = var.size,
              aes(x = Dim.1, y = Dim.2, label = columna, 
                  colour = columna)) +
    scale_color_manual(values = c(pal.man, "gray50", "gray20"))
}




# ......................................... #

# Asociación de país total 1992 a 2017
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 15)) %>% 
  group_by(type_death, country_killed_c) %>% 
  summarise(n = n_distinct(name))
tt$country_killed_c %>% table

tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(type_death, n, fill = 0) %>% 
  data.frame(check.names = F)
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit) +
  ggtitle("México asociado a asesinatos, similar a\nBrasil, Filipinas y Colombia",
          "Asociación entre tipo de muerte y paises") + 
  theme(legend.position = "none", 
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank() )
ggsave(filename = "graphs/typeofdeath/typed_ca_total.png", width = 7,height = 6)



# ......................................... #

# Asociación de pais por cuatrienio
tt <- tab.motive %>% 
  filter(!is.na(year)) %>%
  mutate(periodo = cut(year, breaks = c(1992, 2005, 2017),
                       include.lowest = T,
                       dig.lab = 5)) %>%
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(type_death, country_killed_c, periodo) %>% 
  summarise(n = n_distinct(name))

tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(type_death, n, fill = 0) %>% 
  data.frame(check.names = F)


# type_death != "desconocido"
ggCA_year <- function(sub){
  # sub <- tab %>% filter(cuatrienio == "(1996,2000]")
  print(unique(sub$periodo))
  sub <- sub %>% data.frame(check.names = F)
  row.names(sub) <- sub$country_killed_c
  tab.ca <- sub[, c(-1,-2)]
  ca.fit <- CA(tab.ca[, apply(tab.ca, 2, sum) > 0], graph = F)
  # summary(ca.fit, nb.dec = 2, ncp = 2)
  ggCA(ca.fit) +
    ggtitle("En México la muerte de periodistas se asocia a asesinatos,\nsimilar a Colombia y Filipinas",
            paste("Asociación de tipo de muerte por paises en ", 
                  unique(sub$periodo))) + 
    theme(legend.position = "none") 
}

ggca.tib <- tab %>% 
  group_by(periodo) %>% 
  do(ggca = ggCA_year(.))
ggca.tib$ggca

sapply(1:nrow(ggca.tib), function(num){
  ggsave(filename = paste0("graphs/typeofdeath/typed_ca_periodo_", num, ".png"),
         plot = ggca.tib$ggca[[num]], width = 7,height = 6)
  "fin"
})
