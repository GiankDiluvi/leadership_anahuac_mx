### ### ### ### ### ### ### ### ###
# The perceptions of leadership   #
# in Anáhuac University           #
#                                 #
# Author: Gian Carlo Diluvi       #
#               2019              #
### ### ### ### ### ### ### ### ###


# Preamble ####
library(tidyverse)
library(readr)
library(readxl)
library(scales)
library(wordcloud)
library(corrplot)
library(ggridges)

theme_set(theme_classic())


# Import data
anahuac <- readxl::read_xlsx("Leadership in Anahuac University data.xlsx")
n <- nrow(anahuac)


# Change column names
colnames(anahuac) <- c("timestamp",
                       "accion_social",
                       "carisma",
                       "elocuencia",
                       "iniciativa_proactividad",
                       "inspirar",
                       "integridad_moral",
                       "inteligencia",
                       "relaciones_interpersonales",
                       "tener_pasion",
                       "trabajo_en_equipo",
                       "frase",
                       "edad",
                       "genero",
                       "semestre",
                       "carrera")
# Diccionario nombres columbas
columns <- c("timestamp" = "Timestamp",
             "accion_social" = "Social Action",
             "carisma" = "Charisma",
             "elocuencia" = "Eloquence",
             "iniciativa_proactividad" = "Proactive behaviour",
             "inspirar" = "Inspire others",
             "integridad_moral" = "Moral integrity",
             "inteligencia" = "Intelligence",
             "relaciones_interpersonales" = "Interpersonal relations",
             "tener_pasion" = "Having passion",
             "trabajo_en_equipo" = "Team work",
             "frase" = "Frase",
             "edad" = "Edad",
             "genero" = "Género",
             "semestre" = "Semestre",
             "carrera" = "Carrera")


# Correlation matrix between variables
anahuac %>% 
    dplyr::select(accion_social:trabajo_en_equipo) %>% 
    dplyr::rename("Social Action" = "accion_social",
                  "Charisma" = "carisma",
                  "Eloquence" = "elocuencia",
                  "Proactive behaviour" = "iniciativa_proactividad",
                  "Inspire others" = "inspirar",
                  "Moral integrity" = "integridad_moral",
                  "Intelligence" = "inteligencia",
                  "Interpersonal relations" = "relaciones_interpersonales",
                  "Having passion" = "tener_pasion",
                  "Team work" = "trabajo_en_equipo") %>% 
    cor() %>% 
    corrplot::corrplot(type = "upper",
                       tl.col = "black",
                       tl.srt = 45)
    


# Make data tidy
anahuac_tidy <- anahuac %>%
    tidyr::gather(accion_social:trabajo_en_equipo,
                  key = "cualidad",
                  value = "importancia")


# Joy plot ####
# Order importance by average
average_importance <- anahuac_tidy %>% 
    dplyr::group_by(cualidad) %>% 
    dplyr::summarise(importancia_promedio = mean(importancia)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(importancia_promedio)

joyplot_density <- anahuac_tidy %>% 
    dplyr::mutate(cualidad = factor(cualidad,
                                    levels = average_importance$cualidad)) %>% 
    ggplot(aes(x = importancia, y = cualidad, fill = ..y..)) +
    geom_density_ridges(fill = "darkorange1",
                        alpha = 0.5) +
    scale_y_discrete(labels = columns) +
    scale_x_continuous(limits = c(4, NA)) +
    labs(x = "Importance",
         y = "Quality") +
    guides(fill=FALSE) +
    theme(axis.text.y = element_text(angle = 30))

ggsave("Plots/joyploy_density.pdf")
ggsave("Plots/joyploy_density.png")



# Boxplot ####
    boxplot_cualidades <- anahuac_tidy %>% 
        dplyr::mutate(cualidad = factor(cualidad,
                                        levels = average_importance$cualidad),
                      cualidad = forcats::fct_rev(cualidad)) %>% 
        ggplot(aes(x = cualidad, y = importancia)) +
        geom_boxplot(outlier.colour = "grey") +
        geom_jitter(width = 0.1,
                    color = "darkorange1") +
        scale_x_discrete(labels = columns) +
        scale_y_continuous(limits = c(4, NA)) +
        labs(x = "Quality",
             y = "Importance") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave("Plots/boxplot_cualidades.pdf")
    ggsave("Plots/boxplot_cualidades.png")




# Wordcloud ####
wordcloud_frases <- anahuac %>% 
    dplyr::filter(!(frase %in% c(".",
                                 "Habilidad de alinear los intereses propios con los de la gente que trabajas"))) %>% 
    dplyr::group_by(frase) %>% 
    dplyr::summarise(freq = n()) %>% 
    dplyr::arrange(desc(freq))

# Save in png
png("Plots/wordcloud_frases.png", width=12,height=8, units='in', res=300)
wordcloud(words = wordcloud_frases$frase,
          freq = wordcloud_frases$freq,
          scale = c(2.5, 0.5),
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.25, 
          colors = brewer.pal(5, "Dark2"))
dev.off()

# Save in pdf
pdf("Plots/wordcloud_frases.pdf", width=12,height=8)
wordcloud(words = wordcloud_frases$frase,
          freq = wordcloud_frases$freq,
          scale = c(2.5, 0.5),
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.25, 
          colors = brewer.pal(5, "Dark2"))
dev.off()



# Gender split
anahuac %>% 
    dplyr::group_by(genero) %>% 
    dplyr::summarise(total = n()) %>% 
    dplyr::mutate(total = total/sum(total))

gender_split_graph <- anahuac %>% 
    dplyr::group_by(genero) %>% 
    dplyr::summarise(total = n()) %>% 
    dplyr::mutate(total = total/sum(total)) %>% 
    ggplot(aes(x = reorder(genero, total), y = total)) +
    geom_bar(stat = "identity",
             fill = "darkorange1") +
    scale_y_continuous(labels = percent) +
    labs(x = "Gender",
         y = "% of respondents") +
    coord_flip()

ggsave("Plots/gender_split.pdf")
ggsave("Plots/gender_split.png")
    



# Boxplot w/gender ####
boxplot_cualidades_gender <- anahuac_tidy %>% 
    dplyr::mutate(cualidad = factor(cualidad,
                                    levels = average_importance$cualidad),
                  cualidad = forcats::fct_rev(cualidad)) %>% 
    ggplot(aes(x = cualidad, y = importancia)) +
    geom_boxplot(outlier.colour = "grey") +
    geom_jitter(width = 0.1,
                aes(color = genero)) +
    scale_x_discrete(labels = columns) +
    scale_y_continuous(limits = c(4, NA)) +
    labs(x = "Quality",
         y = "Importance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Average importance by gender
average_importance_gender <- anahuac_tidy %>% 
    dplyr::filter(genero %in% c("Hombre", "Mujer")) %>% 
    dplyr::group_by(cualidad, genero) %>% 
    dplyr::summarise(importancia_promedio = mean(importancia)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(genero, desc(importancia_promedio))



# Twitter plots ####

# Correlation matrix between variables
anahuac %>% 
    dplyr::select(accion_social:trabajo_en_equipo) %>% 
    dplyr::rename("Social Action" = "accion_social",
                  "Charisma" = "carisma",
                  "Eloquence" = "elocuencia",
                  "Proactive behaviour" = "iniciativa_proactividad",
                  "Inspire others" = "inspirar",
                  "Moral integrity" = "integridad_moral",
                  "Intelligence" = "inteligencia",
                  "Interpersonal relations" = "relaciones_interpersonales",
                  "Having passion" = "tener_pasion",
                  "Team work" = "trabajo_en_equipo") %>% 
    cor() %>% 
    corrplot::corrplot(type = "upper",
                       tl.col = "black",
                       tl.srt = 45,
                       title = "Correlation between qualities",
                       mar=c(0,0,1,0))


# Joy plot
joyplot_density_tw <- anahuac_tidy %>% 
    dplyr::mutate(cualidad = factor(cualidad,
                                    levels = average_importance$cualidad)) %>% 
    ggplot(aes(x = importancia, y = cualidad, fill = ..y..)) +
    geom_density_ridges(fill = "darkorange1",
                        alpha = 0.5) +
    scale_y_discrete(labels = columns) +
    scale_x_continuous(limits = c(4, NA)) +
    labs(x = "Importance",
         y = "Trait",
         title = "Is intelligence as relevant as we think?",
         subtitle = "Densities of answers of each trait, in decreasing order according to their mean") +
    guides(fill=FALSE) +
    theme(axis.text.y = element_text(angle = 30))

ggsave("Plots/Twitter/Joyplot.png")
