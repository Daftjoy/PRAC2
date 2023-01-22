library(dplyr)
library(stringr)   

data <- read.csv2('datamarket_productos_de_supermercados.csv', sep=',')

data <- data[which(as.numeric(data$price) < 200),]


data$final_cat <- data$category 
data$new_cat <- str_split_fixed(data$category,"_", 2) [,1]
data <- data[!(str_split_fixed(data$category, "_", 4)[,3] == "cestas" |
                 str_split_fixed(data$category, "_", 5)[,4] %in% c( "menaje", "bazar") | data$new_cat == 'soy'),]

data[ (which(data$new_cat %in% c("carne", "marisco", "charcuteria", "fruta",
                                           "huevos","panaderia","arroz", "cereales", "aceite",
                                           "lacteos",  "frescos",  "al", "desayuno", "despensa", "alimentacion",
                                           "el", "la", "productos"))), 'final_cat'] <- 'Frescos'


data[ (which(data$new_cat %in% c("cuidado", "bebe", "fitoterapia", "limpieza", "maquillaje", "mascotas",
                                           "drogueria", "perfumeria", "cuidado", "parafarmacia"))), 'final_cat'] <- 'Drogueria/Perfumeria'

data[ (which(data$new_cat %in% c("agua", "bodega", "zumos", "cacao", "bebidas") | data$new_cat %in% c("desayuno", "cacao") &
                    str_split_fixed(data$category, "_", 6)[,5] %in% c("cafe", "infusiones", "cacao"))), 'final_cat'] <- 'Bebidas'



data[ (which(data$new_cat %in% c("aperitivos", "pizzas", "postres", "azucar", "cacao", "platos", "dulces", "productos") |
                                             (data$new_cat == "despensa"  & 
                                             (str_split_fixed(data$category, "_", 5)[,4] == "dulces"| 
                                             str_split_fixed(data$category, "_", 2)[,2] == "aperitivos" )) |
                                              (data$new_cat == "desayuno" &
                                             str_split_fixed(data$category, "_", 6)[,5] %in% c("azucar", "caramelos", "bolleria")) |
               (data$new_cat=="la" & str_split_fixed(data$category, "_", 4)[,3]  %in% c("aperitivos", "dulce")) |
               (data$new_cat == "productos" & str_split_fixed(data$category, "_", 5)[,4]  == "cocina" |
               str_split_fixed(data$category, "_", 4)[,3]  == "sushi"))) , 'final_cat'] <- 'Procesados'



data[ (which(data$new_cat %in% c("congelados", "conservas") |
                    (data$new_cat =="despensa" & str_split_fixed(data$category, "_", 4)[,2] %in% c("conservas", "sopas", "cocina", "pates"))|
                    (data$new_cat %in% c("alimentacion") &
                    str_split_fixed(data$category, "_", 3)[,2] %in% c("conservas", "pizza", "patatas", "caldos", "aceitunas")) |
                    (data$new_cat =="la" & str_split_fixed(data$category, "_", 4)[,3] %in% c( "conservas", "helados") ))), 'final_cat'] <- 'Congelados_y_Conservas'



mercadona <- data[which(data$supermarket == 'mercadona-es'),]

dia <- data[which(data$supermarket == 'dia-es'),]

carrefour <- data[which(data$supermarket == 'carrefour-es'),]



media_m <- aggregate(as.numeric(mercadona$price), list(mercadona$insert_date), FUN=mean)
media_d <- aggregate(as.numeric(dia$price), list(dia$insert_date), FUN=mean)
media_c <- aggregate(as.numeric(carrefour$price), list(carrefour$insert_date), FUN=mean)
media_t <- aggregate(as.numeric(data$price), list(data$insert_date), FUN=mean)


media_total <- media_m %>% left_join(media_d, by='Group.1') %>% left_join(media_c, by='Group.1') %>% left_join(media_t, by='Group.1')
media_total

colnames(media_total) <- c('Fecha', 'Total Mercadona', 'Total Dia', 'Total Carrefour', 'Total Todos')

media_m_frescos <- aggregate(as.numeric(mercadona[which(mercadona$final_cat == 'Frescos'), 'price']), list(mercadona[which(mercadona$final_cat == 'Frescos'), 'insert_date']), FUN=mean)
media_d_frescos <- aggregate(as.numeric(dia[which(dia$final_cat == 'Frescos'), 'price']), list(dia[which(dia$final_cat == 'Frescos'), 'insert_date']), FUN=mean)
media_c_frescos <- aggregate(as.numeric(carrefour[which(carrefour$final_cat == 'Frescos'), 'price']), list(carrefour[which(carrefour$final_cat == 'Frescos'), 'insert_date']), FUN=mean)
media_t_frescos <- aggregate(as.numeric(data[which(data$final_cat == 'Frescos'), 'price']), list(data[which(data$final_cat == 'Frescos'), 'insert_date']), FUN=mean)

media_frescos <- media_m_frescos %>% left_join(media_d_frescos, by='Group.1') %>% left_join(media_c_frescos, by='Group.1') %>% left_join(media_t_frescos, by='Group.1')
colnames(media_frescos) <- c('Fecha', 'Frescos Mercadona', 'Frescos Dia', 'Frescos Carrefour', 'Frescos Todos')


media_m_drogueria <- aggregate(as.numeric(mercadona[which(mercadona$final_cat == 'Drogueria/Perfumeria'), 'price']), list(mercadona[which(mercadona$final_cat == 'Drogueria/Perfumeria'), 'insert_date']), FUN=mean)
media_d_drogueria <- aggregate(as.numeric(dia[which(dia$final_cat == 'Drogueria/Perfumeria'), 'price']), list(dia[which(dia$final_cat == 'Drogueria/Perfumeria'), 'insert_date']), FUN=mean)
media_c_drogueria <- aggregate(as.numeric(carrefour[which(carrefour$final_cat == 'Drogueria/Perfumeria'), 'price']), list(carrefour[which(carrefour$final_cat == 'Drogueria/Perfumeria'), 'insert_date']), FUN=mean)
media_t_drogueria <- aggregate(as.numeric(data[which(data$final_cat == 'Drogueria/Perfumeria'), 'price']), list(data[which(data$final_cat == 'Drogueria/Perfumeria'), 'insert_date']), FUN=mean)

media_drogueria <- media_m_drogueria %>% left_join(media_d_drogueria, by='Group.1') %>% left_join(media_c_drogueria, by='Group.1') %>% left_join(media_t_drogueria, by='Group.1')
colnames(media_drogueria) <- c('Fecha', 'Drogueria Mercadona', 'Drogueria Dia', 'Drogueria Carrefour', 'Drogueria Todos')

media_m_bebidas <- aggregate(as.numeric(mercadona[which(mercadona$final_cat == 'Bebidas'), 'price']), list(mercadona[which(mercadona$final_cat == 'Bebidas'), 'insert_date']), FUN=mean)
media_d_bebidas <- aggregate(as.numeric(dia[which(dia$final_cat == 'Bebidas'), 'price']), list(dia[which(dia$final_cat == 'Bebidas'), 'insert_date']), FUN=mean)
media_c_bebidas <- aggregate(as.numeric(carrefour[which(carrefour$final_cat == 'Bebidas'), 'price']), list(carrefour[which(carrefour$final_cat == 'Bebidas'), 'insert_date']), FUN=mean)
media_t_bebidas <- aggregate(as.numeric(data[which(data$final_cat == 'Bebidas'), 'price']), list(data[which(data$final_cat == 'Bebidas'), 'insert_date']), FUN=mean)

media_bebidas <- media_m_bebidas %>% left_join(media_d_bebidas, by='Group.1') %>% left_join(media_c_bebidas, by='Group.1') %>% left_join(media_t_drogueria, by='Group.1')
colnames(media_bebidas) <- c('Fecha', 'Bebidas Mercadona', 'Bebidas Dia', 'Bebidas Carrefour', 'Bebidas Todos')


media_m_congelados <- aggregate(as.numeric(mercadona[which(mercadona$final_cat == 'Congelados_y_Conservas'), 'price']), list(mercadona[which(mercadona$final_cat == 'Congelados_y_Conservas'), 'insert_date']), FUN=mean)
media_d_congelados <- aggregate(as.numeric(dia[which(dia$final_cat == 'Congelados_y_Conservas'), 'price']), list(dia[which(dia$final_cat == 'Congelados_y_Conservas'), 'insert_date']), FUN=mean)
media_c_congelados <- aggregate(as.numeric(carrefour[which(carrefour$final_cat == 'Congelados_y_Conservas'), 'price']), list(carrefour[which(carrefour$final_cat == 'Congelados_y_Conservas'), 'insert_date']), FUN=mean)
media_t_congelados <- aggregate(as.numeric(data[which(data$final_cat == 'Congelados_y_Conservas'), 'price']), list(data[which(data$final_cat == 'Congelados_y_Conservas'), 'insert_date']), FUN=mean)

media_congelados <- media_m_congelados %>% left_join(media_d_congelados, by='Group.1') %>% left_join(media_c_congelados, by='Group.1') %>% left_join(media_t_congelados, by='Group.1')
colnames(media_congelados) <- c('Fecha', 'Congelados Mercadona', 'Congelados Dia', 'Congelados Carrefour', 'Congelados Todos')

media_m_procesados <- aggregate(as.numeric(mercadona[which(mercadona$final_cat == 'Procesados'), 'price']), list(mercadona[which(mercadona$final_cat == 'Procesados'), 'insert_date']), FUN=mean)
media_d_procesados <- aggregate(as.numeric(dia[which(dia$final_cat == 'Procesados'), 'price']), list(dia[which(dia$final_cat == 'Procesados'), 'insert_date']), FUN=mean)
media_c_procesados <- aggregate(as.numeric(carrefour[which(carrefour$final_cat == 'Procesados'), 'price']), list(carrefour[which(carrefour$final_cat == 'Procesados'), 'insert_date']), FUN=mean)
media_t_procesados <- aggregate(as.numeric(data[which(data$final_cat == 'Procesados'), 'price']), list(data[which(data$final_cat == 'Procesados'), 'insert_date']), FUN=mean)

media_procesados <- media_m_procesados %>% left_join(media_d_procesados, by='Group.1') %>% left_join(media_c_procesados, by='Group.1') %>% left_join(media_t_procesados, by='Group.1')
colnames(media_procesados) <- c('Fecha', 'Procesados Mercadona', 'Procesados Dia', 'Procesados Carrefour' , 'Procesados Todos')




luz <- read.csv("evolucion_diaria_del_precio_de_la_luz_en_el_mercado_mayorista_español_en_2021_y_2022.csv", sep =';')
colnames(luz) <- c('temp', 'KWh', 'dummy')
luz$Fecha <-paste( substr(luz$temp, 1, 5), str_sub(luz$temp, -2, -1), str_sub(luz$temp, -6, -4) , ' 00:00:00', sep = '')
luz <- luz[, c('Fecha', 'KWh')]
luz


luz2 <- read.csv("evolucion_diaria_del_precio_de_la_luz_en_el_mercado_mayorista_español.csv", sep=';')
colnames(luz2) <- c('temp', 'KWh')
luz2$Fecha <-paste( substr(luz2$temp, 1, 5), str_sub(luz2$temp, -2, -1), str_sub(luz2$temp, -6, -4) , ' 00:00:00', sep = '')
luz2 <- luz2[, c('Fecha', 'KWh')]
luz2

luz_final <- union(luz, luz2)
luz_final <-luz_final[!(duplicated(luz_final) ), ]
luz_final$KWh <- sub(',', '.', luz_final$KWh)






final_dataset <- media_total %>% left_join(media_frescos, by='Fecha') %>% left_join(media_drogueria, by='Fecha') %>%
  left_join(media_bebidas, by='Fecha') %>% left_join(media_congelados, by='Fecha') %>% left_join(media_procesados, by='Fecha') %>%
  left_join(luz_final, by='Fecha')
  
final_dataset <- na.omit(final_dataset)

write.csv(final_dataset, file = 'prac2_dataset_amartinespi.csv')
