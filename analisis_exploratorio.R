library(ggplot2)
library(forcats)


##SUMMARY
summary(df_datosfinal$precio)
summary(df_plazas$precio)
summary(df_madeirense$precio)
summary(df_gama$precio)
summary(df_producto$precio)


##HISTOGRAMAS
hist(main = 'Precios en DataPrecio (En Dólares $)',
     df_datosfinal$precio,
     col = 'purple',
     ylab = 'Frecuencia de productos',
     xlab = 'Intervalos de precios')

hist(main = 'Canasta Básica: Plazas (En Dólares $)',
     df_plazas$precio,
     breaks = seq(0, 20, by = 2),
     col = 'green',
     ylab = 'Frecuencia de productos',
     xlab = 'Intervalos de precios')

hist(main = 'Canasta Básica: Central Madeirense (En Dólares $)',
     df_madeirense$precio,
     breaks = seq(0, 20, by = 2),
     col = '#EA4E3C',
     ylab = 'Frecuencia de productos',
     xlab = 'Intervalos de precios')

hist(main = 'Canasta Básica: Gama (En Dólares $)',
     df_gama$precio,
     breaks = seq(0, 20, by = 2),
     col = 'orange',
     ylab = 'Frecuencia de productos',
     xlab = 'Intervalos de precios')

hist(main = 'Precios del Filtrado de la Canasta Básica, Precios Promedios de Gama, Plaza, CM. (En Dólares $)',
     df_producto$precio,
     breaks = seq(0, 20, by = 2),
     col = 'blue',
     ylab = 'Frecuencia de productos',
     xlab = 'Intervalos de precios')


##DENSIDAD
plot(density(df_datosfinal$precio), 
     col = 'purple', 
     main = 'Precios en DataPrecio (En Dólares $)') 

plot(density(df_plazas$precio), 
     col = 'green', 
     main = 'Canasta Básica: Plazas (En Dólares $)') 

plot(density(df_madeirense$precio), 
     col = '#EA4E3C', 
     main = 'Canasta Básica: Central Madeirense (En Dólares $)') 

plot(density(df_gama$precio), 
     col = 'orange', 
     main = 'Canasta Básica: Gama (En Dólares $)')

plot(density(df_producto$precio), 
     col = 'blue', 
     main = 'Precios del Filtrado de la Canasta Básica, Precios Promedios de Gama, Plaza, CM. (En Dólares $)') 


##CANTIDAD DE PRODUCTOS POR CATEGORÍA
#CANASTA BÁSICA
conteo_subcategorias <- df_datosfinal %>%
  group_by(subcategoria) %>% 
  filter(subcategoria %in% c('Aceites De Maíz',
                             'Leches En Polvo',
                             'Arroces',
                             'Panes Empacados',
                             'Huevos De Gallina',
                             'Carnes De Pollo',
                             'Carnes De Res',
                             'Frutas',
                             'Quesos Blancos Pasteurizados',
                             'Quesos Blancos Duros',
                             'Quesos Blancos Suaves',
                             'Ketchups Y Salsas De Tomate',
                             'Tuberculos',
                             'Hortalizas',
                             'Aguas Minerales',
                             'Cervezas',
                             'Cigarrillos Y Tabacos',
                             'Harinas De Maiz',
                             'Harinas De Trigo',
                             'Papeles De Baño',
                             'Jabones En Barra',
                             'Jabones Para Lavar',
                             'Champus',
                             'Cremas Dentales',
                             'Desodorantes Roll-On',
                             'Cloros',
                             'Limpiadores Y Desinfectantes',
                             'Pastas',
                             'Leches Liquidas',
                             'Leches En Polvo',
                             'Cremas Dentales',
                             'Sardinas',
                             'Sales Comestibles',
                             'Avena En Hojuelas',
                             'Margarinas',
                             'Mantequillas',
                             'Pastas Cortas',
                             'Pastas Largas',
                             'Jugos Azucarados',
                             'Leguminosas')) %>% 
  reframe(cantidad = n())
dim(conteo_subcategorias)

ggplot(conteo_subcategorias, aes(x = reorder(subcategoria, -cantidad), 
                                  y = cantidad)) +
  geom_bar(stat = "identity", fill = "darkred") + 
  labs(title = "Cantidad de Productos de la Canasta Básica", 
       x = "Productos",
       y = "Cantidades") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45,      
                               hjust = 1,      
                               vjust = 1,        
                               family = "serif", 
                               size = 12))

#CANASTA BÁSICA FILTRADA
conteo_canasta_basica <- df_ipc %>%
  group_by(categoria) %>% 
  reframe(cantidad = n())
dim(conteo_canasta_basica)

ggplot(conteo_canasta_basica, aes(x = reorder(categoria, -cantidad), 
                                 y = cantidad)) +
  geom_bar(stat = "identity", fill = "purple") + 
  labs(title = "Cantidad de Productos de la Canasta Básica Filtrada", 
       x = "Productos",
       y = "Cantidades") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45,      
                               hjust = 1,      
                               vjust = 1,        
                               family = "serif", 
                               size = 12))

#CATEGORÍAS
conteo_categorias <- df_datosfinal %>%
  group_by(categoria) %>% 
  reframe(cantidad = n())
dim(conteo_categorias)

ggplot(conteo_categorias, aes(x = reorder(categoria, -cantidad), 
                              y = cantidad)) + 
  geom_bar(stat = "identity", fill = "darkblue") + 
  labs(title = "Cantidad de Productos por Categoría",
       x = "Categoría", 
       y = "Cantidades") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 65,
                               hjust = 1,
                               vjust = 1,
                               family = "serif",
                               size = 12))
