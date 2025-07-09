library(httr2)
library(jsonlite)
library(rvest)
library(tidyverse)

# Scrapper tipo de cambio
valor_dolar <- numeric()
j=1

while(length(valor_dolar)==0 & j<=5){
  print(j)
  valor_dolar <- read_html('https://www.bcv.org.ve/')%>%
    html_nodes('#dolar strong')%>%
    html_text()%>%
    str_replace_all(',','.')%>%
    as.numeric()%>%
    round(2)
}

valor_dolar

#SCRAPPER aportado por el profesor José Avendaño

# 1. Definir la URL base, el path/query
base_url <- "https://dataprecio-com-backend.onrender.com/api/search?"
numero_pagina <- '&page=0'
url_total_query <- paste0(base_url, vector_categorias, numero_pagina)

# 2. Obtener cdad paginas

info_paginas_visitar <- request(url_total_query)%>%
  req_perform()%>%
  resp_body_string()%>%
  fromJSON()%>%
  .$totalPages%>%
  as.numeric()

info_paginas_visitar
# 3. Descargar info de páginas 

df_datos <- data.frame() # df con datos descargados

for (i in 0:(info_paginas_visitar)){
  print(paste('descarga', (i+1)))
  
  base_url <- "https://dataprecio-com-backend.onrender.com/api/search?"
  numero_pagina <- '&page='
  full_url <- paste0(base_url, vector_categorias, numero_pagina,i)
  
  datos_obtenidos <- request(full_url)%>%
    req_perform()%>%
    resp_body_string()%>%
    fromJSON()%>%
    .$hits%>%
    unnest(tiendas)%>%
    mutate(fecha= Sys.Date(),
           valor_dolar=valor_dolar,
           precio_bs= round(precio*valor_dolar,2))
  
  if(nrow(datos_obtenidos)>0){
    df_datos <- bind_rows(df_datos,datos_obtenidos)%>%
      distinct()
  }
}

nrow(df_datos)


saveRDS(df_datos,paste0('C:/Users/igngo/Documents/_proyecto_final/data_precio/data/',
                        Sys.Date(),'.rds'))


#############################FILTRADO###########################################



df_datosfinal <- df_datos %>%
  select(categoria, marca, nombre, productID, subcategoria, precio, tienda, fecha, valor_dolar, precio_bs)%>%
  filter(tienda %in% c('Central Madeirense','Gama','Plazas'))

#filtrado por por productID en concreto, coincidentes en todos las tiendas:
  
#por alguna razon solo filtra con df_datos y no df_indice, tambien, 
#por nombre de productos no crea el nuevo dataframe, asi que utilice el ID de los productos (tambien mas sencillo)
df_producto <- df_datos %>%
  filter(productID %in% c('0177943kejl','0688778optx','2641618farn',
                          '5048664dees','1024483riau','8472293qffh','4924587jfyq',
                          '1740352zdgy','3766471ywnj','0911518zdav', '6686895ahgv','8760443ykjc',
                          '8542675nksj','9772063bimy','1766177glna','9749784sngv','0239406dlxq',
                          '4900792iskg','0847963tefn','0784532cajo', '8367304bxdp',
                          '6061140bgks','9989685wiow','2047677hulx','6088928jhpv',
                          '8639719lsdo','3931084tute','8700182dssr','4306829tzkm', '8388768ahuk','5326580nfkz',
                          '3394414nvey','0441922bxhp','9685540yucb','5391468yxyb','2004318ktog','1255154xcbq',
                          '5383221flfl','7195430vlnt','6605107fwwz','3733323rpeq','8105706gvye','5899485kuas',
                          '9203707wfno','2570585fktz','9888007ennt','0541816ugnt','1451804pngj','2240343zudb',
                          '3733839dnep','6889212ognt','4723042vpcz','9733477wmxj'
  ))%>%
  select(categoria, marca, nombre, productID, subcategoria, precio, tienda, fecha, valor_dolar, precio_bs)%>%
  filter(tienda %in% c('Central Madeirense','Gama','Plazas'))

####LOS SIGUIENTES CODIGOS FUERON LOS QUE SE REALIZARON PARA DETERMINAR CUALES ERAN LOS SUPERMERCADOS QUE
#TENIAN MAYOR NUMERO DE COINCIDENCIAS EN LOS PRODUCTOS DE NUESTRA CANASTA BASICA: (El analisis fue previo al filtrado: 
#filter(tienda %in% c('Central Madeirense','Gama','Plazas'))) del dataframe df_producto
####para verificar que tiendas tienen los mismos productos de la canasta:


#para filtrar tiendas que venden lo mismo:
df_tiendas_mismos_productos1 <- df_producto %>%
  group_by(tienda) %>%
  reframe(mismo_producto=n_distinct(productID))%>%
  filter(mismo_producto>20)
#
tiendas_mismos_productos <- df_producto %>%
  filter(tienda %in% df_tiendas_mismos_productos1$tienda)

productos_no_repetidos <- df_producto %>%
  group_by(productID) %>%
  reframe(mismo_producto=n_distinct(tienda))%>%
  filter(mismo_producto == 2)

#Los supermercados que tenian mayor numero de coindencias (exactas), fueron el Central Madeirense, Plazas y Gama.


#PARA UNIR DATAFRAMES DE DISTINTAS FECHAS PARA EL ANALISIS DE LA EVOLUCION DEL IPC 
#EN EL ARCHIVO .RDATA, SE ENCUENTRAN LOS DATAFRAMES: df_0, df_01, df_1, df_2, df_3, df_4


df_ipc <- bind_rows(df_datos, df_0, df_01, df_1, df_2, df_3, df_4)  %>%
  select(categoria, marca, nombre, productID, subcategoria, precio, tienda, fecha, valor_dolar, precio_bs)%>%
  filter(productID %in% c('0177943kejl','0688778optx','2641618farn',
                          '5048664dees','1024483riau','8472293qffh','4924587jfyq',
                          '1740352zdgy','3766471ywnj','0911518zdav', '6686895ahgv','8760443ykjc',
                          '8542675nksj','9772063bimy','1766177glna','9749784sngv','0239406dlxq',
                          '4900792iskg','0847963tefn','0784532cajo', '8367304bxdp',
                          '6061140bgks','9989685wiow','2047677hulx','6088928jhpv',
                          '8639719lsdo','3931084tute','8700182dssr','4306829tzkm', '8388768ahuk','5326580nfkz',
                          '3394414nvey','0441922bxhp','9685540yucb','5391468yxyb','2004318ktog','1255154xcbq',
                          '5383221flfl','7195430vlnt','6605107fwwz','3733323rpeq','8105706gvye','5899485kuas',
                          '9203707wfno','2570585fktz','9888007ennt','0541816ugnt','1451804pngj','2240343zudb',
                          '3733839dnep','6889212ognt','4723042vpcz','9733477wmxj'
                          )) %>%
  select(categoria, marca, nombre, productID, subcategoria, precio, tienda, fecha, valor_dolar, precio_bs)%>%
  filter(tienda %in% c('Central Madeirense','Gama','Plazas'))


#####CALCULO IPC SUPERMERCADOS del dia de descarga(hoy - df_datos es el dataframe con los datos de hoy):

df_madeirense <- df_producto %>%
  filter(tienda %in% 'Central Madeirense')

ipc_madeirense <- sum(df_madeirense$precio)

df_plazas <- df_producto %>%
  filter(tienda %in% 'Plazas')

ipc_plazas <- sum(df_plazas$precio)

df_gama <- df_producto %>%
  filter(tienda %in% 'Gama')

ipc_gama <- sum(df_gama$precio)

#IPC EN BOLIVARES: 

df_madeirense_bs <- sum(df_madeirense$precio_bs)
df_plazas_bs <- sum(df_plazas$precio_bs)
df_gama_bs <- sum(df_gama$precio_bs)

#### CALCULO IPC POR DIA PARA EVALUAR LAS VARIACIONES EN LOS PRECIOS, 
#ESTE DATAFRAME TIENE EL PRECIO DE LA CANASTA POR DIA:
ipc_datapanel <- df_ipc %>%
  group_by(fecha, tienda)%>%
  reframe(ipc_dia_tienda= sum(precio))





  




