Ejercicios3
================
Grupo 2

# Ejercicios Parte 3

## 15. Manipule los dataframe según se solicite

Se tiene el conjuntos de datos de precipitación diaria (período 1980 -
2013) de ciertas estaciones meteorológicas (**raingaugeDataset.csv**),
donde cada una de estas están asociadas a un código único (**p.e.
qc00000208**). Asimismo, se tiene una lista con los nombres, códigos,
coordenadas y elevación de cada una de las estaciones
(**listRaingauge.csv**). A cada grupo le corresponde la siguiente
estación:

-   **Grupo 01**: MALLARES
-   **Grupo 02**: TABACONAS
-   **Grupo 03**: PUERTO PIZARRO
-   **Grupo 04**: MORROPON
-   **Grupo 05**: SAN MIGUEL
-   **Grupo 06**: CHULUCANAS
-   **Grupo 07**: LAMBAYEQUE
-   **Grupo 08**: EL LIMON
-   **Grupo 09**: EL SALTO
-   **Grupo 10**: CHUSIS

De lo descrito anteriormente, se solicita:

``` r
#Ubicamos en codigo de la ESTACION
codigo<-codigos[codigos$NOM_EST=="TABACONAS",]
codigo_n<-codigo[,"CODIGO"]
```

``` r
#Código de estacion de trabajo
codigo_n
```

    ## [1] "qc00000240"

``` r
#Generamos vectores por el codigo de la estacion y le damos formato a la fecha
estacion<-estaciones[,codigo_n]
fecha<-estaciones[,"date"]
fecha2<-lubridate::dmy(fecha)
#Creamo un data frame con ambos vectores
  df1<-data.frame(fecha2,estacion)
```

``` r
#Seleccionamosla estacion
tibble(df1)
```

    ## # A tibble: 12,419 x 2
    ##    fecha2     estacion
    ##    <date>        <dbl>
    ##  1 1980-01-01      8.6
    ##  2 1980-01-02      4  
    ##  3 1980-01-03      8.5
    ##  4 1980-01-04      3  
    ##  5 1980-01-05      7  
    ##  6 1980-01-06      1  
    ##  7 1980-01-07      0  
    ##  8 1980-01-08      2.8
    ##  9 1980-01-09      0  
    ## 10 1980-01-10      0  
    ## # ... with 12,409 more rows

``` r
#Verificamos los datos
seq(as.Date("1980-01-01"),as.Date("2013-12-31"), by = "day") %>% 
  length()
```

    ## [1] 12419

------------------------------------------------------------------------

1.  Determine la cantidad de **missing values** de la serie de tiempo a
    paso diario.

``` r
missvaluedaily<-df1 %>% 
  group_by(fecha2=str_sub(fecha2,1,10)) %>% 
  mutate(missval=sum(is.na(estacion))) # Cantidad "NA" en cada dia
summarise_all(df1, funs(sum(is.na(.)))) #Total de "NA" en la estacion por dia 
```

    ##   fecha2 estacion
    ## 1      0      308

2.  Calcule la serie de tiempo de precipitación **acumulada mensual**
    (si el \# de días con missing values, en un mes, supera el 10%, la
    precipitación acumulada mensual será considerado como un **`NA`**).

``` r
ppm<-
  df1 %>% 
  group_by(fecha2=str_sub(fecha2,1,7)) %>% #porcentaje de DATOS faltantes
  mutate(
    missval=sum(is.na(estacion)) * 100 / n()
  ) %>% #Precipitacion acumulada mensual
  summarize(
    estacion=sum(estacion,na.rm=T),
    missval=unique(missval)
  ) %>% 
  mutate(
    estacion=ifelse(missval>=10,NA,estacion),#NA, cuando superan o son mayor a 10%
    fecha2 = as.Date(sprintf("%1$s-01",fecha2)),
    month=str_sub(fecha2,6,7)
  )
ppm
```

    ## # A tibble: 408 x 4
    ##    fecha2     estacion missval month
    ##    <date>        <dbl>   <dbl> <chr>
    ##  1 1980-01-01     87.9       0 01   
    ##  2 1980-02-01     48.3       0 02   
    ##  3 1980-03-01    128.        0 03   
    ##  4 1980-04-01     44.3       0 04   
    ##  5 1980-05-01     40         0 05   
    ##  6 1980-06-01     93.5       0 06   
    ##  7 1980-07-01     33.6       0 07   
    ##  8 1980-08-01     13         0 08   
    ##  9 1980-09-01     13.2       0 09   
    ## 10 1980-10-01    130.        0 10   
    ## # ... with 398 more rows

3.  Determine la cantidad de **missing values** de la serie de tiempo a
    paso mensual.

``` r
sum(summarise_all(ppm, funs(sum(is.na(.)))))
```

    ## [1] 11

4.  Cree una función que calcule, a partir de los datos de preicpitación
    mensual, la **climatología (Ene-Dic)** para el **período
    1980-2010**.

``` r
ppM10<- tibble(df1) %>% 
  select(fecha2, estacion) %>%   #Seleccionamos
  mutate(date = as.Date(fecha2, format = "%d/%m/%Y")) %>% 
  rename(Fecha = date, pp = estacion) %>%   #Renombramos
  filter(Fecha>="1980-01-01",Fecha<="2010-12-31") %>%   #SOLO de 1980 a 2010
  group_by(Fecha = str_sub(Fecha,1,7)) %>%   #Agrupamos  por Año y Mes
  mutate(
    missVal = sum(is.na(pp))*100/n()    #porcentaje de DATOS faltantes
  ) %>% 
  summarise(
    pp = sum(pp, na.rm = T),
    missVal = unique(missVal)          #Precipitacion acumulada
  ) %>% 
  mutate(
    pp = ifelse(missVal>=10,NA,pp),    #NA, cuando superan o son mayor a 10%
    Fecha = as.Date(sprintf("%1$s-01", Fecha)),
    Mes = str_sub(Fecha, 6, 7)
  )
ggplot(ppM10, aes(Fecha,pp))+
  geom_line()
```

![](Ejercicios_3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> e)
Plotear (boxplot) la variabilidad de los valores mensuales (Ene-Dic)
para el período 1980-2013.

``` r
ggplot(ppm, aes(month,estacion))+
  geom_boxplot()+
  theme_bw()+
  scale_x_discrete(
    labels=month.abb
  )
```

![](Ejercicios_3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
