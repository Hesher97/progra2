Tarea
================
Grupo 2

# EJERCICIOS PARTE 1

Cargar las variables almacenadas en el siguiente archivo
[Rdata](https://goo.gl/uDzU8v)

### 1. Calcula los valores numéricos aproximados de:

1.  $\\frac{0.3 \\cdot 0.15}{0.3 \\cdot 0.15 + 0.2 \\cdot 0.8 + 0.5 \\cdot 0.12}$

``` r
a<- (0.3*0.5)/(0.3*0.15+0.2*0.8+0.5*0.12)
a 
```

    ## [1] 0.5660377

2.  $\\frac{5^6}{6!} e^{-5}$

``` r
b<-(exp(-5)*5**6)/factorial(6)
b
```

    ## [1] 0.1462228

3.  $\\begin{pmatrix} 20 \\\\ 7 \\end{pmatrix} 0.4^7 0.6^{13}$

``` r
c<-choose(20,7)*(0.4**7)*(0.6**13)
c
```

    ## [1] 0.1658823

### 2. Realizar la siguiente suma

1.  1 + 2 + 3 + ... + 1000

``` r
sum(1:1000)
```

    ## [1] 500500

2.  1 + 2 + 4 + 8 + 16 + ... + 1024

``` r
sum(seq(2,1024,2))       #Creamos una secuencia cada "2" y sumamos
```

    ## [1] 262656

### 3. El vector `grupo` representa el grupo al que pertenece una serie de alumnos

1.  ¿Cuántos elementos tiene?

``` r
tibble(grupo) %>%            #Cantidad de datos
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1   192

2.  ¿En qué posiciones del vector está la letra “A”?

``` r
tibble(grupo) %>% 
  mutate(orden = 1:n(), grupo = grupo) %>% #Agregamos una columna de posicion
  filter(grupo == "A")                     #Filtramos solo "A"
```

    ## # A tibble: 14 x 2
    ##    grupo orden
    ##    <chr> <int>
    ##  1 A         2
    ##  2 A         8
    ##  3 A        17
    ##  4 A        21
    ##  5 A        28
    ##  6 A        84
    ##  7 A       101
    ##  8 A       108
    ##  9 A       111
    ## 10 A       115
    ## 11 A       123
    ## 12 A       136
    ## 13 A       190
    ## 14 A       192

### 4. El vector `nota` representa la nota de un examen de los alumnos que están en los grupos del vector `grupo`.

1.  ¿Cuanto suman todas las notas?

``` r
sum(nota)                            #Sumamos las notas
```

    ## [1] 962

2.  ¿Cual es la media aritmética de todas las notas?

``` r
median(nota)                         #Media de las notas
```

    ## [1] 5

3.  ¿En qué posiciones están las notas mayores de *7.0*?

``` r
tibble(nota) %>% 
  mutate(orden = 1:n() , nota =nota) %>% #Agregamos una columna de posicion
  filter(nota>7)                         #Notas mayores de "7"
```

    ## # A tibble: 4 x 2
    ##    nota orden
    ##   <dbl> <int>
    ## 1   7.2    81
    ## 2   7.4   103
    ## 3   7.7   120
    ## 4   7.5   151

4.  Visualiza las notas ordenadas de mayor a menor

``` r
tibble(grupo,nota) %>%                   #Unimos "Grupo" y "Nota"
  arrange(nota) %>%                     #Ordenamos de mmenor a mayor NOTAS
  mutate(Orden = 1:n())                 #Agregamos posicion
```

    ## # A tibble: 192 x 3
    ##    grupo  nota Orden
    ##    <chr> <dbl> <int>
    ##  1 B       1.7     1
    ##  2 D       2.5     2
    ##  3 D       2.6     3
    ##  4 E       2.7     4
    ##  5 E       2.9     5
    ##  6 D       2.9     6
    ##  7 A       2.9     7
    ##  8 E       3       8
    ##  9 E       3.1     9
    ## 10 C       3.2    10
    ## # ... with 182 more rows

5.  ¿En qué posición está la nota máxima?

``` r
tibble(nota) %>% 
  mutate(orden = 1:n() , nota =nota) %>%   #Agregamos posicion
  slice_max(nota)                         #Seleccionamos la maxima "Nota"
```

    ## # A tibble: 1 x 2
    ##    nota orden
    ##   <dbl> <int>
    ## 1   7.7   120

### 5. A partir de los vectores `grupo` y `nota` definidos.

1.  Suma las notas de los 10 primeros alumnos del vector

``` r
sum(nota[1:10])                             #Sumamos los primeros 10
```

    ## [1] 51.8

2.  ¿Cuántos alumnos hay del grupo *C*?

``` r
tibble(grupo) %>% 
  filter(grupo == "C") %>%                 #Filtramos solo "C"
  count()                                  #Contamos
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    39

3.  ¿Cuántos alumnos han aprobado?

``` r
tibble(nota) %>% 
  filter(nota >= 5.1) %>%                 #Filtramos solo "C"
  count()                                  #Contamos
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    93

4.  ¿Cuántos alumnos del grupo *B* han aprobado?

``` r
tibble(grupo, nota) %>% 
  filter(nota >= 5.1 ,grupo == "B") %>%   #Filtramos Aprobado y solo "B"
  count() 
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    10

5.  ¿Qué porcentaje de alumnos del grupo *C* han aprobado?

``` r
(tibble(grupo, nota) %>%
  filter(nota >= 5.1 ,grupo == "C") %>%   #Filtramos Aprobado y solo "B"
  count()/tibble(grupo) %>%
  filter(grupo == "C") %>%   #Filtramos solo "C"
  count())*100
```

    ##          n
    ## 1 56.41026

6.  ¿De qué grupos son la máxima y mínima notas de toda la muestra?

``` r
tibble(grupo,nota) %>%    
  slice_max(nota)      #Maxima nota
```

    ## # A tibble: 1 x 2
    ##   grupo  nota
    ##   <chr> <dbl>
    ## 1 E       7.7

``` r
tibble(grupo,nota) %>%    
  slice_min(nota)      #Minima nota
```

    ## # A tibble: 1 x 2
    ##   grupo  nota
    ##   <chr> <dbl>
    ## 1 B       1.7

7.  Nota media de los alumnos de grupo *A* y *B*, juntos, considerando
    sólo a los que han aprobado.

``` r
tibble(grupo, nota) %>% 
  filter(grupo == "A" & nota > 5|         #Grupo A y Aprobado
         grupo == "B" & nota > 5) %>%     #Grupo B y Aprobado
  summarise(mean(nota))                   #Media
```

    ## # A tibble: 1 x 1
    ##   `mean(nota)`
    ##          <dbl>
    ## 1         5.94

### 6. Calcula el percentil 66 de las notas de todos los alumnos, y también de los alumnos del grupo C.

``` r
quantile(nota, c(0.66))    #Percentil 66
```

    ## 66% 
    ## 5.5

``` r
tibble(grupo, nota) %>% 
  filter(grupo == "C") %>%       #Filtramos solo C
  summarise(quantile(nota, c(0.66)))  #Percentil 66
```

    ## # A tibble: 1 x 1
    ##   `quantile(nota, c(0.66))`
    ##                       <dbl>
    ## 1                      5.81

### 7. Un alumno tiene una nota de 4.9. ¿Qué porcentaje, del total de alumnos, tiene una nota menor o igual que la suya? ¿Y qué porcentaje tiene una nota mayor o igual que la suya?

``` r
(tibble(nota) %>%
  filter(nota <= 4.9) %>%   #Filtramos Nota menor a 4.9
  count()/tibble(grupo) %>%
  count())*100   
```

    ##        n
    ## 1 46.875

``` r
(tibble(nota) %>%
  filter(nota >= 4.9) %>%   #Filtramos Nota mayor a 4.9
  count()/tibble(grupo) %>%
  count())*100  
```

    ##       n
    ## 1 56.25

### 8. Realiza el gráfico de diagramas de caja de las notas de cada grupo, para poder comparar el nivel de cada uno de ellos.

``` r
df<- tibble(grupo,nota)
boxplot(`nota` ~ `grupo`,df, col = palette(rainbow(4)))
```

![](ejercicio_1_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

### 9. Si la variable `conc` recoge la concentración de plomo (en ppm) en el aire de cierta zona durante un día completo.

1.  ¿Cuál ha sido la concentración máxima?

``` r
tibble(conc) %>% 
  slice_max(conc)
```

    ## # A tibble: 1 x 1
    ##    conc
    ##   <dbl>
    ## 1  47.3

2.  ¿En cuántos de los muestreos se ha superado la concentración de 40.0
    ppm?

``` r
tibble(conc) %>% 
  filter(conc > 40) %>% 
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    61

3.  ¿Cuál ha sido la concentración media del día?

``` r
tibble(conc) %>% 
  summarise(median(conc))
```

    ## # A tibble: 1 x 1
    ##   `median(conc)`
    ##            <dbl>
    ## 1           24.4

4.  ¿Cuáles fueron las 10 mediciones más bajas del día?

``` r
tibble(conc) %>%                   #Unimos "Grupo" y "Nota"
  arrange(conc)
```

    ## # A tibble: 288 x 1
    ##     conc
    ##    <dbl>
    ##  1  0.93
    ##  2  1.07
    ##  3  1.77
    ##  4  2.03
    ##  5  2.58
    ##  6  2.73
    ##  7  2.75
    ##  8  2.88
    ##  9  2.88
    ## 10  2.91
    ## # ... with 278 more rows

5.  Si la primera medida fue a las 00:00. ¿A qué hora del día se alcanzó
    la concentración máxima?

``` r
Hora<- seq(0,24-1/12,1/12)
length(Hora)
```

    ## [1] 288

``` r
tibble(Hora,conc) %>%
  slice_max(conc)
```

    ## # A tibble: 1 x 2
    ##    Hora  conc
    ##   <dbl> <dbl>
    ## 1  11.8  47.3

``` r
#seq(as.double())
#seq(as.POSIXct("2020-01-01 00:00"),as.POSIXct("2020-01-01 23:00"),by=" min")
#seq(as.POSIXct("2020-01-01 00:00"),as.POSIXct("2020-01-01 24:00"),by = "1/12 min")

#seq(as.POSIXct("2020-01-01 00:00"),as.POSIXct("2020-01-01 24:00"),by="1/12 min")
```

``` r
length(conc)
```

    ## [1] 288

``` r
tiempo_recojo <- 23/length(conc) #tiempo de recojo de cada concentracion
which(conc == max(conc)) #ocupacion dd la maxima concentracion
```

    ## [1] 142

``` r
produc <- tiempo_recojo * which(conc == max(conc))
produc
```

    ## [1] 11.34028
