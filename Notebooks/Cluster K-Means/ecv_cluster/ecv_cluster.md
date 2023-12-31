# Introducción

## dataset

En este cuaderno vamos a analizar el dataset llamado
[*ecv_cluster.xlsx*](https://github.com/davidperezros/ine_sg_difusion_explica_datasets/blob/93774ea45559a2fec3bf0d6b9b6b3cd1066b730b/Datasets/ecv_cluster.xlsx).
Este contiene datos por Comunidades Autónomas sobre la tasa de riesgo de
pobreza, la carencia material o la situación laboral que encontramos
dentro de la Encuesta de Condiciones de Vida (ECV). Datos
correspondientes al año 2021. Las variables de interés son las
siguientes:

-   **ccaa**: Comunidades Autónomas
-   **taspobex**: Tasa de riesgo de pobreza o exclusión social
    (indicador AROPE).
-   **taspob**: Tasa en riesgo de pobreza (renta año anterior a la
    entrevista).
-   **tascar**: Tasa con carencia material severa.
-   **tasvivtrab**: Tasa de hogares viviendo con baja intensidad en el
    trabajo (de 0 a 59 años).

El objetivo de este estudio será aplicar un Análisis **Cluster** para
hacer grupos de comunidades autónomas en función de las variables
definidas arriba. Concretamente usaremos un cluster K-Means

``` r
# Librerias
library(readxl) # Para leer los excels
library(dplyr) # Para tratamiento de dataframes
library(ggplot2) # Nice plots
library(factoextra) # fviz_cluster function
```

Cargamos entonces el conjunto de datos:

``` r
datos <- read_excel("/Users/davpero/ine_sg_difusion_explica_datasets/Datasets/ecv_cluster.xlsx", sheet = "Datos")
```

## Descripción del trabajo a realizar

**(Esto irá en la web de explica)** Se pretende hacer un Análisis
Cluster empleando el procedimiento Cluster K-Means de las **CCAA** en
función a las variables **taspobex** y **tascar** .

-   Hacer un análisis exploratorio.Ver si hay NA’s y si es necesario
    escalar los datos.
-   Variables sobre las que se buscan cluster (*taspobex*, *tascar*).
-   Estandarizar datos y probar cluster k-means con k=3.
-   Interpretar resultados.
-   Ver métodos Elbrow y Silhouette si hay otro número óptimo de
    clusters y en ese caso repetir el estudio.

# Análisis Exploratorio (EDA[1])

Lo primero de todo vamos a cargar las librearias necesarias para
ejecutar el resto del código del trabajo:

# Clustering: Cluster K-means

## Introducción

El **Análisis de clúster** es una técnica de aprendizaje no supervisado
que agrupa datos similares en conjuntos, llamados clústeres. El objetivo
es dividir un conjunto de datos en grupos homogéneos, donde los miembros
de cada grupo son más similares entre sí que con los miembros de otros
grupos, según algún criterio de similitud predefinido.

Concretamente, el **Cluster K-Means** define clusters de modo que se
**minimice la variación total dentro del grupo** de acuerdo con el
algoritmo Hartigan-Wong (Hartigan y Wong 1979), que define la variación
total dentro del grupo como la suma de las distancias al cuadrado de las
distancias euclidianas entre elementos y el centroide correspondiente.
Se describe a continuación.

Los pasos generales de este algoritmo son:

1.  **Especificar** el número de clusters (K) que se se desean obtener.

2.  **Seleccionar aleatoriamente k** objetos del conjunto de datos como
    centros del grupo (centroides). Asigna cada observación a su
    centroide más cercano, según la distancia euclidiana entre el objeto
    y el centroide.

3.  Para cada uno de los k grupos, **actualizar el centroide** del grupo
    calculando los nuevos valores medios de todos los puntos de datos
    del grupo. El **centoide de un grupo K-ésimo es** un vector de
    longitud p que contiene las medias de todas las variables para las
    observaciones en el grupo K-ésimo; p es el número de variables.

4.  **Minimizar iterativamente** el total dentro de la suma del
    cuadrado. Es decir, repetir los pasos 3 y 4 hasta que las
    asignaciones del clúster dejen de cambiar o se alcance el número
    máximo de iteraciones. De forma predeterminada, el software R
    utiliza 10 como valor predeterminado para el número máximo de
    iteraciones.

*Véase funciones de R stats::kmeans(x, centers, iter.max, nstart) que
realizan los pasos 2-5 automáticamente.*

# Modelo

## Formulación

**IMPORTANTE**:

-   Ver que no hay ningún **NA** en el dataset.
-   El **escalado** es un paso esencial en la fase de preprocesamiento
    de datos para los algoritmos de agrupación. Garantiza que cada
    característica contribuya por igual al proceso de decisión del
    algoritmo, lo que lleva a resultados de agrupación más precisos e
    interpretables.

``` r
ifelse(sum(is.na(data)) == 0, print("There is no NA in the dataset."), print("There is some NA in the dataset."))
```

    ## [1] "There is no NA in the dataset."

    ## [1] "There is no NA in the dataset."

Si quueremos que el código sea reproducible, es necesario fijar semilla
(función `set.seed(n)`) ya que el algoritmo *k-means* elige los
centroides iniciales aleatoriamente.

``` r
# Preparación de los datos
resultado <- datos[, c("taspobex", "tascar")]

resultado <- scale(resultado) # scaling/standardizing
rownames(resultado) <- datos$CCAA # Para que nos salgan luego los nombres
comunidades <- datos$CCAA


# K-MEANS algortihm
set.seed(785248) # reproducibilidad
k1 <- kmeans(resultado, centers = 3, nstart = 25)
k1
```

    ## K-means clustering with 3 clusters of sizes 11, 3, 5
    ## 
    ## Cluster means:
    ##     taspobex      tascar
    ## 1 -0.7548531 -0.51757600
    ## 2  1.3941669  1.99603588
    ## 3  0.8241767 -0.05895433
    ## 
    ## Clustering vector:
    ##                   Andalucía                      Aragón 
    ##                           3                           1 
    ##     Asturias, Principado de              Balears, Illes 
    ##                           1                           1 
    ##                    Canarias                   Cantabria 
    ##                           2                           1 
    ##             Castilla y León        Castilla - La Mancha 
    ##                           1                           3 
    ##                    Cataluña        Comunitat Valenciana 
    ##                           1                           3 
    ##                 Extremadura                     Galicia 
    ##                           3                           1 
    ##        Madrid, Comunidad de           Murcia, Región de 
    ##                           1                           3 
    ## Navarra, Comunidad Foral de                  País Vasco 
    ##                           1                           1 
    ##                   Rioja, La                       Ceuta 
    ##                           1                           2 
    ##                     Melilla 
    ##                           2 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 2.413697 1.571024 1.603415
    ##  (between_SS / total_SS =  84.5 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
fviz_cluster(k1, data = resultado) # plot
```

<img src="ecv_cluster_files/figure-markdown_github/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

Podemos observar que la agrupación en 3 clusters que ha hecho el
algortimo K-MEANS es bastante similar a la que obtuvimos con el cluster
jerárquico. Por un lado tenemos un clsuter de los valores que se
encuentran más a la derecha, luego otro con los que están más arriba y
otros con los más cercanos al origen. En cierto modo:

-   El **cluster rojo** representa las CCAA que presentan una tasa de
    riesgo de probeza y una tasa de personas con carencia material
    severa muy alta. Estas son Canarias, Ceuta y Melilla, lo cual cabría
    pensar que tiene sentido ya que ambas tres regiones contienen
    fuertes corrientes migratorias debido a que se encuentran en puntos
    fronterizos en el que hay mucha inmigración ilegal procedente de
    países como Marruecos.

-   En el **cluster rojo** se encuentran las CCAA que presentan una tasa
    de riesgo de probeza similares a las anteriores pero la de personas
    con carencia materil alta no es tan grande como en las citadas
    anteriormente. Estas son Andalucía, Murica y Extremadura, que
    efectivamnete es común que aparezcan en la prensa anualmente como
    regiones con más pobreza dentro de España (a excepción de Murica y
    Valencia) y sin embargo presentan una tasa de personas con carencia
    material no tan alta como las anteriores peusto que no hay tanta
    población en situació irregular que pueda derivar en una carencia
    material sustancial.

-   Por último, el **cluster verde** presenta comunidades que tuvieron
    una incidencia parecida en la primera y segunda ola.

Notar que podemos mostrar los datos originales junto al cluster al que
pertenecen.

``` r
# Mostrar Clusters
datos %>%
  mutate(cluster = k1$cluster) %>%
  select("CCAA", "taspobex", "tascar","cluster")
```

    ## # A tibble: 19 × 4
    ##    CCAA                        taspobex tascar cluster
    ##    <chr>                          <dbl>  <dbl>   <int>
    ##  1 Andalucía                       38.4   10.2       3
    ##  2 Aragón                          20.3    5.6       1
    ##  3 Asturias, Principado de         26.6    5.5       1
    ##  4 Balears, Illes                  24.5    8.5       1
    ##  5 Canarias                        38.3   13.5       2
    ##  6 Cantabria                       21.6    5.7       1
    ##  7 Castilla y León                 22.4    3.8       1
    ##  8 Castilla - La Mancha            31.4    5.1       3
    ##  9 Cataluña                        22.1    7.3       1
    ## 10 Comunitat Valenciana            30.3    7.1       3
    ## 11 Extremadura                     39.1    6.9       3
    ## 12 Galicia                         24.5    3.8       1
    ## 13 Madrid, Comunidad de            21.1    6         1
    ## 14 Murcia, Región de               34.7    9.1       3
    ## 15 Navarra, Comunidad Foral de     16.6    5.5       1
    ## 16 País Vasco                      15.9    5.2       1
    ## 17 Rioja, La                       20.1    3.8       1
    ## 18 Ceuta                           42.4   21.4       2
    ## 19 Melilla                         38.1   17.2       2

# Número Clusters Óptimo

Encontrar el número óptimo de clusters implica identificar la cantidad
ideal de grupos en los que se pueden dividir los datos de manera
significativa y coherente. Es crucial porque determina la calidad y
utilidad de los resultados del análisis de agrupamiento.

## Método Elbrow

Una de las formas comunes de determinar este número es a través del
método del **codo** o **elbow** en inglés. Este método busca identificar
el punto donde la adición de más clusters ya no proporciona un beneficio
significativo en la varianza explicada o la cohesión dentro de los
grupos.

Al representar la variación explicada en función del número de clusters,
observamos un **gráfico** que se asemeja a la **forma** de un codo. A
medida que aumentamos el número de clusters, la varianza explicada
tiende a disminuir. El punto en el que esta disminución se estabiliza o
se aplana marca el número óptimo de clusters, indicando un equilibrio
entre una mayor partición (más clusters) y una adecuada
interpretabilidad de los grupos.

``` r
#  Método Elbrow
set.seed(785248)
factoextra::fviz_nbclust(resultado, kmeans, method = "wss",print.summary = TRUE)
```

<img src="ecv_cluster_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

El número óptimo de k parece ser 3 que es donde más se reduce la
pendiente y la variabilidad explicada no parece disminuir de forma tan
rápida. De todos modos, también podría parecer razonable tomar el 2 o 4.
Es por ello que vamos a usar algún método adicional.

## Método Silhouette

El **método Silhouette** es una técnica utilizada para determinar la
calidad de la agrupación en un conjunto de datos. Consiste en calcular
el valor de la silueta para cada punto de datos, que mide qué tan
similar es un punto a su propio grupo (cohesión) en comparación con
otros grupos vecinos (separación).

El proceso implica:

1.  **Cálculo de la silueta individual**: Para cada punto de datos, se
    calcula la silueta, que es la diferencia entre la distancia media
    intra-cluster (distancia al resto de puntos en su mismo grupo) y la
    distancia media al cluster más cercano (distancia a los puntos del
    grupo más próximo, excluyendo el propio grupo).

2.  **Valor de la silueta global**: Se obtiene el promedio de las
    siluetas individuales de todos los puntos de datos en el conjunto.
    Contra más cercano a 1, mejor formado estará el clsuter.

La siguiente función generará un gráfico que muestra los valores de
Silhouette en función del número de clusters. El número óptimo de
clusters es típicamente aquel que maximiza el valor de Silhouette,
representando una mejor cohesión intra-cluster y separación
inter-cluster.

``` r
#  Método Silhouette
set.seed(785248)
factoextra::fviz_nbclust(resultado, kmeans, method = "silhouette")
```

<img src="ecv_cluster_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

Este método nos reafirma que el número óptimo es 5 puesto que es el caso
cuyos clusters maximiza el valor de Silhouette, representando una mejor
cohesión intra-cluster y separación inter-cluster.

**NOTA**: Ahora podríamos repetir el estudio anterior con el número de
clusters igual a 2 e intentar analizar de neuvo los resultados.

``` r
# K-MEANS algortihm
set.seed(785248) # reproducibilidad
k1 <- kmeans(resultado, centers = 2, nstart = 25)
k1
```

    ## K-means clustering with 2 clusters of sizes 13, 6
    ## 
    ## Cluster means:
    ##     taspobex     tascar
    ## 1 -0.5834244 -0.4985867
    ## 2  1.2640861  1.0802711
    ## 
    ## Clustering vector:
    ##                   Andalucía                      Aragón 
    ##                           2                           1 
    ##     Asturias, Principado de              Balears, Illes 
    ##                           1                           1 
    ##                    Canarias                   Cantabria 
    ##                           2                           1 
    ##             Castilla y León        Castilla - La Mancha 
    ##                           1                           1 
    ##                    Cataluña        Comunitat Valenciana 
    ##                           1                           1 
    ##                 Extremadura                     Galicia 
    ##                           2                           1 
    ##        Madrid, Comunidad de           Murcia, Región de 
    ##                           1                           2 
    ## Navarra, Comunidad Foral de                  País Vasco 
    ##                           1                           1 
    ##                   Rioja, La                       Ceuta 
    ##                           1                           2 
    ##                     Melilla 
    ##                           2 
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 4.639180 7.114779
    ##  (between_SS / total_SS =  67.4 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
fviz_cluster(k1, data = resultado) # plot
```

<img src="ecv_cluster_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

# Conclusiones

Aquí se han explicado los supuestos del cluster K-MEANS con un dataset
en el que se han creado clusters de CCAA según tasa de riesgo de pobreza
y la carencia material. La evidencia nos ha mostrado que se tiende a
grupar las comunidades con incidencias más altas por un lado y más bajas
por otro.

[1] EDA viene del Inglés *Exploratory Data Analysis* y son los pasos
relativos en los que se exploran las variables para tener una idea de
que forma toma el dataset.
