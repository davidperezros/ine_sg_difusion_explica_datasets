# Introducción

## dataset

En este cuaderno vamos a analizar el dataset llamado
[*VarCovid_provincias.xlsx*](https://github.com/davidperezros/ine_sg_difusion_explica_datasets/blob/93774ea45559a2fec3bf0d6b9b6b3cd1066b730b/Datasets/VarCovid_provincias.xlsx),
elaborado a partir de los datos publicados por el INE. Este dataset está
compuesto por el número de fallecidos durante las semanas que duraron
las respectivas olas, así como un promedio entre los años 2017, 2018 y
2019 del número de fallecidos en esas mismas semanas y la tasa de
variación entre ambas para ver el exceso de muertes que se ha producido.
Esta última variable será la que nosotros utilicemos en nuestro estudio.

-   **provincia**: provincia
-   **1Ola**: Tasa de variación entre el acumulado entre la semana 11 de
    2020 y la semana 18, ambas incluidas, respecto a las mismas semanas
    del año anterior. Tiempo correspondiente a la primera ola
-   **2Ola**: Tasa de variación entre el acumulado entre la semana 32 de
    2020 y la semana 49, ambas incluidas, respecto a las mismas semanas
    del año anterior. Tiempo correspondiente a la segunda ola.

El objetivo de este estudio será aplicar un Análisis **Cluster** para
hacer grupos de provincias en función de las variables **1Ola** y
**2Ola**. Concretamente usaremos un cluster jerárquico.

``` r
# Librerias
library(readxl) # Para leer los excels
library(dendextend) # Para dendogramas
library(dplyr) # Para tratamiento de dataframes
library(ggplot2) # Nice plots
library(stats) # hclust package

library(factoextra) # fviz_cluster function
```

Cargamos entonces el conjunto de datos:

``` r
datos <- read_excel("/Users/davpero/ine_sg_difusion_explica_datasets/Datasets/VarCovid_provincias.xlsx", sheet = "Datos")
```

## Descripción del trabajo a realizar

**(Esto irá en la web de explica)** Se pretende hacer un Análisis
Cluster empleando el procedimiento Cluster Jerárquico de las
**provincias** en función a las variables **1Ola** y **2Ola**.

-   Hacer un análisis exploratorio.
-   Ver si hay NA’s y si es necesario escalar los datos.
-   Plantear variables sobre las que se van a hacer los cluster.
-   Interpretar resultados.
-   Ver métodos Elbrow y Silhouette si hay otro número óptimo de
    clusters y en ese caso repetir el estudio.

# Análisis Exploratorio (EDA[1])

Lo primero de todo vamos a cargar las librearias necesarias para
ejecutar el resto del código del trabajo:

# Clustering: Cluster Jerárquico

## Introducción

El **Análisis de clúster** es una técnica de aprendizaje no supervisado
que agrupa datos similares en conjuntos, llamados clústeres. El objetivo
es dividir un conjunto de datos en grupos homogéneos, donde los miembros
de cada grupo son más similares entre sí que con los miembros de otros
grupos, según algún criterio de similitud predefinido.

Concretamente, el **Cluster Jerárquico** realiza estos grupos -o
clusters- de manera jerárquica y ascendente, es decir que sucesivamente
van fusionando grupos desde el elemento individual (mayor nivel de
grupos, uno por individuo) hacia arriba.

La **representación** de la jerarquía de clúster se representa por medio
de un **dendograma**, en el que las sucesivas fusiones de las ramas a
los distintos niveles nos informan de las sucesivas fusiones de los
grupos en grupos de superior nivel (mayor tamaño, menor homogeneidad)
sucesivamente:

Los **pasos** conretos del Cluster Jerárquico son:

1.  **Matriz de distancia o similitud**: Se calcula una matriz que mide
    la distancia o similitud entre cada par de observaciones. Algunas de
    las medidas comunes son:
    -   **Euclidiana**: Mide la distancia más corta entre dos puntos en
        un espacio euclidiano. Es útil cuando las dimensiones tienen una
        **escala similar** y se desea tener en cuenta la magnitud
        absoluta de las diferencias.
    -   **Manhattan (o Cityblock)**: Calcula la suma de las diferencias
        absolutas entre las coordenadas de dos puntos. Es útil cuando
        las dimensiones **no están en la misma escala** y se quiere una
        medida robusta a los valores atípicos.
    -   **Gower**: métrica de distancia utilizada específicamente para
        conjuntos de **datos mixtos** que contienen variables numéricas
        y categóricas. Esta distancia tiene en cuenta diferentes tipos
        de variables al calcular la similitud entre dos observaciones.
        Se define como una combinación ponderada de las distancias entre
        variables.
2.  **Fusión de clústeres**: En el enfoque aglomerativo, se fusionan
    gradualmente los clústeres más cercanos según la medida de distancia
    o similitud elegida. Esto nos lleva a la pregunta, ¿Cómo se calcula
    la distancia entre Clusters calcular la distancia o similitud entre
    clústeres en el proceso de agrupamiento jerárquico?. Existen varios
    **métodos de enlace**, destacando:
    -   **Enlace Simple (Single Linkage)**: Calcula la distancia entre
        clústeres como la distancia más corta entre cualquier punto de
        un clúster y cualquier punto del otro clúster. Es sensible a la
        presencia de valores atípicos y al fenómeno del encadenamiento.
    -   **Enlace Completo (Complete Linkage)**: Mide la distancia entre
        clústeres como la distancia más larga entre cualquier punto de
        un clúster y cualquier punto del otro clúster. Menos sensible a
        valores atípicos, pero puede generar clústeres de tamaño
        desigual.
    -   **Enlace Promedio (Average Linkage)**: Calcula la distancia
        entre clústeres como la media de todas las distancias entre
        pares de puntos, uno de cada clúster. Más robusto frente a
        valores atípicos que el enlace simple y menos propenso al
        encadenamiento que el enlace completo.
    -   **Enlace de Ward**: Minimiza la varianza dentro de los clústeres
        al fusionarlos. Intenta minimizar la suma de cuadrados dentro de
        cada clúster después de la fusión.
3.  **Representación jerárquica**: Esto resulta en un dendrograma que
    muestra la jerarquía de agrupamiento, donde la altura en el
    dendrograma indica la distancia o disimilitud en la que se unen los
    clústeres.

El clustering jerárquico permite explorar diferentes niveles de
granularidad en los datos, pero puede ser computacionalmente costoso
para grandes conjuntos de datos. Es **crucial** elegir la medida de
similitud adecuada y el método de enlace (criterio para unir clústeres,
single linkage, complete linkage, average linkage,…) para obtener
resultados significativos.

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

Notar que la distancia más apropiada para usar es la Euclidea ya que
ambas variables *1Ola* y *2Ola* son del mismo tipo y corresponden a
meses consecutivos, es decir, representan el mismo fenómeno demográfico
y en la misma escala (una vez hayamos escalado). Además como estamos
interesados en la diferencia de estas variables a la hora de hacer
cluster, esta es la distancia más adecuada.

En cuanto al método para hacer los clusters, vamos a dejar el que viene
por defecto, el complete. Este se basa en medir la distancia entre
clústeres como la distancia más larga entre cualquier punto de un
clúster y cualquier punto del otro clúster. Menos sensible a valores
atípicos, pero puede generar clústeres de tamaño desigual.

``` r
# Preparación de los datos
resultado <- datos[, c("1Ola", "2Ola")]

resultado <- scale(resultado) #scaling/standardizing
rownames(resultado) <- datos$provincia # Para que nos salgan luego los nombres
provincias <- datos$provincia

# Matriz de distancias
d <- dist(resultado, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete")

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
abline(h = 3, col = "red", lty = 2)
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

En el dendrograma mostrado arriba, cada hoja corresponde a una
observación. A medida que avanzamos en el árbol, las observaciones
similares se combinan en ramas, las cuales a su vez se fusionan a una
altura mayor.

La altura de la fusión, representada en el eje vertical, indica la (des)
similitud entre dos observaciones. Cuanto mayor sea la altura de la
fusión, menos similares son las observaciones. Es importante destacar
que las conclusiones sobre la proximidad de dos observaciones solo se
pueden inferir en función de la altura donde las ramas que contienen
esas dos observaciones se fusionan inicialmente. No podemos usar la
proximidad de dos observaciones a lo largo del eje horizontal como
criterio de su similitud.

La altura del corte en el dendrograma controla el número de clusters
obtenidos. **Cumple el mismo papel que ‘k’** en la agrupación
**k-means**. Para identificar subgrupos (es decir, clusters), podemos
cortar el dendrograma con la función `cutree`. Suponer que queremos 3
clusteres:

``` r
# Cut tree into 4 groups
sub1 <- cutree(hc1, k = 3)

# Number of members in each cluster
table(sub1)
```

    ## sub1
    ##  1  2  3 
    ## 31 11 11

Podemos mostrar los grupos junto al dataframe con la función mutate.

``` r
# Mostrar Clusters
datos %>%
  mutate(cluster = sub1) %>%
  head()
```

    ## # A tibble: 6 × 17
    ##   provincia  falle_1Ola falle1_med17_19 `1Ola` falle_2Ola falle2_med17_19 `2Ola`
    ##   <chr>           <dbl>           <dbl>  <dbl>      <dbl>           <dbl>  <dbl>
    ## 1 Total nac…     113148          63962.  76.9      157968         133845.  18.0 
    ## 2 Albacete         1600            553  189.         1287           1173.   9.75
    ## 3 Alicante/…       3011           2404.  25.3        5852           5082.  15.1 
    ## 4 Almería           851            801    6.24       2003           1702   17.7 
    ## 5 Araba/Ala…        841            410. 105.         1022            935.   9.34
    ## 6 Asturias         2445           1962.  24.6        5157           4207.  22.6 
    ## # ℹ 10 more variables: falle_3Ola <dbl>, falle3_med17_19 <dbl>, `3Ola` <dbl>,
    ## #   falle_4Ola <dbl>, falle4_med17_19 <dbl>, `4Ola` <dbl>, falle_5Ola <dbl>,
    ## #   falle5_med17_19 <dbl>, `5Ola` <dbl>, cluster <int>

``` r
datos <- datos[,c("provincia","1Ola","2Ola")]
colnames(datos)<-c( "provincia","PrimOla","SegOla" )
resultado <- as.data.frame(resultado)
resultado$provincia <-datos$provincia
colnames(resultado)<-c( "PrimOla","SegOla" ,"provincia")



# Gráfico de puntos
g1<- ggplot(datos, aes(PrimOla, SegOla, label=provincia))+ geom_point(aes(colour = factor(sub1)))+geom_text(hjust=0, vjust=0, size = 2,aes(colour = factor(sub1)))+labs(colour="Clusters")

g1
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

**Del anterior gráfico y de los clusters pordemos concluir**: hay
grandes diferencias entre los tres clusters. El **Cluster Verde**
corresponde a las provincias donde la primera ola de COVID generó un
gran exceso de mortalidad respecto al año anterior, son los casos de las
provincias relativas a la comunidad de Madrid y Castilla-La Mancha. En
cambio, el **Cluster Azul** corresponde a las provincias donde se
produjo un mayor exceso de fallecidos en la segunda ola, como son los
casos de las provincias relativas a la comunidad de Aragón y Melilla en
general. El **Cluster Rojo** corresponde a las provincias donde hubo un
exceso de fallecidos más parecido entre ambas olas, aunque podemos
observar algunas diferencias entre algunas de ellas, por ejemplo, en
Cataluña hubo un mayor exceso en la primera y en Ceuta hubo un mayor
exceso en la segunda.

Notar que estos resultados son en gran medida coherentes con los
presentados en el mismo estudio por provincias Autónomas en vez de por
provincias.

## Con otros métodos de Enlace

Vamos a probar a usar otros métodos de Enlace descritos previamente a
ver si seguimos obteniendo los mismos clusters y poder llegar a una
conclusión sólida.

``` r
# For Complete
plot(hc1, cex = 0.6, sub = "")
rect.hclust(hc1, k = 3, border = 2:5)
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
# Ward.D method
hc2 <- hclust(d, method = "ward.D")
sub2 <- cutree(hc2, k = 3)

plot(hc2, cex = 0.6, sub = "")
rect.hclust(hc2, k = 3, border = 2:5)
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-4-2.png" style="display: block; margin: auto;" />

``` r
# Average method
hc3 <- hclust(d, method = "average")
sub3 <- cutree(hc3, k = 3)

plot(hc3, cex = 0.6, sub = "")
rect.hclust(hc3, k = 3, border = 2:5)
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-4-3.png" style="display: block; margin: auto;" />

``` r
# Single method
hc4 <- hclust(d, method = "single")
sub4 <- cutree(hc4, k = 3)

plot(hc4, cex = 0.6, sub = "")
rect.hclust(hc4, k = 3, border = 2:5)
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-4-4.png" style="display: block; margin: auto;" />

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

De 4 tipos diferentes que hemos probado, nos salen 3 que se han
clusterizado de igual manera, luego parece razonable la interpretación
anterior. Para todos casos menos el de `ward`, el **Cluster Verde**
corresponde a las provincias donde la primera ola de COVID generó un
gran exceso de mortalidad respecto al año anterior, son los casos de las
provincias relativas a la comunidad de Madrid y Castilla la Mancha. En
cambio, el **Cluster Azul** corresponde a las provincias donde se
produjo un mayor exceso de fallecidos en la segunda ola, como son los
casos de Melilla y Aragón. El **Cluster Rojo** corresponde a las
provincias donde hubo un exceso de fallecidos más parecido entre ambas
olas, aunque podemos observar algunas diferencias entre algunas de
ellas, por ejemplo, en Cataluña hubo un mayor exceso en la primera y en
Ceuta hubo un mayor exceso en la segunda. Resumiendo:

-   El **cluster azul** representa las provincia donde el exceso de
    mortalidad respecto al año anterior fue mucho mayor en la primera
    que en la segunda ola, es decir, hubo más muertes en la primera que
    en la segunda ola. Notar que en este cluster encontramos la
    provincia con más población y flujo de visitantes del país,
    provincia de Madrid , luego tiene sentido que fuera la pionera en
    tener una tasa alta de muertes. Además, durante las primeras semanas
    de virus en España, fueron Castilla-La Mancha y Madrid las que
    presentaban peores números.

-   El **cluster verde** representa las provincia donde el exceso de
    mortalidad respecto al año anterior fue mucho mayor en la segunda
    que en la primera ola, es decir, hubo más muertes en la segunda que
    en la primera ola. Notar, que a excepción de Melilla, Aragóon es una
    provincia con gran población residente en núcleos rurales y por ello
    la propagación del virus tardó en extenderse. Debido a que no tienen
    grandes ciudades esta propagación inicial fue más lenta y por ello
    la segunda ola causo más exceso de mortalidad que la priemra.

-   Por último, el **cluster rojo** presenta provincias que tuvieron una
    incidencia parecida en la primera y segunda ola.

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
factoextra::fviz_nbclust(resultado,hcut,method="wss")
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

El número óptimo de k parece ser 4 que es donde más se reduce la
pendiente y la variabilidad explicada no parece disminuir de forma tan
rápida. De todos modos, también podría parecer razonable tomar el 3 Es
por ello que vamos a usar algún método adicional.

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
factoextra::fviz_nbclust(resultado,hcut,method="silhouette")
```

<img src="VarCovid_provincias_files/figure-markdown_github/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

Este método nos reafirma que el número óptimo es 3 puesto que es el caso
cuyos clusters maximiza el valor de Silhouette, representando una mejor
cohesión intra-cluster y separación inter-cluster.

**NOTA**: Ahora podríamos repetir el estudio anterior con el número de
clusters igual a 5 e intentar analizar de neuvo los resultados.

# Conclusiones

Aquí se han explicado los supuestos del hierarchical clustering.

[1] EDA viene del Inglés *Exploratory Data Analysis* y son los pasos
relativos en los que se exploran las variables para tener una idea de
que forma toma el dataset.
