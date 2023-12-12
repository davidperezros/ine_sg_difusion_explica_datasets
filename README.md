# ine_sg_difusion_explica_datasets

Repositorio que conteine todos archivos relativos al proyecto **Datasets** de la web Explica de la *S.G. Difusión del INE*.

# Estructura

El repositorio está organizado en diferentes carpetas con el fin de organizar los documentos que se contienen en ellas. Vamos a dar una breve descripción de ellas:

-   \~\\ **Notebooks**: Contiene los estudios estadísticos para cada dataset según las diferentes técnicas estadísticas. En su interior se encuentran carpetas por cada **Técnica Estadística**, conteniendo cada una de ellas una subcarpeta por cada dataset al que se aplique dicha téncica. Dentro de estas últimas encontraremos: 

    - Informe en formato **.html** realizando la técnica estadística para el dataset en cuestión (descargar para visualizar el informe completo).
    - Informe en formato **.rmd** (por si se quiere editar).
    - Informe en formato **.md**, el cual es útil **para ver el informe en línea** sobre GitHub (aunque puede tener distorsiones con determinadas formulas latex). 
    - *Dataset_cleaning* (OPCIONAL): Presente cuando el fichero de datos ha sido necesario procesarlo para su posterior análisis. En este informe se precisan los pasos seguidos para el procesado del fichero de datos final.
  
-   \~\\**Datasets**: Contiene los ficheros de datos que se incluirán en la web explica con la finalidad de analizar los datos según la técnica estadísitca. En la carpeta de **Notebooks** se encunetran dichos estudios.

| Dataset           | Técnica Estadística | Descripción         |
|-------------------|---------------------|---------------------|
| IMCV              | Regresión           | Lineal              |
| suicidios2019CCAA | Regresión Lineal    | Lineal              |
| ECV_microdatos    | Clasificación       | Reg. Logística      |
| Partos            | Clasificación       | Reg. Logística      |
| VarCovid          | Cluster             | Jerárquico          |
| VarCovid          | Cluster             | K-Means.            |




# Créditos

```
\@book{PerezRos-David2023,

    title        = {Dataset project},
    author       = {S.G. Difusión. INE},
    year         = {2023},
    note         = {Version 0.0.9. },
    url          = {})

}
```
