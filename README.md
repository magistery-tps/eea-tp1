# UBA - Maestria en Explotación de Datos y Descubrimiento de Conocimiento - Enfoque Estadistico del Aprendizaje


## TP 1: Regresión lineal

* [Consignas](https://github.com/magistery-tps/eea-tp1/blob/master/docs/consignas.pdf)
* **Notebooks**
  * [Analisis exploratorio (Punto 1)](https://rpubs.com/adrianmarino/eea-tp1-eda)
  * [Definicion de modelos (Punto 2 a 6)](https://rpubs.com/adrianmarino/eea-tp1-models)

## Requisitos

* Git
* R/R Studio

## Comenzando

**Paso 1**:  Clonar el repositorio.

```bash
$ git clone https://github.com/magistery-tps/eea-tp1.git
$ cd dm-tp2
```

**Paso 2**:  Abrir proyecto en Rstudio.

```bash
$ open -na Rstudio .
```

### Ejecutar Notebook

antes de ejecutar loas notebooks en necesario isntalas el sistema el gestor de paquetes que se se utiliza en las mismas. Para esto se debe isntalar el paquete [pacman](https://github.com/trinker/pacman) usando el mode de instalacion pro defecto en R como sigue:

```R
install.packages('pacman')
```

[pacman](https://github.com/trinker/pacman) instala y carga los paquetes mediante la funcion `p_load` por no es necesario preocuparse en instalar las librerias.

Finalmente es importante aclarar que el dataset ya esta incluido en el proyecto.
