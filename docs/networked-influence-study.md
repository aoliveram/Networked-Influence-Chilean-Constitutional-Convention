---
title: "Networked Influence in a Tabula Rasa Legislature"
subtitle: "Redes de co-patrocinio, dinámicas ideológicas y éxito político en la Convención Constitucional de Chile (2021--2022)"
author: "A. Olivera, J. Fábrega"
date: \today
geometry: "margin=2.5cm"
fontsize: 10pt
colorlinks: true
linkcolor: teal
toc: true
toc-depth: 2
header-includes:
  - \usepackage{amsmath}
  - \usepackage{amssymb}
  - \usepackage{booktabs}
---

# 1. La pregunta y el caso

La Convención Constitucional chilena (julio 2021 -- julio 2022) permite observar algo que casi nunca se observa: cómo se organiza la colaboración política desde cero. A diferencia de una legislatura ordinaria, donde las redes son el sedimento de décadas de carreras y disciplinas de partido, la Convención partió sin jerarquías heredadas: una mayoría inédita de independientes, 17 escaños reservados para pueblos originarios, un órgano que se disolvía al entregar el texto (sin "sombra del futuro" electoral), y reglas escritas por los propios convencionales.

Dos de esas reglas estructuran el estudio. Primero, cada iniciativa convencional constituyente requería entre 8 y 16 patrocinantes: firmar era formar una coalición visible, fechada y acotada. Segundo, cada norma constitucional necesitaba dos tercios de los convencionales en ejercicio en el Pleno (103 de 154), mientras que dentro de las comisiones bastaba mayoría simple: nacer era barato, sobrevivir era caro. La Figura 1 ubica los hitos y las ventanas de datos que usa cada modelo sobre el calendario real del proceso.

![Figura 1. Hitos de la Convención y ventanas de datos del estudio, sobre el eje temporal real.](../results/figures/cc_timeline.pdf){width=100%}

Sobre ese escenario hacemos tres preguntas:

- RQ1 (formación): ¿qué organiza la decisión de co-firmar entre extraños? En particular, ¿cuánto se puede predecir con lo que ya se sabía de cada convencional *antes* de que la Convención empezara — su distrito, su lista, su profesión, su experiencia — y cuánto requiere lo que ocurrió adentro?
- RQ2 (efectos sobre las personas): ¿la red mueve las posiciones de los convencionales (influencia), o los convencionales eligen la red según sus posiciones (selección)? ¿Y mueve su conducta?
- RQ3 (efectos sobre los textos): ¿qué hace que un artículo sobreviva hasta el borrador final — y cuánto de eso se debe al grupo concreto de convencionales que firmó la iniciativa que lo contenía (su "coalición firmante", que no debe confundirse con la lista electoral)?

# 2. Datos

Todo el material proviene del registro documental de la Convención, procesado en un pipeline reproducible (los datos viven como snapshot versionado dentro del repositorio).

- Actores: los 154 convencionales, con perfil curado por doble fuente (BCN y Wikipedia, con capa de validación manual). Covariables:
    - género; profesión de abogado/a (39% de la Convención); experiencia institucional previa (haber sido parlamentario/a, alcalde/sa, concejal/a, ministro/a u otro cargo público antes de jul-2021);
    - grado académico en escala 0--3 (0 = sin estudios universitarios terminados, 1 = educación superior, 2 = magíster, 3 = doctorado);
    - distrito electoral (28 distritos; para los 17 escaños reservados, su pueblo originario — 10 pueblos);
    - lista electoral de origen, agrupada en conglomerados (Vamos por Chile 37, Apruebo Dignidad 28, Lista del Apruebo 25, Lista del Pueblo 23, escaños reservados 17, Independientes No Neutrales 3, otras listas locales 21);
    - comisión temática de pertenencia.
- Iniciativas: 996 ingresadas a la plataforma oficial entre nov-2021 y feb-2022.
    - 528 tienen dos o más firmantes-persona (el resto son populares/institucionales o de firmante único recuperable).
    - El análisis usa 487: las 41 restantes registran más de 16 firmantes — imposible bajo la regla del reglamento — y están en auditoría como duplicaciones de documentos transversales entre comisiones.
    - Para 448 de las 487 tenemos la fecha exacta de ingreso (3-nov-2021 al 2-feb-2022); 156 ingresaron el día del plazo final. Las 39 sin fecha solo quedan fuera del modelo que necesita orden temporal (sección 3.3).
- Red de co-patrocinio: la proyección de las firmas sobre pares de convencionales.
    - 154 nodos y 5.870 aristas; el peso $w_{ij}$ = número de iniciativas que $i$ y $j$ co-firmaron.
    - Firmar no escasea para el firmante: mediana 42 iniciativas firmadas por convencional, máximo 157 (Figura 2a).
    - El tope legal de 16 patrocinantes se llenaba con frecuencia: es el valor modal (Figura 2b).
- Trazabilidad de textos: 1.809 artículos génesis con desenlace conocido frente al borrador del 14-mayo-2022.
    - 498 artículos llegaron al borrador (tasa de supervivencia 20%); el resto murió en comisión o en el Pleno.
    - Cada artículo conserva su texto original, sus autores y el historial fechado de indicaciones que recibió.
- Votaciones: 4.707 votaciones nominales del Pleno (jul-2021 a jun-2022), base del voto revelado y de los puntos ideales.

![Figura 2. (a) Iniciativas firmadas por convencional; (b) firmantes por iniciativa, con la regla 8--16.](../results/figures/signature_distributions.pdf){width=100%}

Las siete comisiones temáticas difieren fuertemente en composición y en productividad — diferencias que los modelos explotan y controlan:

| | Nombre (corto) | Miembros | % abog. | % exper. | Edad | Grado (0--3) | Iniciativas | Ondas ind. | Indicaciones | Ind. multifirm. |
|:-:|:---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| C1 | Sistema Político | 25 | 60 | 36 | 42.2 | 1.44 | 27 | 4 | 365 | 266 |
| C2 | Principios Const. | 18 | 22 | 22 | 45.8 | 1.28 | 51 | 3 | 53 | 1 |
| C3 | Forma de Estado | 30 | 37 | 40 | 45.1 | 1.43 | 38 | 6 | 241 | 212 |
| C4 | Derechos Fund. | 30 | 30 | 13 | 45.9 | 1.03 | 59 | 5 | 395 | 154 |
| C5 | Medio Ambiente | 19 | 21 | 16 | 44.7 | 1.26 | 142 | 5 | 559 | 0 |
| C6 | Sist. de Justicia | 17 | 88 | 18 | 42.6 | 1.65 | 75 | 6 | 381 | 317 |
| C7 | Conocimientos | 15 | 13 | 0 | 51.7 | 1.33 | 95 | 8 | 332 | 54 |

(Iniciativas = con 2--16 firmantes, set de análisis. Ondas ind. = número de informes de indicaciones de la comisión. Indicaciones = actos de enmienda registrados con autoría, deduplicados. Ind. multifirm. = el subconjunto de esas indicaciones con dos o más firmantes — las únicas que agregan lazos a la red de la sección 4. Los ceros de C5 son una limitación de registro: sus indicaciones fueron colectivas pero los informes anotan solo al primer firmante.)

## 2.1 Cómo medimos la ideología

Usamos dos mediciones, con papeles deliberadamente distintos (sus ventanas están en la Figura 1).

Ideología pre-red. Puntos ideales en dos dimensiones estimados con W-NOMINATE usando solo las votaciones del primer mes del Pleno (147 votaciones, jul--ago 2021), replicando el diseño de Fábrega (2022) casi exactamente (clasificación correcta 89.4% / 91.6%). Esa ventana es anterior a las comisiones temáticas (octubre), a las iniciativas (noviembre en adelante) y a la regla de 2/3 (operativa desde febrero; todo el primer mes se votó por mayoría). Por eso $\theta_1$ (izquierda--derecha; negativo = izquierda) y $\theta_2$ (el eje que separa a los escaños reservados del clivaje clásico) pueden usarse como covariables exógenas: nada de la red que estudiamos existía cuando se generaron. De aquí sale también el pívot de 2/3: ordenados los 154 valores de $\theta_1$ de izquierda a derecha, el convencional en la posición 103 marca $\theta_{1,(103)} = -0.15$ — el punto que cualquier coalición ganadora necesita alcanzar.

Voto revelado dinámico. Un modelo dinámico de puntos ideales (dynIRT, unidimensional) sobre las 4.707 votaciones produce una trayectoria $\theta_{i,t}$ por convencional en 91 períodos. Como después de agosto el Pleno vota bajo reglas y agendas cambiantes, esta serie se interpreta como comportamiento de voto revelado, no como ideología latente pura; es el insumo de la sección 4.

# 3. RQ1 — La formación de la red

## 3.1 El modelo de elección de firma

Una iniciativa de 16 firmantes es un solo acto político, no 120 parejas independientes. Por eso el modelo base trata cada iniciativa $a$ como un menú frente al cual cada uno de los 154 convencionales decide firmar o no. La decisión se representa con una utilidad latente

$$U_{ia} = \beta^\top x_{ia} + \alpha_a + \varepsilon_{ia},$$

donde $x_{ia}$ es el vector de covariables que describe la relación del convencional $i$ con la coalición firmante $S_a$; $\alpha_a$ es un efecto fijo de la iniciativa (todo lo que la hace atractiva en sí: tema, redactor, momento); y $\varepsilon_{ia}$ es un shock idiosincrático. Las covariables, cada una definida sobre la coalición sin contar al propio $i$ ("leave-one-out"):

- $d^{\theta_1}_{ia} = |\theta_{1i} - \bar\theta_{1,S_a}|$ y $d^{\theta_2}_{ia}$: distancia de $i$ a la posición media de la coalición en cada dimensión.
- $comision_{ia} \in \{0,1\}$: si $i$ pertenece a la comisión temática del texto. Cualquier convencional podía patrocinar iniciativas de cualquier comisión, y de hecho lo hacía: el 70% de las firmas observadas proviene de convencionales de otra comisión. La variable mide, entonces, una inclinación real y no una restricción: los miembros de la comisión son el 12% del pool de candidatos pero aportan el 30% de las firmas.
- $lista^c_{ia} \in \{0,1\}$: si la lista de $i$ coincide con la lista mayoritaria de la coalición, con un coeficiente $\lambda_c$ distinto por conglomerado $c$.
- $distrito_{ia} \in [0,1]$: proporción de la coalición que comparte distrito (o pueblo originario) con $i$.
- $abogado_{ia} = abogado_i \times$ (proporción de abogados en $S_a$), y análogos para experiencia y género.
- $d^{grado}_{ia} = |grado_i - \overline{grado}_{S_a}|$: distancia en credenciales educativas.

El modelo de regresión efectivamente estimado es el logit condicional de McFadden: la probabilidad del conjunto de firmantes observado, condicional a su tamaño, es

$$P\big(S_a \,\big|\, |S_a|\big) = \frac{\exp\big(\sum_{i \in S_a} \beta^\top x_{ia}\big)}{\sum_{R \subseteq \{1..154\},\, |R| = |S_a|} \exp\big(\sum_{i \in R} \beta^\top x_{ia}\big)},$$

y $\hat\beta$ maximiza $\ell(\beta) = \sum_a \log P(S_a \mid |S_a|)$. Como $\alpha_a$ es común a todos los candidatos de la iniciativa $a$, aparece idéntico en numerador y denominador y se cancela: no hay que estimarlo, y $\beta$ queda identificado solo por comparaciones entre convencionales frente al mismo menú. La suma del denominador se aproxima con el método de Efron (estándar para este diseño), y los errores estándar se agrupan por convencional. Datos: 487 menús $\times$ 154 convencionales = 74.998 decisiones.

La tabla completa, con las variables ordenadas según la pregunta de esta sección — primero lo que se sabía de cada convencional antes de que la Convención existiera, después lo que solo se supo adentro:

| Variable | Coef. | OR | EE | $p$ |
|:---|:-:|:-:|:-:|:-:|
| *Observables pre-Convención* | | | | |
| Afinidad de distrito/pueblo | $+1.76$ | 5.8 | 0.54 | $0.001$ |
| Misma lista: Escaños Reservados PPOO | $+1.80$ | 6.0 | 0.21 | $<10^{-16}$ |
| Misma lista: Vamos por Chile | $+1.05$ | 2.9 | 0.22 | $<10^{-5}$ |
| Misma lista: Otras listas locales | $+1.09$ | 3.0 | 0.15 | $<10^{-12}$ |
| Misma lista: Lista del Pueblo | $+1.02$ | 2.8 | 0.15 | $<10^{-10}$ |
| Misma lista: Lista del Apruebo | $+1.01$ | 2.8 | 0.17 | $<10^{-9}$ |
| Misma lista: Apruebo Dignidad | $+0.79$ | 2.2 | 0.13 | $<10^{-9}$ |
| Afinidad de abogados | $+0.38$ | 1.5 | 0.20 | $0.059$ |
| Afinidad de experiencia previa | $+0.33$ | 1.4 | 0.26 | $0.195$ |
| Afinidad de género | $+0.39$ | 1.5 | 0.18 | $0.028$ |
| Distancia de grado académico | $-0.08$ | 0.9 | 0.08 | $0.297$ |
| *Estructura y posiciones formadas en la Convención (controles)* | | | | |
| Misma comisión que el texto | $+1.21$ | 3.4 | 0.05 | $<10^{-117}$ |
| Distancia en $\theta_1$ a la coalición | $-3.46$ | 0.03 | 0.23 | $<10^{-52}$ |
| Distancia en $\theta_2$ a la coalición | $-1.22$ | 0.30 | 0.13 | $<10^{-19}$ |

(La lista Independientes No Neutrales, con 3 miembros, nunca es mayoritaria en una coalición y su coeficiente no está identificado.)

Tres lecturas. Primera, la sorpresa territorial: compartir distrito es uno de los predictores pre-Convención más fuertes — pasar de una coalición sin coterráneos a una llena de ellos multiplica las odds de firmar por casi seis. Es conocimiento disponible el día uno: dos convencionales del mismo distrito hicieron campaña en el mismo territorio, comparten electores y problemas locales. Segunda, la lista coordina en todos los conglomerados (todos los $\lambda_c$ entre 0.8 y 1.1, con los escaños reservados aparte en 1.8): también predecible ex ante. Tercera, las credenciales no organizan la firma: abogados marginal, experiencia nula, educación nula — con lo que se sabía de las profesiones antes de julio de 2021 no se predice quién firma con quién (sección 3.4 explica por qué igual "se ven" juntos). Los controles se comportan como se espera: la distancia ideológica es el mayor inhibidor del modelo (una unidad de $\theta_1$ divide las odds por 30) y la comisión triplica las odds — pero son variables *formadas dentro* de la Convención, y por eso las tratamos como controles y no como hallazgo.

El mismo modelo, agregando el término de escaños reservados y sus interacciones (segunda columna: ¿los PPOO eligen distinto?):

| Variable (modelo con interacciones PPOO) | Coef. | EE | $p$ |
|:---|:-:|:-:|:-:|
| PPOO (efecto base) | $-0.84$ | 0.28 | $0.002$ |
| PPOO $\times$ distancia en $\theta_1$ | $+1.34$ | 0.63 | $0.034$ |
| PPOO $\times$ distancia en $\theta_2$ | $-0.30$ | 0.47 | $0.530$ |
| Misma lista PPOO | $+2.58$ | 0.33 | $<10^{-14}$ |
| Afinidad de distrito/pueblo | $+1.45$ | 0.53 | $0.006$ |

Los escaños reservados firman menos iniciativas ajenas, coordinan entre sí más que nadie, y — la interacción positiva — cruzan distancias en $\theta_1$ que el resto no cruza: su pendiente ideológica clásica es $-3.59 + 1.34 = -2.25$ contra $-3.59$ del resto. Su coordenada operativa no es el eje izquierda-derecha.

## 3.2 Las listas: coordinación sin disciplina

¿Funcionaron las listas ad hoc como partidos? Un partido hace al menos dos cosas por sus miembros: los ayuda a coordinar con quién trabajan, y los hace votar juntos.

La primera está medida en la tabla de 3.1, en las filas "Misma lista": cada $\lambda_c$ es el coeficiente del indicador "mi lista coincide con la lista mayoritaria de esta coalición", estimado por separado para cada conglomerado $c$. En palabras: $\lambda_c$ mide cuánto más probable es que un miembro de la lista $c$ firme una iniciativa cuando la coalición ya está dominada por su propia gente — todo lo demás igual, incluida la cercanía ideológica. Es la firma de la coordinación organizacional: si las listas fueran meras etiquetas sin vida interna, sus miembros firmarían con los suyos solo en la medida en que los suyos piensan parecido, y $\lambda_c$ sería cero. El recuento:

| Conglomerado | $\hat\lambda_c$ | OR |
|:---|:-:|:-:|
| Escaños Reservados PPOO | $1.80$ | 6.0 |
| Otras listas locales | $1.09$ | 3.0 |
| Vamos por Chile | $1.05$ | 2.9 |
| Lista del Pueblo | $1.02$ | 2.8 |
| Lista del Apruebo | $1.01$ | 2.8 |
| Apruebo Dignidad | $0.79$ | 2.2 |

Todos positivos, todos entre 0.8 y 1.1 salvo los escaños reservados: la lista ad hoc por excelencia (Lista del Pueblo) coordinó firmas igual que los pactos de partidos tradicionales.

Para la segunda — votar juntos — usamos el índice de Rice, que conviene explicar con calma.

Para una lista $\ell$ y una votación $v$, sean $Y_{\ell v}$ los miembros de la lista que votaron a favor y $N_{\ell v}$ los que votaron en contra. El índice de Rice es

$$R_{\ell v} = \frac{|Y_{\ell v} - N_{\ell v}|}{Y_{\ell v} + N_{\ell v}}.$$

La intuición: si la lista vota en bloque (todos sí o todos no), el numerador iguala al denominador y $R = 1$; si se parte por la mitad, $R = 0$. Promediando sobre votaciones, $\bar R_\ell$ mide cuán "en bloque" vota una lista. Pero un $\bar R$ alto no prueba disciplina: gente que piensa parecido vota parecido sin que nadie la discipline. Por eso el benchmark: para cada lista construimos 500 pseudo-listas — grupos ficticios del mismo tamaño, sorteados entre convencionales de ideología similar a la de la lista real — y preguntamos si la lista real vota más unida que sus dobles ficticios. El "premio de disciplina" es la diferencia.

| Lista | $\bar R$ real | $\bar R$ pseudo | Premio | $p$ |
|:---|:-:|:-:|:-:|:-:|
| Vamos por Chile | 0.855 | 0.833 | $+0.023$ | 0.32 |
| Apruebo Dignidad | 0.816 | 0.808 | $+0.008$ | 0.32 |
| Lista del Apruebo | 0.713 | 0.740 | $-0.027$ | 0.91 |
| Lista del Pueblo | 0.873 | 0.858 | $+0.015$ | 0.10 |
| Escaños Reservados PPOO | 0.875 | 0.859 | $+0.015$ | 0.16 |

Ninguna lista tiene premio. Pero antes de concluir "no hubo disciplina", hay que confesar un problema — y esta es la explicación para la abuela. Imagina que quieres saber si los hinchas de un club van al estadio porque aman al club o porque sus amigos van. El problema: conociste quiénes son hinchas *mirando quién va al estadio*. Con ese dato no puedes separar amor y amistad — están pegados en la misma observación. Aquí pasa igual: la "ideología" de cada convencional la medimos con sus votos, y si una lista disciplinó los votos desde el día uno, esa disciplina quedó *dentro* de lo que llamamos ideología. Las pseudo-listas emparejadas por ideología llevan la disciplina escondida adentro, y el premio sale cero por construcción parcial. Este problema tiene nombre en la literatura (Krehbiel: ¿partidos o preferencias?) y no tiene solución con votos solamente. La conclusión honesta es: ninguna lista vota más unida *de lo que su alineamiento estable ya implica*. Lo que sí es limpio es el contraste con la sección anterior: en la conducta de *firma* — que no entra en la medición de ideología — las listas sí coordinan, todas y en magnitud similar. La Figura 3 agrega la dinámica: la Lista del Pueblo se desploma en dic-2021/ene-2022, su fragmentación documentada, y se recompone después.

![Figura 3. Cohesión de voto (Rice mensual) por lista.](../results/figures/rice_cohesion_monthly.pdf){width=100%}

## 3.3 ¿Afines o conocidos? El modelo de eventos (RHEM)

El logit condicional mira cada iniciativa como si fuera la primera de la historia. Pero las iniciativas ocurrieron en orden — tenemos la fecha de cada una — y el orden permite hacer la pregunta que ninguna foto responde: cuando dos personas firman juntas, ¿es porque se parecen, o porque ya habían firmado juntas antes? Parecerse y conocerse suelen venir juntos, y solo un modelo con tiempo puede separarlos.

La idea del modelo de hipereventos relacionales (RHEM), contada primero sin fórmulas. La historia de la Convención es una película de 448 escenas; en cada escena, una coalición concreta aparece firmando una iniciativa. Para cada escena preguntamos: de todas las coaliciones de ese mismo tamaño que podrían haberse formado ese día, ¿por qué exactamente esta? Y respondemos comparando la coalición real con 50 coaliciones ficticias del mismo tamaño (25 sorteadas entre los 154 convencionales y 25 entre los miembros de la comisión del texto, para que la comparación sea exigente). El modelo aprende qué características separan las coaliciones reales de las ficticias. Algunas características son de composición — las mismas del logit condicional: compacidad ideológica, misma lista, mismo distrito. La novedad son las características de historia: cuánto habían firmado antes, hasta el día anterior, los miembros de la coalición candidata.

Las variables de historia se llaman repetición de subconjuntos ($sub.rep$) y se entienden mejor con un ejemplo. Tomemos la coalición candidata {Ana, Berta, Carlos} evaluada el 10 de enero:

- $sub.rep^{(1)}$ (actividad): de los tres, ¿cuántas iniciativas había firmado cada uno antes del 10 de enero? Se promedia. Mide si la coalición está hecha de firmantes activos.
- $sub.rep^{(2)}$ (familiaridad de pares): para cada par — Ana-Berta, Ana-Carlos, Berta-Carlos — ¿en cuántas iniciativas anteriores aparecieron *juntos*? Se promedia sobre los tres pares. Mide si la coalición está hecha de duplas que ya trabajaron juntas.
- $sub.rep^{(3)}$ (familiaridad de tríos): ¿en cuántas iniciativas anteriores aparecieron los tres *a la vez*? Mide si la coalición recicla equipos completos.

Formalmente, si $deg(t, h')$ cuenta los eventos anteriores a $t$ que contienen al subconjunto $h'$, entonces para una coalición candidata $h$:

$$sub.rep^{(p)}(t, h) = \binom{|h|}{p}^{-1} \sum_{h' \subseteq h,\ |h'| = p} deg(t, h'),$$

donde $\binom{|h|}{p}$ es el número de subconjuntos de tamaño $p$ que tiene $h$ (con eso la suma se vuelve promedio). Cada estadística se calcula en dos versiones de memoria: infinita (todo el pasado pesa igual) y con semivida de 30 días (un evento de hace un mes pesa la mitad: cada evento pasado se pondera por $w(\Delta) = e^{-\Delta \ln 2 / 30}$, con $\Delta$ los días transcurridos).

El modelo de regresión efectivamente estimado es, de nuevo, un logit condicional — pero ahora el estrato es el evento fechado, el "menú" son la coalición real y sus 50 controles, y las covariables incluyen la historia:

$$P\big(h_e \text{ real} \,\big|\, \text{estrato } e\big) = \frac{\exp\big(\theta^\top s(t_e, h_e)\big)}{\sum_{h \in \{h_e\} \cup \text{controles}_e} \exp\big(\theta^\top s(t_e, h)\big)},$$

con las mismas covariables de composición del logit condicional (para que las tablas se lean en paralelo) más las tres estadísticas de historia: $s(t,h)$ reúne la dispersión ideológica de la coalición en cada dimensión (la distancia promedio entre sus pares), la proporción de pares de la misma lista, del mismo distrito/pueblo, ambos abogados, ambos con experiencia, ambos mujeres, la dispersión de grado académico, la proporción de miembros de la comisión del texto, y $sub.rep^{(1,2,3)}$. Todas las covariables se estandarizan (media 0, desviación 1), así que cada coeficiente responde: si esta característica sube una desviación estándar, ¿cuánto más "real" parece la coalición? La estimación se repite con 10 sorteos independientes de controles; la tabla reporta el promedio (los coeficientes apenas se mueven entre sorteos) y la mediana de los $p$, con los mismos bloques que la tabla de 3.1:

| Variable | Memoria infinita | $p$ | Semivida 30 días |
|:---|:-:|:-:|:-:|
| *Observables pre-Convención* | | | |
| Prop. pares mismo distrito/pueblo | $+0.78$ | $2\times10^{-6}$ | $+0.75$ |
| Prop. pares misma lista | $+1.03$ | $5\times10^{-5}$ | $+1.09$ |
| Prop. pares ambos abogados | $+0.38$ | $0.30$ | $+0.44$ |
| Prop. pares ambos con experiencia | $+0.09$ | $0.72$ | $+0.03$ |
| Prop. pares ambas mujeres | $+0.10$ | $0.54$ | $+0.11$ |
| Dispersión de grado académico | $+0.25$ | $0.30$ | $+0.27$ |
| *Estructura y posiciones formadas en la Convención (controles)* | | | |
| Prop. de la comisión del texto (‡) | $-0.31$ | $0.36$ | $-0.22$ |
| Dispersión ideológica $\theta_1$ | $-2.72$ | $9\times10^{-16}$ | $-2.71$ |
| Dispersión ideológica $\theta_2$ | $-0.55$ | $0.017$ | $-0.57$ |
| *Historia de co-firma (lo nuevo del RHEM)* | | | |
| $sub.rep^{(2)}$ — familiaridad de pares | $+7.53$ | $2\times10^{-7}$ | $+6.92$ |
| $sub.rep^{(1)}$ — actividad individual (†) | $-7.23$ | $1\times10^{-5}$ | $-6.32$ |
| $sub.rep^{(3)}$ — familiaridad de tríos (†) | $-2.61$ | $6\times10^{-5}$ | $-2.31$ |

(†) Estos signos negativos no se leen por separado — ver el párrafo siguiente. (‡) No interpretable: la mitad de los controles se sortea dentro de la comisión, así que este contraste queda absorbido por el diseño; el efecto comisión ya está medido en 3.1.

El truco de los signos. Las tres estadísticas de historia son casi la misma variable con distinto zoom (correlaciones 0.78 y 0.84: la gente activa acumula pares familiares, y los pares familiares componen tríos). Ajustadas de a una, las tres son positivas: actividad $+1.63$ ($p = 0.002$), pares $+1.62$ ($p < 10^{-6}$), tríos $+0.77$ ($p < 10^{-3}$). Juntas, la de pares absorbe toda la señal y las otras dos se vuelven negativas — un reparto engañoso típico entre variables casi colineales, no una paradoja. La lectura conjunta correcta: lo que distingue a una coalición real es específicamente la familiaridad de sus pares; condicional en ella, "mucha actividad individual sin familiaridad mutua" es marca de coalición ficticia (así lucen los controles: firmantes seriales que no se conocen), y los tríos no agregan sobre los pares. En una frase: la Convención se tejió de a dos — se reclutaban duplas consolidadas, no equipos completos. Entre las dos memorias, la infinita ajusta mejor (log-verosimilitud $-42.7$ contra $-48.3$): el capital de co-firma se acumula, no se evapora en un mes.

Qué agrega el RHEM sobre el logit condicional — la comparación en detalle. Los dos modelos usan los mismos eventos y la misma forma estadística; difieren en una sola cosa: el RHEM deja que el pasado entre a la ecuación. Eso tiene tres consecuencias. (i) El logit condicional es el RHEM del primer día: cuando nadie ha firmado con nadie, las estadísticas de historia valen cero para todos y el RHEM se reduce exactamente al logit de 3.1 — por eso no son rivales sino el mismo modelo en dos momentos, y por eso los coeficientes de composición del logit deben leerse como "la fuerza de los atributos cuando no hay historia que consultar" (los primeros momentos). (ii) La dinámica que el RHEM revela es acumulativa: cada co-firma de hoy se vuelve familiaridad mañana, y esa familiaridad es el predictor más fuerte de la próxima coalición — un mecanismo de rieles: los primeros encuentros (guiados por distrito, lista e ideología, como muestra el logit) crean los pares por los que después circula todo lo demás. (iii) La pregunta contrafactual cambia: el logit pregunta "¿a quién se parece el que firma?"; el RHEM pregunta "dado todo lo que ya pasó, ¿quién más podría haber firmado hoy?". Que la ideología ($-2.70$) sobreviva con toda su fuerza en la segunda pregunta es el hallazgo: la homofilia no era un espejismo de la historia acumulada. Y que lista y distrito también sobrevivan dice que la organización territorial y de etiqueta opera en cada coalición nueva, no solo en la primera.

Una cautela final: la "familiaridad" medida solo ve lo firmado desde noviembre de 2021. Amistades previas, militancias compartidas u otras afinidades estables que no observamos quedan dentro de $sub.rep^{(2)}$; por eso la llamamos persistencia relacional y no amistad.

## 3.4 Lo que no organiza la red: la profesión

La versión para la abuela. Uno esperaría que los abogados de la Convención se buscaran entre ellos para escribir juntos — al fin y al cabo, escribir una constitución es trabajo de abogados. Y si uno mira las iniciativas, efectivamente hay varias llenas de abogados. Pero mirar quién termina junto engaña: hay que preguntar quién *elige* a quién. Nuestros modelos hacen exactamente eso, y la respuesta es que un abogado, puesto frente a dos coaliciones idénticas donde una tiene más abogados, no prefiere la de los abogados (efecto marginal en 3.1, nulo en 3.3). ¿Y entonces por qué se los ve juntos? Porque hay temas que son de abogados. Es el tema el que junta a los abogados, como un asado junta parrilleros: nadie eligió a sus amigos por saber hacer fuego, pero alrededor de la parrilla terminan los que saben. Lo mismo vale para la experiencia política previa: nula en la elección de socios. Las credenciales no tejieron la red.

Las dos figuras siguientes muestran la evidencia descriptiva. Cómo leer la Figura 4: cada iniciativa tiene una "proporción de abogados" (si la firman 10 personas y 4 son abogados, vale 0.4). El panel (a) apila las 487 iniciativas en un histograma (barras azules) y lo compara con un mundo ficticio donde las mismas iniciativas hubieran sorteado a sus firmantes al azar entre los 154 (barras grises): la distribución real es más ancha que la del azar por ambos lados — sobran iniciativas casi sin abogados y sobran iniciativas cargadas de abogados. Esa doble cola es la marca de la segregación temática. El panel (b) muestra la misma proporción separada por comisión, como cajas (la caja cubre la mitad central de las iniciativas de esa comisión; la línea es la mediana; el punto rojo, el promedio). La Figura 5 resume el panel (b) en su versión más simple: el promedio por comisión como barra, contra la línea roja del peso de los abogados en la Convención entera. Ahí se lee directamente el contraste clave: en Sistemas de Justicia — jueces, fiscales, control constitucional — las coaliciones firmantes promedian 53% de abogados, catorce puntos sobre la tasa global (y el 88% de los miembros de esa comisión son abogados: tabla de la sección 2); en Principios, Derechos Fundamentales, Medio Ambiente y Conocimientos, promedian 29--31%, bajo la tasa global.

![Figura 4. (a) Proporción de abogados por iniciativa contra un sorteo aleatorio de firmantes; (b) por comisión.](../results/figures/lawyer_share_initiatives.pdf){width=100%}

![Figura 5. Promedio de la proporción de abogados de las coaliciones firmantes, por comisión; la línea roja marca la proporción de abogados en la Convención completa.](../results/figures/lawyer_share_by_commission.pdf){width=78%}

# 4. RQ2 — ¿Qué le hace la red a las personas?

## 4.1 Posiciones: selección, no influencia

Si la red influyera sobre las ideas, la posición de tus co-firmantes debería arrastrar la tuya con el tiempo. Definimos la exposición de $i$ en la onda $t$ de su comisión como la posición media de sus co-firmantes, ponderada por la intensidad de la colaboración acumulada:

$$E_{i,t} = \frac{\sum_{j \neq i} w_{ij,t}\, \theta_{j,t}}{\sum_{j \neq i} w_{ij,t}},$$

donde $w_{ij,t}$ es el número de veces que $i$ y $j$ co-firmaron desde el inicio hasta la onda $t$ (las ondas son los informes de indicaciones de cada comisión; ver Figura 1), y $\theta_{j,t}$ es el voto revelado de $j$ en ese momento. El modelo de regresión estimado es un panel con efectos fijos individuales:

$$\Delta\theta_{i,t} = \alpha_i + \beta\,\theta_{i,t-1} + \lambda\,E_{i,t-1} + \varepsilon_{it},$$

donde $\Delta\theta_{i,t} = \theta_{i,t} - \theta_{i,t-1}$ es el cambio de posición, $\alpha_i$ absorbe todo lo estable de cada convencional (el estimador "within" usa solo la variación de cada persona respecto de su propia media), $\beta$ captura la reversión a la media y $\lambda$ es el parámetro de interés: si $\lambda > 0$, me muevo hacia donde está mi vecindario. Errores agrupados por convencional; 4.224 observaciones persona-onda.

La tabla completa, incluyendo las dos preguntas de robustez que importan — ¿cambia el resultado según la ventana temporal? (¿pudo la influencia operar temprano, sobre los novatos, y agotarse?) y ¿cambia según cuánta historia carga la exposición? (¿cuál es la "dosis" relevante?):

| Ventana temporal | Definición de exposición | $\hat\lambda$ | EE | $p$ | N |
|:---|:---|:-:|:-:|:-:|:-:|
| Completa | Acumulada desde T0 | $+0.008$ | 0.005 | $0.095$ | 4.224 |
| Completa | Solo última onda | $+0.007$ | 0.013 | $0.60$ | 463 |
| Completa | Últimas 2 ondas | $+0.007$ | 0.008 | $0.41$ | 1.450 |
| Completa | Últimas 3 ondas | $+0.006$ | 0.006 | $0.33$ | 2.340 |
| Completa | Decaimiento $\lambda_w = 0.25$ | $+0.008$ | 0.004 | $0.062$ | 4.224 |
| Completa | Decaimiento $\lambda_w = 0.50$ | $+0.009$ | 0.004 | $0.042$ | 4.224 |
| Completa | Decaimiento $\lambda_w = 0.75$ | $+0.009$ | 0.005 | $0.061$ | 4.224 |
| Completa | Falsificación: exposición futura | $+0.009$ | 0.005 | $0.069$ | 3.255 |
| Temprana (ondas $\leq$ 31-mar) | Acumulada | $+0.005$ | 0.007 | $0.42$ | 1.587 |
| Temprana | Últimas 2 ondas | $+0.013$ | 0.010 | $0.19$ | 937 |
| Temprana | Últimas 3 ondas | $+0.001$ | 0.007 | $0.95$ | 1.452 |
| Temprana | Decaimiento $\lambda_w = 0.25$ | $+0.004$ | 0.006 | $0.50$ | 1.587 |
| Temprana | Decaimiento $\lambda_w = 0.50$ | $+0.005$ | 0.006 | $0.44$ | 1.587 |
| Temprana | Decaimiento $\lambda_w = 0.75$ | $+0.005$ | 0.007 | $0.42$ | 1.587 |
| Temprana | Falsificación: exposición futura | $+0.007$ | 0.007 | $0.30$ | 1.589 |
| Tardía (ondas $\geq$ 1-abr) | Acumulada | $+0.004$ | 0.005 | $0.46$ | 2.637 |
| Tardía | Solo última onda | $-0.002$ | 0.018 | $0.92$ | 332 |
| Tardía | Últimas 2 ondas | $-0.005$ | 0.008 | $0.55$ | 513 |
| Tardía | Últimas 3 ondas | $-0.002$ | 0.008 | $0.79$ | 888 |
| Tardía | Decaimiento $\lambda_w = 0.25$ | $+0.003$ | 0.005 | $0.52$ | 2.637 |
| Tardía | Decaimiento $\lambda_w = 0.50$ | $+0.004$ | 0.005 | $0.35$ | 2.637 |
| Tardía | Decaimiento $\lambda_w = 0.75$ | $+0.004$ | 0.005 | $0.39$ | 2.637 |
| Tardía | Falsificación: exposición futura | $-0.001$ | 0.005 | $0.83$ | 1.666 |

Sobre las ventanas: la ventana ideal para la hipótesis "la influencia operó temprano sobre los nuevos" sería el primer mes de la Convención, pero ahí la red de co-firma no existía todavía (las primeras iniciativas son de noviembre; los primeros informes, de enero-febrero — Figura 1). Las ventanas factibles cortan la era de colaboración activa en sus dos primeros meses (febrero-marzo de 2022) y el resto (abril-junio). En ninguna ventana ni con ninguna dosis de exposición aparece influencia: los coeficientes rondan $+0.005$ a $+0.009$ y el único que roza la significancia (decaimiento 0.50, $p = 0.042$) no se concentra en la ventana temprana, como exigiría la historia de los novatos susceptibles.

La falsificación merece su propia explicación. Si el coeficiente de la exposición *pasada* reflejara influencia causal, la exposición *futura* no debería "predecir" el cambio de hoy. Pero lo hace, y con la misma magnitud que la pasada ($+0.009$ contra $+0.008$). Esa simetría es la firma de la selección: la exposición no causa el cambio — acompaña al cambio, antes y después, porque elijo co-firmantes hacia cuya posición ya me estoy moviendo. (Una precaución que verificamos: la exposición es persistente en el tiempo, así que pasada y futura comparten señal; por eso el argumento no es "el futuro predice", sino que pasado y futuro predicen *igual* — si hubiera causalidad, el pasado debería dominar, y no domina.)

¿Y si el nulo fuera falta de poder? La versión para la abuela: imagina que quieres saber si tu grupo de amigas te cambia los gustos musicales. Hay dos formas de que el experimento fracase sin que signifique nada: que tus amigas ya tengan exactamente tus gustos (no habría nada que copiar — sin espacio), o que tu termómetro de gustos sea tan malo que no note cambios chicos (sin instrumento). Verificamos ambas. Espacio: la distancia promedio entre la posición de cada convencional y la de su vecindario es 0.59; si los vecindarios se armaran al azar sería 2.26 — la selección cerró el 74% del espacio, pero el 0.59 restante es espacio real donde la influencia se habría notado. Instrumento: el efecto mínimo detectable con nuestros datos es $\lambda = 0.013$ (es decir, habríamos detectado una influencia que cerrara apenas 1.3% de la distancia por onda); lo estimado es $0.008$, por debajo incluso de eso. Conclusión: hubo espacio y hubo instrumento — la influencia simplemente no está, o es sustantivamente despreciable. (Precisión técnica: ese 0.013 sale del error estándar del coeficiente de la regresión — la incertidumbre muestral —, no del error de medición de $\theta$, que aún no está propagado; ver sección 7.)

## 4.2 Conducta: la defección viaja por la red

La versión para la abuela primero. En cada votación, casi todos los convencionales votan igual que su lista — el libreto se respeta el 92% de las veces. Pero a veces alguien se sale del libreto. La pregunta: cuando alguien se sale, ¿se sale solo, o se sale acompañado de la gente con la que escribió iniciativas al comienzo? Y si acompañado — ¿es de verdad por esos lazos, o es casualidad de votaciones que dividen a todos?

Las definiciones, una a una. Para el convencional $i$ y la votación $v$: la defección es $D_{iv} = 1$ si $i$ votó distinto de la mayoría de su lista en $v$ (y 0 si votó con ella); ocurre en el 7.9% de los casos. La exposición a defectores es la fracción ponderada de los co-firmantes de $i$ que defeccionaron en esa misma votación:

$$X_{iv} = \frac{\sum_{j \neq i} w_{ij}\, D_{jv}}{\sum_{j \neq i} w_{ij}},$$

donde $w_{ij}$ es el peso de co-firma de la red génesis (cuántas iniciativas firmaron juntos $i$ y $j$). El modelo de regresión estimado es un logit con dos familias de efectos fijos:

$$\Pr(D_{iv} = 1) = \Lambda\big(\eta_i + \mu_v + \phi\, X_{iv}\big),$$

donde $\Lambda$ es la función logística, $\eta_i$ absorbe la propensión individual a rebelarse (hay personalidades díscolas), $\mu_v$ absorbe la votación (hay votaciones que rompen a todo el mundo), y $\phi$ es el parámetro de interés: ¿defecciono más cuando defeccionan los míos? Errores agrupados por convencional; 374.039 observaciones en la era de votaciones de normas.

El problema es que $\phi$ crudo exagera: si una votación parte a mi lista en dos, varios defeccionamos a la vez aunque no nos conozcamos. El contrafactual duro: barajamos los nombres de los defectores dentro de cada lista y votación — manteniendo exactamente cuántos defeccionaron en cada una — y re-estimamos 200 veces. Todo lo mecánico sobrevive al barajado; solo muere el alineamiento con la red. Y el chequeo adicional contra una historia alternativa: "defeccionamos juntos porque somos de la misma comisión y conocemos el artículo en tabla" — separamos la exposición según si el co-firmante es de mi comisión o de otra, controlando además la tasa de defección de mi comisión en esa votación ($C_{iv}$, la fracción de miembros de mi comisión que defeccionó, sin contarme).

| Modelo | Variable | Coef. | EE | $p$ |
|:---|:---|:-:|:-:|:-:|
| Principal | Exposición a defectores ($\phi$) | $+11.46$ | 0.57 | $<10^{-89}$ |
| Benchmark permutado (200 réplicas) | $\phi$ esperado por mecánica | $5.93$ [p95: $6.00$] | — | $< 0.005$ |
| Período completo (robustez) | $\phi$ | $+11.30$ | 0.51 | $<10^{-108}$ |
| Split por comisión | Exposición co-firmantes de otra comisión | $+8.97$ | 0.49 | $<10^{-15}$ |
| | Exposición co-firmantes de mi comisión | $+2.76$ | 0.32 | $<10^{-15}$ |
| | Tasa de defección de mi comisión ($C_{iv}$) | $-2.65$ | 0.42 | $<10^{-9}$ |

Lectura: la mitad del efecto crudo era mecánica de bloques ($11.5$ observado contra $5.9$ del mundo barajado), pero lo que sobra es enorme y real. Y la historia de "compartir sala con el artículo" queda descartada: la co-defección viaja más fuerte por los co-firmantes de *otras* comisiones que por los de la propia, y la tasa de defección de mi comisión, lejos de arrastrarme, tiene signo negativo. En una frase: la red no cambia lo que piensas (4.1), pero cuando llega el momento de desmarcarse del bloque, no te desmarcas solo — te desmarcas con los tuyos.

# 5. RQ3 — ¿Qué hace ganar?

## 5.1 El mecanismo: la supervivencia de cada artículo

Lo que sobrevive o muere no es el convencional sino el artículo. El modelo de mecanismo se estima entonces a nivel de artículo: 1.565 artículos génesis con coalición firmante de 2 a 16 personas (389 coaliciones distintas), desenlace binario $sobrevive_a = 1$ si el artículo llegó al borrador (idéntico o similar; ocurre en el 20.2% de los casos). El modelo de regresión estimado es un logit con efectos fijos de comisión:

$$\Pr(sobrevive_a = 1) = \Lambda\Big(\alpha_c + \gamma_1\, \big|\bar\theta_{1,S_a} - \theta_{1,(103)}\big| + \gamma_2\, sd(\theta_{1,S_a}) + \gamma_3\, |S_a| + \boldsymbol{\gamma_4}^\top C_{S_a} + \boldsymbol{\gamma_5}^\top H_{S_a}\Big),$$

donde $S_a$ es la coalición firmante del artículo $a$; $\bar\theta_{1,S_a}$ su posición media y $\theta_{1,(103)}$ el pívot de 2/3, de modo que el primer término es la distancia de la coalición al pívot (la teoría pivotal predice $\gamma_1 < 0$); $sd(\theta_{1,S_a})$ es la desviación estándar interna (¿coaliciones anchas sobreviven más?); $|S_a|$ el tamaño; $C_{S_a}$ el bloque de red de la coalición — grado medio, betweenness media, constraint media de sus miembros, y densidad interna (proporción de pares que habían co-firmado en otras iniciativas además de esta); y $H_{S_a}$ el bloque de capital humano — proporción de abogados, de experimentados, y grado académico medio. Todos los predictores continuos están estandarizados (los coeficientes son comparables entre sí); errores agrupados por coalición; $\alpha_c$ son efectos fijos de las siete comisiones. Los tres modelos anidados, completos:

| Variable | (1) Pivotal | (2) + Red | (3) + Capital humano |
|:---|:-:|:-:|:-:|
| Distancia de la coalición al pívot | $-0.35$ ($p=.001$) | $-0.64$ ($p<10^{-4}$) | $-0.70$ ($p<10^{-4}$) |
| Heterogeneidad ideológica $sd(\theta_1)$ | $+0.27$ ($p=.008$) | $+0.47$ ($p<10^{-3}$) | $+0.50$ ($p<10^{-3}$) |
| Tamaño de la coalición | $+0.20$ ($p=.13$) | $+0.21$ ($p=.07$) | $+0.19$ ($p=.11$) |
| Grado medio | | $+0.56$ ($p=.11$) | $+0.41$ ($p=.24$) |
| Betweenness media | | $-0.38$ ($p=.004$) | $-0.37$ ($p=.007$) |
| Constraint media | | $+0.74$ ($p=.020$) | $+0.73$ ($p=.016$) |
| Densidad interna (pares con historia) | | $+0.27$ ($p=.018$) | $+0.29$ ($p=.009$) |
| Prop. abogados | | | $-0.06$ ($p=.71$) |
| Prop. con experiencia previa | | | $-0.09$ ($p=.66$) |
| Grado académico medio | | | $-0.10$ ($p=.52$) |
| AIC | 1393 | 1372 | 1375 |

(Advertencia: grado medio y constraint media son colineales — VIF cercano a 11 — y sus coeficientes individuales se leen con cautela; el resto tiene VIF bajo 4. Con la variable dependiente continua — la similitud textual con el borrador, con los fracasos en cero — los signos y significancias se mantienen.)

La geometría manda: coaliciones lejanas al pívot mueren, y condicional a dónde está su centro, las coaliciones ideológicamente anchas sobreviven más. Los equipos consolidados (densidad interna) y de miembros incrustados (constraint alta) ganan; las coaliciones de intermediarios (betweenness) pierden. Y el capital humano no aparece: la proporción de abogados o de experimentados no predice nada — la pericia ni forma lazos (3.4) ni gana votaciones.

¿Heterogeneidad, o heterogeneidad dentro de la izquierda? Una objeción importante: el 87% de las coaliciones tiene posición media a la izquierda de cero (mediana $-0.58$) — la Convención *era* de izquierda. ¿El premio a la anchura será entonces solo dispersión dentro de la izquierda? Partimos las coaliciones en terciles de posición media y re-estimamos el modelo (2) en cada tramo:

| Tramo (posición media de la coalición) | Supervivencia | $sd(\theta_1)$ | $p$ |
|:---|:-:|:-:|:-:|
| T1: izquierda ($\bar\theta_1 \in [-0.83, -0.69]$) | 10.5% | $+1.34$ | $0.019$ |
| T2: centro-izquierda ($[-0.69, -0.41]$) | 29.0% | $+0.32$ | $0.22$ |
| T3: centro ($[-0.41, +0.81]$) | 21.0% | $-0.26$ | $0.42$ |

La respuesta es sí — y eso no debilita el resultado sino que revela el mecanismo. El premio a la heterogeneidad vive exactamente en las coaliciones más de izquierda: para ellas, "ensancharse" significa extenderse hacia el pívot, y eso es lo que salva artículos (nótese además el gradiente bruto: las coaliciones puramente de izquierda sobreviven 10.5%, las de centro-izquierda 29%). Para las coaliciones ya centradas, ensancharse no agrega nada — ya están donde hay que estar. Es la lógica de los 103 votos operando por dentro. (La versión más fina de este chequeo — clasificar los artículos según *quiénes los votaron* en el Pleno — requiere un vínculo votación-artículo que los datos aún no tienen; queda anotada como extensión.)

## 5.2 La vista agregada: el éxito se comparte

A nivel de convencional, definimos el éxito de $i$ como su retención léxica media: $y_i = \frac{1}{|A_i|} \sum_{a \in A_i} sim(a)$, donde $A_i$ son los artículos que $i$ co-firmó y $sim(a)$ es la similitud textual (coseno TF-IDF) entre el texto génesis del artículo y su versión en el borrador — con $sim(a) = 0$ si el artículo murió. El éxito así medido está fuertemente correlacionado entre vecinos de red ($I$ de Moran 0.46, $p \approx 10^{-177}$), y el modelo que lo formaliza es el espacial de Durbin:

$$y = \rho\, W y + X\beta + W X \gamma + \varepsilon,$$

donde $W$ es la red de co-patrocinio normalizada por filas (cada fila suma 1: $W y$ es, para cada convencional, el éxito promedio de sus co-firmantes), $X$ son sus atributos, $WX$ los mismos atributos promediados sobre el vecindario, $\rho$ mide el acoplamiento entre mi éxito y el de mis vecinos, y la estimación es por máxima verosimilitud. Siguiendo el comentario metodológico obvio — alguien puede ser "exitoso" solo por firmar mucho — $X$ incluye el número total de iniciativas firmadas. La tabla completa del modelo con la distancia al pívot:

| Variable | Directo ($\beta$) | $p$ | Vecindario ($\gamma$, lag) | $p$ |
|:---|:-:|:-:|:-:|:-:|
| N° de iniciativas firmadas | $-0.0003$ | 0.16 | $-0.0013$ | 0.26 |
| Grado (red) | $+0.0004$ | 0.26 | $+0.0039$ | 0.11 |
| Betweenness | $+0.0000$ | 0.93 | $-0.0009$ | 0.12 |
| Abogado | $+0.009$ | 0.15 | $+0.207$ | 0.004 |
| Experiencia previa | $+0.015$ | 0.06 | $+0.052$ | 0.53 |
| Mujer | $+0.003$ | 0.57 | $+0.142$ | 0.08 |
| Edad | $-0.0003$ | 0.20 | $-0.005$ | 0.07 |
| Grado académico | $-0.001$ | 0.84 | $-0.117$ | 0.02 |
| Voto revelado medio ($\theta$) | $-0.000$ | 1.00 | $+0.047$ | 0.01 |
| Desv. est. de $\theta$ | $-0.012$ | 0.53 | $+0.047$ | 0.72 |
| Heterofilia del ego | $-0.015$ | 0.22 | $-0.128$ | 0.07 |
| Distancia propia al pívot | $+0.011$ | 0.67 | $-0.182$ | 0.048 |
| $\rho$ (acoplamiento de red) | $0.898$ | $<10^{-15}$ | — | — |

Tres lecturas. Primera, el control de actividad responde la pregunta que lo motivó: firmar muchas iniciativas no hace más exitoso a nadie (coeficiente nulo) y las demás conclusiones no cambian al incluirlo. Segunda, el patrón general del estudio se repite: los atributos propios no predicen casi nada; los del vecindario, sí — incluida la distancia al pívot, que solo importa en su versión "de la compañía" ($-0.18$) y no en la propia ($p = 0.67$). Tercera, $\rho = 0.90$: el éxito está acoplado entre co-firmantes incluso tras todos los controles. Parte de ese acoplamiento es composición — co-firmantes comparten artículos, y por tanto las propiedades de coalición de 5.1 —, por lo que leemos 5.1 como el mecanismo y esta sección como su sombra agregada (la dependencia sobrevive además con red binaria, con otra definición de lazo, con similitud semántica en vez de léxica, y con la retención condicional: $\rho$ entre 0.79 y 0.95 en todas las variantes). Nota sobre el lag de abogado ($+0.21$): a nivel de convencional, estar rodeado de abogados acompaña al éxito, pero el modelo de mecanismo (5.1) muestra que la proporción de abogados de la coalición no salva artículos — preferimos la lectura del nivel artículo, que es el diseño limpio.

# 6. Síntesis

En una asamblea de extraños, la colaboración se organizó con lo que la gente traía puesto: el territorio (compartir distrito multiplica por seis las odds de firmar juntos), la etiqueta electoral (todas las listas coordinan patrocinio, las ad hoc igual que los pactos tradicionales — aunque ninguna compra unidad de voto más allá de su alineamiento), y la afinidad ideológica; no las credenciales (ni abogados ni experimentados se eligen entre sí — los juntan los temas). Sobre esos primeros encuentros la red se rigidiza rápido: el mejor predictor de la próxima coalición es qué pares ya trabajaron juntos, y aun así la afinidad ideológica conserva fuerza propia — conocerse y parecerse operan a la vez. Esa red no cambia las posiciones de nadie (selección, no influencia: exposición pasada y futura predicen igual, y el nulo tiene espacio e instrumento verificados), pero sí coordina la conducta en el margen: las rupturas de disciplina viajan por los lazos de co-firma, incluso entre comisiones distintas. Y cuando los textos llegan al Pleno, sobreviven los de coaliciones cercanas al pívot de 2/3, ideológicamente anchas — el ensancharse salva artículos precisamente a las coaliciones de izquierda, que necesitan estirarse hacia el 103 — y relacionalmente consolidadas; no los de coaliciones con más títulos. El capital que rindió fue territorial, posicional y relacional. Se colaboró por cercanía; se ganó por amplitud.

# 7. Limitaciones y trabajo en curso

1. Los puntos ideales se estiman de votos: la distinción entre ideología y disciplina estable (sección 3.2) es parcialmente circular, y el error de medición de $\theta$ no está propagado a los modelos (el efecto mínimo detectable de 4.1 refleja solo incertidumbre muestral). Está diseñado el bootstrap paramétrico para ambas cosas.
2. La familiaridad del modelo de eventos mezcla dependencia del estado con afinidades estables no observadas; la ventana es corta (tres meses) y un tercio de los eventos cae el día del plazo, donde no existe orden intradiario.
3. 39 iniciativas del análisis no tienen fecha (quedan fuera del modelo de eventos) y 41 con más de 16 firmantes están en auditoría; 21 convencionales de listas locales esperan un crosswalk fino de conglomerado.
4. En el modelo de supervivencia, grado medio y constraint media de la coalición son colineales (VIF $\approx$ 11).
5. Con $\rho$ cercano a 1, la descomposición de impactos del modelo espacial es numéricamente inestable; se reportan coeficientes y $\rho$.

En curso: robustez del modelo de eventos (excluir el bloque del plazo; imputar fechas faltantes); la variante dirigida (quién recluta a quién, con el autor principal de cada iniciativa); la extensión del modelo de eventos y de supervivencia a las indicaciones (incorporar a quienes modificaron cada artículo, no solo a quienes lo iniciaron); y el vínculo votación-artículo para clasificar artículos por su coalición de votantes.
