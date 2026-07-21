---
title: "Collaboration Networks in a Tabula Rasa Legislature"
subtitle: "Tie Formation, Voting Behavior, and Legislative Success in Chile's Constitutional Convention (2021--2022)"
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
- Iniciativas: 995 ingresadas a la plataforma oficial de la Convención entre nov-2021 y feb-2022, con la lista de firmantes armonizada al padrón de 154.
    - 947 tienen entre 2 y 16 firmantes-persona y forman el set de análisis; 46 registran más de 16 firmantes — imposible bajo la regla del reglamento — y quedan excluidas mientras se auditan como duplicaciones de documentos transversales entre comisiones; 2 tienen un solo firmante recuperable.
    - Todas tienen fecha de ingreso (3-nov-2021 al 2-feb-2022; 123 del set de análisis con fecha imputada desde las notas del propio registro). 339 — un 36% — ingresaron el día del plazo final.
    - 827 del set de análisis tienen comisión temática asignada en la plataforma; las 120 restantes entran a la red agregada y al modelo de eventos, pero no a los análisis por comisión.
- Red de co-patrocinio: la proyección de las firmas sobre pares de convencionales.
    - 154 nodos y 6.946 aristas; el peso $w_{ij}$ = número de iniciativas que $i$ y $j$ co-firmaron.
    - Firmar no escasea para el firmante: mediana 62 iniciativas firmadas por convencional, máximo 227 (Figura 2a).
    - El tope legal de 16 patrocinantes se llenaba con frecuencia: es el valor modal (Figura 2b).
- Trazabilidad de textos: 1.809 artículos génesis con desenlace conocido frente al borrador del 14-mayo-2022.
    - 498 artículos llegaron al borrador (tasa de supervivencia 20%); el resto murió en comisión o en el Pleno.
    - Cada artículo conserva su texto original, sus autores y el historial fechado de indicaciones que recibió.
- Votaciones: 4.707 votaciones nominales del Pleno (jul-2021 a jun-2022), base del voto revelado y de los puntos ideales.

![Figura 2. (a) Iniciativas firmadas por convencional; (b) firmantes por iniciativa, con la regla 8--16.](../results/figures/signature_distributions.pdf){width=100%}

Las siete comisiones temáticas difieren fuertemente en composición y en productividad — diferencias que los modelos explotan y controlan:

**Tabla 1 — Las siete comisiones temáticas: composición y producción documental.**

| | Nombre (corto) | Miembros | % abog. | % exper. | Edad | Grado (0--3) | Iniciativas | Ondas ind. | Indicaciones | Ind. multifirm. |
|:-:|:---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| C1 | Sistema Político | 25 | 60 | 36 | 42.2 | 1.44 | 89 | 4 | 365 | 266 |
| C2 | Principios Const. | 18 | 22 | 22 | 45.8 | 1.28 | 114 | 3 | 53 | 1 |
| C3 | Forma de Estado | 30 | 37 | 40 | 45.1 | 1.43 | 51 | 6 | 241 | 212 |
| C4 | Derechos Fund. | 30 | 30 | 13 | 45.9 | 1.03 | 283 | 5 | 395 | 154 |
| C5 | Medio Ambiente | 19 | 21 | 16 | 44.7 | 1.26 | 133 | 5 | 559 | 0 |
| C6 | Sist. de Justicia | 17 | 88 | 18 | 42.6 | 1.65 | 93 | 6 | 381 | 317 |
| C7 | Conocimientos | 15 | 13 | 0 | 51.7 | 1.33 | 64 | 8 | 332 | 54 |

(Iniciativas = con 2--16 firmantes asignadas a la comisión en la plataforma; otras 120 del set de análisis no tienen comisión asignada. Ondas ind. = número de informes de indicaciones de la comisión. Indicaciones = actos de enmienda registrados con autoría, deduplicados. Ind. multifirm. = el subconjunto de esas indicaciones con dos o más firmantes — las únicas que agregan lazos a la red de la sección 4. Los ceros de C5 son una limitación de registro: sus indicaciones fueron colectivas pero los informes anotan solo al primer firmante.)

La diferencia de composición más visible es la de los abogados, y conviene retenerla desde ya porque reaparece en la sección 3.4: la Figura 3 muestra el promedio de la proporción de abogados de las coaliciones firmantes de cada comisión, contra la línea de la tasa global (39%).

![Figura 3. Promedio de la proporción de abogados de las coaliciones firmantes, por comisión; la línea roja marca la proporción de abogados en la Convención completa.](../results/figures/lawyer_share_by_commission.pdf){width=62%}

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
- $comision_{ia} \in \{0,1\}$: si $i$ pertenece a la comisión temática del texto. Cualquier convencional podía patrocinar iniciativas de cualquier comisión, y de hecho lo hacía: el 72% de las firmas observadas proviene de convencionales de otra comisión. La variable mide, entonces, una inclinación real y no una restricción: los miembros de la comisión son el 12% del pool de candidatos pero aportan el 28% de las firmas.
- $lista^c_{ia} \in \{0,1\}$: si la lista de $i$ coincide con la lista mayoritaria de la coalición, con un coeficiente $\lambda_c$ distinto por conglomerado $c$.
- $distrito_{ia} \in [0,1]$: proporción de la coalición que comparte distrito (o pueblo originario) con $i$.
- $abogado_{ia} = abogado_i \times$ (proporción de abogados en $S_a$), y análogos para experiencia y género.
- $d^{grado}_{ia} = |grado_i - \overline{grado}_{S_a}|$: distancia en credenciales educativas.

El modelo de regresión efectivamente estimado es el logit condicional de McFadden: la probabilidad del conjunto de firmantes observado, condicional a su tamaño, es

$$P\big(S_a \,\big|\, |S_a|\big) = \frac{\exp\big(\sum_{i \in S_a} \beta^\top x_{ia}\big)}{\sum_{R \subseteq \{1..154\},\, |R| = |S_a|} \exp\big(\sum_{i \in R} \beta^\top x_{ia}\big)},$$

y $\hat\beta$ maximiza $\ell(\beta) = \sum_a \log P(S_a \mid |S_a|)$. Como $\alpha_a$ es común a todos los candidatos de la iniciativa $a$, aparece idéntico en numerador y denominador y se cancela: no hay que estimarlo, y $\beta$ queda identificado solo por comparaciones entre convencionales frente al mismo menú. La suma del denominador se aproxima con el método de Efron (estándar para este diseño), y los errores estándar se agrupan por convencional. Datos: 949 menús $\times$ 154 convencionales = 145.838 decisiones.

La tabla completa, en cuatro bloques: primero los controles (la estructura y las posiciones que se formaron dentro de la Convención), después los tres bloques de observables pre-Convención — listas, territorio, y perfil personal:

**Tabla 2 — Logit condicional de elección de firma (modelo principal).**

| Variable | Coef. | OR | EE | $p$ |
|:---|:-:|:-:|:-:|:-:|
| *Controles: estructura y posiciones formadas en la Convención* | | | | |
| Misma comisión que el texto | $+0.92$ | 2.5 | 0.05 | $<10^{-69}$ |
| Distancia en $\theta_1$ a la coalición | $-3.64$ | 0.03 | 0.24 | $<10^{-51}$ |
| Distancia en $\theta_2$ a la coalición | $-1.37$ | 0.25 | 0.14 | $<10^{-21}$ |
| *Listas electorales (pre-Convención)* | | | | |
| Misma lista: Escaños Reservados PPOO | $+1.44$ | 4.2 | 0.21 | $<10^{-11}$ |
| Misma lista: Otras listas locales | $+1.33$ | 3.8 | 0.16 | $<10^{-16}$ |
| Misma lista: Lista del Pueblo | $+1.07$ | 2.9 | 0.17 | $<10^{-10}$ |
| Misma lista: Lista del Apruebo | $+1.04$ | 2.8 | 0.19 | $<10^{-7}$ |
| Misma lista: Vamos por Chile | $+0.95$ | 2.6 | 0.21 | $<10^{-5}$ |
| Misma lista: Apruebo Dignidad | $+0.93$ | 2.5 | 0.14 | $<10^{-10}$ |
| *Territorio (pre-Convención)* | | | | |
| Afinidad de distrito/pueblo | $+2.45$ | 11.6 | 0.57 | $<10^{-4}$ |
| *Perfil pre-Convención* | | | | |
| Afinidad de abogados | $+0.14$ | 1.1 | 0.18 | $0.439$ |
| Afinidad de experiencia previa | $+0.51$ | 1.7 | 0.23 | $0.026$ |
| Distancia de grado académico | $-0.09$ | 0.9 | 0.08 | $0.237$ |
| Afinidad de género | $+0.29$ | 1.3 | 0.17 | $0.076$ |
| AIC = 87.344; pseudo-$R^2$ (McFadden) = 0.209 | | | | |

(La lista Independientes No Neutrales, con 3 miembros, nunca es mayoritaria en una coalición y su coeficiente no está identificado.)

Tres lecturas. Primera, la sorpresa territorial: compartir distrito es el predictor pre-Convención más fuerte — pasar de una coalición sin coterráneos a una llena de ellos multiplica las odds de firmar por casi doce. Es conocimiento disponible el día uno: dos convencionales del mismo distrito hicieron campaña en el mismo territorio, comparten electores y problemas locales. Segunda, la lista coordina en todos los conglomerados (todos los $\lambda_c$ entre 0.9 y 1.4, con los escaños reservados y las listas locales en la parte alta): también predecible ex ante. Tercera, las credenciales se parten en dos: ser abogado no organiza la firma en absoluto (y la educación tampoco), pero la experiencia institucional previa sí — dos personas con carrera pública tienen odds 1.7 veces mayores de terminar en la misma coalición (la sección 3.4 muestra que ese eco no llega a marcar coaliciones reales en el modelo dinámico). Los controles se comportan como se espera: la distancia ideológica es el mayor inhibidor del modelo (una unidad de $\theta_1$ divide las odds por casi 40) y la comisión multiplica las odds por 2.5 — pero son variables *formadas dentro* de la Convención, y por eso las tratamos como controles y no como hallazgo.

El mismo modelo, agregando el término de escaños reservados y sus interacciones (segunda columna: ¿los PPOO eligen distinto?):

**Tabla 3 — Logit condicional con interacciones de escaños reservados (PPOO).**

| Variable (modelo con interacciones PPOO) | Coef. | EE | $p$ |
|:---|:-:|:-:|:-:|
| PPOO (efecto base) | $-0.36$ | 0.36 | $0.31$ |
| PPOO $\times$ distancia en $\theta_1$ | $+0.65$ | 0.60 | $0.27$ |
| PPOO $\times$ distancia en $\theta_2$ | $-0.66$ | 0.58 | $0.26$ |
| Misma lista PPOO | $+1.89$ | 0.38 | $<10^{-6}$ |
| Afinidad de distrito/pueblo | $+2.26$ | 0.56 | $<10^{-4}$ |
| AIC = 87.232; pseudo-$R^2$ = 0.210 | | | |

Con el pool completo de iniciativas, ninguna interacción PPOO es significativa: una vez controlados su lista (la más cohesionada del modelo) y su territorio, los escaños reservados no eligen distinto del resto. El contraste que sí existe aparece recién al mirar la pendiente ideológica *por lista* (efectos principales de lista más interacciones con la distancia en $\theta_1$; referencia = Vamos por Chile):

**Tabla 4 — Robustez: pendiente ideológica por lista (interacciones con la distancia en $\theta_1$).**

| Lista | Pendiente base (VC) | Interacción | Pendiente total |
|:---|:-:|:-:|:-:|
| Vamos por Chile | $-6.10$ | — | $-6.10$ |
| Independientes No Neutrales | $-6.10$ | $+0.71$ (n.s.) | $-5.38$ |
| Otras listas locales | $-6.10$ | $+1.96$ | $-4.13$ |
| Lista del Apruebo | $-6.10$ | $+2.69$ | $-3.41$ |
| Apruebo Dignidad | $-6.10$ | $+3.02$ | $-3.08$ |
| Escaños Reservados PPOO | $-6.10$ | $+3.26$ | $-2.83$ |
| Lista del Pueblo | $-6.10$ | $+4.14$ | $-1.95$ |
| AIC = 86.821; pseudo-$R^2$ = 0.213 | | | |

La lectura honesta: los PPOO están entre los más planos, pero no solos — la Lista del Pueblo lo es incluso más, y la diferencia entre ambos no es estadísticamente distinguible. El patrón grueso es una asimetría izquierda-derecha: la derecha (VC) casi no cruza distancias ideológicas al firmar; los bloques de izquierda cruzan mucho más. Lo que sigue distinguiendo a los PPOO es la combinación de puentes ideológicos largos con la mayor cohesión interna de lista del modelo ($\lambda = 1.44$).

## 3.2 Las listas: coordinación sin disciplina

¿Funcionaron las listas ad hoc como partidos? Un partido hace al menos dos cosas por sus miembros: los ayuda a coordinar con quién trabajan, y los hace votar juntos.

La primera está medida en la tabla de 3.1, en las filas "Misma lista": cada $\lambda_c$ es el coeficiente del indicador "mi lista coincide con la lista mayoritaria de esta coalición", estimado por separado para cada conglomerado $c$. En palabras: $\lambda_c$ mide cuánto más probable es que un miembro de la lista $c$ firme una iniciativa cuando la coalición ya está dominada por su propia gente — todo lo demás igual, incluida la cercanía ideológica. Es la firma de la coordinación organizacional: si las listas fueran meras etiquetas sin vida interna, sus miembros firmarían con los suyos solo en la medida en que los suyos piensan parecido, y $\lambda_c$ sería cero. El recuento:

**Tabla 5 — Coordinación de firma por lista: los coeficientes $\lambda_c$ de la Tabla 2, reunidos.**

| Conglomerado | $\hat\lambda_c$ | OR |
|:---|:-:|:-:|
| Escaños Reservados PPOO | $1.44$ | 4.2 |
| Otras listas locales | $1.33$ | 3.8 |
| Lista del Pueblo | $1.07$ | 2.9 |
| Lista del Apruebo | $1.04$ | 2.8 |
| Vamos por Chile | $0.95$ | 2.6 |
| Apruebo Dignidad | $0.93$ | 2.5 |

Todos positivos y de magnitud comparable (odds entre 2.5 y 4.2), con los escaños reservados y las listas locales en la parte alta: la lista ad hoc por excelencia (Lista del Pueblo) coordinó firmas igual que los pactos de partidos tradicionales.

Para la segunda — votar juntos — usamos el índice de Rice, que conviene explicar con calma.

Para una lista $\ell$ y una votación $v$, sean $Y_{\ell v}$ los miembros de la lista que votaron a favor y $N_{\ell v}$ los que votaron en contra. El índice de Rice es

$$R_{\ell v} = \frac{|Y_{\ell v} - N_{\ell v}|}{Y_{\ell v} + N_{\ell v}}.$$

La intuición: si la lista vota en bloque (todos sí o todos no), el numerador iguala al denominador y $R = 1$; si se parte por la mitad, $R = 0$. Se computa sobre las 4.707 votaciones nominales del Pleno (jul-2021 a jun-2022), contando solo votos sí/no (abstenciones y ausencias fuera) y solo cuando al menos 5 miembros de la lista votaron. Promediando sobre votaciones, $\bar R_\ell$ mide cuán "en bloque" vota una lista. Pero un $\bar R$ alto no prueba disciplina: gente que piensa parecido vota parecido sin que nadie la discipline. Por eso el benchmark: para cada lista construimos 500 pseudo-listas — grupos ficticios del mismo tamaño, sorteados entre convencionales de ideología similar a la de la lista real — y preguntamos si la lista real vota más unida que sus dobles ficticios. El "premio de disciplina" es la diferencia.

**Tabla 6 — Cohesión de voto: índice de Rice real contra pseudo-listas emparejadas por ideología.**

| Lista | $\bar R$ real | $\bar R$ pseudo | Premio | $p$ |
|:---|:-:|:-:|:-:|:-:|
| Vamos por Chile | 0.855 | 0.833 | $+0.023$ | 0.32 |
| Apruebo Dignidad | 0.816 | 0.808 | $+0.008$ | 0.32 |
| Lista del Apruebo | 0.713 | 0.740 | $-0.027$ | 0.91 |
| Lista del Pueblo | 0.873 | 0.858 | $+0.015$ | 0.10 |
| Escaños Reservados PPOO | 0.875 | 0.859 | $+0.015$ | 0.16 |

Ninguna lista tiene premio. Pero antes de concluir "no hubo disciplina", hay que confesar un problema — y esta es la explicación para la abuela. Imagina que quieres saber si los hinchas de un club van al estadio porque aman al club o porque sus amigos van. El problema: conociste quiénes son hinchas *mirando quién va al estadio*. Con ese dato no puedes separar amor y amistad — están pegados en la misma observación. Aquí pasa igual: la "ideología" de cada convencional la medimos con sus votos, y si una lista disciplinó los votos desde el día uno, esa disciplina quedó *dentro* de lo que llamamos ideología. Las pseudo-listas emparejadas por ideología llevan la disciplina escondida adentro, y el premio sale cero por construcción parcial. Este problema tiene nombre en la literatura (Krehbiel: ¿partidos o preferencias?) y no tiene solución con votos solamente. La conclusión honesta es: ninguna lista vota más unida *de lo que su alineamiento estable ya implica*. Lo que sí es limpio es el contraste con la sección anterior: en la conducta de *firma* — que no entra en la medición de ideología — las listas sí coordinan, todas y en magnitud similar. La Figura 4 agrega la dinámica: la Lista del Pueblo se desploma en dic-2021/ene-2022, su fragmentación documentada, y se recompone después.

![Figura 4. Cohesión de voto (Rice mensual) por lista.](../results/figures/rice_cohesion_monthly.pdf){width=100%}

## 3.3 ¿Afines o conocidos? El modelo de eventos (RHEM)

El logit condicional mira cada iniciativa como si fuera la primera de la historia. Pero las iniciativas ocurrieron en orden — tenemos la fecha de cada una — y el orden permite hacer la pregunta que ninguna foto responde: cuando dos personas firman juntas, ¿es porque se parecen, o porque ya habían firmado juntas antes? Parecerse y conocerse suelen venir juntos, y solo un modelo con tiempo puede separarlos.

La idea del modelo de hipereventos relacionales (RHEM), contada primero sin fórmulas. La historia de la Convención es una película de 947 escenas; en cada escena, una coalición concreta aparece firmando una iniciativa. Para cada escena preguntamos: de todas las coaliciones de ese mismo tamaño que podrían haberse formado ese día, ¿por qué exactamente esta? Y respondemos comparando la coalición real con 50 coaliciones ficticias del mismo tamaño (25 sorteadas entre los 154 convencionales y 25 entre los miembros de la comisión del texto, para que la comparación sea exigente). El modelo aprende qué características separan las coaliciones reales de las ficticias. Algunas características son de composición — las mismas del logit condicional: compacidad ideológica, misma lista, mismo distrito. La novedad son las características de historia: cuánto habían firmado antes, hasta el día anterior, los miembros de la coalición candidata.

Las variables de historia se llaman repetición de subconjuntos ($sub.rep$) y se entienden mejor con un ejemplo. Tomemos la coalición candidata {Ana, Berta, Carlos} evaluada el 10 de enero:

- $sub.rep^{(1)}$ (actividad): de los tres, ¿cuántas iniciativas había firmado cada uno antes del 10 de enero? Se promedia. Mide si la coalición está hecha de firmantes activos.
- $sub.rep^{(2)}$ (familiaridad de pares): para cada par — Ana-Berta, Ana-Carlos, Berta-Carlos — ¿en cuántas iniciativas anteriores aparecieron *juntos*? Se promedia sobre los tres pares. Mide si la coalición está hecha de duplas que ya trabajaron juntas.
- $sub.rep^{(3)}$ (familiaridad de tríos): ¿en cuántas iniciativas anteriores aparecieron los tres *a la vez*? Mide si la coalición recicla equipos completos.

Formalmente, si $deg(t, h')$ cuenta los eventos anteriores a $t$ que contienen al subconjunto $h'$, entonces para una coalición candidata $h$:

$$sub.rep^{(p)}(t, h) = \binom{|h|}{p}^{-1} \sum_{h' \subseteq h,\ |h'| = p} deg(t, h'),$$

donde $\binom{|h|}{p}$ es el número de subconjuntos de tamaño $p$ que tiene $h$ (con eso la suma se vuelve promedio). Cada estadística se calcula en dos versiones de memoria: infinita (todo el pasado pesa igual) y con semivida de 15 días (un evento de hace dos semanas pesa la mitad: cada evento pasado se pondera por $w(\Delta) = e^{-\Delta \ln 2 / 15}$, con $\Delta$ los días transcurridos; una semivida de 30 días daba resultados casi indistinguibles de la memoria infinita, así que la robustez usa la memoria corta, que es la exigente).

El modelo de regresión efectivamente estimado es, de nuevo, un logit condicional — pero ahora el estrato es el evento fechado, el "menú" son la coalición real y sus 50 controles, y las covariables incluyen la historia:

$$P\big(h_e \text{ real} \,\big|\, \text{estrato } e\big) = \frac{\exp\big(\theta^\top s(t_e, h_e)\big)}{\sum_{h \in \{h_e\} \cup \text{controles}_e} \exp\big(\theta^\top s(t_e, h)\big)},$$

con las mismas covariables de composición del logit condicional (para que las tablas se lean en paralelo) más las tres estadísticas de historia: $s(t,h)$ reúne la dispersión ideológica de la coalición en cada dimensión (la distancia promedio entre sus pares), la proporción de pares de la misma lista, del mismo distrito/pueblo, ambos abogados, ambos con experiencia, ambos mujeres, la dispersión de grado académico, la proporción de miembros de la comisión del texto, y $sub.rep^{(1,2,3)}$. Todas las covariables se estandarizan (media 0, desviación 1), así que cada coeficiente responde: si esta característica sube una desviación estándar, ¿cuánto más "real" parece la coalición? La estimación se repite con 10 sorteos independientes de controles; la tabla reporta el promedio (los coeficientes apenas se mueven entre sorteos) y la mediana de los $p$, con los mismos bloques que la tabla de 3.1:

Como estadística de historia, el modelo principal usa solo $sub.rep^{(2)}$: el par es la unidad mínima de una relación ($sub.rep^{(1)}$ mide actividad individual, no relación, y $sub.rep^{(3)}$ recicla la información de los pares), y el párrafo siguiente muestra qué pasa cuando entran las tres a la vez.

**Tabla 7 — RHEM sobre las 947 iniciativas fechadas (especificación principal, $sub.rep^{(2)}$).**

| Variable | Memoria infinita | $p$ | Semivida 15 días | $p$ |
|:---|:-:|:-:|:-:|:-:|
| *Controles: estructura y posiciones formadas en la Convención* | | | | |
| Prop. de la comisión del texto (‡) | $+0.05$ | $0.81$ | $+0.14$ | $0.55$ |
| Dispersión ideológica $\theta_1$ | $-2.67$ | $2\times10^{-40}$ | $-2.62$ | $4\times10^{-38}$ |
| Dispersión ideológica $\theta_2$ | $-0.33$ | $0.042$ | $-0.37$ | $0.026$ |
| *Listas electorales (pre-Convención)* | | | | |
| Prop. pares misma lista | $+0.93$ | $3\times10^{-8}$ | $+1.05$ | $2\times10^{-9}$ |
| *Territorio (pre-Convención)* | | | | |
| Prop. pares mismo distrito/pueblo | $+0.58$ | $2\times10^{-7}$ | $+0.57$ | $2\times10^{-7}$ |
| *Perfil pre-Convención* | | | | |
| Prop. pares ambos abogados | $+0.06$ | $0.39$ | $+0.07$ | $0.54$ |
| Prop. pares ambos con experiencia | $+0.22$ | $0.33$ | $+0.26$ | $0.23$ |
| Dispersión de grado académico | $+0.35$ | $0.031$ | $+0.37$ | $0.030$ |
| Prop. pares ambas mujeres | $-0.10$ | $0.47$ | $-0.05$ | $0.74$ |
| *Historia de co-firma (lo nuevo del RHEM)* | | | | |
| $sub.rep^{(2)}$ — familiaridad de pares | $+2.10$ | $8\times10^{-21}$ | $+2.05$ | $2\times10^{-20}$ |
| Log-verosimilitud (re-muestreo 1); AIC | $-110.9$; $242$ | | $-112.8$; $246$ | |

(‡) No interpretable: la mitad de los controles se sortea dentro de la comisión, así que este contraste queda absorbido por el diseño; el efecto comisión ya está medido en 3.1.

¿Y las otras dos estadísticas de historia? Las tres son casi la misma variable con distinto zoom (correlación 0.69 entre actividad y pares, 0.82 entre pares y tríos: la gente activa acumula pares familiares, y los pares familiares componen tríos). Ajustadas de a una, las tres son positivas y de tamaño casi idéntico: actividad $+2.52$, pares $+2.57$, tríos $+2.65$ — pero la de pares es la que mejor ajusta sola (log-verosimilitud $-111$ contra $-190$ y $-115$). Y en la robustez con las tres juntas, la de pares absorbe toda la señal ($+4.48$) mientras actividad ($-5.23$) y tríos ($-0.87$, n.s.) se vuelven negativas — el reparto engañoso típico entre variables casi colineales, no una paradoja. La lectura conjunta: lo que distingue a una coalición real es específicamente la familiaridad de sus pares; condicional en ella, "mucha actividad individual sin familiaridad mutua" es marca de coalición ficticia (así lucen los controles: firmantes seriales que no se conocen), y los tríos no agregan sobre los pares. En una frase: la Convención se tejió de a dos — se reclutaban duplas consolidadas, no equipos completos. Entre memorias, la infinita ajusta levemente mejor que la semivida de 15 días (log-verosimilitud $-110.9$ contra $-112.8$), y los coeficientes casi no se mueven: en la ventana de tres meses del proceso, el capital de co-firma no muestra señales de evaporarse ni siquiera a escala de dos semanas. Un detalle nuevo: la dispersión de grado académico entra *positiva* ($+0.35$, $p = 0.03$) — las coaliciones reales mezclan niveles educativos más que el azar, otra señal de que las credenciales no segregan.

Qué agrega el RHEM sobre el logit condicional — la comparación en detalle. Los dos modelos usan los mismos eventos y la misma forma estadística; difieren en una sola cosa: el RHEM deja que el pasado entre a la ecuación. Eso tiene tres consecuencias. (i) El logit condicional es el RHEM del primer día: cuando nadie ha firmado con nadie, las estadísticas de historia valen cero para todos y el RHEM se reduce exactamente al logit de 3.1 — por eso no son rivales sino el mismo modelo en dos momentos, y por eso los coeficientes de composición del logit deben leerse como "la fuerza de los atributos cuando no hay historia que consultar" (los primeros momentos). (ii) La dinámica que el RHEM revela es acumulativa: cada co-firma de hoy se vuelve familiaridad mañana, y esa familiaridad es el predictor más fuerte de la próxima coalición — un mecanismo de rieles: los primeros encuentros (guiados por distrito, lista e ideología, como muestra el logit) crean los pares por los que después circula todo lo demás. (iii) La pregunta contrafactual cambia: el logit pregunta "¿a quién se parece el que firma?"; el RHEM pregunta "dado todo lo que ya pasó, ¿quién más podría haber firmado hoy?". Que la ideología ($-2.67$) sobreviva con toda su fuerza en la segunda pregunta es el hallazgo: la homofilia no era un espejismo de la historia acumulada. Y que lista y distrito también sobrevivan dice que la organización territorial y de etiqueta opera en cada coalición nueva, no solo en la primera.

Una cautela final: la "familiaridad" medida solo ve lo firmado desde noviembre de 2021. Amistades previas, militancias compartidas u otras afinidades estables que no observamos quedan dentro de $sub.rep^{(2)}$; por eso la llamamos persistencia relacional y no amistad.

## 3.4 Lo que no organiza la red: la profesión

La versión para la abuela. Uno esperaría que los abogados de la Convención se buscaran entre ellos para escribir juntos — al fin y al cabo, escribir una constitución es trabajo de abogados. Y si uno mira las iniciativas, efectivamente hay varias llenas de abogados. Pero mirar quién termina junto engaña: hay que preguntar quién *elige* a quién. Nuestros modelos hacen exactamente eso, y la respuesta es que un abogado, puesto frente a dos coaliciones idénticas donde una tiene más abogados, no prefiere la de los abogados (nulo en 3.1, nulo en 3.3). ¿Y entonces por qué se los ve juntos? Porque hay temas que son de abogados. Es el tema el que junta a los abogados, como un asado junta parrilleros: nadie eligió a sus amigos por saber hacer fuego, pero alrededor de la parrilla terminan los que saben. La experiencia política previa es el matiz: sí aparece en la elección de socios (3.1, odds 1.7), aunque no llega a marcar coaliciones reales una vez que la historia entra al modelo (3.3). La profesión, en cambio, no tejió la red.

La Figura 5 muestra la evidencia descriptiva. Cómo leerla: cada iniciativa tiene una "proporción de abogados" (si la firman 10 personas y 4 son abogados, vale 0.4). El panel (a) apila las 947 iniciativas en un histograma (barras azules) y lo compara con un mundo ficticio donde las mismas iniciativas hubieran sorteado a sus firmantes al azar entre los 154 (barras ámbar); las curvas suavizadas del color de cada histograma dibujan la forma de cada distribución, y las líneas punteadas verticales marcan sus medias. La distribución real es más ancha que la del azar por ambos lados — sobran iniciativas casi sin abogados y sobran iniciativas cargadas de abogados. Esa doble cola es la marca de la segregación temática. El panel (b) muestra la misma proporción separada por comisión, como cajas (la caja cubre la mitad central de las iniciativas de esa comisión; la línea es la mediana; el punto rojo, el promedio); la Figura 3 (sección 2) es su resumen en barras. Ahí se lee directamente el contraste clave: en Sistemas de Justicia — jueces, fiscales, control constitucional — las coaliciones firmantes promedian 47% de abogados, ocho puntos sobre la tasa global (y el 88% de los miembros de esa comisión son abogados: Tabla 1); en Derechos Fundamentales, Medio Ambiente y Conocimientos, promedian 27--33%, bajo la tasa global.

![Figura 5. (a) Proporción de abogados por iniciativa contra un sorteo aleatorio de firmantes; (b) por comisión.](../results/figures/lawyer_share_initiatives.pdf){width=100%}

## 3.5 La misma pregunta sin proyectar: ERGM bipartito por comisión

Todos los modelos anteriores trabajan sobre la red *proyectada*: convertimos cada iniciativa en lazos entre pares de firmantes. Esa conversión tiene un costo conocido: una sola iniciativa de 16 firmantes fabrica 120 pares de una vez, así que un acto colectivo grande "pesa" mucho más que varios chicos, y parte de la estructura que vemos entre pares es un artefacto aritmético de los tamaños. La forma de mirar los datos sin ese artefacto es la red bipartita: dos tipos de nodos — los 154 convencionales y las iniciativas de la comisión — y un lazo convencional–iniciativa por cada firma. Nada se proyecta; cada firma cuenta una vez.

Sobre esa red estimamos un ERGM (exponential random graph model), que conviene explicar en una frase: es un modelo de probabilidad sobre la red completa, donde cada término pregunta "¿el patrón X aparece en la red real más (o menos) que en redes aleatorias comparables?", y su coeficiente es el análogo de un log-odds: positivo = el patrón sobra respecto del azar, negativo = falta. Los términos que usamos:

- *edges*: la propensión base a firmar (el intercepto del modelo);
- *miembro*: ¿los miembros de la comisión firman las iniciativas de su comisión más que el resto?
- *misma lista, mismo quintil de $\theta_1$, ambos abogados, ambos con experiencia, mismo género*: cada uno cuenta los pares de co-firmantes de una misma iniciativa que comparten ese atributo — homofilia de co-firma medida sin proyección.

¿Por qué siete modelos y no uno? Por dos razones, una sustantiva y una práctica. La sustantiva: la comisión es el mayor confundidor de composición (la sección 3.4 mostró que los temas arman las coaliciones), y estimar dentro de cada comisión es condicionar por ese confundidor por diseño — en un modelo único de las 947 iniciativas, con un solo intercepto, las diferencias de composición *entre* comisiones contaminan la homofilia *dentro* de cada una (lo verificamos: el modelo agregado invierte los signos de la homofilia, la paradoja de Simpson de manual). La práctica: en estas redes la estimación MCMC completa toma horas por comisión; la tabla reporta máxima pseudo-verosimilitud (MPLE), que entrega los mismos puntos en segundos pero errores estándar que subestiman la incertidumbre — las estrellas se leen como indicativas, y el run MCMC completo (en curso, nocturno) entrega la inferencia definitiva. Los siete modelos ($^{*}$ $p<.05$, $^{**}$ $p<.01$, $^{***}$ $p<.001$; errores estándar completos en `M1_bipartite_commissions.csv`):

**Tabla 8 — ERGM bipartito por comisión (siete modelos, estimación MPLE).**

| Término | C1 | C2 | C3 | C4 | C5 | C6 | C7 |
|:---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| Edges (base) | $-3.36^{***}$ | $-4.06^{***}$ | $-3.68^{***}$ | $-3.82^{***}$ | $-3.82^{***}$ | $-3.56^{***}$ | $-3.81^{***}$ |
| Miembro de la comisión | $+0.74^{***}$ | $+1.20^{***}$ | $+1.76^{***}$ | $+0.57^{***}$ | $+1.59^{***}$ | $+1.30^{***}$ | $+2.32^{***}$ |
| Misma lista | $+0.20^{***}$ | $+0.16^{***}$ | $+0.20^{***}$ | $+0.17^{***}$ | $+0.18^{***}$ | $+0.21^{***}$ | $+0.22^{***}$ |
| Mismo quintil $\theta_1$ | $+0.15^{***}$ | $+0.16^{***}$ | $+0.12^{***}$ | $+0.14^{***}$ | $+0.16^{***}$ | $+0.12^{***}$ | $+0.16^{***}$ |
| Ambos abogados | $-0.10^{***}$ | $-0.04^{**}$ | $-0.08^{***}$ | $-0.03^{***}$ | $-0.02^{*}$ | $-0.04^{*}$ | $-0.01$ |
| Ambos con experiencia | $+0.04^{**}$ | $+0.06^{***}$ | $+0.05^{***}$ | $+0.05^{***}$ | $+0.05^{***}$ | $+0.05^{***}$ | $+0.02$ |
| Mismo género | $-0.04^{*}$ | $-0.01$ | $-0.06^{*}$ | $-0.02^{*}$ | $-0.06^{***}$ | $-0.09^{***}$ | $-0.04^{*}$ |

Cómo leer el conjunto. Primero, la pertenencia: en las siete comisiones, ser miembro multiplica la propensión a firmar los textos de esa comisión ($+0.57$ a $+2.32$). Segundo — el resultado central de la sección — la homofilia de lista ($+0.16$ a $+0.22$) y la ideológica ($+0.12$ a $+0.16$) son positivas y notablemente estables en las siete comisiones: la tercera lente del estudio, sin proyección y dentro de cada pool temático, confirma lo que el logit condicional (3.1) y el RHEM (3.3) ya habían mostrado con diseños completamente distintos. Tercero, el perfil repite su patrón: los abogados no se buscan entre sí (coeficiente levemente *negativo* en seis comisiones — los junta el tema, no la elección, como mostró 3.4), la experiencia compartida suma poco pero parejo, y el género es consistentemente negativo: las coaliciones firmantes mezclan géneros más de lo que el azar produciría, en todas las comisiones.

La relación con 3.1 es de triangulación, no de redundancia: el logit condicional pregunta por la *elección* frente a un menú concreto, fecha por fecha; el ERGM bipartito pregunta por la *estructura agregada* de cada pool de documentos, sin proyectar ni condicionar en tamaños. Que lista e ideología organicen la firma en las dos vistas — y en la dinámica del RHEM — permite afirmar la homofilia como propiedad del proceso y no como artefacto de un modelo.

# 4. RQ2 — ¿Qué le hace la red a las personas?

## 4.1 Posiciones: selección, no influencia

Si la red influyera sobre las ideas, la posición de tus co-firmantes debería arrastrar la tuya con el tiempo. Definimos la exposición de $i$ en la onda $t$ de su comisión como la posición media de sus co-firmantes, ponderada por la intensidad de la colaboración acumulada:

$$E_{i,t} = \frac{\sum_{j \neq i} w_{ij,t}\, \theta_{j,t}}{\sum_{j \neq i} w_{ij,t}},$$

donde $w_{ij,t}$ es el número de veces que $i$ y $j$ co-firmaron desde el inicio hasta la onda $t$ (las ondas son los informes de indicaciones de cada comisión; ver Figura 1), y $\theta_{j,t}$ es el voto revelado de $j$ en ese momento. El modelo de regresión estimado es un panel con efectos fijos individuales:

$$\Delta\theta_{i,t} = \alpha_i + \beta\,\theta_{i,t-1} + \lambda\,E_{i,t-1} + \varepsilon_{it},$$

donde $\Delta\theta_{i,t} = \theta_{i,t} - \theta_{i,t-1}$ es el cambio de posición, $\alpha_i$ absorbe todo lo estable de cada convencional (el estimador "within" usa solo la variación de cada persona respecto de su propia media), $\beta$ captura la reversión a la media y $\lambda$ es el parámetro de interés: si $\lambda > 0$, me muevo hacia donde está mi vecindario. Errores agrupados por convencional; 4.355 observaciones persona-onda.

La tabla completa, incluyendo las dos preguntas de robustez que importan — ¿cambia el resultado según la ventana temporal? (¿pudo la influencia operar temprano, sobre los novatos, y agotarse?) y ¿cambia según cuánta historia carga la exposición? (¿cuál es la "dosis" relevante?):

**Tabla 9 — M2: influencia de la exposición sobre el cambio de posición, por ventana temporal y dosis de exposición (FE por convencional).**

| Ventana temporal | Definición de exposición | $\hat\lambda$ | EE | $p$ | N |
|:---|:---|:-:|:-:|:-:|:-:|
| Completa | Acumulada desde T0 | $+0.007$ | 0.004 | $0.119$ | 4.355 |
| Completa | Solo última onda | $+0.007$ | 0.013 | $0.60$ | 463 |
| Completa | Últimas 2 ondas | $+0.003$ | 0.009 | $0.72$ | 1.504 |
| Completa | Últimas 3 ondas | $-0.002$ | 0.007 | $0.76$ | 2.447 |
| Completa | Decaimiento $\lambda_w = 0.25$ | $+0.008$ | 0.004 | $0.051$ | 4.355 |
| Completa | Decaimiento $\lambda_w = 0.50$ | $+0.008$ | 0.004 | $0.042$ | 4.355 |
| Completa | Decaimiento $\lambda_w = 0.75$ | $+0.007$ | 0.004 | $0.078$ | 4.355 |
| Completa | Falsificación: exposición futura | $+0.010$ | 0.004 | $0.030$ | 3.329 |
| Temprana (ondas $\leq$ 31-mar) | Acumulada | $+0.005$ | 0.007 | $0.50$ | 1.586 |
| Temprana | Últimas 2 ondas | $+0.013$ | 0.014 | $0.35$ | 955 |
| Temprana | Últimas 3 ondas | $+0.002$ | 0.010 | $0.81$ | 1.466 |
| Temprana | Decaimiento $\lambda_w = 0.25$ | $+0.005$ | 0.007 | $0.46$ | 1.586 |
| Temprana | Decaimiento $\lambda_w = 0.50$ | $+0.005$ | 0.007 | $0.48$ | 1.586 |
| Temprana | Decaimiento $\lambda_w = 0.75$ | $+0.005$ | 0.007 | $0.49$ | 1.586 |
| Temprana | Falsificación: exposición futura | $+0.006$ | 0.007 | $0.41$ | 1.587 |
| Tardía (ondas $\geq$ 1-abr) | Acumulada | $+0.011$ | 0.005 | $0.018$ | 2.769 |
| Tardía | Solo última onda | $-0.002$ | 0.018 | $0.92$ | 332 |
| Tardía | Últimas 2 ondas | $-0.006$ | 0.008 | $0.45$ | 549 |
| Tardía | Últimas 3 ondas | $-0.011$ | 0.008 | $0.21$ | 981 |
| Tardía | Decaimiento $\lambda_w = 0.25$ | $+0.009$ | 0.004 | $0.032$ | 2.769 |
| Tardía | Decaimiento $\lambda_w = 0.50$ | $+0.011$ | 0.004 | $0.008$ | 2.769 |
| Tardía | Decaimiento $\lambda_w = 0.75$ | $+0.011$ | 0.004 | $0.011$ | 2.769 |
| Tardía | Falsificación: exposición futura | $+0.010$ | 0.004 | $0.004$ | 1.742 |

Sobre las ventanas: la ventana ideal para la hipótesis "la influencia operó temprano sobre los nuevos" sería el primer mes de la Convención, pero ahí la red de co-firma no existía todavía (las primeras iniciativas son de noviembre; los primeros informes, de enero-febrero — Figura 1). Las ventanas factibles cortan la era de colaboración activa en sus dos primeros meses (febrero-marzo de 2022) y el resto (abril-junio). El patrón: en la ventana temprana no hay nada; en la tardía varios coeficientes se vuelven significativos ($+0.009$ a $+0.011$) — pero su falsificación también ($+0.010$, $p = 0.004$), y con la misma magnitud. La historia de los novatos susceptibles exigiría exactamente lo contrario (efecto temprano, falsificación limpia).

La falsificación merece su propia explicación. Si el coeficiente de la exposición *pasada* reflejara influencia causal, la exposición *futura* no debería "predecir" el cambio de hoy. Pero lo hace — en la ventana completa incluso algo más que la pasada ($+0.010$ contra $+0.007$), y en la tardía exactamente igual. Esa simetría es la firma de la selección: la exposición no causa el cambio — acompaña al cambio, antes y después, porque elijo co-firmantes hacia cuya posición ya me estoy moviendo. Una precaución que verificamos en serio: como la exposición es acumulada, la pasada y la futura son casi la misma variable (correlación 0.995 en niveles; 0.93 dentro de cada persona), así que compararlas por separado no basta. El test limpio separa la *novedad*: descomponemos la exposición futura en la parte que ya estaba en la pasada y la parte nueva (las co-firmas que voy a agregar y los movimientos de mis socios futuros), y ponemos ambas en la misma regresión. El resultado es nítido: la parte nueva del futuro predice mi cambio de hoy ($+0.062$, $p = 0.007$) y la exposición pasada, condicional en ella, queda en cero ($p = 0.71$). Me muevo hoy hacia donde estará mi vecindario de mañana — no hacia donde estuvo el de ayer. Es la definición operativa de selección. Un chequeo adicional en la misma línea: como casi todas las ondas caen después del cambio de reglas del 15-feb-2022, re-estimamos el panel usando un $\theta$ recalculado solo con las votaciones de la era de dos tercios (mismo régimen de agenda para toda la serie). Los coeficientes suben — $+0.021$ la exposición pasada, $+0.020$ la futura, ambos $p < 10^{-3}$ — pero la simetría se mantiene intacta: más señal compartida, ninguna asimetría causal.

¿Y si el nulo fuera falta de poder? La versión para la abuela: imagina que quieres saber si tu grupo de amigas te cambia los gustos musicales. Hay dos formas de que el experimento fracase sin que signifique nada: que tus amigas ya tengan exactamente tus gustos (no habría nada que copiar — sin espacio), o que tu termómetro de gustos sea tan malo que no note cambios chicos (sin instrumento). Verificamos ambas. Espacio: la distancia promedio entre la posición de cada convencional y la de su vecindario es 0.59; si los vecindarios se armaran al azar sería 2.25 — la selección cerró el 74% del espacio, pero el 0.59 restante es espacio real donde la influencia se habría notado. Instrumento: el efecto mínimo detectable con nuestros datos es $\lambda = 0.012$ (es decir, habríamos detectado una influencia que cerrara apenas 1.2% de la distancia por onda); lo estimado es $0.007$, por debajo incluso de eso. Conclusión: hubo espacio y hubo instrumento — la influencia simplemente no está, o es sustantivamente despreciable. (Precisión técnica: ese 0.012 sale del error estándar del coeficiente de la regresión — la incertidumbre muestral —, no del error de medición de $\theta$, que aún no está propagado; ver sección 7.)

## 4.2 Conducta: la defección viaja por la red

La versión para la abuela primero. En cada votación, casi todos los convencionales votan igual que su lista — el libreto se respeta el 92% de las veces. Pero a veces alguien se sale del libreto. La pregunta: cuando alguien se sale, ¿se sale solo, o se sale acompañado de la gente con la que escribió iniciativas al comienzo? Y si acompañado — ¿es de verdad por esos lazos, o es casualidad de votaciones que dividen a todos?

Las definiciones, una a una. Para el convencional $i$ y la votación $v$: la defección es $D_{iv} = 1$ si $i$ votó distinto de la mayoría de su lista en $v$ (y 0 si votó con ella); ocurre en el 7.9% de los casos. La exposición a defectores es la fracción ponderada de los co-firmantes de $i$ que defeccionaron en esa misma votación:

$$X_{iv} = \frac{\sum_{j \neq i} w_{ij}\, D_{jv}}{\sum_{j \neq i} w_{ij}},$$

donde $w_{ij}$ es el peso de co-firma de la red génesis (cuántas iniciativas firmaron juntos $i$ y $j$). El modelo de regresión estimado es un logit con dos familias de efectos fijos:

$$\Pr(D_{iv} = 1) = \Lambda\big(\eta_i + \mu_v + \phi\, X_{iv}\big),$$

donde $\Lambda$ es la función logística, $\eta_i$ absorbe la propensión individual a rebelarse (hay personalidades díscolas), $\mu_v$ absorbe la votación (hay votaciones que rompen a todo el mundo), y $\phi$ es el parámetro de interés: ¿defecciono más cuando defeccionan los míos? Errores agrupados por convencional; 374.047 observaciones en la era de votaciones de normas.

El problema es que $\phi$ crudo exagera: si una votación parte a mi lista en dos, varios defeccionamos a la vez aunque no nos conozcamos. El contrafactual duro: barajamos los nombres de los defectores dentro de cada lista y votación — manteniendo exactamente cuántos defeccionaron en cada una — y re-estimamos 200 veces. Todo lo mecánico sobrevive al barajado; solo muere el alineamiento con la red. Y el chequeo adicional contra una historia alternativa: "defeccionamos juntos porque somos de la misma comisión y conocemos el artículo en tabla" — separamos la exposición según si el co-firmante es de mi comisión o de otra, controlando además la tasa de defección de mi comisión en esa votación ($C_{iv}$, la fracción de miembros de mi comisión que defeccionó, sin contarme).

**Tabla 10 — Co-defección: exposición a defectores en la red de co-firma.**

| Modelo | Variable | Coef. | EE | $p$ |
|:---|:---|:-:|:-:|:-:|
| Principal | Exposición a defectores ($\phi$) | $+11.21$ | 0.52 | $<10^{-100}$ |
| Benchmark permutado (200 réplicas) | $\phi$ esperado por mecánica | $6.02$ [p95: $6.08$] | — | $< 0.005$ |
| Período completo (robustez) | $\phi$ | $+11.06$ | 0.47 | $<10^{-121}$ |
| Split por comisión | Exposición co-firmantes de otra comisión | $+8.67$ | 0.46 | $<10^{-15}$ |
| | Exposición co-firmantes de mi comisión | $+2.99$ | 0.32 | $<10^{-15}$ |
| | Tasa de defección de mi comisión ($C_{iv}$) | $-3.01$ | 0.45 | $<10^{-10}$ |

Lectura: la mitad del efecto crudo era mecánica de bloques ($11.2$ observado contra $6.0$ del mundo barajado), pero lo que sobra es enorme y real. Y la historia de "compartir sala con el artículo" queda descartada: la co-defección viaja más fuerte por los co-firmantes de *otras* comisiones que por los de la propia, y la tasa de defección de mi comisión, lejos de arrastrarme, tiene signo negativo. En una frase: la red no cambia lo que piensas (4.1), pero cuando llega el momento de desmarcarse del bloque, no te desmarcas solo — te desmarcas con los tuyos.

# 5. RQ3 — ¿Qué hace ganar?

## 5.1 El mecanismo: la supervivencia de cada artículo

Lo que sobrevive o muere no es el convencional sino el artículo. El modelo de mecanismo se estima entonces a nivel de artículo: 1.565 artículos génesis con coalición firmante de 2 a 16 personas (389 coaliciones distintas), desenlace binario $sobrevive_a = 1$ si el artículo llegó al borrador (idéntico o similar; ocurre en el 20.2% de los casos). El modelo de regresión estimado es un logit con efectos fijos de comisión:

$$\Pr(sobrevive_a = 1) = \Lambda\Big(\alpha_c + \gamma_1\, \big|\bar\theta_{1,S_a} - \theta_{1,(103)}\big| + \gamma_2\, sd(\theta_{1,S_a}) + \gamma_3\, |S_a| + \boldsymbol{\gamma_4}^\top C_{S_a} + \boldsymbol{\gamma_5}^\top H_{S_a}\Big),$$

donde $S_a$ es la coalición firmante del artículo $a$; $\bar\theta_{1,S_a}$ su posición media y $\theta_{1,(103)}$ el pívot de 2/3, de modo que el primer término es la distancia de la coalición al pívot (la teoría pivotal predice $\gamma_1 < 0$); $sd(\theta_{1,S_a})$ es la desviación estándar interna (¿coaliciones anchas sobreviven más?); $|S_a|$ el tamaño; $C_{S_a}$ el bloque de red de la coalición — betweenness media y constraint media de sus miembros, y densidad interna; y $H_{S_a}$ el bloque de capital humano — proporción de abogados, de experimentados, y grado académico medio. (El grado medio de red se excluye del modelo: es casi la misma variable que la constraint media — VIF cercano a 10 entre ambas — y con una basta; sin él, todos los VIF quedan bajo 4.)

La densidad interna merece su explicación de una línea: de todas las parejas que se pueden formar entre los firmantes de este artículo, ¿qué fracción ya había co-firmado junta en *otras* iniciativas? Densidad alta = la coalición es un equipo con historia; densidad baja = es un grupo de desconocidos ensamblado para la ocasión. Es la versión "por coalición" de la familiaridad de pares del RHEM (3.3).

Todos los predictores continuos están estandarizados (los coeficientes son comparables entre sí); errores agrupados por coalición; $\alpha_c$ son efectos fijos de las siete comisiones. Los tres modelos anidados, completos:

**Tabla 11 — M4: supervivencia de cada artículo hasta el borrador (logit, FE de comisión, EE por coalición).**

| Variable | (1) Pivotal | (2) + Red | (3) + Capital humano |
|:---|:-:|:-:|:-:|
| *Geometría ideológica de la coalición* | | | |
| Distancia de la coalición al pívot | $-0.35$ ($p=.001$) | $-0.48$ ($p=.009$) | $-0.61$ ($p=.002$) |
| Heterogeneidad ideológica $sd(\theta_1)$ | $+0.27$ ($p=.008$) | $+0.46$ ($p=.001$) | $+0.53$ ($p<10^{-4}$) |
| Tamaño de la coalición | $+0.20$ ($p=.12$) | $+0.18$ ($p=.12$) | $+0.15$ ($p=.21$) |
| *Red de la coalición* | | | |
| Betweenness media | | $-0.22$ ($p=.083$) | $-0.23$ ($p=.076$) |
| Constraint media | | $+0.01$ ($p=.97$) | $+0.26$ ($p=.28$) |
| Densidad interna (pares con historia) | | $+0.25$ ($p=.024$) | $+0.32$ ($p=.002$) |
| *Capital humano de la coalición* | | | |
| Prop. abogados | | | $-0.09$ ($p=.58$) |
| Prop. con experiencia previa | | | $-0.16$ ($p=.44$) |
| Grado académico medio | | | $-0.06$ ($p=.67$) |
| AIC | 1393 | 1384 | 1386 |

(Con la variable dependiente continua — la similitud textual con el borrador, con los fracasos en cero — los signos y significancias se mantienen.)

La geometría manda: coaliciones lejanas al pívot mueren, y condicional a dónde está su centro, las coaliciones ideológicamente anchas sobreviven más. Del bloque de red, lo único que sobrevive con claridad es la densidad interna: los equipos con historia de co-firma previa ganan (las posiciones estructurales — betweenness marginal negativa, constraint nula — no distinguen nada claro una vez que la geometría está controlada). Y el capital humano no aparece: la proporción de abogados o de experimentados no predice nada — la pericia ni forma lazos (3.4) ni gana votaciones.

¿Heterogeneidad, o heterogeneidad dentro de la izquierda? Una objeción importante: el 87% de las coaliciones tiene posición media a la izquierda de cero (mediana $-0.58$) — la Convención *era* de izquierda. ¿El premio a la anchura será entonces solo dispersión dentro de la izquierda? Partimos las coaliciones en terciles de posición media y re-estimamos el modelo (2) en cada tramo:

**Tabla 12 — M4 por terciles de posición media de la coalición.**

| Tramo (posición media de la coalición) | Supervivencia | $sd(\theta_1)$ | $p$ |
|:---|:-:|:-:|:-:|
| T1: izquierda ($\bar\theta_1 \in [-0.83, -0.69]$) | 10.5% | $+1.63$ | $0.004$ |
| T2: centro-izquierda ($[-0.69, -0.41]$) | 29.0% | $+0.39$ | $0.13$ |
| T3: centro ($[-0.41, +0.81]$) | 21.0% | $-0.28$ | $0.34$ |

La respuesta es sí — y eso no debilita el resultado sino que revela el mecanismo. El premio a la heterogeneidad vive exactamente en las coaliciones más de izquierda: para ellas, "ensancharse" significa extenderse hacia el pívot, y eso es lo que salva artículos (nótese además el gradiente bruto: las coaliciones puramente de izquierda sobreviven 10.5%, las de centro-izquierda 29%). Para las coaliciones ya centradas, ensancharse no agrega nada — ya están donde hay que estar. Es la lógica de los 103 votos operando por dentro. (La versión más fina de este chequeo — clasificar los artículos según *quiénes los votaron* en el Pleno — requiere un vínculo votación-artículo que los datos aún no tienen; queda anotada como extensión.)

## 5.2 La vista agregada: el éxito se comparte

A nivel de convencional, definimos el éxito de $i$ como su retención léxica media: $y_i = \frac{1}{|A_i|} \sum_{a \in A_i} sim(a)$, donde $A_i$ son los artículos que $i$ co-firmó y $sim(a)$ es la similitud textual (coseno TF-IDF) entre el texto génesis del artículo y su versión en el borrador — con $sim(a) = 0$ si el artículo murió. El éxito así medido está fuertemente correlacionado entre vecinos de red ($I$ de Moran 0.44, $p \approx 10^{-163}$), y el modelo que lo formaliza es el espacial de Durbin:

$$y = \rho\, W y + X\beta + W X \gamma + \varepsilon,$$

donde $W$ es la red de co-patrocinio normalizada por filas (cada fila suma 1: $W y$ es, para cada convencional, el éxito promedio de sus co-firmantes), $X$ son sus atributos, $WX$ los mismos atributos promediados sobre el vecindario, $\rho$ mide el acoplamiento entre mi éxito y el de mis vecinos, y la estimación es por máxima verosimilitud. Siguiendo el comentario metodológico obvio — alguien puede ser "exitoso" solo por firmar mucho — $X$ incluye el número total de iniciativas firmadas. La tabla completa del modelo con la distancia al pívot:

**Tabla 13 — Modelo espacial de Durbin del éxito por convencional (con distancia al pívot).**

| Variable | Directo ($\beta$) | $p$ | Vecindario ($\gamma$, lag) | $p$ |
|:---|:-:|:-:|:-:|:-:|
| *Actividad y posición en la red* | | | | |
| N° de iniciativas firmadas | $-0.0001$ | 0.28 | $+0.0002$ | 0.78 |
| Grado (red) | $+0.0004$ | 0.30 | $+0.0052$ | 0.31 |
| Betweenness | $-0.0000$ | 0.98 | $-0.0020$ | 0.40 |
| *Posición ideológica* | | | | |
| Voto revelado medio ($\theta$) | $+0.008$ | 0.33 | $+0.056$ | 0.11 |
| Desv. est. de $\theta$ | $-0.018$ | 0.42 | $+0.026$ | 0.85 |
| Distancia propia al pívot | $-0.002$ | 0.95 | $-0.265$ | 0.050 |
| Heterofilia del ego | $-0.024$ | 0.088 | $-0.255$ | 0.055 |
| *Credenciales y perfil* | | | | |
| Abogado | $+0.011$ | 0.13 | $+0.193$ | 0.041 |
| Experiencia previa | $+0.019$ | 0.031 | $+0.170$ | 0.11 |
| Grado académico | $-0.003$ | 0.49 | $-0.154$ | 0.012 |
| Mujer | $+0.002$ | 0.78 | $+0.208$ | 0.075 |
| Edad | $-0.0002$ | 0.53 | $-0.0033$ | 0.40 |
| *Acoplamiento* | | | | |
| $\rho$ | $0.891$ | $<10^{-15}$ | — | — |

Tres lecturas. Primera, el control de actividad responde la pregunta que lo motivó: firmar muchas iniciativas no hace más exitoso a nadie (coeficiente nulo) y las demás conclusiones no cambian al incluirlo. Segunda, el patrón general del estudio se repite: los atributos propios no predicen casi nada — la única excepción es la experiencia previa propia, positiva y chica ($+0.019$) —; los del vecindario pesan más, incluida la distancia al pívot, que solo importa en su versión "de la compañía" ($-0.27$) y no en la propia ($p = 0.95$). Tercera, $\rho = 0.89$: el éxito está acoplado entre co-firmantes incluso tras todos los controles. Parte de ese acoplamiento es composición — co-firmantes comparten artículos, y por tanto las propiedades de coalición de 5.1 —, por lo que leemos 5.1 como el mecanismo y esta sección como su sombra agregada (la dependencia sobrevive además con red binaria, con otra definición de lazo, con similitud semántica en vez de léxica, y con la retención condicional: $\rho$ entre 0.63 y 0.95 en todas las variantes). Nota sobre el lag de abogado ($+0.19$): a nivel de convencional, estar rodeado de abogados acompaña al éxito, pero el modelo de mecanismo (5.1) muestra que la proporción de abogados de la coalición no salva artículos — preferimos la lectura del nivel artículo, que es el diseño limpio.

# 6. Síntesis

En una asamblea de extraños, la colaboración se organizó con lo que la gente traía puesto: el territorio (compartir distrito multiplica por casi doce las odds de firmar juntos), la etiqueta electoral (todas las listas coordinan patrocinio, las ad hoc igual que los pactos tradicionales — aunque ninguna compra unidad de voto más allá de su alineamiento), y la afinidad ideológica; no la profesión (los abogados no se eligen entre sí — los juntan los temas; la experiencia previa deja apenas un eco en la elección de socios que no llega a marcar coaliciones reales). Sobre esos primeros encuentros la red se rigidiza rápido: el mejor predictor de la próxima coalición es qué pares ya trabajaron juntos, y aun así la afinidad ideológica conserva fuerza propia — conocerse y parecerse operan a la vez. Esa red no cambia las posiciones de nadie (selección, no influencia: exposición pasada y futura predicen igual, y el nulo tiene espacio e instrumento verificados), pero sí coordina la conducta en el margen: las rupturas de disciplina viajan por los lazos de co-firma, incluso entre comisiones distintas. Y cuando los textos llegan al Pleno, sobreviven los de coaliciones cercanas al pívot de 2/3, ideológicamente anchas — el ensancharse salva artículos precisamente a las coaliciones de izquierda, que necesitan estirarse hacia el 103 — y relacionalmente consolidadas; no los de coaliciones con más títulos. El capital que rindió fue territorial, posicional y relacional. Se colaboró por cercanía; se ganó por amplitud.

# 7. Limitaciones y trabajo en curso

1. Los puntos ideales se estiman de votos: la distinción entre ideología y disciplina estable (sección 3.2) es parcialmente circular, y el error de medición de $\theta$ no está propagado a los modelos (el efecto mínimo detectable de 4.1 refleja solo incertidumbre muestral). Está diseñado el bootstrap paramétrico para ambas cosas.
2. La familiaridad del modelo de eventos mezcla dependencia del estado con afinidades estables no observadas; la ventana es corta (tres meses), un 36% de los eventos cae el día del plazo (donde no existe orden intradiario) y 123 fechas están imputadas desde las notas del registro.
3. 46 iniciativas con más de 16 firmantes están excluidas mientras se auditan como duplicaciones transversales (quedan además 24 grupos de texto casi duplicado sin resolver en la fuente); 120 iniciativas utilizables no tienen comisión asignada en la plataforma (entran a la red agregada y al modelo de eventos, no a los análisis por comisión); 21 convencionales de listas locales esperan un crosswalk fino de conglomerado.
4. En el modelo de supervivencia, el grado medio de red de la coalición se excluye por colinealidad con la constraint media (VIF $\approx$ 10 entre ambas); con esa exclusión todos los VIF quedan bajo 4.
5. Con $\rho$ cercano a 1, la descomposición de impactos del modelo espacial es numéricamente inestable; se reportan coeficientes y $\rho$.

En curso: robustez del modelo de eventos (excluir el bloque del plazo; imputar fechas faltantes); la variante dirigida (quién recluta a quién, con el autor principal de cada iniciativa); la extensión del modelo de eventos y de supervivencia a las indicaciones (incorporar a quienes modificaron cada artículo, no solo a quienes lo iniciaron); y el vínculo votación-artículo para clasificar artículos por su coalición de votantes.
