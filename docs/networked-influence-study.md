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

## 1.1 El argumento, contado de corrido

Antes de los modelos, el argumento completo en una página — el orden en que este reporte debe leerse y contarse.

La Convención fue un laboratorio casi perfecto para una pregunta que la sociología de redes persigue hace décadas: ¿qué organiza la colaboración cuando no hay historia acumulada que la organice? En una legislatura normal, la respuesta está contaminada — las redes de hoy son el sedimento de carreras, comités y favores de veinte años. Aquí no: mayoría de novatos, órgano que se disuelve al terminar, reglas escritas por los propios miembros. Lo que se observe emerger, emergió de lo que la gente *traía puesto*.

Primer acto — qué traían puesto que formó la red. Tres cosas, todas anteriores al primer día: el territorio (compartir distrito multiplica por doce las odds de co-firmar — y es un fenómeno de derecha y centro: la izquierda organizada coordina por lista, no por geografía), la etiqueta electoral (todas las listas coordinan el patrocinio con fuerza similar, la lista improvisada igual que el pacto tradicional — aunque ninguna compra unidad de *voto* más allá de su alineamiento), y la afinidad ideológica. Y una cosa que no formó nada: las credenciales. Los abogados no eligen abogados — en ningún bloque político, bajo ninguno de nuestros tres diseños (elección, dinámica, estructura); los junta el tema, no la preferencia. Las coaliciones, además, mezclan generaciones y niveles educativos más de lo que produciría el azar.

Segundo acto — el capital relacional se acumula. Con fechas en la mano, el mejor predictor de la próxima coalición no es ningún atributo: es qué *pares* ya habían firmado juntos. La Convención se tejió de a dos — se reclutaban duplas consolidadas, no equipos completos — y aun así la afinidad ideológica conserva toda su fuerza: conocerse y parecerse operan a la vez, sin que uno sea espejismo del otro.

Tercer acto — qué compra ese capital. Tres respuestas distintas, y ahí vive la contribución del estudio. ¿Compra *mentes*? No: las posiciones no se mueven hacia el vecindario (la exposición pasada y la futura predicen el cambio por igual; la parte *nueva* del futuro es la que predice — la firma de la selección —, y el nulo sobrevive con espacio, instrumento y error de medición verificados). ¿Compra *conducta*? Sí: cuando alguien rompe con su bloque, rompe acompañado de sus co-firmantes — el doble de lo que la mecánica de bloques explica, más fuerte entre comisiones distintas que dentro de la propia, y transportado por los pares novatos, no por los senior. ¿Compra *textos*? Sí: sobreviven los artículos de coaliciones relacionalmente consolidadas (equipos con historia de co-firma) — y de nuevo, no los de coaliciones con más títulos.

Y el cierre, que devuelve todo a la institución: la aritmética del 103. Con las mentes quietas, la geometría era destino. La regla de 2/3 fijó un pívot ($\theta_{(103)} = -0.15$), y la supervivencia de los artículos dibuja exactamente lo que esa aritmética predice: el premio a ensancharse vive en las coaliciones de izquierda que aún alcanzan el pívot estirándose (máximo en el segundo quintil), y el punto dulce del éxito no está sobre el pívot sino un poco a su izquierda — el centro de masa de la mayoría que redactaba. En una frase: se colaboró por cercanía, se ganó por amplitud; y la red fue el mecanismo por el que una regla de quórum se volvió comportamiento.

# 2. Datos

Todo el material proviene del registro documental de la Convención, procesado en un pipeline reproducible (los datos viven como snapshot versionado dentro del repositorio).

- Actores: los 154 convencionales, con perfil curado por doble fuente (BCN y Wikipedia, con capa de validación manual). Covariables:
    - género; profesión de abogado/a (39% de la Convención); experiencia institucional previa (haber sido parlamentario/a, alcalde/sa, concejal/a, ministro/a u otro cargo público antes de jul-2021);
    - grado académico en escala 0--3 (0 = sin estudios universitarios terminados, 1 = educación superior, 2 = magíster, 3 = doctorado);
    - distrito electoral (28 distritos; para los 17 escaños reservados, su pueblo originario — 10 pueblos);
    - lista electoral de origen, agrupada en conglomerados (Vamos por Chile 37, Apruebo Dignidad 28, Lista del Apruebo 25, Lista del Pueblo 23, escaños reservados 17, Independientes No Neutrales 3, otras listas locales 21); la agrupación sigue el pacto electoral de origen y difiere en los bordes de la de Fábrega (2022, *Revista de Ciencia Política* 42(1)), cuya Tabla 1 usa una categoría residual "Otras candidaturas fuera de pacto" (10 escaños, "Izquierda y Centro-Izquierda") sin enumerar su composición en el texto — el material suplementario del artículo codifica 15 convencionales como "O"; nuestro "otras listas locales" (21) es más ancho porque agrupa por lista de registro, no por orientación (el crosswalk fino está anotado como pendiente);
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

La Figura 3 pone la columna de producción en barras: las iniciativas con dos o más firmantes (sólido) sobre los artículos génesis en que se descompusieron (translúcido).

![Figura 3. Iniciativas (sólido) y artículos génesis (translúcido) por comisión, con dos o más firmantes.](../results/figures/initiatives_per_commission.pdf){width=85%}

La diferencia de composición más visible es la de los abogados, y conviene retenerla desde ya porque reaparece en la sección 3.4: la Figura 4 muestra el promedio de la proporción de abogados de las coaliciones firmantes de cada comisión, contra la línea de la tasa global (39%).

![Figura 4. Promedio de la proporción de abogados de las coaliciones firmantes, por comisión; la línea roja marca la proporción de abogados en la Convención completa.](../results/figures/lawyer_share_by_commission.pdf){width=62%}

Las dos figuras siguientes muestran la materia prima de las redes del estudio. La Figura 5 es la red bipartita génesis completa: arriba las 947 iniciativas (coloreadas por comisión), abajo los 154 convencionales ordenados por punto ideal, y un trazo por cada firma — a simple vista, las firmas no cruzan el espectro al azar. La Figura 6 es el calendario de las indicaciones: cada barra es un día de informe de comisión, con el total de indicaciones fechadas (claro) y las multi-firmantes (sólido) — las únicas que agregan lazos a la red dinámica de la sección 4; ahí se aprecia también la limitación de registro de C5 (actividad alta, cero multi-firmante).

![Figura 5. El co-patrocinio génesis como red bipartita: iniciativas arriba (por comisión), convencionales abajo (ordenados por punto ideal), un trazo por firma.](../results/figures/bipartite_initiative.pdf){width=100%}

![Figura 6. Indicaciones por fecha de informe y comisión: total fechado (claro) y multi-firmantes (sólido).](../results/figures/indication_events_timeline.pdf){width=100%}

## 2.1 Cómo medimos la ideología

Usamos dos mediciones, con papeles deliberadamente distintos (sus ventanas están en la Figura 1).

Ideología pre-red. Puntos ideales en dos dimensiones estimados con W-NOMINATE usando solo las votaciones del primer mes del Pleno (147 votaciones, jul--ago 2021), replicando el diseño de Fábrega (2022) casi exactamente (clasificación correcta 89.4% / 91.6%). Esa ventana es anterior a las comisiones temáticas (octubre), a las iniciativas (noviembre en adelante) y a la regla de 2/3 (operativa desde febrero; todo el primer mes se votó por mayoría). Por eso $\theta_1$ (izquierda--derecha; negativo = izquierda) y $\theta_2$ (el eje que separa a los escaños reservados del clivaje clásico) pueden usarse como covariables exógenas: nada de la red que estudiamos existía cuando se generaron. De aquí sale también el pívot de 2/3: ordenados los 154 valores de $\theta_1$ de izquierda a derecha, el convencional en la posición 103 marca $\theta_{1,(103)} = -0.15$ — el punto que cualquier coalición ganadora necesita alcanzar.

Voto revelado dinámico. Un modelo dinámico de puntos ideales (dynIRT, unidimensional) sobre las 4.707 votaciones produce una trayectoria $\theta_{i,t}$ por convencional en 91 períodos. Como después de agosto el Pleno vota bajo reglas y agendas cambiantes, esta serie se interpreta como comportamiento de voto revelado, no como ideología latente pura; es el insumo de la sección 4. Las trayectorias se ven en las Figuras 7 y 8: la primera muestra el abanico completo de trayectorias individuales de $\theta_{i,t}$ (y el tamaño del cambio por onda, debajo); la segunda, las mismas trayectorias promediadas por comisión.

![Figura 7. Trayectorias del voto revelado dinámico $\theta_{i,t}$ (arriba) y magnitud del cambio por onda (abajo).](../results/figures/positions_dynamics.pdf){width=100%}

![Figura 8. Voto revelado medio por comisión a lo largo de los períodos.](../results/figures/positions_dynamics_by_commission.pdf){width=100%}

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
- $d^{edad}_{ia} = |edad_i - \overline{edad}_{S_a}|/10$: distancia de edad a la coalición, en décadas.

El modelo de regresión efectivamente estimado es el logit condicional de McFadden: la probabilidad del conjunto de firmantes observado, condicional a su tamaño, es

$$P\big(S_a \,\big|\, |S_a|\big) = \frac{\exp\big(\sum_{i \in S_a} \beta^\top x_{ia}\big)}{\sum_{R \subseteq \{1..154\},\, |R| = |S_a|} \exp\big(\sum_{i \in R} \beta^\top x_{ia}\big)},$$

y $\hat\beta$ maximiza $\ell(\beta) = \sum_a \log P(S_a \mid |S_a|)$. Como $\alpha_a$ es común a todos los candidatos de la iniciativa $a$, aparece idéntico en numerador y denominador y se cancela: no hay que estimarlo, y $\beta$ queda identificado solo por comparaciones entre convencionales frente al mismo menú. La suma del denominador se aproxima con el método de Efron (estándar para este diseño), y los errores estándar se agrupan por convencional. Datos: 949 menús $\times$ 154 convencionales = 145.838 decisiones.

La tabla completa, en cuatro bloques: primero los controles (la estructura y las posiciones que se formaron dentro de la Convención), después los tres bloques de observables pre-Convención — listas, territorio, y perfil personal:

**Tabla 2 — Logit condicional de elección de firma (modelo principal).**

```{=latex}
\begin{center}\small
\begin{tabular}{lcccc}
\toprule
Variable & Coef. & OR & EE & $p$ \\
\midrule
\multicolumn{5}{l}{\textbf{\textit{--- Controles: estructura y posiciones formadas en la Convención}}}\\
Misma comisión que el texto & $+0.93$ & 2.5 & 0.05 & $<10^{-72}$ \\
Distancia en $\theta_1$ a la coalición & $-3.64$ & 0.03 & 0.24 & $<10^{-51}$ \\
Distancia en $\theta_2$ a la coalición & $-1.38$ & 0.25 & 0.14 & $<10^{-20}$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Listas electorales (pre-Convención)}}}\\
Misma lista: Escaños Reservados PPOO & $+1.43$ & 4.2 & 0.21 & $<10^{-10}$ \\
Misma lista: Otras listas locales & $+1.33$ & 3.8 & 0.16 & $<10^{-16}$ \\
Misma lista: Lista del Pueblo & $+1.07$ & 2.9 & 0.16 & $<10^{-10}$ \\
Misma lista: Lista del Apruebo & $+1.05$ & 2.9 & 0.19 & $<10^{-7}$ \\
Misma lista: Vamos por Chile & $+0.94$ & 2.6 & 0.21 & $<10^{-4}$ \\
Misma lista: Apruebo Dignidad & $+0.93$ & 2.5 & 0.14 & $<10^{-9}$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Territorio (pre-Convención)}}}\\
Afinidad de distrito/pueblo & $+2.52$ & 12.4 & 0.56 & $<10^{-5}$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Perfil pre-Convención}}}\\
Afinidad de abogados & $+0.12$ & 1.1 & 0.18 & $0.48$ \\
Afinidad de experiencia previa & $+0.52$ & 1.7 & 0.23 & $0.024$ \\
Distancia de grado académico & $-0.10$ & 0.9 & 0.08 & $0.21$ \\
Distancia de edad (décadas) & $-0.07$ & 0.9 & 0.06 & $0.23$ \\
Afinidad de género & $+0.28$ & 1.3 & 0.16 & $0.093$ \\
\midrule
\multicolumn{5}{l}{AIC = 87.317; \quad pseudo-$R^2$ (McFadden) = 0.209; \quad 947 menús $\times$ 154 = 145.838 decisiones}\\
\bottomrule
\end{tabular}
\end{center}
```

(La lista Independientes No Neutrales, con 3 miembros, nunca es mayoritaria en una coalición y su coeficiente no está identificado.)

Tres lecturas. Primera, la sorpresa territorial: compartir distrito es el predictor pre-Convención más fuerte — pasar de una coalición sin coterráneos a una llena de ellos multiplica las odds de firmar por casi doce. Es conocimiento disponible el día uno: dos convencionales del mismo distrito hicieron campaña en el mismo territorio, comparten electores y problemas locales. Segunda, la lista coordina en todos los conglomerados (todos los $\lambda_c$ entre 0.9 y 1.4, con los escaños reservados y las listas locales en la parte alta): también predecible ex ante. Tercera, las credenciales se parten en dos: ser abogado no organiza la firma en absoluto (y la educación y la edad tampoco), pero la experiencia institucional previa sí — dos personas con carrera pública tienen odds 1.7 veces mayores de terminar en la misma coalición (la sección 3.4 muestra que ese eco no llega a marcar coaliciones reales en el modelo dinámico). Los controles se comportan como se espera: la distancia ideológica es el mayor inhibidor del modelo (una unidad de $\theta_1$ divide las odds por casi 40) y la comisión multiplica las odds por 2.5 — pero son variables *formadas dentro* de la Convención, y por eso las tratamos como controles y no como hallazgo.

¿Los promedios de la Tabla 2 esconden diferencias entre bloques? Que "ser abogado no organiza la firma" en el promedio podría convivir con una derecha que sí se organiza por profesión, o una izquierda que no usa el territorio. La robustez natural: re-estimar el modelo por separado para cada bloque político *del elector* — los menús no cambian (los estratos siguen siendo las iniciativas), solo se restringe la muestra a los convencionales del bloque, así que cada columna es la regla de decisión de ese bloque. Cinco bloques: Derecha (Vamos por Chile, 37 convencionales), Centro-izquierda (Lista del Apruebo + Independientes No Neutrales, 28), Izquierda (Apruebo Dignidad + Lista del Pueblo, 51), Escaños Reservados PPOO (17) y Otras listas locales (21). Una nota técnica: "misma lista" no es identificable dentro de un bloque de un solo conglomerado (todos comparten lista, el indicador no varía dentro del estrato), así que sale de estas especificaciones — la coordinación de lista ya está medida en la Tabla 2.

**Tabla 3 — Logit condicional por bloque político del elector** ($^{+}p<.1$, $^{*}p<.05$, $^{**}p<.01$, $^{***}p<.001$; EE cluster por convencional, completos en `M1_clogit_by_bloc.csv`).

| Variable | Derecha (VC) | Centro-izq (LA+INN) | Izquierda (AD+LdP) | PPOO | Otras |
|:---|:-:|:-:|:-:|:-:|:-:|
| ***— Controles*** | | | | | |
| Misma comisión | $+0.62^{***}$ | $+0.90^{***}$ | $+1.03^{***}$ | $+0.96^{***}$ | $+1.25^{***}$ |
| Distancia en $\theta_1$ | $-5.14^{***}$ | $-2.83^{**}$ | $-0.78$ | $+0.10$ | $-6.05^{***}$ |
| Distancia en $\theta_2$ | $-3.66^{***}$ | $-1.30^{***}$ | $-1.99^{***}$ | $-1.78^{*}$ | $+0.30$ |
| ***— Territorio*** | | | | | |
| Afinidad de distrito/pueblo | $+7.93^{***}$ | $+5.07^{***}$ | $+0.10$ | $-1.35^{+}$ | $+2.37^{+}$ |
| ***— Perfil pre-Convención*** | | | | | |
| Afinidad de abogados | $+0.04$ | $+0.46$ | $-0.00$ | $+0.89$ | $+1.18^{*}$ |
| Afinidad de experiencia | $+0.37$ | $+1.36^{+}$ | $+1.73^{+}$ | $-1.20$ | $-2.86^{**}$ |
| Afinidad de género | $+0.62^{*}$ | $+1.88^{***}$ | $+0.28$ | $+0.03$ | $-0.04$ |
| Distancia de grado | $-0.15$ | $+0.28$ | $-0.32^{*}$ | $+0.11$ | $+0.13$ |
| Distancia de edad | $+0.06$ | $-0.12$ | $-0.24^{**}$ | $-0.11$ | $+0.07$ |

Cuatro heterogeneidades que el promedio escondía. Primera, la territorial es un fenómeno de derecha y centro: enorme en Vamos por Chile ($+7.9$) y en la centro-izquierda ($+5.1$), *nula* en la izquierda organizada (AD+LdP) — que coordina por lista e ideología, no por territorio — y negativa marginal en los PPOO, cuya geografía de pueblos funciona con otra lógica. Segunda, la afinidad de género vive en la centro-izquierda ($+1.88$), con un eco menor en la derecha. Tercera — y responde la pregunta que motivó este corte — el nulo de los abogados es transversal: no hay ningún bloque grande donde los abogados se busquen entre sí. Cuarta, la izquierda es el único bloque que recluta cercano en edad y en credenciales ($-0.24$ y $-0.32$). Y la pendiente ideológica replica desde el lado del elector el gradiente que la tabla siguiente muestra por lista: la derecha casi no cruza distancias; la izquierda las cruza todas.

El mismo gradiente, estimado ahora como pendiente de $\theta_1$ *por lista de la coalición* (efectos principales de lista más interacciones; referencia = Vamos por Chile):

**Tabla 4 — Robustez: pendiente ideológica por lista (interacciones con la distancia en $\theta_1$).**

```{=latex}
\begin{center}\small
\begin{tabular}{lccc}
\toprule
Lista & Pendiente base (VC) & Interacción & Pendiente total \\
\midrule
Vamos por Chile & $-6.10$ & --- & $-6.10$ \\
Independientes No Neutrales & $-6.10$ & $+0.70$ (n.s.) & $-5.40$ \\
Otras listas locales & $-6.10$ & $+1.98$ & $-4.12$ \\
Lista del Apruebo & $-6.10$ & $+2.72$ & $-3.38$ \\
Apruebo Dignidad & $-6.10$ & $+3.03$ & $-3.07$ \\
Escaños Reservados PPOO & $-6.10$ & $+3.23$ & $-2.87$ \\
Lista del Pueblo & $-6.10$ & $+4.17$ & $-1.93$ \\
\midrule
\multicolumn{4}{l}{AIC = 86.787; \quad pseudo-$R^2$ (McFadden) = 0.214}\\
\bottomrule
\end{tabular}
\end{center}
```

La lectura honesta: los PPOO están entre los más planos, pero no solos — la Lista del Pueblo lo es incluso más, y la diferencia entre ambos no es estadísticamente distinguible. El patrón grueso es una asimetría izquierda-derecha: la derecha (VC) casi no cruza distancias ideológicas al firmar; los bloques de izquierda cruzan mucho más. Lo que sigue distinguiendo a los PPOO es la combinación de puentes ideológicos largos con la mayor cohesión interna de lista del modelo ($\lambda = 1.43$).

## 3.2 Las listas: coordinación sin disciplina

¿Funcionaron las listas ad hoc como partidos? Un partido hace al menos dos cosas por sus miembros: los ayuda a coordinar con quién trabajan, y los hace votar juntos.

La primera está medida en la tabla de 3.1, en las filas "Misma lista": cada $\lambda_c$ es el coeficiente del indicador "mi lista coincide con la lista mayoritaria de esta coalición", estimado por separado para cada conglomerado $c$. En palabras: $\lambda_c$ mide cuánto más probable es que un miembro de la lista $c$ firme una iniciativa cuando la coalición ya está dominada por su propia gente — todo lo demás igual, incluida la cercanía ideológica. Es la firma de la coordinación organizacional: si las listas fueran meras etiquetas sin vida interna, sus miembros firmarían con los suyos solo en la medida en que los suyos piensan parecido, y $\lambda_c$ sería cero. El recuento:

**Tabla 5 — Coordinación de firma por lista: los coeficientes $\lambda_c$ de la Tabla 2, reunidos.**

| Conglomerado | $\hat\lambda_c$ | OR |
|:---|:-:|:-:|
| Escaños Reservados PPOO | $1.43$ | 4.2 |
| Otras listas locales | $1.33$ | 3.8 |
| Lista del Pueblo | $1.07$ | 2.9 |
| Lista del Apruebo | $1.05$ | 2.9 |
| Vamos por Chile | $0.94$ | 2.6 |
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

Ninguna lista tiene premio. Pero antes de concluir "no hubo disciplina", hay que confesar un problema — y esta es la explicación para la abuela. Imagina que quieres saber si los hinchas de un club van al estadio porque aman al club o porque sus amigos van. El problema: conociste quiénes son hinchas *mirando quién va al estadio*. Con ese dato no puedes separar amor y amistad — están pegados en la misma observación. Aquí pasa igual: la "ideología" de cada convencional la medimos con sus votos, y si una lista disciplinó los votos desde el día uno, esa disciplina quedó *dentro* de lo que llamamos ideología. Las pseudo-listas emparejadas por ideología llevan la disciplina escondida adentro, y el premio sale cero por construcción parcial. Este problema tiene nombre en la literatura (Krehbiel: ¿partidos o preferencias?) y no tiene solución con votos solamente. La conclusión honesta es: ninguna lista vota más unida *de lo que su alineamiento estable ya implica*. Lo que sí es limpio es el contraste con la sección anterior: en la conducta de *firma* — que no entra en la medición de ideología — las listas sí coordinan, todas y en magnitud similar. La Figura 9 agrega la dinámica: la Lista del Pueblo se desploma en dic-2021/ene-2022, su fragmentación documentada, y se recompone después.

![Figura 9. Cohesión de voto (Rice mensual) por lista.](../results/figures/rice_cohesion_monthly.pdf){width=100%}

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

con las mismas covariables de composición del logit condicional (para que las tablas se lean en paralelo) más las tres estadísticas de historia: $s(t,h)$ reúne la dispersión ideológica de la coalición en cada dimensión (la distancia promedio entre sus pares), la proporción de pares de la misma lista, del mismo distrito/pueblo, ambos abogados, ambos con experiencia, ambos mujeres, la dispersión de grado académico y de edad, la proporción de miembros de la comisión del texto, y $sub.rep^{(1,2,3)}$. Todas las covariables se estandarizan (media 0, desviación 1), así que cada coeficiente responde: si esta característica sube una desviación estándar, ¿cuánto más "real" parece la coalición? La estimación se repite con 10 sorteos independientes de controles; la tabla reporta el promedio (los coeficientes apenas se mueven entre sorteos) y la mediana de los $p$, con los mismos bloques que la tabla de 3.1:

Como estadística de historia, el modelo principal usa solo $sub.rep^{(2)}$: el par es la unidad mínima de una relación ($sub.rep^{(1)}$ mide actividad individual, no relación, y $sub.rep^{(3)}$ recicla la información de los pares), y el párrafo siguiente muestra qué pasa cuando entran las tres a la vez.

**Tabla 7 — RHEM sobre las 947 iniciativas fechadas (especificación principal, $sub.rep^{(2)}$).**

```{=latex}
\begin{center}\small
\begin{tabular}{lcccc}
\toprule
Variable & Memoria infinita & $p$ & Semivida 15 días & $p$ \\
\midrule
\multicolumn{5}{l}{\textbf{\textit{--- Controles: estructura y posiciones formadas en la Convención}}}\\
Prop. de la comisión del texto (\ddag) & $+0.07$ & $0.75$ & $+0.16$ & $0.47$ \\
Dispersión ideológica $\theta_1$ & $-2.66$ & $3\times10^{-39}$ & $-2.60$ & $2\times10^{-37}$ \\
Dispersión ideológica $\theta_2$ & $-0.33$ & $0.042$ & $-0.37$ & $0.026$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Listas electorales (pre-Convención)}}}\\
Prop. pares misma lista & $+0.92$ & $3\times10^{-8}$ & $+1.05$ & $2\times10^{-9}$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Territorio (pre-Convención)}}}\\
Prop. pares mismo distrito/pueblo & $+0.59$ & $5\times10^{-8}$ & $+0.59$ & $3\times10^{-8}$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Perfil pre-Convención}}}\\
Prop. pares ambos abogados & $+0.04$ & $0.44$ & $+0.04$ & $0.53$ \\
Prop. pares ambos con experiencia & $+0.25$ & $0.25$ & $+0.30$ & $0.16$ \\
Dispersión de grado académico & $+0.36$ & $0.026$ & $+0.38$ & $0.024$ \\
Dispersión de edad (décadas) & $-0.17$ & $0.30$ & $-0.21$ & $0.19$ \\
Prop. pares ambas mujeres & $-0.11$ & $0.38$ & $-0.06$ & $0.63$ \\
\multicolumn{5}{l}{\textbf{\textit{--- Historia de co-firma (lo nuevo del RHEM)}}}\\
$sub.rep^{(2)}$ --- familiaridad de pares & $+2.11$ & $1\times10^{-20}$ & $+2.07$ & $2\times10^{-20}$ \\
\midrule
\multicolumn{5}{l}{Log-verosimilitud (re-muestreo 1) = $-110.1$ / $-111.8$; \quad AIC = $242$ / $246$}\\
\bottomrule
\end{tabular}
\end{center}
```

(‡) No interpretable: la mitad de los controles se sortea dentro de la comisión, así que este contraste queda absorbido por el diseño; el efecto comisión ya está medido en 3.1.

¿Y las otras dos estadísticas de historia? Las tres son casi la misma variable con distinto zoom (correlación 0.69 entre actividad y pares, 0.82 entre pares y tríos: la gente activa acumula pares familiares, y los pares familiares componen tríos). Ajustadas de a una, las tres son positivas y de tamaño casi idéntico: actividad $+2.52$, pares $+2.57$, tríos $+2.65$ — pero la de pares es la que mejor ajusta sola (log-verosimilitud $-111$ contra $-190$ y $-115$). Y en la robustez con las tres juntas, la de pares absorbe toda la señal ($+4.48$) mientras actividad ($-5.23$) y tríos ($-0.87$, n.s.) se vuelven negativas — el reparto engañoso típico entre variables casi colineales, no una paradoja. La lectura conjunta: lo que distingue a una coalición real es específicamente la familiaridad de sus pares; condicional en ella, "mucha actividad individual sin familiaridad mutua" es marca de coalición ficticia (así lucen los controles: firmantes seriales que no se conocen), y los tríos no agregan sobre los pares. En una frase: la Convención se tejió de a dos — se reclutaban duplas consolidadas, no equipos completos. Entre memorias, la infinita ajusta levemente mejor que la semivida de 15 días (log-verosimilitud $-110.1$ contra $-111.8$), y los coeficientes casi no se mueven: en la ventana de tres meses del proceso, el capital de co-firma no muestra señales de evaporarse ni siquiera a escala de dos semanas. Un detalle nuevo: la dispersión de grado académico entra *positiva* ($+0.36$, $p = 0.03$) — las coaliciones reales mezclan niveles educativos más que el azar, otra señal de que las credenciales no segregan (la dispersión de edad, en cambio, no distingue: $-0.17$, n.s.).

Qué agrega el RHEM sobre el logit condicional — la comparación en detalle. Los dos modelos usan los mismos eventos y la misma forma estadística; difieren en una sola cosa: el RHEM deja que el pasado entre a la ecuación. Eso tiene tres consecuencias. (i) El logit condicional es el RHEM del primer día: cuando nadie ha firmado con nadie, las estadísticas de historia valen cero para todos y el RHEM se reduce exactamente al logit de 3.1 — por eso no son rivales sino el mismo modelo en dos momentos, y por eso los coeficientes de composición del logit deben leerse como "la fuerza de los atributos cuando no hay historia que consultar" (los primeros momentos). (ii) La dinámica que el RHEM revela es acumulativa: cada co-firma de hoy se vuelve familiaridad mañana, y esa familiaridad es el predictor más fuerte de la próxima coalición — un mecanismo de rieles: los primeros encuentros (guiados por distrito, lista e ideología, como muestra el logit) crean los pares por los que después circula todo lo demás. (iii) La pregunta contrafactual cambia: el logit pregunta "¿a quién se parece el que firma?"; el RHEM pregunta "dado todo lo que ya pasó, ¿quién más podría haber firmado hoy?". Que la ideología ($-2.67$) sobreviva con toda su fuerza en la segunda pregunta es el hallazgo: la homofilia no era un espejismo de la historia acumulada. Y que lista y distrito también sobrevivan dice que la organización territorial y de etiqueta opera en cada coalición nueva, no solo en la primera.

Una cautela final: la "familiaridad" medida solo ve lo firmado desde noviembre de 2021. Amistades previas, militancias compartidas u otras afinidades estables que no observamos quedan dentro de $sub.rep^{(2)}$; por eso la llamamos persistencia relacional y no amistad. Una nota final: el corte por bloques de la Tabla 3 no se traslada directamente al RHEM — aquí la unidad no es un elector sino la coalición-evento, así que no existe "muestra del bloque"; las variantes que sí lo permitirían (interacciones con el bloque mayoritario de la coalición, o modelos separados según el bloque del autor principal) quedan anotadas como extensión.

## 3.4 Lo que no organiza la red: la profesión

La versión para la abuela. Uno esperaría que los abogados de la Convención se buscaran entre ellos para escribir juntos — al fin y al cabo, escribir una constitución es trabajo de abogados. Y si uno mira las iniciativas, efectivamente hay varias llenas de abogados. Pero mirar quién termina junto engaña: hay que preguntar quién *elige* a quién. Nuestros modelos hacen exactamente eso, y la respuesta es que un abogado, puesto frente a dos coaliciones idénticas donde una tiene más abogados, no prefiere la de los abogados (nulo en 3.1, nulo en 3.3). ¿Y entonces por qué se los ve juntos? Porque hay temas que son de abogados. Es el tema el que junta a los abogados, como un asado junta parrilleros: nadie eligió a sus amigos por saber hacer fuego, pero alrededor de la parrilla terminan los que saben. La experiencia política previa es el matiz: sí aparece en la elección de socios (3.1, odds 1.7), aunque no llega a marcar coaliciones reales una vez que la historia entra al modelo (3.3). La profesión, en cambio, no tejió la red.

La Figura 10 muestra la evidencia descriptiva. Cómo leerla: cada iniciativa tiene una "proporción de abogados" (si la firman 10 personas y 4 son abogados, vale 0.4). El panel (a) apila las 947 iniciativas en un histograma (barras azules) y lo compara con un mundo ficticio donde las mismas iniciativas hubieran sorteado a sus firmantes al azar entre los 154 (barras ámbar); las curvas suavizadas del color de cada histograma dibujan la forma de cada distribución, y las líneas punteadas verticales marcan sus medias. La distribución real es más ancha que la del azar por ambos lados — sobran iniciativas casi sin abogados y sobran iniciativas cargadas de abogados. Esa doble cola es la marca de la segregación temática. El panel (b) muestra la misma proporción separada por comisión, como cajas (la caja cubre la mitad central de las iniciativas de esa comisión; la línea es la mediana; el punto rojo, el promedio); la Figura 4 (sección 2) es su resumen en barras. Ahí se lee directamente el contraste clave: en Sistemas de Justicia — jueces, fiscales, control constitucional — las coaliciones firmantes promedian 47% de abogados, ocho puntos sobre la tasa global (y el 88% de los miembros de esa comisión son abogados: Tabla 1); en Derechos Fundamentales, Medio Ambiente y Conocimientos, promedian 27--33%, bajo la tasa global.

![Figura 10. (a) Proporción de abogados por iniciativa contra un sorteo aleatorio de firmantes; (b) por comisión.](../results/figures/lawyer_share_initiatives.pdf){width=100%}

## 3.5 La misma pregunta sin proyectar: ERGM bipartito por comisión

Todos los modelos anteriores trabajan sobre la red *proyectada*: convertimos cada iniciativa en lazos entre pares de firmantes. Esa conversión tiene un costo conocido: una sola iniciativa de 16 firmantes fabrica 120 pares de una vez, así que un acto colectivo grande "pesa" mucho más que varios chicos, y parte de la estructura que vemos entre pares es un artefacto aritmético de los tamaños. La forma de mirar los datos sin ese artefacto es la red bipartita: dos tipos de nodos — los 154 convencionales y las iniciativas de la comisión — y un lazo convencional–iniciativa por cada firma. Nada se proyecta; cada firma cuenta una vez.

Sobre esa red estimamos un ERGM (exponential random graph model), que conviene explicar en una frase: es un modelo de probabilidad sobre la red completa, donde cada término pregunta "¿el patrón X aparece en la red real más (o menos) que en redes aleatorias comparables?", y su coeficiente es el análogo de un log-odds: positivo = el patrón sobra respecto del azar, negativo = falta. Los términos que usamos:

- *edges*: la propensión base a firmar (el intercepto del modelo);
- *miembro*: ¿los miembros de la comisión firman las iniciativas de su comisión más que el resto?
- *misma lista, mismo quintil de $\theta_1$, mismo distrito, ambos abogados, ambos con experiencia, mismo género, mismo nivel educativo, mismo quintil de edad*: cada uno cuenta los pares de co-firmantes de una misma iniciativa que comparten ese atributo — homofilia de co-firma medida sin proyección. (Una diferencia de forma con el logit condicional y el RHEM: allá las variables continuas entran como distancias — $|\theta_i - \bar\theta|$, dispersión de edad por pares —; en el ERGM bipartito la homofilia estándar entre co-firmantes es por *coincidencia de categoría* (`b1nodematch`), así que ideología, educación y edad entran discretizadas. No es una limitación del ERGM en general — en redes de un modo existe `absdiff`, la diferencia continua sobre el lazo — sino del vocabulario de dos-caminos bipartito; la alternativa continua nativa que sí existe es el *rango* por documento (`b2covrange`: cuán dispersos son los firmantes de cada texto en una variable), anotada como extensión. La paridad entre los tres modelos es de variables, no de forma funcional.)

¿Por qué siete modelos y no uno? Por dos razones, una sustantiva y una práctica. La sustantiva: la comisión es el mayor confundidor de composición (la sección 3.4 mostró que los temas arman las coaliciones), y estimar dentro de cada comisión es condicionar por ese confundidor por diseño — en un modelo único de las 947 iniciativas, con un solo intercepto, las diferencias de composición *entre* comisiones contaminan la homofilia *dentro* de cada una (lo verificamos: el modelo agregado invierte los signos de la homofilia, la paradoja de Simpson de manual). La práctica: en estas redes la estimación MCMC completa toma horas por comisión; la tabla reporta máxima pseudo-verosimilitud (MPLE), que entrega los mismos puntos en segundos pero errores estándar que subestiman la incertidumbre — las estrellas se leen como indicativas, y el run MCMC completo (en curso, nocturno) entrega la inferencia definitiva. Los siete modelos ($^{*}$ $p<.05$, $^{**}$ $p<.01$, $^{***}$ $p<.001$; errores estándar completos en `M1_bipartite_commissions.csv`):

**Tabla 8 — ERGM bipartito por comisión (siete modelos, especificación extendida a paridad con el logit condicional; estimación MPLE).**

| Término | C1 | C2 | C3 | C4 | C5 | C6 | C7 |
|:---|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| ***— Controles: base, pertenencia e ideología*** | | | | | | | |
| Edges (base) | $-3.14^{***}$ | $-3.90^{***}$ | $-3.70^{***}$ | $-3.60^{***}$ | $-3.78^{***}$ | $-3.38^{***}$ | $-3.79^{***}$ |
| Mismo quintil $\theta_1$ | $+0.16^{***}$ | $+0.16^{***}$ | $+0.12^{***}$ | $+0.14^{***}$ | $+0.16^{***}$ | $+0.13^{***}$ | $+0.16^{***}$ |
| Miembro de la comisión | $+0.73^{***}$ | $+1.24^{***}$ | $+1.77^{***}$ | $+0.57^{***}$ | $+1.56^{***}$ | $+1.31^{***}$ | $+2.29^{***}$ |
| ***— Listas electorales*** | | | | | | | |
| Misma lista | $+0.21^{***}$ | $+0.18^{***}$ | $+0.20^{***}$ | $+0.18^{***}$ | $+0.18^{***}$ | $+0.21^{***}$ | $+0.22^{***}$ |
| ***— Territorio*** | | | | | | | |
| Mismo distrito/pueblo | $+0.23^{***}$ | $+0.19^{***}$ | $+0.36^{***}$ | $-0.01$ | $+0.07$ | $-0.02$ | $+0.30^{***}$ |
| ***— Perfil pre-Convención*** | | | | | | | |
| Ambos abogados | $-0.08^{***}$ | $-0.02^{+}$ | $-0.08^{***}$ | $-0.01^{+}$ | $-0.03^{**}$ | $-0.02$ | $-0.01$ |
| Ambos con experiencia | $+0.04^{**}$ | $+0.06^{***}$ | $+0.05^{**}$ | $+0.06^{***}$ | $+0.06^{***}$ | $+0.05^{***}$ | $+0.02$ |
| Mismo género | $-0.03^{+}$ | $-0.00$ | $-0.05^{*}$ | $-0.01$ | $-0.06^{***}$ | $-0.08^{***}$ | $-0.04^{+}$ |
| Mismo nivel educativo | $-0.10^{***}$ | $-0.08^{***}$ | $-0.03^{+}$ | $-0.09^{***}$ | $+0.02^{*}$ | $-0.05^{***}$ | $+0.00$ |
| Mismo quintil de edad | $-0.06^{*}$ | $-0.04^{+}$ | $-0.01$ | $-0.05^{***}$ | $-0.08^{***}$ | $-0.05^{*}$ | $-0.09^{**}$ |

Cómo leer el conjunto. Primero, la pertenencia: en las siete comisiones, ser miembro multiplica la propensión a firmar los textos de esa comisión ($+0.57$ a $+2.32$). Segundo — el resultado central de la sección — la homofilia de lista ($+0.16$ a $+0.22$) y la ideológica ($+0.12$ a $+0.16$) son positivas y notablemente estables en las siete comisiones: la tercera lente del estudio, sin proyección y dentro de cada pool temático, confirma lo que el logit condicional (3.1) y el RHEM (3.3) ya habían mostrado con diseños completamente distintos. Tercero, el territorio reaparece con una heterogeneidad nueva: la homofilia de distrito es fuerte en cuatro comisiones ($+0.19$ a $+0.36$ en C1, C2, C3 y C7) y nula en las otras tres (C4, C5, C6) — los pools temáticos más masivos y transversales diluyen al coterráneo. Cuarto, el perfil repite y amplía su patrón: los abogados no se buscan entre sí (coeficiente levemente *negativo* en seis comisiones — los junta el tema, no la elección, como mostró 3.4), la experiencia compartida suma poco pero parejo, y las tres variables de mezcla van todas en la misma dirección — género, nivel educativo y edad tienen homofilia *negativa* casi en todas partes: las coaliciones firmantes mezclan géneros, credenciales y generaciones más de lo que el azar produciría.

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

¿Y si el nulo fuera falta de poder? La versión para la abuela: imagina que quieres saber si tu grupo de amigas te cambia los gustos musicales. Hay dos formas de que el experimento fracase sin que signifique nada: que tus amigas ya tengan exactamente tus gustos (no habría nada que copiar — sin espacio), o que tu termómetro de gustos sea tan malo que no note cambios chicos (sin instrumento). Verificamos ambas. Espacio: la distancia promedio entre la posición de cada convencional y la de su vecindario es 0.59; si los vecindarios se armaran al azar sería 2.25 — la selección cerró el 74% del espacio, pero el 0.59 restante es espacio real donde la influencia se habría notado. Instrumento: el efecto mínimo detectable con nuestros datos es $\lambda = 0.012$ (es decir, habríamos detectado una influencia que cerrara apenas 1.2% de la distancia por onda); lo estimado es $0.007$, por debajo incluso de eso. Conclusión: hubo espacio y hubo instrumento — la influencia simplemente no está, o es sustantivamente despreciable. Y la última objeción posible: $\theta$ no se observa — se estima desde los votos, con error. ¿No estará el nulo fabricado por ese ruido? Lo medimos y lo propagamos. Primero el tamaño del ruido: re-simulamos 50 veces las votaciones desde el propio modelo dinIRT y re-estimamos $\theta$ en cada réplica (bootstrap paramétrico); el error de medición resultante tiene mediana 0.14 — grande: 3.6 veces el movimiento mediano de $\theta$ entre períodos consecutivos (0.04). Después la propagación: para cada una de las 50 versiones de $\theta$ reconstruimos exposición, cambios y el modelo FE completo, y miramos cuánto se mueve $\hat\lambda$ entre réplicas. Respuesta: poco — desviación 0.0014 entre réplicas contra un error muestral de 0.0036, porque el ruido de cada persona-período se promedia sobre 154 convencionales y todas las ondas. El error total honesto (regla de Rubin: muestral más medición) es 0.0039, casi igual al muestral solo, y el efecto mínimo detectable queda en $\lambda = 0.011$: aun cobrándole al modelo todo el error de medición, habríamos detectado una influencia que cerrara 1.1% de la distancia por onda, y lo estimado ($+0.007$) sigue por debajo. El nulo no es un artefacto del termómetro.

La Figura 11 resume la sección completa en dos paneles. En (a), entre personas, posición y exposición van pegadas ($r = 0.95$): elegimos vecindarios que se nos parecen. En (b), dentro de cada persona, la nube de cambios es casi vertical (el $r = 0.36$ crudo que queda es el co-movimiento común de cada fecha — todos los $\theta$ se mueven algo en los mismos días — y es exactamente lo que el modelo FE absorbe). Y el detalle más exigente: los puntos ámbar de (a) son el 5% más alejado de la recta — la gente cuyo vecindario *no* se le parece, es decir, los únicos con espacio grande para ser arrastrados. Si hubiera influencia, ahí debería vérsele. En (b) esos mismos puntos caen como nube sin pendiente ($r = -0.04$): ni siquiera los más desalineados se mueven hacia su vecindario.

![Figura 11. Selección vs. influencia: (a) entre personas, posición propia y del vecindario correlacionan 0.95; en ámbar, el 5% más alejado de la recta. (b) Los cambios onda a onda dentro de cada persona: los mismos puntos ámbar caen sin pendiente ($r = -0.04$).](../results/figures/m2_selection_vs_influence_preview.pdf){width=100%}

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
| Rezagada: votación anterior (sola) | Exposición a defectores en $v-1$ | $+2.98$ | 0.17 | $<10^{-65}$ |
| Horse race contemporánea + rezagada | Exposición contemporánea ($\phi$) | $+11.08$ | 0.52 | $<10^{-100}$ |
| | Exposición en $v-1$ | $+0.72$ | 0.11 | $<10^{-11}$ |
| Rezagada: día anterior de Pleno | Exposición del día anterior | $-0.10$ | 0.30 | $0.75$ |

Lectura: la mitad del efecto crudo era mecánica de bloques ($11.2$ observado contra $6.0$ del mundo barajado), pero lo que sobra es enorme y real. Y la historia de "compartir sala con el artículo" queda descartada: la co-defección viaja más fuerte por los co-firmantes de *otras* comisiones que por los de la propia, y la tasa de defección de mi comisión, lejos de arrastrarme, tiene signo negativo. En una frase: la red no cambia lo que piensas (4.1), pero cuando llega el momento de desmarcarse del bloque, no te desmarcas solo — te desmarcas con los tuyos.

¿Por qué habría de creerse que la defección de mis co-firmantes *en esta misma votación* me hace defeccionar a mí? La pregunta merece literatura, y la literatura tiene dos estantes. El estante a favor parte con la teoría clásica de las señales (*cue-taking*): un legislador no puede estudiar cada votación, así que decide mirando a colegas en los que confía (Matthews y Stimson, *Yeas and Nays*, 1975) — y si mis socios de escritura rompen con el bloque en esta votación, esa es exactamente la señal que miraría. La evidencia moderna respalda que la cercanía transmite votos: compañeros de escritorio en la asamblea de California votan más parecido (Masket, *QJPS* 2008); en el Parlamento Europeo, donde el asiento se asigna por orden alfabético — un experimento natural —, sentarse junto a un colega de partido reduce las discrepancias de voto, y el efecto *persiste* cuando ya no se sientan juntos (Harmon, Fisman y Kamenica, *AEJ: Applied* 2019); y un experimento de campo real muestra que informar a un legislador contagia el copatrocinio de sus vecinos de oficina (Zelizer, *APSR* 2019). El copatrocinio como lazo significativo viene de Fowler (*Political Analysis* 2006) y Kirkland (*JOP* 2011).

El estante en contra es igual de serio, y hay que decirlo con todas sus letras. Manski (*REStud* 1993) mostró que regresionar mi conducta sobre la conducta *simultánea* de mi grupo no identifica influencia: mi defección está dentro de la de ellos y la de ellos dentro de la mía (el "problema del reflejo"). Angrist (*Labour Economics* 2014) agrega que el coeficiente de un promedio de pares es casi una correlación intraclase disfrazada — por eso NO leemos el $11.2$ como magnitud causal, sino que reportamos el *exceso* sobre el mundo barajado ($11.2$ contra $6.0$), que es la comparación que Angrist exige y la permutación construye. Shalizi y Thomas (*SMR* 2011) señalan el confundidor que ninguna permutación arregla: si los lazos se forman por parecidos no observados, los parecidos — no el lazo — pueden producir la co-defección; ese es el residuo honesto de esta sección. Y Rogowski y Sinclair (*Political Analysis* 2012), usando la lotería de oficinas del Congreso — identificación limpia de verdad —, encuentran efecto *cero* de la proximidad: recordatorio de que correlaciones observacionales pueden evaporarse.

¿Y si miramos el pasado en vez del mismo instante? Los modelos rezagados (filas inferiores de la Tabla 10) rompen la simultaneidad de Manski: la defección de mis co-firmantes en la votación *anterior* predice la mía sola ($+2.98$), y en el horse race con la contemporánea, la contemporánea domina ($+11.1$) pero el rezago sobrevive ($+0.72$). El eco muere rápido: la exposición del *día anterior* de Pleno ya no predice nada. La lectura conjunta: coordinación de mismo momento sobre todo (consistente con señales dentro de la sesión), con un eco corto real — no contagio duradero. Dos cautelas de la propia literatura: los rezagos tampoco curan la homofilia latente (Lyons, *Statistics, Politics, and Policy* 2011, sobre los estudios de contagio de Christakis-Fowler; Aral, Muchnik y Sundararajan, *PNAS* 2009, muestran que el rezago ingenuo sobreestima varias veces), y una votación y la anterior pueden ser del mismo paquete temático. Por eso el titular de la sección se sostiene en su versión modesta y defendible: *la defección se agrupa por las líneas de la red de co-firma, mucho más allá de lo mecánico*; influencia es la interpretación natural, homofilia fina la alternativa que no podemos descartar.

¿Y *quién* transmite? La exposición trata a todos los vecinos por igual; partirla por atributo del emisor pregunta si hay contagiadores especiales. Para cada atributo $a$ separamos $E^{a}_{iv}$ (exposición a defectores vecinos *con* el atributo) de $E^{\neg a}_{iv}$ (sin él) y estimamos ambos canales juntos. El resultado va en contra de la hipótesis de los emisores de élite: los abogados transmiten exactamente igual que los no abogados ($\phi = +5.8$ contra $+6.0$, diferencia nula — el tercer nulo de los abogados en este estudio, ahora como emisores); los posgraduados transmiten *menos* que el resto ($+4.3$ contra $+7.0$, $p = .003$); y los experimentados, mucho menos ($+2.6$ contra $+9.3$, $p < .001$). La co-defección viaja por los pares *novatos* — la mayoría nueva se mueve junta; la vieja guardia no arrastra a nadie. (Misma cautela de siempre: es clustering direccional por canal, no influencia causal; y los emisores con experiencia son solo 35, así que su canal se mide con menos precisión.)

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
| ***— Geometría ideológica de la coalición*** | | | |
| Distancia de la coalición al pívot | $-0.35$ ($p=.001$) | $-0.48$ ($p=.009$) | $-0.61$ ($p=.002$) |
| Heterogeneidad ideológica $sd(\theta_1)$ | $+0.27$ ($p=.008$) | $+0.46$ ($p=.001$) | $+0.53$ ($p<10^{-4}$) |
| Tamaño de la coalición | $+0.20$ ($p=.12$) | $+0.18$ ($p=.12$) | $+0.15$ ($p=.21$) |
| ***— Red de la coalición*** | | | |
| Betweenness media | | $-0.22$ ($p=.083$) | $-0.23$ ($p=.076$) |
| Constraint media | | $+0.01$ ($p=.97$) | $+0.26$ ($p=.28$) |
| Densidad interna (pares con historia) | | $+0.25$ ($p=.024$) | $+0.32$ ($p=.002$) |
| ***— Capital humano de la coalición*** | | | |
| Prop. abogados | | | $-0.09$ ($p=.58$) |
| Prop. con experiencia previa | | | $-0.16$ ($p=.44$) |
| Grado académico medio | | | $-0.06$ ($p=.67$) |
| AIC | 1393 | 1384 | 1386 |

(Con la variable dependiente continua — la similitud textual con el borrador, con los fracasos en cero — los signos y significancias se mantienen.)

La geometría manda: coaliciones lejanas al pívot mueren, y condicional a dónde está su centro, las coaliciones ideológicamente anchas sobreviven más. Del bloque de red, lo único que sobrevive con claridad es la densidad interna: los equipos con historia de co-firma previa ganan (las posiciones estructurales — betweenness marginal negativa, constraint nula — no distinguen nada claro una vez que la geometría está controlada). Y el capital humano no aparece: la proporción de abogados o de experimentados no predice nada — la pericia ni forma lazos (3.4) ni gana votaciones.

¿Heterogeneidad, o heterogeneidad dentro de la izquierda? Una objeción importante: el 87% de las coaliciones tiene posición media a la izquierda de cero (mediana $-0.58$) — la Convención *era* de izquierda. ¿El premio a la anchura será entonces solo dispersión dentro de la izquierda? Partimos las coaliciones en quintiles según su posición media (cinco grupos de igual tamaño, del más izquierdista al más centrado) y re-estimamos el modelo (2) dentro de cada tramo:

**Tabla 12 — M4 por quintiles de posición media de la coalición.**

| Tramo (posición media de la coalición) | Supervivencia | $sd(\theta_1)$ | $p$ |
|:---|:-:|:-:|:-:|
| Q1: izquierda ($\bar\theta_1 \in [-0.83, -0.73]$) | 7.3% | $+1.18$ | $0.050$ |
| Q2: izquierda moderada ($[-0.73, -0.67]$) | 14.0% | $+2.46$ | $0.001$ |
| Q3: centro-izquierda ($[-0.66, -0.50]$) | 34.1% | $+0.36$ | $0.49$ |
| Q4: centro ($[-0.50, -0.18]$) | 29.6% | $-0.09$ | $0.82$ |
| Q5: centro y derecha ($[-0.17, +0.81]$) | 15.4% | $+0.18$ | $0.72$ |

La respuesta es sí — y con los quintiles el mecanismo se ve con más resolución que nunca. Léase la tabla de arriba hacia abajo como un viaje desde la izquierda hacia el pívot. El premio a la anchura no es máximo en el extremo sino en el *segundo* escalón: en Q2 — la izquierda casi dura — una desviación estándar más de anchura ideológica es la diferencia entre la vida y la muerte ($+2.46$, el coeficiente más grande de todo M4), porque para una coalición anclada en $-0.7$ ensancharse significa literalmente estirar la mano hacia el convencional 103, y todavía le alcanza el brazo. En el extremo puro (Q1) el premio existe pero es menor y marginal ($+1.18$, $p = 0.050$): desde $-0.8$, ni estirándose se llega. Y desde Q3 en adelante el premio desaparece de golpe: esas coaliciones ya viven en la zona de la supervivencia alta (34% y 30%, contra 7% en Q1) y no necesitan ensancharse — están donde hay que estar. (Tomados juntos los tres quintiles de la izquierda, el premio promedio sigue positivo: $+0.63$, $p = 0.012$; el gradiente completo decae monótono desde Q2 hasta cero.) Los niveles brutos cuentan la misma historia por otro lado: la supervivencia sube de 7% a 34% al acercarse al pívot y vuelve a caer a 15% al pasarlo. La Figura 12 muestra dónde está exactamente el punto dulce, y no es *sobre* el pívot: es un poco a su izquierda. (La versión más fina de este chequeo — clasificar los artículos según *quiénes los votaron* en el Pleno — requiere un vínculo votación-artículo que los datos aún no tienen; queda anotada como extensión.)

La Figura 12 muestra esta geometría directamente, sin cortar en tramos: la tasa de supervivencia contra la posición media de la coalición, en bins de igual tamaño. El máximo (cerca del 45%) no está sobre el pívot sino algo a su izquierda ($\bar\theta_1 \approx -0.55$): subida empinada desde la izquierda dura hasta ese punto, descenso suave al acercarse al pívot y caída al cruzarlo. La interpretación encaja con la aritmética de la sala: en una Convención mayoritariamente de izquierda, la coalición óptima es la que está en el centro de masa de esa mayoría — lo bastante central para estirarse hasta el voto 103, sin despegarse del bloque que redacta. Las barandillas superiores de la figura marcan los cortes por quintiles y cuartiles de las medias de coalición — la Tabla 12 es esta curva leída por el primero de esos dos rieles.

![Figura 12. Tasa de supervivencia de los artículos según la posición media de su coalición firmante (bins de igual tamaño; el tamaño del punto es el n del bin). Las bandas sombreadas son los terciles de los puntos ideales de los 154 convencionales — y como $103 = 2/3 \times 154$, el corte entre T2 y T3 es *exactamente* el pívot $\theta_{(103)}$ — con la tasa de supervivencia de las coaliciones cuya media cae en cada banda; las barandillas rojas sobre el plano marcan los cortes por quintiles (fila superior) y cuartiles (fila inferior) de las medias de coalición; la línea roja discontinua es el pívot.](../results/figures/survival_by_position.pdf){width=100%}

## 5.2 La vista agregada: el éxito se comparte

A nivel de convencional, definimos el éxito de $i$ como su retención léxica media: $y_i = \frac{1}{|A_i|} \sum_{a \in A_i} sim(a)$, donde $A_i$ son los artículos que $i$ co-firmó y $sim(a)$ es la similitud textual (coseno TF-IDF) entre el texto génesis del artículo y su versión en el borrador — con $sim(a) = 0$ si el artículo murió. El éxito así medido está fuertemente correlacionado entre vecinos de red ($I$ de Moran 0.44, $p \approx 10^{-163}$), y el modelo que lo formaliza es el espacial de Durbin:

$$y = \rho\, W y + X\beta + W X \gamma + \varepsilon,$$

donde $W$ es la red de co-patrocinio normalizada por filas (cada fila suma 1: $W y$ es, para cada convencional, el éxito promedio de sus co-firmantes), $X$ son sus atributos, $WX$ los mismos atributos promediados sobre el vecindario, $\rho$ mide el acoplamiento entre mi éxito y el de mis vecinos, y la estimación es por máxima verosimilitud. Siguiendo el comentario metodológico obvio — alguien puede ser "exitoso" solo por firmar mucho — $X$ incluye el número total de iniciativas firmadas. La tabla completa del modelo con la distancia al pívot:

**Tabla 13 — Modelo espacial de Durbin del éxito por convencional (con distancia al pívot).**

| Variable | Directo ($\beta$) | $p$ | Vecindario ($\gamma$, lag) | $p$ |
|:---|:-:|:-:|:-:|:-:|
| ***— Actividad y posición en la red*** | | | | |
| N° de iniciativas firmadas | $-0.0001$ | 0.28 | $+0.0002$ | 0.78 |
| Grado (red) | $+0.0004$ | 0.30 | $+0.0052$ | 0.31 |
| Betweenness | $-0.0000$ | 0.98 | $-0.0020$ | 0.40 |
| ***— Posición ideológica*** | | | | |
| Voto revelado medio ($\theta$) | $+0.008$ | 0.33 | $+0.056$ | 0.11 |
| Desv. est. de $\theta$ | $-0.018$ | 0.42 | $+0.026$ | 0.85 |
| Distancia propia al pívot | $-0.002$ | 0.95 | $-0.265$ | 0.050 |
| Heterofilia del ego | $-0.024$ | 0.088 | $-0.255$ | 0.055 |
| ***— Credenciales y perfil*** | | | | |
| Abogado | $+0.011$ | 0.13 | $+0.193$ | 0.041 |
| Experiencia previa | $+0.019$ | 0.031 | $+0.170$ | 0.11 |
| Grado académico | $-0.003$ | 0.49 | $-0.154$ | 0.012 |
| Mujer | $+0.002$ | 0.78 | $+0.208$ | 0.075 |
| Edad | $-0.0002$ | 0.53 | $-0.0033$ | 0.40 |
| ***— Acoplamiento*** | | | | |
| $\rho$ | $0.891$ | $<10^{-15}$ | — | — |

Tres lecturas. Primera, el control de actividad responde la pregunta que lo motivó: firmar muchas iniciativas no hace más exitoso a nadie (coeficiente nulo) y las demás conclusiones no cambian al incluirlo. Segunda, el patrón general del estudio se repite: los atributos propios no predicen casi nada — la única excepción es la experiencia previa propia, positiva y chica ($+0.019$) —; los del vecindario pesan más, incluida la distancia al pívot, que solo importa en su versión "de la compañía" ($-0.27$) y no en la propia ($p = 0.95$). Tercera, $\rho = 0.89$: el éxito está acoplado entre co-firmantes incluso tras todos los controles. Parte de ese acoplamiento es composición — co-firmantes comparten artículos, y por tanto las propiedades de coalición de 5.1 —, por lo que leemos 5.1 como el mecanismo y esta sección como su sombra agregada (la dependencia sobrevive además con red binaria, con otra definición de lazo, con similitud semántica en vez de léxica, y con la retención condicional: $\rho$ entre 0.63 y 0.95 en todas las variantes). Nota sobre el lag de abogado ($+0.19$): a nivel de convencional, estar rodeado de abogados acompaña al éxito, pero el modelo de mecanismo (5.1) muestra que la proporción de abogados de la coalición no salva artículos — preferimos la lectura del nivel artículo, que es el diseño limpio.

¿Qué pasa si tomamos el modelo en serio y calculamos sus efectos completos? En un modelo espacial, el coeficiente crudo de una variable no es su efecto total: si mi éxito sube, el de mis vecinos sube ($\rho$), lo que vuelve a subir el mío — un eco infinito. La descomposición estándar (LeSage-Pace) separa el efecto *directo* (sobre mí, incluido mi propio eco), el *indirecto* (el derrame sobre los demás) y el *total*. Con solo 154 nodos podemos calcularla de forma exacta — el multiplicador $(I - \rho W)^{-1}$ completo, sin las aproximaciones que se vuelven inestables cuando $\rho$ se acerca a 1:

**Tabla 14 — SDM: descomposición exacta de impactos (modelo de la Tabla 13) y comparación con OLS.**

| Variable | $\beta$ OLS | Directo | Indirecto | Total |
|:---|:-:|:-:|:-:|:-:|
| ***— Actividad y posición en la red*** | | | | |
| N° de iniciativas firmadas | $-0.0004$ | $-0.0001$ | $+0.0009$ | $+0.0007$ |
| Grado (red) | $+0.0011$ | $+0.0010$ | $+0.051$ | $+0.052$ |
| Betweenness | $-0.0004$ | $-0.0002$ | $-0.018$ | $-0.018$ |
| ***— Posición ideológica*** | | | | |
| Voto revelado medio ($\theta$) | $+0.003$ | $+0.015$ | $+0.57$ | $+0.58$ |
| Desv. est. de $\theta$ | $+0.025$ | $-0.017$ | $+0.09$ | $+0.07$ |
| Distancia propia al pívot | $-0.093$ | $-0.030$ | $-2.41$ | $-2.44$ |
| Heterofilia del ego | $-0.011$ | $-0.053$ | $-2.50$ | $-2.56$ |
| ***— Credenciales y perfil*** | | | | |
| Abogado | $+0.010$ | $+0.032$ | $+1.83$ | $+1.86$ |
| Experiencia previa | $+0.013$ | $+0.038$ | $+1.68$ | $+1.72$ |
| Mujer | $+0.001$ | $+0.024$ | $+1.90$ | $+1.92$ |
| Edad | $-0.0004$ | $-0.0005$ | $-0.031$ | $-0.032$ |
| Grado académico | $+0.002$ | $-0.020$ | $-1.42$ | $-1.44$ |

La tabla enseña más por lo que revela del modelo que por sus números sueltos, y hay que leerla con tres claves. Primera: los efectos *directos* son chicos y del orden de los OLS — sobre uno mismo, nada mueve mucho. Segunda: los *totales* son enormes porque con $\rho = 0.89$ el multiplicador de equilibrio vale $1/(1-\rho) \approx 9$ — el modelo afirma, tomado literalmente, que convertir a alguien en abogado sumaría $+1.86$ de retención repartida por toda la sala, cuando el éxito individual promedio es $0.09$. Eso no es un efecto creíble: es el diagnóstico cuantitativo de por qué el acoplamiento $\rho$ debe leerse como asociación (composición más selección) y no como difusión causal — la advertencia que este mismo reporte hace desde 5.1. (La inferencia sobre los totales tampoco es utilizable en este régimen: en el Monte Carlo con la covarianza ML, los sorteos con $\rho \to 1$ disparan el multiplicador y los errores estándar explotan; por eso la tabla no trae $p$-values de totales, y la columna de "% de reducción vs OLS" que sugiere la comparación estándar pierde sentido — los porcentajes salen en decenas de miles.) Tercera clave, los dos contrafactuales que sí son tangibles: mover a un convencional de un vecindario de bajo éxito (percentil 10 de $Wy$) a uno de alto éxito (percentil 90) asocia $+0.078$ de retención — 1.3 desviaciones estándar del éxito, casi duplicar la media — y mover a *una sola* persona del P10 al P90 de una covariable produce efectos propios minúsculos (abogado: $+0.03$; distancia al pívot: $-0.03$) pero derrames agregados gigantes según el modelo ($+1.8$ y $-2.0$ sumados sobre la sala). El contraste entre lo propio (chico) y lo derramado (enorme) es la firma aritmética de $\rho \approx 0.9$.

La Figura 13 baja todo esto a la evidencia visual más simple: el éxito de cada convencional contra su posición del primer mes. El perfil replica a nivel individual lo que la Figura 12 mostró a nivel de coalición — máximo justo a la izquierda del pívot (y los individuos más exitosos son de listas locales, INN y Lista del Apruebo parados ahí), niveles moderados en la izquierda, y el piso plano en la derecha.

![Figura 13. Éxito individual (retención léxica media) según el punto ideal del primer mes, coloreado por conglomerado; línea = medias por bins de igual tamaño; vertical = pívot de 2/3.](../results/figures/success_by_ideology.pdf){width=100%}

La Figura 14 muestra la materia prima de este éxito en el tiempo: la similitud de los artículos de cada comisión con el borrador final, onda a onda — la convergencia gradual (y desigual entre comisiones) cuyo punto de llegada es el numerador del éxito $y_i$.

![Figura 14. Retención textual por comisión a lo largo de las ondas de indicaciones: similitud media de los artículos con el borrador final.](../results/figures/retention_dynamics_all_commissions.pdf){width=100%}

# 6. Síntesis

En una asamblea de extraños, la colaboración se organizó con lo que la gente traía puesto: el territorio (compartir distrito multiplica por casi doce las odds de firmar juntos), la etiqueta electoral (todas las listas coordinan patrocinio, las ad hoc igual que los pactos tradicionales — aunque ninguna compra unidad de voto más allá de su alineamiento), y la afinidad ideológica; no la profesión (los abogados no se eligen entre sí — los juntan los temas; la experiencia previa deja apenas un eco en la elección de socios que no llega a marcar coaliciones reales). Sobre esos primeros encuentros la red se rigidiza rápido: el mejor predictor de la próxima coalición es qué pares ya trabajaron juntos, y aun así la afinidad ideológica conserva fuerza propia — conocerse y parecerse operan a la vez. Esa red no cambia las posiciones de nadie (selección, no influencia: exposición pasada y futura predicen igual, y el nulo tiene espacio e instrumento verificados), pero sí coordina la conducta en el margen: las rupturas de disciplina viajan por los lazos de co-firma, incluso entre comisiones distintas. Y cuando los textos llegan al Pleno, sobreviven los de coaliciones cercanas al pívot de 2/3, ideológicamente anchas — el ensancharse salva artículos precisamente a las coaliciones de izquierda, que necesitan estirarse hacia el 103 — y relacionalmente consolidadas; no los de coaliciones con más títulos. El capital que rindió fue territorial, posicional y relacional. Se colaboró por cercanía; se ganó por amplitud.

# 7. Limitaciones y trabajo en curso

1. Los puntos ideales se estiman de votos: la distinción entre ideología y disciplina estable (sección 3.2) es parcialmente circular. El error de medición de $\theta$ ya está cuantificado y propagado a M2 (bootstrap paramétrico, sección 4.1: el nulo sobrevive); su propagación a los demás modelos que usan $\theta$ (formación, supervivencia) queda pendiente.
2. La familiaridad del modelo de eventos mezcla dependencia del estado con afinidades estables no observadas; la ventana es corta (tres meses), un 36% de los eventos cae el día del plazo (donde no existe orden intradiario) y 123 fechas están imputadas desde las notas del registro.
3. 46 iniciativas con más de 16 firmantes están excluidas mientras se auditan como duplicaciones transversales (quedan además 24 grupos de texto casi duplicado sin resolver en la fuente); 120 iniciativas utilizables no tienen comisión asignada en la plataforma (entran a la red agregada y al modelo de eventos, no a los análisis por comisión); 21 convencionales de listas locales esperan un crosswalk fino de conglomerado.
4. El ERGM bipartito por comisión (Tabla 8) reporta por ahora puntos MPLE: los errores estándar definitivos requieren el MCMC completo (en curso; horas por comisión en las redes reales). Los términos *estructurales* (heterogeneidad de grado, clustering más allá de atributos) siguen fuera de todos los modelos de formación: el más simple (`gwb1degree`) cuesta ~15 minutos por iteración MCMC incluso en la comisión más chica, y los intentos con términos `gw` más ricos degeneraron.
5. No existe un "ERGM bipartito total" válido con intercepto único: al juntar las 947 iniciativas en una sola red, la composición entre comisiones invierte los signos de la homofilia (paradoja de Simpson, verificada numéricamente); el modelo general correcto es la suite por comisión (o su versión conjunta con interceptos por red), que es lo que se reporta.
6. Con $\rho \approx 0.9$, la descomposición de impactos del modelo espacial se calcula exacta (Tabla 14) pero sus totales son efectos de equilibrio amplificados $\sim 9\times$ y su inferencia Monte Carlo no es utilizable (sorteos con $\rho \to 1$); se leen como diagnóstico del acoplamiento, no como tamaños de efecto.

En curso: robustez del modelo de eventos (excluir el bloque del plazo; imputar fechas faltantes); la variante dirigida (quién recluta a quién, con el autor principal de cada iniciativa); la extensión del modelo de eventos y de supervivencia a las indicaciones (incorporar a quienes modificaron cada artículo, no solo a quienes lo iniciaron); y el vínculo votación-artículo para clasificar artículos por su coalición de votantes.
