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

La Convención Constitucional chilena (julio 2021 -- julio 2022) permite observar algo que casi nunca se observa: cómo se organiza la colaboración política *desde cero*. A diferencia de una legislatura ordinaria, donde las redes son el sedimento de décadas de carreras y disciplinas de partido, la Convención partió sin jerarquías heredadas: una mayoría inédita de independientes, 17 escaños reservados para pueblos originarios, un órgano que se disolvía al entregar el texto (sin "sombra del futuro" electoral), y reglas escritas por los propios convencionales.

Dos de esas reglas estructuran todo el estudio. Primero, cada *iniciativa convencional constituyente* requería entre 8 y 16 patrocinantes: firmar era formar una coalición visible, fechada y acotada. Segundo, cada norma constitucional necesitaba dos tercios de los convencionales en ejercicio en el Pleno (103 de 154), mientras que dentro de las comisiones bastaba mayoría simple: nacer era barato, sobrevivir era caro.

Sobre ese escenario hacemos tres preguntas:

- RQ1 (formación): ¿qué organiza la decisión de co-firmar entre extraños — la ideología, la lista electoral, la comisión, la profesión, o haber firmado juntos antes?
- RQ2 (efectos sobre las personas): ¿la red mueve las posiciones de los convencionales (influencia), o los convencionales eligen la red según sus posiciones (selección)? ¿Y mueve algo su conducta?
- RQ3 (efectos sobre los textos): ¿qué hace que lo que uno propone sobreviva en el borrador final — y cuánto de eso es la coalición que lo firma?

# 2. Datos

Todo el material proviene del registro documental de la Convención, procesado en un pipeline reproducible (los datos viven como snapshot versionado dentro del repositorio).

- *Actores*: 154 convencionales con perfil curado por doble fuente (BCN y Wikipedia, con validación manual): género, profesión de abogado/a, experiencia institucional previa, grado académico (escala 0--3), lista electoral de origen y comisión temática de pertenencia.
- *Iniciativas*: 996 ingresadas a la plataforma oficial; 528 con dos o más firmantes-persona. El análisis usa 487: las 41 restantes registran más de 16 firmantes — imposible bajo el reglamento — y están en auditoría como duplicaciones de documentos transversales entre comisiones. Para 448 de las 487 tenemos la fecha exacta de ingreso (3-nov-2021 al 2-feb-2022; un tercio ingresó el día del plazo final, 1-feb-2022).
- *Red de co-patrocinio*: proyectando las firmas, 154 nodos y 5.870 aristas (peso = nº de iniciativas co-firmadas). Firmar no escasea para el firmante: la mediana es 42 iniciativas firmadas por convencional y el máximo 157, con fuerte acumulación en el tope legal de 16 firmantes por iniciativa (Figura 1).
- *Trazabilidad de textos*: 1.809 artículos génesis con desenlace conocido frente al borrador del 14-mayo-2022 (498 artículos finales); la tasa de supervivencia es 20%. Cada artículo conserva su texto, sus autores y su historial fechado de indicaciones.
- *Votaciones*: 4.707 votaciones nominales del Pleno (jul-2021 a jun-2022).

![Figura 1. (a) Iniciativas firmadas por convencional; (b) firmantes por iniciativa, con la regla 8--16.](../results/figures/signature_distributions.pdf){width=100%}

# 3. Cómo medimos la ideología

Usamos dos mediciones, con papeles deliberadamente distintos.

*Ideología pre-red.* Puntos ideales en dos dimensiones estimados con W-NOMINATE usando solo las votaciones del primer mes del Pleno (147 votaciones, jul--ago 2021), replicando el diseño de Fábrega (2022) casi exactamente (clasificación correcta 89.4% / 91.6%). Esa ventana es anterior a la creación de las comisiones temáticas (octubre 2021), anterior a las iniciativas (noviembre en adelante) y anterior a la regla de 2/3 (operativa desde febrero 2022: todo el primer mes se votó por mayoría). Por eso $\theta_1$ (eje izquierda--derecha) y $\theta_2$ (el eje que separa a los escaños reservados del clivaje clásico) pueden usarse como covariables exógenas de los modelos de formación y éxito. De aquí sale también el pívot de 2/3: el convencional en la posición 103 del ordenamiento, $\theta_{1,(103)} = -0.15$.

*Voto revelado dinámico.* Un modelo dinámico de puntos ideales (dynIRT, unidimensional) sobre las 4.707 votaciones produce una trayectoria $\theta_{i,t}$ por convencional en 91 períodos. Como después de agosto el Pleno vota bajo reglas y agendas cambiantes, interpretamos esta serie como comportamiento de voto revelado, no como ideología latente pura; es el insumo del modelo de influencia (sección 5).

# 4. RQ1 — La formación de la red

## 4.1 La unidad correcta: la decisión de firma

Una iniciativa de 16 firmantes es un solo acto político, no 120 parejas independientes. Por eso el modelo base trata cada iniciativa $a$ como un menú frente al cual cada convencional $i$ decide firmar o no:

$$U_{ia} = \beta^\top x_{ia} + \alpha_a + \varepsilon_{ia},$$

donde $x_{ia}$ mide la relación de $i$ con la coalición firmante $S_a$ (distancia ideológica a su posición media, pertenencia a la comisión del texto, coincidencia de lista, afinidades demográficas) y $\alpha_a$ es un efecto fijo de la iniciativa: todo lo que la hace atractiva en sí (tema, redactor, momento). El logit condicional de McFadden estima $\beta$ sin conocer $\alpha_a$, porque condiciona en cuántos firmaron:

$$P\big(\text{firmaron } S_a \mid \textstyle\sum_i y_{ia} = |S_a|\big) = \frac{\exp\big(\sum_{i \in S_a} \beta^\top x_{ia}\big)}{\sum_{R:\, |R| = |S_a|} \exp\big(\sum_{i \in R} \beta^\top x_{ia}\big)};$$

al comparar solo coaliciones del mismo tamaño frente al mismo menú, $\alpha_a$ se cancela algebraicamente. Con 487 menús $\times$ 154 convencionales (74.998 decisiones; errores agrupados por convencional):

| Variable | Coef. | Odds ratio | $p$ |
|:---|:-:|:-:|:-:|
| Distancia en $\theta_1$ a la coalición | $-3.46$ | 0.03 | $<10^{-52}$ |
| Distancia en $\theta_2$ a la coalición | $-1.20$ | 0.30 | $<10^{-19}$ |
| Misma comisión que el texto | $+1.21$ | 3.4 | $<10^{-117}$ |
| Misma lista que la coalición | $+0.8$ a $+2.0$ | 2.2--7.4 | $<10^{-5}$ |
| Afinidad de abogados | $+0.37$ | 1.5 | $0.060$ |
| Afinidad de experiencia previa | $+0.27$ | 1.3 | $0.29$ |
| Afinidad de género | $+0.39$ | 1.5 | $0.029$ |

La lectura: la ideología domina (alejarse una unidad de $\theta_1$ de la coalición divide las odds de firmar por 30), la segunda dimensión tiene efecto propio, la comisión triplica las odds, y la lista coordina por encima de la ideología. Las afinidades profesionales, en cambio, no organizan la decisión: la de abogados queda marginal y la de experiencia en cero (volvemos a esto en 4.4).

## 4.2 Las listas: coordinación sin disciplina

¿Funcionaron las listas ad hoc como partidos? Separamos dos cosas que un partido hace. La primera es coordinar con quién firmas: el coeficiente de "misma lista" del modelo anterior, estimado por conglomerado, da 1.04 (Vamos por Chile), 1.02 (Lista del Pueblo), 0.99 (Lista del Apruebo), 0.78 (Apruebo Dignidad) — estadísticamente indistinguibles entre sí — y 2.00 (escaños reservados). Es decir, la lista ad hoc por excelencia coordinó firmas igual que los pactos de partidos tradicionales.

La segunda es votar unidos. Para cada lista $\ell$ y votación $v$ usamos el índice de Rice, $R_{\ell v} = |Y_{\ell v} - N_{\ell v}| / (Y_{\ell v} + N_{\ell v})$, y comparamos su promedio contra 500 pseudo-listas del mismo tamaño y vecindad ideológica: ninguna lista muestra un premio de cohesión (todas $p > 0.10$; Figura 2). Una advertencia de interpretación importante: como los puntos ideales se estiman de los propios votos, una disciplina estable queda absorbida dentro de $\theta$; el resultado debe leerse como "ninguna lista vota más unida *de lo que su alineamiento estable ya implica*", no como ausencia de disciplina a secas (es el clásico problema partidos-versus-preferencias de Krehbiel).

Con esa cautela, el contraste entre ambos ejes es el hallazgo: las listas compraron coordinación de patrocinio pero no unidad de voto extra. Partidos embrionarios: el aparato de coordinación aparece antes que el de disciplina.

![Figura 2. Cohesión de voto (Rice mensual) por lista. La Lista del Pueblo se desploma en dic-2021/ene-2022 — su fragmentación documentada — y se recompone en la era de normas.](../results/figures/rice_cohesion_monthly.pdf){width=100%}

## 4.3 ¿Afines o conocidos? El modelo de eventos

El logit condicional mira cada iniciativa como si fuera la primera. Pero las iniciativas ocurrieron en orden (tenemos la fecha de ingreso de cada una), y eso permite preguntar algo que ninguna foto estática responde: ¿firmas con alguien porque se parecen, o porque ya firmaron juntos? Para eso usamos un modelo de hipereventos relacionales (RHEM): la secuencia de coaliciones fechadas $\{(t_1, S_1), (t_2, S_2), \dots\}$ se modela con una tasa para cada coalición posible $h$,

$$\lambda(t, h) = \exp\big(\theta^\top s(t, h, H_t)\big),$$

donde las estadísticas $s(\cdot)$ pueden depender de la historia $H_t$ (todo lo firmado antes de $t$). La estadística central es la repetición de subconjuntos: con $deg(t, h') = \#\{\text{eventos previos que contienen } h'\}$,

$$sub.rep^{(p)}(t, h) = \binom{|h|}{p}^{-1} \sum_{h' \subseteq h,\, |h'| = p} deg(t, h'),$$

que con $p = 1$ mide la actividad previa de los miembros, con $p = 2$ la familiaridad de sus pares y con $p = 3$ la de sus tríos. La verosimilitud compara cada coalición observada contra coaliciones alternativas del mismo tamaño en esa fecha (estimada por caso-control: 50 controles por evento, la mitad sorteada dentro de la comisión del texto para forzar comparaciones difíciles; paquete `amorem`). El logit condicional de 4.1 es exactamente este modelo con las estadísticas de historia en cero — el mundo de los primeros momentos, antes de que exista pasado.

Resultados (448 eventos fechados; coeficientes estandarizados, estables en 10 re-muestreos de controles):

| Variable | Coef. | $p$ |
|:---|:-:|:-:|
| Familiaridad de pares ($sub.rep^{(2)}$) | $+6.2$ | $4\times10^{-8}$ |
| Dispersión ideológica de la coalición ($\theta_1$) | $-2.3$ | $2\times10^{-21}$ |
| Pares de la misma lista | $+0.9$ | $1\times10^{-5}$ |
| Dispersión en $\theta_2$ | $-0.4$ | $0.05$ |
| Pares ambos abogados | $+0.06$ | $0.65$ |
| Pares ambos con experiencia | $-0.04$ | $0.69$ |

(La actividad y los tríos, muy correlacionados con la familiaridad de pares, son positivos por separado y cambian de signo al entrar juntos; la lectura conjunta correcta es que la señal vive en los pares.) Tres conclusiones. Primero, la historia importa: la familiaridad de pares es el efecto más grande del modelo — la Convención se tejió de a dos, reclutando parejas que ya habían trabajado juntas, no reciclando equipos completos. Segundo, la homofilia ideológica sobrevive entera condicional a la historia: firmar con afines no era un espejismo de "ya se conocían". Tercero, la coordinación de lista persiste por encima de ideología e historia: hay organización operando en cada coalición nueva. Una cautela: la familiaridad medida puede incluir afinidades estables que no observamos (amistades previas, militancias); la llamamos persistencia relacional, no amistad.

## 4.4 Lo que no organiza la red: la profesión

En la literatura, los actores con recursos escasos (pericia jurídica, experiencia) deberían usarlos estratégicamente al elegir socios. Aquí no: la afinidad de abogados es marginal en el logit condicional, nula en el modelo de eventos, y los abogados tampoco ocupan posiciones de intermediación (su *constraint* de Burt es igual al del resto; los brokers de la Convención son los moderados ideológicos). Los abogados sí co-ocurren en iniciativas algo más que el azar — pero la Figura 3 muestra por qué sin necesidad de clasificar contenidos: la proporción de abogados por iniciativa se dispersa mucho más que bajo un sorteo aleatorio de firmantes, y la comisión de Sistemas de Justicia concentra abogados (53%) muy por sobre la tasa de la Convención (39%). Los temas convocan abogados; los abogados no se buscan entre sí.

![Figura 3. (a) Proporción de abogados por iniciativa contra el sorteo aleatorio de firmantes; (b) por comisión temática.](../results/figures/lawyer_share_initiatives.pdf){width=100%}

# 5. RQ2 — ¿Qué le hace la red a las personas?

## 5.1 Posiciones: selección, no influencia

Si la red influyera, la posición de tus co-firmantes debería arrastrar la tuya. Con las ondas de colaboración de cada comisión definimos la exposición de $i$ en la onda $t$ como la posición media ponderada de sus co-firmantes acumulados, $E_{i,t} = \sum_j w_{ij,t}\,\theta_{j,t} / \sum_j w_{ij,t}$, y estimamos el panel con efectos fijos individuales

$$\Delta\theta_{i,t} = \alpha_i + \beta\,\theta_{i,t-1} + \lambda\,E_{i,t-1} + \varepsilon_{it}.$$

En corte transversal la exposición "predice" la posición ($+0.071$, $p < 10^{-8}$): los conectados se parecen. Pero bajo efectos fijos el coeficiente cae a $+0.008$ ($p = 0.095$), y la falsificación es decisiva: reemplazando el rezago por la exposición futura, $E_{i,t+1}$ "predice" el cambio pasado con un coeficiente doce veces mayor ($+0.093$, $p < 10^{-10}$). Eso es la firma de la selección: elijo co-firmantes hacia cuya posición ya me estoy moviendo.

¿Y si el nulo fuera falta de poder — vecindarios tan parecidos a uno que la influencia no tuviera espacio para verse? Lo verificamos: la brecha $|\theta_i - E_i|$ observada promedia 0.59, contra 2.26 si los mismos vecindarios tuvieran posiciones barajadas (la selección cerró el 74% del espacio, pero dejó espacio); y la variación identificadora del estimador es incluso mayor que en un mundo aleatorio, con un efecto mínimo detectable de $\lambda = 0.013$. El nulo no es ceguera del diseño: con capacidad para detectar una influencia que cerrara 1.3% de la brecha por onda, lo observado queda por debajo.

## 5.2 Conducta: la defección viaja por la red

Las posiciones no se mueven, pero el voto en el margen sí. Definimos defección como votar distinto del voto mayoritario de la propia lista en una votación ($D_{iv} = 1$; ocurre en 7.9% de los casos) y estimamos

$$\Pr(D_{iv} = 1) = \Lambda\Big(\eta_i + \mu_v + \phi\, \frac{\sum_{j \neq i} w_{ij} D_{jv}}{\sum_{j \neq i} w_{ij}}\Big),$$

con efectos fijos de persona y de votación: ¿defecciono cuando mis co-firmantes defeccionan en esa misma votación? El coeficiente crudo es $\hat\phi = 11.5$, pero parte es mecánica (una votación que divide a una lista hace defeccionar a varios a la vez). El contrafactual duro — permutar las identidades de los defectores manteniendo exacta la tasa de defección de cada lista en cada votación — produce $\phi$ de 5.9: la co-defección real está a decenas de errores estándar del mundo mecánico. Y no es un artefacto de compartir comisión (defeccionar juntos porque ambos conocen el artículo en tabla): separando la exposición, la co-defección viaja más fuerte por los co-firmantes de otras comisiones ($8.97$) que por los de la propia ($2.76$), controlando la tasa de defección de la comisión. La red no cambia lo que piensas; coordina cuándo te desmarcas.

# 6. RQ3 — ¿Qué hace ganar?

## 6.1 El mecanismo: la supervivencia de cada artículo

Lo que sobrevive o muere no es el convencional sino el artículo, así que el modelo de mecanismo se estima a ese nivel: 1.565 artículos génesis (con coalición firmante de 2 a 16 personas; 389 coaliciones distintas), desenlace binario sobrevivir al borrador (20%), y como predictores las propiedades de la coalición $S_a$ que lo firmó:

$$\Pr(\text{sobrevive}_a) = \Lambda\Big(\alpha_c + \gamma_1\, \big|\bar\theta_{1,S_a} - \theta_{1,(103)}\big| + \gamma_2\, sd(\theta_{1,S_a}) + \boldsymbol{\gamma_3}^\top C_{S_a} + \delta\,|S_a|\Big),$$

con efectos fijos de comisión ($\alpha_c$), errores agrupados por coalición y predictores estandarizados. El primer término es la teoría pivotal: la distancia de la posición media de la coalición al pívot de 2/3. El bloque $C_{S_a}$ es la composición de red de la coalición: centralidades medias de sus miembros, su *constraint* media, y la densidad interna (proporción de pares que habían co-firmado más allá de esta iniciativa).

| Variable | Coef. | $p$ |
|:---|:-:|:-:|
| Distancia de la coalición al pívot | $-0.70$ | $<10^{-4}$ |
| Heterogeneidad ideológica de la coalición | $+0.50$ | $<10^{-3}$ |
| Densidad interna (equipo consolidado) | $+0.29$ | $0.009$ |
| Constraint media (miembros incrustados) | $+0.73$ | $0.016$ |
| Betweenness media | $-0.37$ | $0.007$ |
| Prop. abogados / experiencia / grado académico | $\approx 0$ | $0.5$--$0.7$ |

Cuatro lecturas. La geometría institucional manda: coaliciones lejanas al pívot mueren. La heterogeneidad ayuda: condicional a dónde está su centro, una coalición ideológicamente ancha sobrevive más — y esto forma, con la sección 4, la tensión central del estudio: se firma con los afines, pero ganan las coaliciones anchas, que es exactamente lo que una regla de 103 votos premia. Los equipos consolidados y de miembros incrustados ganan; las coaliciones de intermediarios pierden. Y el capital humano no aparece: la proporción de abogados o experimentados de la coalición no predice nada — la pericia jurídica ni forma lazos (sección 4.4) ni gana votaciones.

## 6.2 La vista agregada: el éxito se comparte

A nivel de convencional, el éxito individual $y_i$ (retención léxica media de sus artículos, con los fracasos en cero: similitud TF-IDF entre el texto génesis y el borrador) muestra fuerte dependencia de red ($I$ de Moran 0.46). El modelo espacial de Durbin

$$y = \rho\,W y + X\beta + W X\gamma + \varepsilon,$$

con $W$ la red de co-patrocinio row-normalizada, estima $\rho = 0.95$: tu retención está acoplada a la de tus co-firmantes, y son los atributos del vecindario ($WX$), no los propios, los que predicen. Agregar la distancia individual al pívot no destrona a la red ($\rho$ apenas baja a 0.91) pero deja una pista consistente con 6.1: la distancia al pívot propia no importa ($p = 0.99$) — importa la del entorno de co-firma ($-0.26$, $p < 10^{-3}$). La dependencia sobrevive cinco variantes (otra definición de lazo, red binaria, similitud semántica, DV condicional; $\rho$ entre 0.79 y 0.95). Parte de este acoplamiento es composición — co-firmantes comparten artículos, y por tanto las propiedades de coalición de 6.1 — por lo que leemos 6.1 como el mecanismo y esta sección como su sombra agregada.

# 7. Síntesis

En una asamblea de extraños, la colaboración se organizó así: la ideología ordena con quién se firma, las listas coordinan el patrocinio sin comprar disciplina de voto extra, y la red se teje de a pares que repiten — no por gremios profesionales. Esa red no mueve las posiciones de nadie (selección, no influencia, con poder estadístico verificado), pero coordina la conducta en el margen: las rupturas de disciplina viajan por los lazos de co-firma. Y cuando los textos llegan al Pleno, sobreviven los de coaliciones cercanas al pívot de 2/3, ideológicamente anchas y relacionalmente consolidadas — no los de coaliciones con más credenciales. La imagen de conjunto: en la Convención el capital que rindió fue posicional y relacional, no profesional; se colaboró por afinidad, pero se ganó por amplitud.

# 8. Limitaciones y trabajo en curso

1. Los puntos ideales se estiman de votos: la distinción ideología/disciplina estable en 4.2 es parcialmente circular (Krehbiel), y el error de medición de $\theta$ aún no se propaga a los modelos (está diseñado un bootstrap paramétrico para ambas cosas).
2. La familiaridad del modelo de eventos mezcla dependencia del estado con afinidades estables no observadas (condiciones iniciales); la ventana es corta (tres meses) y un tercio de los eventos cae el día del plazo, donde el orden intradiario no existe.
3. 39 iniciativas del análisis no tienen fecha (quedan fuera del modelo de eventos) y 41 con más de 16 firmantes están en auditoría aguas arriba; 21 convencionales de listas locales esperan un crosswalk fino de conglomerado.
4. En el modelo de supervivencia, el grado y la constraint medias de la coalición son colineales (VIF $\approx$ 11); se reportan con esa advertencia.
5. Con $\rho$ cercano a 1, la descomposición de impactos del modelo espacial es numéricamente inestable; se reportan coeficientes y $\rho$.

En curso: robustez del modelo de eventos (excluir el bloque del plazo; imputar fechas faltantes), la variante dirigida (autor que recluta), y la extensión del modelo de eventos a las indicaciones (la colaboración posterior a la génesis, hasta junio de 2022).
