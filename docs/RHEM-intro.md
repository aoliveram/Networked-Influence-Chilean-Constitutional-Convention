---
title: "Modelos de eventos relacionales de hiperevento (RHEM)"
subtitle: "Introducción pedagógica para el proyecto Networked Influence — de los ERGM y el logit condicional al modelado de co-firmas fechadas"
author: "Preparado a partir de los papers de arXiv de la familia Butts / Lerner–Lomi (verificación cruzada de fuentes, 2026-07-10)"
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

# 1. Para qué existe esta familia

Un ERGM responde "¿por qué la red se ve así?" mirando **una foto**. Nuestro dato real no es una foto: son **528 eventos fechados** — cada iniciativa es un instante en que un conjunto de 8--16 convencionales hizo algo juntos — y la foto la fabricamos nosotros al agregarlos. Al agregar se destruye la información más valiosa: el **orden** (quién firmó con quién *después* de qué) y con ella toda posibilidad de separar homofilia de historia ("firmo contigo porque nos parecemos" vs. "firmo contigo porque ya firmamos juntos").

El **modelo de eventos relacionales** (REM) invierte el objeto: en vez de modelar la red, modela **la secuencia de eventos** como un proceso puntual en tiempo continuo, donde la tasa a la que cada evento posible ocurre depende de la historia acumulada y de covariables. Su extensión a eventos **multi-actor** — el **modelo de hiperevento relacional** (RHEM) — es la única familia estadística cuya unidad nativa es exactamente nuestro dato: un evento fechado protagonizado por un *conjunto* de actores, sin proyección a díadas y sin noción de "lazo que se disuelve".

# 2. El punto de partida: el REM de Butts (2008)

Butts (2008, *Sociological Methodology*) modela secuencias de acciones diádicas $(i \to j)$ con una **intensidad** (tasa instantánea) para cada díada posible:

$$\lambda_{ij}(t \mid A_t; \theta) = \exp\{\theta^\top s_{ij}(A_t, X)\} \cdot \mathbf{1}\{(i,j) \in \mathcal{R}(t)\}$$

Término a término: $A_t$ es la **historia** (todos los eventos anteriores a $t$); $s_{ij}(A_t, X)$ es un vector de **estadísticas** calculadas sobre esa historia y sobre atributos exógenos $X$ — por ejemplo *repetición* ($N_{ij}(t^-)$: cuántas veces $i$ ya actuó sobre $j$), *reciprocidad* ($N_{ji}(t^-)$), *cierre transitivo* ($\sum_a N_{ia}N_{aj}$: amigos de amigos) y *homofilia* ($\mathbf{1}\{x_i = x_j\}$ o $-|x_i - x_j|$); $\theta$ son los parámetros, que se leen como en un logit ($e^{\theta_k}$ = factor multiplicativo de la tasa por unidad de estadística); y $\mathcal{R}(t)$ es el **conjunto de riesgo** (las díadas que *podrían* ocurrir en $t$).

Hay dos verosimilitudes. La **completa** usa los tiempos exactos entre eventos. La **ordinal** usa solo el *orden* — la relevante cuando se tienen fechas pero no horas, nuestro caso:

$$L_O(\theta) = \prod_{k=1}^{n} \frac{\exp\{\theta^\top s_{i_k j_k}\}}{\sum_{(i,j)\in\mathcal{R}(t_k)} \exp\{\theta^\top s_{ij}\}}$$

Cada factor responde: *dado que ocurrió un evento en $t_k$, ¿qué probabilidad tenía de ser exactamente esa díada y no cualquier otra en riesgo?* Esa es literalmente la forma de un **logit condicional de McFadden** (y coincide con la verosimilitud parcial de Cox estratificada por evento). Este es el puente pedagógico central del documento: **el REM ordinal *es* el logit condicional que ya usamos en M1 (IV.D1), con dos diferencias — el "menú" es el conjunto de riesgo en cada instante, y las covariables pueden depender de la historia.** El logit condicional de M1 es un REM con $\theta_{historia} = 0$.

# 3. Por qué las díadas no bastan para la co-firma

Una iniciativa con 16 firmantes no es un evento diádico. La salida ingenua — descomponerla en $\binom{16}{2} = 120$ díadas "simultáneas" — reproduce en el tiempo exactamente la patología D1 del ERGM proyectado: infla el número de observaciones con eventos que no son independientes, y además **no puede expresar las hipótesis interesantes**. "Este *trío* ya firmó junto tres veces" no es una función de díadas: vive en el subconjunto. Lerner, Tranmer, Mowbray y Hâncean (arXiv:1912.07403) lo formulan de modo quirúrgico para correos multi-destinatario: el REM diádico no distingue "Ana escribe a Berta y a Carlos por separado" de "Ana escribe al par {Berta, Carlos}" — y la sociología de equipos está en la segunda.

La solución es cambiar la unidad: el evento es $e = (t_e, h_e)$ donde $h_e \subseteq V$ es la **hiperarista** — el conjunto completo de actores del evento, de tamaño arbitrario. Para nosotros: iniciativa = hiperevento; $h_e$ = los 8--16 firmantes; $V$ = los 154 convencionales.

# 4. El RHEM: tasas sobre conjuntos

## 4.1 La versión no dirigida (nuestra plantilla)

Lerner & Hâncean (*Network Science* 2023; arXiv:2105.01562) modelan coautoría científica — equipos de tamaño variable que publican en fechas — con una tasa relativa sobre cada equipo posible $h$:

$$\lambda_1(t, h; \theta) = \exp\Big(\sum_{i} \theta_i \, s_i(t, h, G[E;t])\Big),$$

donde $G[E;t]$ es la red de eventos pasados. La verosimilitud parcial compara el equipo observado contra los equipos alternativos **del mismo tamaño**:

$$L(\theta) = \prod_{e \in E} \frac{\lambda_1(t_e, h_e; \theta)}{\sum_{h \in R_{t_e}} \lambda_1(t_e, h; \theta)}, \qquad R_{t_e} = \text{hiperaristas candidatas con } |h| = |h_e|.$$

Condicionar en el tamaño es la decisión de diseño clave: el modelo **no** explica *cuántos* firman (eso lo fija la regla 8--16) sino ***cuáles*** — el análogo exacto de nuestro `strata(iniciativa)` en el clogit y del `constraints = ~b2degrees` en el ERGM bipartito.

Las estadísticas de historia se construyen con una sola pieza: el **grado de hiperarista** de un subconjunto $h'$,

$$deg(t, h') = \sum_{e \in E_{<t}} \chi(h' \subseteq h_e) \quad \text{(¿en cuántos eventos pasados apareció junto ese subconjunto?)},$$

opcionalmente con **decaimiento exponencial** $w(\Delta) = \exp(-\Delta \ln 2 / T_{1/2})$ ($T_{1/2}$ = semivida: un evento de hace $T_{1/2}$ días pesa la mitad que uno de hoy — la versión principled de nuestras ventanas de exposición de M2). Sobre él se define la **repetición de subconjuntos de orden $p$**:

$$sub.rep^{(p)}(t,h) = \frac{1}{\binom{|h|}{p}} \sum_{h' \in \binom{h}{p}} deg(t, h'),$$

el promedio del grado histórico de los subconjuntos de tamaño $p$ del candidato $h$. Término a término: con $p=1$ es la **actividad** media de los firmantes (¿los que más firman siguen firmando? — apego preferencial); con $p=2$ es la **familiaridad diádica** media (¿pares que ya co-firmaron?); con $p=3$, la **triádica** (¿tríos consolidados? — esto es lo que ninguna díada puede expresar). Y el **cierre**:

$$closure(t,h) = \frac{1}{\binom{|h|}{2}} \sum_{\{u,v\} \in \binom{h}{2}} \max_{w \neq u,v} \min[deg(t,\{u,w\}),\, deg(t,\{v,w\})]$$

— para cada par del equipo, ¿existe un colaborador común fuerte $w$? (cierre triádico en versión hipergráfica: "los amigos de mis co-firmantes acaban firmando conmigo").

## 4.2 Homofilia y composición del grupo

El paper de referencia formal (Lerner & Lomi, *JRSS-A* 2023; arXiv:2112.10552, versión dirigida "multicast") aporta el catálogo de covariables **de composición** sobre un atributo $z$: el promedio del conjunto ($\sum_{j \in h} z_j / |h|$), la diferencia con un actor focal, y — la que usaremos como homofilia ideológica del grupo — la **dispersión interna** del conjunto sobre $z$ (suma de $|z_j - z_{j'}|$ sobre pares del equipo). Con $z = \theta_1^{fm}$ eso es exactamente "¿cuán ideológicamente compacta es la coalición firmante?"; con indicadores de lista/profesión, la proporción de pares coincidentes. Son nuestras covariables del clogit de M1, ahora acompañadas de las estadísticas de historia.

## 4.3 Dos extensiones que conviene conocer

- **Dos modos** (Lerner, Hâncean & Lomi, *JRSS-A* 2025; arXiv:2308.01722): el evento es un par de conjuntos $(I, J)$ — autores y referencias citadas — con la estadística maestra $subrep^{(k,\ell)}$ que generaliza todas las anteriores. Para nosotros abre la puerta a modelar (firmantes, contenido) si algún día queremos preguntar si subgrupos se especializan en "paquetes temáticos".
- **RHOM** — *relational hyperevent outcome model* (mismo paper de coautoría): condicional a que el equipo se formó, un segundo modelo predice el **resultado** del evento (allí, citas; aquí, supervivencia del texto) **con las mismas estadísticas de hiperarista**. Es la unificación natural de M1 (formación) con M3/M4 (éxito) en un solo lenguaje: ¿lo que junta a las coaliciones es también lo que las hace ganar?
- Precedente regional: Espinosa-Rada, Lerner & Fritz (arXiv:2407.21067) aplican el RHEM orientado a grupos a astrónomos chilenos — cita útil de factibilidad.

# 5. Estimación práctica: caso-control anidado

El conjunto de riesgo es astronómico: hay $\binom{154}{12} \approx 10^{17}$ coaliciones posibles de tamaño 12. Nadie suma ese denominador. La práctica estándar (Lerner & Lomi, *Network Science* 2020; arXiv:1905.00630 — validada con 360 millones de eventos de Wikipedia) es el **muestreo caso-control**: para cada evento observado (el *caso*) se sortean $m$ hiperaristas contrafactuales del mismo tamaño (los *controles*), y se estima

$$\Pr(h_e \mid \text{historia}) = \frac{\exp\{\theta^\top s(h_e)\}}{\exp\{\theta^\top s(h_e)\} + \sum_{\tilde h \in \text{controles}} \exp\{\theta^\top s(\tilde h)\}}$$

— un logit condicional 1:$m$ por estrato. La teoría de *nested case-control* (Borgan--Goldstein--Langholz) garantiza consistencia aunque $m$ sea pequeño; el costo es solo varianza. Crucial: **las estadísticas de historia se calculan sobre TODOS los eventos pasados**, no solo los muestreados — se muestrea el denominador, nunca la historia.

Advertencias prácticas de esa literatura, aplicadas a nuestro caso:

1. **Los efectos de repetición son los que más controles necesitan** (están concentrados en pocas coaliciones). Con solo 528 eventos podemos ser generosos: $m = 50$--$100$ controles por evento cuesta segundos, no horas.
2. **Estandarizar** las estadísticas (media 0, DE 1) antes de ajustar, para comparabilidad de coeficientes.
3. **Re-estimar sobre varias muestras independientes de controles** (5--10) y reportar la estabilidad de $\hat\theta$ — el análogo del "seed sensitivity check".
4. **Empates de fecha**: muchas iniciativas comparten día. La verosimilitud ordinal admite empates a la Efron (como nuestro clogit); la historia se congela al inicio del día (los eventos del mismo día no se ven entre sí) — la misma convención `event_hash`/dedup del pipeline.
5. El **riesgo** debe reflejar elegibilidad real: convencionales activos a la fecha (el roster de 154 es estable; la vacante de Rojas Vade ya está fuera).

# 6. Software: evaluación y decisión

| Herramienta | Dónde | ¿Hipereventos? | Veredicto |
|:---|:---|:---|:---|
| **amorem** (Richter, Boschi, Wit, Lembo) | **CRAN v1.0.0** (2026-06-29) + GitHub | **Sí** (`hyperedge_log`, `hyperedge_features`: `subrep_1/2/...`, `activity`) | **Elegido** — ver abajo |
| eventnet (Lerner) | Java, GitHub | Sí (referencia del campo) | Plan B / verificación cruzada |
| relevent / rem / remstats | CRAN | No (diádicos) | No aplica a co-firma |
| goldfish (DyNAM, Stadtfeld) | CRAN | Parcial (coordinación) | Modela elecciones de actor individual; menos natural para un equipo simultáneo |

**La decisión (2026-07-10), tras verificación con instalación y benchmark local.** `amorem` ("Augmented Modelling of Relational Events") **está en CRAN** — v1.0.0 publicada el 2026-06-29, checks de CRAN limpios, 50 archivos de test, y viene del mismo grupo que la literatura RHEM (Boschi, Lerner & Wit 2025, arXiv:2509.05289). Verificado en esta máquina: `hyperedge_log(I, J, time)` acepta exactamente nuestro formato (conjuntos de firmantes, no dirigido), `hyperedge_features()` calcula `subrep_1` (actividad), `subrep_2` (familiaridad diádica) y `activity` (repetición del conjunto completo), y `rem(..., method = "clogit", case =, stratum =)` estima la verosimilitud caso-control con `survival::clogit` — ajuste en centésimas de segundo.

**Lo que falta y cómo se cubre** (la "capa fina", ~100 líneas, **ya escrita en `code/26`**): (a) el muestreador de controles de `amorem` es solo diádico → sorteamos nosotros $m$ coaliciones del mismo tamaño entre los 154; (b) las covariables de composición (dispersión ideológica del grupo, proporción misma lista, misma comisión) se calculan a mano por candidato — ya lo hacíamos en el clogit de M1; (c) el `closure` poliádico no está en el catálogo → codificarlo sobre el log si un revisor lo pide, o correr `eventnet` una vez como verificación.

**Piloto de timing ejecutado sobre los datos reales (2026-07-14; `code/26-rhem-pilot.R`).** Escalera mínimo→complejo con los 487 hipereventos $\leq 16$:

| Nivel | Configuración | Tiempo |
|:---|:---|:-:|
| N0 | features solo sobre los 487 eventos observados | 6.3 s |
| N1--N3 | caso-control $m=5$: subrep$_1$ / +subrep$_2$ / +subrep$_3$ (secuencial) | 4.8 / 38.3 / 165.5 s |
| N4 | = N3 con `mclapply(8 cores)` | 25.6 s (**speedup $\times 6.5$**) |
| N5 | $m=20$, subrep$_{1\text{-}3}$, 8 cores | 99.1 s (escala lineal en $1+m$) |
| FIT | `rem(method = "clogit")` | 0.02 s |

Extrapolación al spec objetivo ($m = 50$, subrep$_{1,2,3}$ + composición): **~4 min por estimación en 8 cores**; el protocolo de 10 re-muestreos de controles, **~40 min**; cota conservadora con decaimientos y cierre, 2--5 h. **No hace falta modificar el paquete**: el cuello (features por candidato) es vergonzosamente paralelo sobre estratos. Dos lecciones del piloto para el run real: (i) con controles uniformes y $m$ chico hay **cuasi-separación** (las coaliciones reales son casi deterministas en comisión e ideología frente a subconjuntos aleatorios) — se maneja con $m$ grande, features estandarizadas y matching blando por comisión (§9); (ii) el costo lo domina subrep$_3$, que es exactamente la estadística que justifica el aparato — se mantiene. Versión fijada (renv) y sanity check de `subrep` contra cálculo manual.

# 7. El modelo para la Convención, en una tabla

Especificación propuesta (RHEM no dirigido, ordinal, estratificado por tamaño; $m = 50$ controles; $T_{1/2} = 30$ días como base y $\infty$ como robustez):

| Estadística | Qué mide | Hipótesis del proyecto |
|:---|:---|:---|
| dispersión de $\theta_1$ (y $\theta_2$) en $h$ | compacidad ideológica de la coalición | homofilia (M1): $\theta < 0$ |
| prop. de pares misma lista en $h$ | coordinación de lista | $\lambda_{lista}$ dinámico (D2.3: ¿persiste?) |
| prop. de firmantes de la comisión del texto | estructura de oportunidad | D10 |
| $sub.rep^{(1)}$ | actividad previa de los firmantes | firmantes seriales (F9a) |
| $sub.rep^{(2)}$ | pares que ya co-firmaron | **selección** (la hipótesis H2 en su forma nativa) |
| $sub.rep^{(3)}$ | tríos consolidados | coaliciones-núcleo (invisible para díadas) |
| $closure$ | co-firmantes de mis co-firmantes | cierre (el `gwesp` temporal) |

Lectura de conjunto: **este único modelo unifica M1 y M2 en un solo reloj** — la homofilia es el efecto de los atributos, la selección es el efecto de la historia, y ambos se estiman juntos sin proyección, con el N honesto y con el tiempo fino que el panel de ondas discretiza. El paso siguiente natural (RHOM) reutiliza las mismas estadísticas para predecir supervivencia del texto, conectando con M3/M4.

# 8. Lecturas, en orden recomendado

1. **Bianchi, Filippi-Mazzola, Lomi & Wit (2024)**, "Relational Event Modeling", *Annu. Rev. Stat. Appl.* 11:297--319 — arXiv:2306.17581. *El survey; el puente desde ERGMs.*
2. **Butts (2008)**, "A Relational Event Framework for Social Action", *Sociol. Methodol.* 38:155--200. *El fundacional (no está en arXiv).*
3. **Lerner & Hâncean (2023)**, "Micro-level network dynamics of scientific collaboration and impact", *Network Science* 11:5--44 — arXiv:2105.01562. *Nuestra plantilla exacta (coautoría, no dirigido, RHOM).*
4. **Lerner & Lomi (2023)**, "Relational hyperevent models for polyadic interaction networks", *JRSS-A* 186:577--600 — arXiv:2112.10552. *El aparato formal completo.*
5. **Lerner & Lomi (2020)**, "Reliability of relational event model estimates under sampling", *Network Science* 8:97--135 — arXiv:1905.00630. *La justificación del caso-control.*
6. **Lerner, Tranmer, Mowbray & Hâncean (2019)**, "REM beyond dyads" — arXiv:1912.07403. *La motivación anti-díada, corta.*
7. **Lerner, Hâncean & Lomi (2025)**, "RHEM for the coevolution of coauthoring and citation networks", *JRSS-A* 188:583--607 — arXiv:2308.01722. *La generalización $subrep^{(k,\ell)}$, dos modos.*
8. **Espinosa-Rada, Lerner & Fritz (2024/25)**, "Socio-cognitive networks between researchers" — arXiv:2407.21067. *RHEM aplicado a un caso chileno.*

# 9. La implementación que correremos: especificación ejecutable

Esta sección fija, pieza por pieza, el run real — con los datos de fechas que ahora tenemos en mano (2026-07-14). Nada de lo que sigue está corrido; es el diseño para aprobar.

## 9.1 El reloj: las fechas de ingreso de las ICC

**Qué tenemos.** El snapshot `data/raw/initiative_submission_dates.csv` (extraído de CPT `submitted_initiatives` con `code/27`; el pipeline nunca lee de CPT en runtime) contiene las **996 ICC** con su fecha de ingreso a la plataforma (**916 con fecha**; 78 sin dato en el origen y 2 con día irrecuperable), el **autor principal** (`autor_matched`, ya armonizado a nuestros 154 nombres) y los firmantes. De nuestras **487 iniciativas de análisis** ($\leq 16$, D8), **448 (92%) quedan fechadas**; 34 cruzan pero sin fecha y 5 no cruzan (ids anómalos heredados de `sources`). Limpieza documentada con flag `fecha_corregida`: 53 fechas tenían typos evidentes de año ("enero de *2021*", "diciembre de *2022*" — imposibles: la plataforma operó de nov-2021 al 1-feb-2022) y un año truncado ("202"); corregidas a la ventana real. Resultado: rango **2021-11-03 → 2022-02-02**, cero anomalías.

**Por qué las fechas importan tanto en este modelo** — cuatro razones, de la más obvia a la más fina:

1. **La verosimilitud ordinal compara contra el riesgo *de ese momento*.** Cada factor del modelo pregunta "*dado que en $t_k$ se formó una coalición de tamaño $|S_k|$, ¿por qué exactamente esa?*". Si el orden está mal, la pregunta se hace en el momento equivocado: el contrafactual pierde sentido.
2. **Las estadísticas de historia no toleran fugas.** $sub.rep^{(2)}(t, h)$ cuenta cuántas veces los pares de $h$ ya co-firmaron *antes de $t$*. Con un orden incorrecto, eventos futuros se cuelan al pasado y el coeficiente de "repetición" queda contaminado con causalidad invertida — el pecado capital de cualquier modelo de eventos. Nuestro proxy anterior (el prefijo numérico del ICC) correlaciona Spearman **0.955** con la fecha real: bueno para el piloto de timing, pero ese 5% de desorden es exactamente el tipo de ruido que un revisor no perdona en el modelo final.
3. **El decaimiento necesita días de verdad.** $w(\Delta) = \exp(-\Delta \ln 2 / T_{1/2})$ con $T_{1/2} = 30$ *días* es interpretable ("una co-firma de hace un mes pesa la mitad"); sobre rangos ordinales, $\Delta$ no significa nada. Con fechas reales podemos estimar la variante con memoria corta y la de memoria infinita y *compararlas* — eso es sustancia (¿la coalición vive de vínculos frescos o acumulados?), no robustez.
4. **Los empates son un hecho institucional, no ruido.** Las 448 fechadas caen en solo **42 días distintos**, y **156 iniciativas (un tercio) ingresaron el 1 de febrero de 2022 — el día del plazo final**. La convención estándar (los eventos del mismo día no se ven entre sí: la historia se congela al inicio del día; empates a la Efron en el clogit) deja de ser un tecnicismo y se vuelve una decisión sustantiva: para el bloque del deadline, el modelo compara cada coalición contra el riesgo *del 31 de enero*, que es lo correcto — esas 156 coaliciones se armaron en paralelo, no secuencialmente.

**Los 39 sin fecha (34 sin dato + 5 sin cruce).** Propuesta: el run principal usa las **448 fechadas**; los 39 restantes se excluyen como *casos* pero — importante — **no desaparecen del mundo**: sus firmas no entran a la historia (no hay dónde ubicarlas en el reloj), y eso se declara como limitación (8% de eventos). Robustez: re-correr imputándoles la fecha modal de su comisión y verificar que $\hat\theta$ no se mueve.

## 9.2 Los términos, uno a uno

Modelo no dirigido (principal), estratificado por evento; para el candidato $h$ en el día $t$:

| Término | Fórmula | Qué pregunta (en criollo) | Hipótesis |
|:---|:---|:---|:---|
| $sub.rep^{(1)}$ | media de $deg(t, \{i\})$ sobre $i \in h$ | ¿el equipo está hecho de firmantes seriales? | actividad (F9a) |
| $sub.rep^{(2)}$ | media de $deg(t, \{i,j\})$ sobre pares de $h$ | ¿estos pares ya firmaron juntos? | **selección/persistencia** (el corazón de M2 en su forma nativa) |
| $sub.rep^{(3)}$ | media sobre tríos de $h$ | ¿hay *núcleos* de a tres consolidados? | coalición-núcleo (invisible para díadas) |
| disp. $\theta_1$ | media de $\lvert\theta_{1i} - \theta_{1j}\rvert$ sobre pares | ¿coalición ideológicamente compacta? | homofilia (M1) |
| disp. $\theta_2$ | ídem con $\theta_2$ | ¿compacta en el eje plurinacional? | 2ª dimensión (D5/Q5) |
| prop. misma lista | proporción de pares del mismo conglomerado | ¿coordinación de etiqueta? | $\lambda$ dinámico (D2.3: ¿la coordinación *persiste* o decae?) |
| prop. comisión | proporción de miembros de la comisión del texto | ¿estructura de oportunidad? | D10 |

Cada estadística de historia en dos versiones: **memoria infinita** ($w \equiv 1$) y **memoria corta** ($T_{1/2} = 30$ días). Todas las features **estandarizadas** (media 0, DE 1 sobre el pool caso+controles) — coeficientes comparables y sin los números monstruosos del piloto.

**La lectura conjunta que hace único a este modelo:** $\beta$ de atributos (homofilia) y $\beta$ de historia (selección) estimados *en la misma ecuación, con el mismo reloj*. Si $sub.rep^{(2)}$ absorbe lo que el clogit estático atribuía a la ideología, la "homofilia" era en parte inercia relacional; si ambos sobreviven, M1 y M2 quedan unificados con un resultado más fino que cualquiera por separado.

## 9.3 Casos, controles y el fantasma de la separación

Para cada evento observado (caso), $m = 50$ coaliciones control del mismo tamaño. El piloto enseñó que con controles *uniformes* el modelo casi separa perfectamente: una coalición real tiene ~70% de miembros de la comisión del texto y dispersión ideológica mínima; un subconjunto aleatorio de los 154, no — el clogit puede "adivinar" el caso sin esfuerzo y los coeficientes explotan (**cuasi-separación**: la verosimilitud mejora empujando $\hat\beta \to \pm\infty$; síntoma: coeficientes de dos dígitos y "did not converge"). Remedios en el run real, en orden de aplicación: (i) $m = 50$ (más contraste), (ii) estandarización, (iii) **matching blando por comisión**: la mitad de los controles se sortea entre los miembros de la comisión del texto — controles "difíciles" que obligan al modelo a discriminar *dentro* del estrato relevante, no entre obviedades. Esto no sesga: el caso-control anidado admite muestreo no uniforme de controles con el ajuste estándar (los controles siguen siendo intercambiables dentro de su regla de muestreo, que queda documentada).

Protocolo de estabilidad: **10 re-muestreos independientes de controles**, reportando media y rango de cada $\hat\beta$ — el análogo del "seed check".

## 9.4 La variante dirigida (nueva, gracias al dato de autor)

El snapshot trae el **autor principal** de cada ICC (`autor_matched`). Eso habilita la versión *multicast* del RHEM (Lerner & Lomi 2023): $i$ = quien encabeza, $J$ = los co-firmantes que recluta, con estadísticas dirigidas (`subrep_1_1`: ¿a quién recluta repetidamente el mismo autor?). Preguntas nuevas: ¿el reclutamiento es más homofílico que la adhesión? ¿los autores seriales (emprendedores legislativos) reclutan distinto? Queda como **extensión secundaria** tras el run no dirigido — la co-firma sigue siendo conceptualmente conjunta y el "autor" administrativo puede no ser el redactor real (la misma cautela de P4b).

## 9.5 Costo esperado y entregables

Por el piloto (§6): **~4 min por estimación** (8 cores), **~40 min** el protocolo completo de 10 re-muestreos por especificación; con las dos memorias ($w \equiv 1$ y $T_{1/2} = 30$) y la variante dirigida, la sesión completa cabe en **~2--3 horas**. Entregables: tabla de coeficientes estandarizados con estabilidad entre re-muestreos, comparación memoria corta/infinita, y la lectura conjunta homofilia-vs-historia para el reporte (sección nueva de M1/M2 unificados).

# 10. El run real: resultados y lectura (2026-07-14)

Ejecutado tal como especifica §9 (`code/28-rhem-run.R`). Ficha técnica: **448 eventos fechados** (39 excluidos sin fecha, 8%), tiempo en días desde 2021-11-01 con historia estricta por día; $m = 50$ controles por evento (25 uniformes + 25 con matching blando por comisión); features **estandarizadas** (los coeficientes se leen "por desviación estándar"); $B = 10$ re-muestreos de controles; **140/140 ajustes convergieron vía `amorem::rem(clogit)`** — sin fallback, sin cuasi-separación. Sanity superado: el motor propio de features (vectorizado: $|S_e \cap h|$ por producto matricial y $\binom{k}{r}$) coincide exactamente con `amorem::hyperedge_subrep` en memoria infinita. Nota honesta de costos: la estimación de §9.5 (~40 min) presumía el motor R del paquete; el motor matricial propio la colapsó a **~6 segundos el protocolo completo** — la sobre-estimación es el mejor error posible.

## 10.1 La tabla principal

Coeficientes estandarizados; media (y DE) sobre los 10 re-muestreos; `RHEM_summary.csv`:

| Término | Memoria infinita | Semivida 30 días |
|:---|:-:|:-:|
| subrep$_2$ — familiaridad de pares | $\mathbf{+6.17}$ (0.69) | $+5.77$ (0.70) |
| subrep$_1$ — actividad individual | $-6.02$ (0.73) | $-5.32$ (0.71) |
| dispersión $\theta_1$ | $\mathbf{-2.35}$ (0.13) | $-2.37$ (0.12) |
| subrep$_3$ — familiaridad de tríos | $-1.92$ (0.44) | $-1.72$ (0.38) |
| prop. misma lista | $\mathbf{+0.89}$ (0.14) | $+0.95$ (0.13) |
| dispersión $\theta_2$ | $-0.43$ (0.16), $p \approx 0.05$ | $-0.44$ (0.16) |
| prop. comisión | $-0.34$ (n.s.) | $-0.29$ (n.s.) |

Los coeficientes son estables entre re-muestreos (las DE son fracciones pequeñas de los efectos) y las dos memorias cuentan la misma historia; por log-verosimilitud en el mismo dataset **gana la memoria infinita** ($-68.0$ vs. $-73.1$): las coaliciones se construyen sobre el capital de co-firma *acumulado*, no solo sobre el del último mes — sensato en una ventana de apenas tres meses.

## 10.2 Cómo leer los signos de las subrep (la parte que engaña)

Las tres estadísticas de historia están fuertemente correlacionadas (sr$_1$--sr$_2$: 0.78; sr$_2$--sr$_3$: 0.84), y eso hace que los coeficientes *conjuntos* no se lean por separado. La prueba (`RHEM_aux.csv`): **cada una a solas es positiva** —

| Modelo | subrep$_1$ | subrep$_2$ | subrep$_3$ | logLik |
|:---|:-:|:-:|:-:|:-:|
| solo sr$_1$ | $+1.26$\*\* | — | — | $-99.9$ |
| solo sr$_2$ | — | $+1.45$\*\*\* | — | $-84.0$ |
| solo sr$_3$ | — | — | $+0.81$\*\*\* | $-93.3$ |
| las tres | $-6.87$ | $+6.89$ | $-2.55$ | $-68.0$ |

Marginalmente, la actividad, la familiaridad de pares y la de tríos *todas* predicen a la coalición real. Juntas, sr$_2$ se lleva la señal y sr$_1$/sr$_3$ quedan negativas: es *partialling* clásico entre regresores casi colineales, no una paradoja. La lectura conjunta correcta: **lo que distingue a una coalición real de una falsa es específicamente la familiaridad de sus pares** — por encima de lo que implicarían la actividad individual de sus miembros ("gente ocupada junta") y el solapamiento de tríos. Condicional en pares familiares, un exceso de actividad individual sin familiaridad mutua es señal de coalición *falsa* (así se ven los controles hechos de firmantes seriales que no se conocen), y los tríos no agregan sobre los pares — las coaliciones de la Convención se tejieron **de a dos**, no reciclando equipos completos (coherente con la falta de "juego repetido" de D8).

## 10.3 La lectura sustantiva: homofilia e historia coexisten

El resultado que este modelo venía a buscar — M1 y M2 en la misma ecuación, con el mismo reloj:

1. **La historia importa** (sr$_2$ es el efecto más grande del modelo): la co-firma pasada de los pares predice la coalición de hoy. Es la *selección/persistencia relacional* de M2, ahora en su unidad nativa.
2. **La homofilia ideológica sobrevive a la historia**: dispersión de $\theta_1$ = $-2.35$\*\*\* *condicional* en las tres subrep. No era inercia relacional disfrazada de afinidad — es afinidad. ($\theta_2$ queda marginal, $p \approx 0.05$.)
3. **La coordinación de lista persiste sobre ideología E historia** (prop. lista $+0.89$\*\*\*): la respuesta a la pregunta D2.3 que quedaba abierta — el sello de "partido embrionario" no es reducible ni a preferencias compartidas ni a haber firmado juntos antes; hay *organización* operando en cada nueva coalición.
4. **Prop. comisión no se lee de esta tabla**: con la mitad de los controles muestreada *dentro* de la comisión del texto, la distribución de controles absorbe deliberadamente ese contraste (era el remedio a la cuasi-separación). El efecto comisión ya está establecido por el clogit de M1 (OR 3.4); aquí es parte del diseño, no un hallazgo.

Caveats registrados: (i) los 39 eventos sin fecha quedan fuera como casos y fuera de la historia (8%; robustez de imputación pendiente si se promueve al paper); (ii) los EE de sr$_1$/sr$_2$ son grandes por colinealidad (aún así $p < 10^{-4}$) — para el paper conviene reportar la tabla conjunta *y* la de "a solas"; (iii) el bloque del deadline (156 eventos el 1-feb) se evalúa contra la historia al 31-ene, la convención correcta pero que comprime un tercio del riesgo en un solo instante.

**Este resultado no entra aún al reporte principal** (`networked-influence-study`): la decisión de qué modelo encabeza M1 — y cómo se reparten clogit estático, Poisson diádica+boot y RHEM — se discute primero con el autor.
