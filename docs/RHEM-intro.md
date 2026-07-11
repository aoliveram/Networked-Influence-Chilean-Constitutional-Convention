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

**Lo que falta y cómo se cubre** (la "capa fina", ~100 líneas): (a) el muestreador de controles de `amorem` es solo diádico → sortear nosotros $m$ coaliciones del mismo tamaño entre los 154 (unas 20 líneas); (b) las covariables de composición (dispersión ideológica del grupo, proporción misma lista, misma comisión) se calculan a mano por candidato — ya lo hacemos en el clogit de M1; (c) el `closure` poliádico no está en el catálogo → codificarlo sobre el log si un revisor lo pide, o correr `eventnet` una vez como verificación.

**Sobre la paralelización — la respuesta a la duda del autor:** el benchmark con la forma exacta de nuestros datos (528 eventos, 8--16 de 154) da **41 s en un solo core** con 5 controles por evento; con 50 controles, ~5--10 min secuencial, y ~1 min envolviendo el loop por estratos en `parallel::mclapply(mc.cores = 8)` — porque cada estrato es independiente dada la historia observada. El ajuste en sí tarda centésimas de segundo. **No existe la "ganancia enorme" que justificaría una implementación propia**: vamos con el paquete, versión fijada (renv), con un sanity check de `subrep` contra cálculo manual en 2--3 eventos.

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
