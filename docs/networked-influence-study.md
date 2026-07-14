---
title: "Networked Influence in a Tabula Rasa Legislature — Reporte del estudio"
subtitle: "Redes de co-patrocinio, dinámicas ideológicas y éxito político en la Convención Constitucional de Chile (2021--2022)"
author: "A. Olivera, J. Fábrega"
date: \today
geometry: "margin=2.5cm"
fontsize: 10pt
colorlinks: true
linkcolor: teal
monofont: "Menlo"
monofontoptions: "Scale=0.82"
toc: true
toc-depth: 3
header-includes:
  - \usepackage{amsmath}
  - \usepackage{amssymb}
  - \usepackage{booktabs}
  - \usepackage{array}
  - \usepackage{pdflscape}
---

# 1. Objetivos del proyecto

## 1.1 Contexto: la Convención Constitucional como experimento natural

La Convención Constitucional chilena (julio 2021 -- julio 2022) es un caso excepcionalmente limpio para estudiar la **génesis de redes políticas**. A diferencia de una legislatura ordinaria, donde las redes de colaboración observadas son el sedimento de décadas de carreras compartidas, disciplinas de partido y expectativas de reelección, la Convención partió de un cuasi-*tabula rasa* organizacional:

1. **Actores mutuamente desconocidos.** De los 155 escaños, una mayoría inédita correspondió a independientes y outsiders sin trayectoria parlamentaria, electos por listas ad hoc y escaños reservados para pueblos originarios (17). Gran parte de los delegados no se conocía entre sí ni compartía socialización partidaria.
2. **Sin jerarquías heredadas.** No existían comités con seniority, liderazgos de bancada consolidados ni "sombra del futuro" electoral común: el órgano se disolvía al entregar el texto.
3. **Reglas endógenas.** La Convención escribió su propio reglamento, incluyendo el mecanismo central que explota este estudio: toda **iniciativa convencional constituyente** requería el patrocinio de **al menos 8 y no más de 16 convencionales** (Reglamento General, art. 83). El piso obliga a formar coaliciones mínimas para participar del proceso; el techo impide que las firmas se diluyan en mega-bloques y convierte cada patrocinio en un recurso escaso y estratégico.
4. **Horizonte comprimido.** Todo el ciclo — formación de lazos, negociación, producción normativa y resultado (el borrador del 14-05-2022) — ocurre en ~10 meses y está íntegramente documentado, con autoría explícita en cada iniciativa e indicación.
5. **Regla de decisión supermayoritaria.** Cada norma constitucional requería **2/3 de los convencionales en ejercicio** en el Pleno (art. 133 CPR; art. 96 del Reglamento; en la práctica de 2022, 103 votos de 154), mientras que dentro de las comisiones bastaba la **mayoría simple** (art. 92). Esa asimetría — nacer es barato, sobrevivir es caro — es la estructura institucional contra la que compiten las explicaciones de red de este estudio.

El trabajo se organizó en **siete comisiones temáticas**, cuyos nombres oficiales completos (verificados contra el registro oficial `cconstituyente.cl`, Wikipedia y UNESCO) son:

1. **C1** — Sistema Político, Gobierno, Poder Legislativo y Sistema Electoral
2. **C2** — Principios Constitucionales, Democracia, Nacionalidad y Ciudadanía
3. **C3** — Forma de Estado, Ordenamiento, Autonomía, Descentralización, Equidad, Justicia Territorial, Gobiernos Locales y Organización Fiscal
4. **C4** — Derechos Fundamentales
5. **C5** — Medio Ambiente, Derechos de la Naturaleza, Bienes Naturales Comunes y Modelo Económico
6. **C6** — Sistemas de Justicia, Órganos Autónomos de Control y Reforma Constitucional
7. **C7** — Sistemas de Conocimientos, Culturas, Ciencia, Tecnología, Artes y Patrimonios

El co-patrocinio es, por tanto, un **proxy observable, costoso y fechado de colaboración política**, y la Convención permite observar el ciclo completo: cómo se forman los lazos entre extraños (RQ1), qué hacen esos lazos con las posiciones de los actores (RQ2), y cómo la posición en la red se traduce en éxito sobre el texto final (RQ3).

## 1.2 Preguntas de investigación

- **RQ1 — Formación.** ¿Qué factores impulsan la decisión de co-firmar cuando no existen jerarquías previas? ¿Domina la ideología, la coordinación de lista, la estructura de oportunidad (la comisión), o la afinidad demográfica/profesional?
- **RQ2 — Influencia vs. selección.** ¿La exposición a los co-autores desplaza el comportamiento de voto revelado de los delegados (influencia social), o los delegados seleccionan co-autores ya afines (selección endógena)?
- **RQ3 — Éxito.** ¿Cómo predice la estructura de red el éxito político individual, operacionalizado como **retención léxica**: cuánto del texto que un delegado patrocinó sobrevive en el borrador constitucional final? ¿Y cuánto de ese "derrame de éxito" es en realidad geometría espacial bajo la regla de 2/3?

## 1.3 Marco teórico

*(Apartado en construcción: esta sección fija el esqueleto argumental y se irá puliendo junto con la especificación final de los modelos.)*

**(a) Formación de lazos en legislaturas: homofilia y sus límites.** La literatura de co-sponsorship en legislaturas consolidadas (Fowler 2006; Bratton & Rouse 2011; Kirkland 2011) documenta con robustez que los lazos siguen líneas de partido, región y género — homofilia en el sentido de McPherson, Smith-Lovin & Cook (2001). Pero esa literatura no puede separar cuánto de la homofilia observada es preferencia y cuánto es estructura heredada (comités, bancadas, historia). La Convención remueve la estructura heredada: lo que se observe en la formación temprana refleja preferencias y estrategias de actores en un campo casi vacío. **H1a (línea base):** persiste la coordinación por afiliación/lista política, el clivaje más visible incluso entre independientes, *condicional en la distancia ideológica*.

**(b) Gatekeeping estratégico: ¿importan los "dotados"?** Para los actores con recursos escasos y transversalmente valiosos — pericia jurídica (abogados, en una asamblea cuyo producto es un texto legal) y experiencia institucional previa — la teoría de brokerage (Burt 1992; Padgett & Ansell 1993) predice que maximizan influencia **dispersándose** entre coaliciones, ocupando agujeros estructurales en vez de agruparse entre sí. **H1b:** abogados y delegados con experiencia institucional organizan su co-firma de forma distinta al resto — sea agrupándose (homofilia) o dispersándose (brokerage). Se testea en las dos métricas que corresponden: afinidad profesional en la *decisión de firma* (M1) y *constraint/betweenness* de Burt en la red resultante (M1 — Brokerage).

**(c) Influencia social vs. selección endógena.** Que los conectados se parezcan ideológicamente admite dos mecanismos generativos: influencia (los lazos mueven las posiciones; Friedkin & Johnsen 1990) o selección (las posiciones crean los lazos). Distinguirlos es el problema clásico de la econometría de redes (Shalizi & Thomas 2011; en la tradición SAOM, Steglich, Snijders & Pearson 2010). Nuestra estrategia: panel con efectos fijos individuales (toda heterogeneidad estable del delegado queda absorbida) más un **test de falsificación** con exposición futura (*lead*). **H2:** en un cuerpo con posiciones políticas pre-formadas, domina la selección; el efecto de exposición desaparece bajo efectos fijos.

**(d) El éxito legislativo como fenómeno colectivo — y su rival institucional.** La efectividad legislativa suele tratarse como atributo individual (Volden & Wiseman 2014). Pero si la unidad de producción es la coalición firmante — y el reglamento fuerza que lo sea — el éxito debería *derramarse* por los lazos de co-autoría. Econométricamente eso es autocorrelación espacial en la variable de resultado (LeSage & Pace 2009). **H3:** la retención léxica exhibe dependencia de red fuerte; los modelos con rezago espacial dominan al OLS. El rival teórico es la **política pivotal** (Krehbiel 1998): bajo 2/3, sobrevive lo que tolera el convencional pivotal $\theta_{(103)}$ — si la "dependencia de red" fuera solo cercanía compartida al pívot, $\rho$ debería colapsar al controlar por la distancia al pívot (M3 — Pívot).

**(e) Anclaje ideológico y de comportamiento.** Dos mediciones complementarias, con papeles distintos (decisión 2026-07-10, tras la revisión crítica):

- **Ideología pre-red (covariable exógena de M1/M3):** puntos ideales **2D estimados solo con las votaciones del primer mes** del Pleno (4-jul a 12-ago-2021; W-NOMINATE, réplica del diseño de Fábrega 2022: clasificación correcta 89.4%/91.6% vs. 89.25%/91.43% del original; $r(\theta_1^{fm}, \theta^{dynIRT}) = 0.979$). Esa ventana es **anterior a las comisiones temáticas** (creadas en oct-2021) y **anterior a la regla de 2/3 operativa** (las 146 votaciones del período se decidieron por mayoría): $\theta^{fm}$ es exógena a la red y al voto estratégico supermayoritario. La segunda dimensión separa a los escaños reservados del eje clásico (el eje "plurinacional" de Fábrega).
- **Voto revelado dinámico (variable de M2):** dynIRT unidimensional (Martin & Quinn 2002; `emIRT`, $\omega^2 = 0.025$, anclas Marinovic/Baradit) sobre los 4.707 roll-calls: $\Theta \in \mathbb{R}^{154 \times 91}$. Tras agosto de 2021 el Pleno vota bajo reglas y agendas cambiantes, por lo que $\theta_{i,t}$ se interpreta como **comportamiento de voto revelado**, no como ideología latente pura (advertencia metodológica de Fábrega 2022, adoptada).

## 1.4 Diseño: tres modelos

**M1 — Formación (logit condicional de elección de firma + ERGM bipartito).** La unidad de observación es la **decisión de firma**: cada iniciativa $a$ es un "menú" y cada convencional $i$ decide sumarse o no. Especificación principal (McFadden):
$$U_{ia} = \beta^\top x_{ia} + \alpha_a + \varepsilon_{ia},$$
con $\alpha_a$ = **efecto fijo por iniciativa** (absorbido por strata) y $x_{ia}$ = distancias/afinidades de $i$ a la coalición firmante. Robustez estructural: **ERGM bipartito** convencionales × iniciativas. *(La especificación anterior — Valued ERGM sobre la proyección persona-persona — fue abandonada tras la revisión crítica por pseudo-replicación de cliques: cada iniciativa de 16 firmantes fabricaba 120 díadas correlacionadas tratadas como independientes. Scripts y resultados archivados en `old-version/`.)*

**M2 — Dinámica del voto revelado (panel FE).** Sobre ondas temporales por comisión:
$$\Delta\theta_{i,t} = \alpha_i + \beta_1 \theta_{i,t-1} + \beta_3\, \text{NetExp}_{i,t-1} + \varepsilon_{it}, \qquad \text{NetExp}_{i,t-1} = \frac{\sum_{j \neq i} w_{ij,t-1}\,\theta_{j,t-1}}{\sum_{j \neq i} w_{ij,t-1}}$$
con $w_{ij,t-1}$ = co-firmas acumuladas hasta la onda $t-1$. Comparación OLS agrupado vs. efectos fijos vs. aleatorios (Hausman), errores agrupados por delegado, y falsificación con *lead*. Complemento conductual: modelo de **co-defección de voto** sobre la red génesis (M2 — Defección).

**M3 — Éxito (Spatial Durbin Model + test pivotal).** Con $y_i'$ = retención léxica esperada del delegado (fracasos = 0) y $\tilde{W}$ = red de co-autoría row-normalizada:
$$y' = \rho \tilde{W} y' + X\beta + \tilde{W}X\gamma + \varepsilon,$$
comparado por AIC con OLS/SAR/SEM, diagnóstico de Moran, y el **test pivotal**: agregar a $X$ la distancia al pívot de 2/3, $|\theta^{fm}_{1,i} - \theta_{1,(103)}|$, y observar qué pasa con $\rho$.

# 2. Datos

## 2.1 Fuentes primarias

Las tablas con las rutas exactas de cada fuente y el desglose por comisión están en el **Anexo B** (orientación horizontal). Desde 2026-07-07 los datos viven como **snapshot versionado dentro de este repositorio** (`data/raw/dataverse-final/`, copiado de CPT `paper-draft` @ `6fac4c4`; ver P8/Anexo A). El test de aceptación `code/0a-verify-dataverse-snapshot.py` valida cada refresco y escribe `QA-report.txt`. En resumen: **1.892 iniciativas génesis**, **2.019 registros de trazabilidad** (`TRACK_full`), **498 artículos** en el borrador final del 14-05-2022, **154 convencionales** con perfil curado, matriz de puntos ideales $\Theta \in \mathbb{R}^{154\times 91}$, puntos ideales 2D del primer mes (`ideal_points_2d_firstmonth.csv`) y mapeo de listas electorales (`electoral_lists.csv`).

## 2.2 Esquema de los datos de trazabilidad

`TRACK_full` es, por comisión, un arreglo de registros. Hay dos tipos de registro — **artículo** e **indicación suelta** — y dentro de cada artículo, un arreglo `history[]` con la trayectoria de sus indicaciones. Este es el mapa de dónde vive cada dato que el pipeline consume (en **negrita**, los campos que alimentan cada modelo):

```text
TRACK_full.json  (un arreglo por comisión: C1..C7)
│
├── registro tipo ARTÍCULO ─────────────── una norma en discusión
│    ├─ article_uid   "C3_GEN_CH01_ART01"     id único (guiones bajos)
│    ├─ article       "Artículo 1"
│    ├─ timestamp     "01-27"                 fecha del informe génesis (MM-DD, año 2022)
│    ├─ text          "Del Estado Regional…"  TEXTO GÉNESIS  → sim. léxica (M3)
│    ├─ sources       ["43-3", …]             iniciativa(s) de origen (= unidad de red)
│    ├─ authors       ["Uribe, Cesar", …]     FIRMANTES DEL GÉNESIS  → decisiones de M1
│    ├─ final_status  "Idéntico a 142.-…"     desenlace  → DV de M3
│    │                 {Idéntico | Similar | ART-FALLIDO | eliminado}
│    └─ history[]  ─────────────────────────  trayectoria de indicaciones (0..n enmiendas)
│         └── entrada tipo INDICACIÓN
│              ├─ timestamp        "02-14"        FECHA DE LA INDICACIÓN  → ondas de M2
│              ├─ authors          ["Mella, …"]   FIRMANTES DE LA INDICACIÓN  → red de M2
│              ├─ step             "Indicacion"
│              ├─ action           "ADD" | "DELETE" | …
│              ├─ content          ", plurinacional…"
│              ├─ content_snapshot "…texto tras aplicar la enmienda…"  → dinámica de retención (M3)
│              └─ target_scope, number, placement_instructions, content_to_remove
│
└── registro tipo INDICACIÓN SUELTA ────── enmienda NO anidada a un artículo (210 en total)
     ├─ action, content, target_scope, …    mismos campos de indicación, pero al nivel superior
     ├─ timestamp   "04-01"                  alimenta ondas de M2 igual que una entrada de history[]
     └─ authors     [...]                    FIRMANTES  → red de M2
```

En palabras: **los autores de la iniciativa original** están en `authors` a nivel de artículo y definen las decisiones de firma de **M1**; **los autores de cada enmienda** están en `authors` dentro de cada entrada de `history[]` (o en el registro suelto) y, junto con su `timestamp`, definen las ondas de **M2**; el desenlace de cada artículo (`final_status`) y su `text` génesis definen la variable de éxito de **M3**, y los `content_snapshot` permiten reconstruir la trayectoria textual (§M3 — Dinámica).

**Idiosincrasias que el pipeline absorbe** (detalle en Anexo A / P8):

- El esquema del GENESIS sigue **heterogéneo** entre comisiones (C1/C2/C3 sin `article_uid`; C4 sin `article`/`sources`; `authors` intermitente): el loader lo unifica.
- Las **210 indicaciones sueltas** (todas las comisiones salvo C2): 205/210 traen `authors` + `timestamp` y alimentan las ondas de M2; no entran a M1 ni a M3.
- `step` normalizado a `"Indicacion"`; `timestamp` es `MM-DD[-bloque]` con año implícito 2022 (66 NA top-level + 5 anidados).
- `authors` puede contener referencias a **iniciativas populares** ("7-2", "Iniciativa Popular Indígena 21-2"): `code/lib_names.py` las marca como no-persona y las excluye. Con esa regla, **75.616 menciones de autor-persona resuelven al 100%** contra los 154 canónicos (QA 0a).

## 2.3 Regla de asignación de covariables de perfil

Los perfiles de `data/raw/conventional-profiles.json` (154 filas) se construyen con `code/0b-audit-profiles-dual-source.py --impute`. Para **cada convencional × cada covariable**, el valor final se decide por esta escalera de prioridad:

```text
  1.  manual_validations.json   ── si hay validación humana para esa celda  →  se usa  (fin)
  2.  BCN (ground truth)        ── si BCN informa el dato                    →  se usa
  3.  Wikipedia                 ── si BCN calla pero Wikipedia informa       →  se usa
  4.  valor base (scraper)      ── si ninguna fuente informa                 →  se conserva

  Extracción:  gemini-3.5-flash lee BCN y Wikipedia por SEPARADO, reportando la
               situación al momento de ser convencional (jul-2021 .. jul-2022).
  Concordancia por celda (profile_audit_table.csv):
     =  las dos fuentes coinciden      ≠  discrepan
     B  solo BCN informa   W  solo Wikipedia   ∅  ninguna
```

Definiciones por campo: `es_mujer` = género informado por la fuente (no inferido del nombre); `edad_al_asumir` = edad al 04-07-2021; `afiliacion_agrupada` = partido con militancia vigente durante la Convención o "Independiente" (los escaños reservados llevan su afiliación partidaria real y el pueblo queda en `distrito` = "Escaño reservado: \<pueblo\>"); `es_abogado` = profesión de abogado/a; `grado_academico_nivel` (0--3) = 0 sin estudios universitarios terminados, 1 educación superior terminada, 2 magíster/máster (no diplomado), 3 doctorado; `experiencia_previa_institucional` = 1 si antes de jul-2021 ocupó cargo público (parlamentario/a, alcalde/sa, concejal/a, CORE, seremi, intendente/a, ministro/a, subsecretario/a, embajador/a, jefatura de servicio).

Estado vigente: **154/154 perfiles**, 0 afiliaciones desconocidas, 0 edades faltantes; grado 0--3 = \{0: 18, 1: 81, 2: 41, 3: 14\}. Además: `data/raw/electoral_lists.csv` mapea los 154 a su **lista electoral de origen** (BCN; conglomerados exactos a la Tabla 1 de Fábrega 2022: Vamos por Chile 37, Apruebo Dignidad 28, Lista del Apruebo 25, Lista del Pueblo 23, PPOO 17, INN 3; 21 listas locales marcadas `REVISAR` a la espera del crosswalk de replicación de Fábrega).

# 3. Exploración de datos

*(El detalle de los scripts del pipeline está en el **Anexo C**.)*

## 3.1 Insumos construidos

**Registro de iniciativas** (`initiative_registry.csv`): **528 iniciativas** con $\geq 2$ firmantes-persona — C1: 29, C2: 56, C3: 41, C4: 68, C5: 148, C6: 78, C7: 108 (las iniciativas populares/indígenas quedan fuera de la red de personas). De ellas, **487 forman el conjunto de análisis**: las 41 con >16 firmantes (imposibles bajo la regla ICC; en auditoría como duplicaciones transversales, §4.1.5) quedan **excluidas de toda red y menú** por decisión del 2026-07-11 (`MAX_SIGNERS = 16` en `code/00`; el registro y F9 las siguen documentando). El tope **no** se aplica a las indicaciones, que legítimamente podían llevar >16 firmas (art. 95). La Figura F1 contrasta el conteo de iniciativas con el de artículos génesis por comisión: la brecha entre ambas barras es el número de artículos por iniciativa (C5 es el caso extremo: 148 iniciativas → 420 artículos).

![**F1.** Iniciativas constitucionales (barra sólida) sobre artículos génesis (barra translúcida) por comisión, ambos con $\geq 2$ firmantes.](../results/figures/initiatives_per_commission.pdf){width=80%}

**Red génesis-iniciativa** (para descriptivos, W de M3 y brokerage; ya con el filtro $\leq 16$): 154 nodos (sin aislados), **5.870 aristas**, peso total 34.857, peso máximo 68. **Red génesis-artículo** (robustez): 1.565 eventos de co-firma, 5.881 aristas, peso 119.469. El filtro no era cosmético: las 41 mega-iniciativas fabricaban ~35% del peso total de la red anterior (7.731 aristas / peso 53.391). La Figura F2 muestra ambas redes como grafos bipartitos documento–convencional, con los convencionales ordenados por punto ideal (rojo = izquierda, azul = derecha); *sus paneles corresponden al registro completo pre-filtro y se regenerarán tras la auditoría*.

![**F2a.** Co-patrocinio génesis como red bipartita, **unidad iniciativa** (principal). Documentos arriba (color por comisión); convencionales abajo, ordenados y coloreados por punto ideal — **rojo = izquierda** ($\theta<0$; Baradit $-1.4$), **azul = derecha** ($\theta>0$; Marinovic $+4.3$), con colorbar.](../results/figures/bipartite_initiative.pdf){width=100%}

![**F2b.** Co-patrocinio génesis como red bipartita, **unidad artículo** (robustez), mismas convenciones.](../results/figures/bipartite_article.pdf){width=100%}

**Distribuciones de firma** (Figura F9): la firma **no tiene presupuesto** — mediana 42 iniciativas firmadas por convencional, media 45, máximo **157** de 528 (hay firmantes seriales y selectivos; la premisa de "firma costosa" del marco se matiza a *costosa para la iniciativa, no para el firmante*). Y la regla 8--16 produce **bunching en el tope**: la moda es exactamente 16 firmantes (108 iniciativas), con masa secundaria en 9--10 — el cupo se llenaba como demostración de fuerza. Anomalía en auditoría: 41 iniciativas registran >16 firmantes (§4.1.5).

![**F9.** (a) Iniciativas firmadas por convencional; (b) firmantes-persona por iniciativa, con la regla 8--16.](../results/figures/signature_distributions.pdf){width=100%}

**Ondas para M2** (T0 génesis + fechas observadas de informes de indicaciones; los duplicados de una misma indicación repetida en el `history[]` de varios artículos se colapsan a un acto):

| Comisión | Ondas de indicaciones | Eventos con lazos | Unipersonales | Duplicados colapsados |
|:-:|:-:|:-:|:-:|:-:|
| C1 | 4 | 266 | 99 | 45 |
| C2 | 3 | 1 | 52 | 0 |
| C3 | 6 | 212 | 29 | 166 |
| C4 | 5 | 154 | 241 | 49 |
| C5 | 5 | 0 | 559 | 0 |
| C6 | 6 | 317 | 64 | 291 |
| C7 | 8 | 54 | 278 | 178 |

**Mapeo y DV de éxito**: 484 pares génesis$\to$final (480 vía `coincidencias` + 4 rescatados parseando `final_status`; 2 uids de indicación no encontrados). Desenlaces de los 1.809 artículos: 223 idéntico, 136 similar, 275 ART-FALLIDO, 1.175 eliminado (fracaso = fallido + eliminado, sim = 0). **Validación del emparejamiento**: similitud alineada 0.554 vs. 0.032 con pares barajados; 96.5% de los pares supera su baseline. TF-IDF idéntico 0.570 / similar 0.565; SBERT 0.857 / 0.872. *Nota sobre el ancla de validación*: la v1 reportaba 0.979 en "idénticos" porque comparaba el texto **post-indicaciones** con el borrador; la v2 usa el texto **génesis verdadero** (la operacionalización correcta de "retención de lo que el delegado propuso"), y la etiqueta idéntico/similar — que refiere al estado *final* del artículo — deja de ordenar la similitud génesis$\to$final; la validación pasa a ser el contraste alineado-vs-barajado. Media de $y'$: 0.094; tasa de supervivencia media: 0.211; **154/154 convencionales con score**.

## 3.2 Limitaciones de datos (transversales)

1. **Cobertura de autores.** 134/2.019 registros TRACK sin autores (65 de iniciativas populares/indígenas — patrocinio institucional —, 45 de ICC no recuperadas, 24 sin referencia); 54 artículos del borrador final quedaron `not_traced` en `coincidencias`; 66 + 5 registros *undated* se excluyen de las ondas.
2. **dynIRT sin errores estándar** (P10, abierto): la incertidumbre de $\theta$ no se propaga a M2/M3.
3. **41 iniciativas con >16 firmantes** (imposible bajo la regla ICC): candidatas a duplicación transversal entre comisiones (la iniciativa 954 aparece con 83 firmantes en C4, C5 y C6 a la vez). **Excluidas de todos los análisis desde v3.1** (decisión 2026-07-11); auditoría aguas arriba pendiente (§4.1.5).

Las limitaciones específicas de cada modelo están al final de su sección (M1/M2/M3 — Limitaciones).

# M1 — Formación de la red (elección de firma)

## M1 — Por qué esta especificación

Las firmas vienen **en paquetes**: cada iniciativa aporta un conjunto de 8--16 firmantes simultáneos. La proyección persona-persona (la base de la especificación anterior, archivada en `old-version/`) convierte cada paquete en $\binom{|S_a|}{2}$ díadas tratadas como observaciones independientes — con 16 firmantes, un solo acto político fabrica 120 "observaciones" — y eso infla la precisión de todo lo que se parezca a homofilia (pseudo-replicación). La corrección es devolver la unidad de análisis al acto real: **la decisión de cada convencional de firmar o no cada iniciativa**.

## M1 — Especificación

**Principal — logit condicional (McFadden).** Para el menú de la iniciativa $a$ con coalición firmante $S_a$ (74.998 decisiones = 487 × 154, tras la exclusión $\leq 16$):
$$U_{ia} = \beta_\theta\, d^{\theta_1}_{ia} + \beta_{\theta_2}\, d^{\theta_2}_{ia} + \kappa\, \text{comisión}_{ia} + \textstyle\sum_c \lambda_c\, L^c_{ia} + \boldsymbol{\pi}' A_{ia} + \alpha_a + \varepsilon_{ia},$$

donde $d^{\theta}_{ia} = |\theta^{fm}_i - \bar\theta^{fm}_{S_a}|$ (distancia a la posición media de la coalición; *leave-one-out* para firmantes), $\text{comisión}_{ia} = \mathbf{1}\{i \in \text{comisión de } a\}$ (estructura de oportunidad), $L^c_{ia} = \mathbf{1}\{lista_i = lista\ modal\ de\ S_a = c\}$ (coordinación de lista, un $\lambda$ por conglomerado), y $A_{ia}$ = afinidades demográficas ($es\_X_i \times$ proporción de $X$ en $S_a$ para abogado/experiencia/mujer; $|grado_i - \overline{grado}_{S_a}|$). El efecto fijo $\alpha_a$ absorbe todo lo que hace atractivo al documento (tema, redactor, momento) y se elimina por strata; los EE se agrupan por convencional. La ideología es la **2D del primer mes** (pre-red, pre-comisiones, pre-2/3; §1.3e).

**Robustez estructural — ERGM bipartito.** Red de dos modos $154 \times 528$ (convencional–iniciativa), donde la dependencia dentro de cada iniciativa está representada por el propio nodo iniciativa: términos de grado (`gwb1degree`, `gwb2degree`), firma cruzada `nodematch(comision)`, y homofilia entre co-firmantes vía `b1nodematch` (conglomerado, quintil de $\theta_1$, abogado, experiencia, mujer). *(La variante que condiciona en el grado de cada iniciativa — `constraints = ~b2degrees`, el análogo exacto del strata — resultó computacionalmente impracticable: el MCMC no mezcla; queda documentado en `code/16`.)*

## M1 — Resultados

Logit condicional principal (`results/tables/M1_clogit.csv`; OR = $e^{\hat\beta}$; EE robustos cluster convencional):

| Término | $\hat\beta$ | OR | $p$ |
|:---|:-:|:-:|:-:|
| $\lvert\Delta\theta_1\rvert$ a la coalición | $-3.46$ | 0.03 | $<10^{-52}$ |
| $\lvert\Delta\theta_2\rvert$ a la coalición | $-1.20$ | 0.30 | $<10^{-19}$ |
| misma comisión | $+1.21$ | 3.37 | $<10^{-117}$ |
| misma lista modal (rango entre conglomerados) | $+0.78$ a $+2.00$ | 2.2--7.4 | $<10^{-5}$ |
| afinidad abogado | $+0.37$ | 1.45 | 0.060 |
| afinidad experiencia previa | $+0.27$ | 1.31 | 0.29 |
| afinidad mujer | $+0.39$ | 1.47 | 0.029 |
| $\lvert\Delta grado\rvert$ (0--3) | $-0.08$ | 0.93 | 0.32 |

**Lectura.** (i) **La ideología domina la formación**: la distancia en $\theta_1$ es, por lejos, el mayor inhibidor de la firma (OR 0.03 por unidad de distancia) — y se *fortaleció* al excluir las mega-iniciativas (de $-3.09$ a $-3.46$: las coaliciones de 80+ firmantes eran ideológicamente anchas y diluían el efecto). La **segunda dimensión tiene efecto propio** ($-1.20$). (ii) **La estructura de oportunidad importa** (D10 de la revisión): pertenecer a la comisión de la iniciativa multiplica las odds por 3.4. (iii) **La coordinación de lista sobrevive a la ideología** (sección siguiente). (iv) **Las afinidades profesionales no organizan la decisión de firma**: la experiencia es indistinguible de cero y el abogado queda *marginal* ($p = 0.060$; en el ERGM proyectado ambas aparecían con $p<10^{-50}$ — la pseudo-replicación fabricaba esa certeza); las afinidades demográficas en pie son **género** (OR 1.47) y, débilmente, abogado. El arco de H1b se cierra así: ni gatekeeping (v1), ni homofilia profesional robusta (v2 proyectada) — a lo sumo una señal marginal de afinidad jurídica, un orden de magnitud menor que ideología, comisión y lista. *(La lectura complementaria de co-ocurrencia marginal — bootstrap por iniciativas — está en "M1 — Inferencia honesta"; ahí abogado y experiencia sí co-ocurren sobre el azar.)*

## M1 — Las listas como partidos embrionarios

Un partido, operacionalmente, hace dos cosas medibles aquí: ayuda a sus miembros a **coordinar lazos** y los hace **votar juntos**. Las dos se separan de la mera afinidad ideológica.

**(1) Coordinación de lazos** ($\hat\lambda_c$ del logit condicional — condicional en distancia ideológica 2D, comisión y afinidades; `M1_lambda_lista.csv`):

| Conglomerado | $\hat\lambda$ | IC 95% |
|:---|:-:|:-:|
| Escaños Reservados PPOO | **2.00** | [1.59, 2.40] |
| Otras listas locales | 1.09 | [0.80, 1.39] |
| Vamos por Chile | 1.04 | [0.61, 1.46] |
| Lista del Pueblo | 1.02 | [0.73, 1.31] |
| Lista del Apruebo | 0.99 | [0.66, 1.32] |
| Apruebo Dignidad | 0.78 | [0.53, 1.03] |

Con la red limpia ($\leq 16$) el resultado es aún más nítido que antes: **todos los conglomerados no-indígenas coordinan prácticamente igual** ($\hat\lambda \approx 0.8$--$1.1$, ICs mutuamente solapados) — la Lista del Pueblo (1.02) es indistinguible de Vamos por Chile (1.04) y de la Lista del Apruebo (0.99). En el eje organizacional, *las listas ad hoc funcionaron como partidos, y los pactos de partidos reales no coordinaron mejor que ellas*. La excepción son los **PPOO**, con el doble de coordinación interna condicional (2.00 — consistente con Q5).

**(2) Cohesión de voto** (índice de Rice $R_{\ell v} = |Y_{\ell v} - N_{\ell v}|/(Y_{\ell v} + N_{\ell v})$ sobre los 4.707 roll-calls, contra 500 **pseudo-listas** del mismo tamaño y vecindad ideológica; `rice_summary.csv`):

| Conglomerado | $\bar R$ real | $\bar R$ pseudo | premio | $p$ perm. |
|:---|:-:|:-:|:-:|:-:|
| Vamos por Chile | 0.855 | 0.833 | $+0.023$ | 0.32 |
| Apruebo Dignidad | 0.816 | 0.808 | $+0.008$ | 0.32 |
| Lista del Apruebo | 0.713 | 0.740 | $-0.027$ | 0.91 |
| Lista del Pueblo | 0.873 | 0.858 | $+0.015$ | 0.10 |
| Escaños Reservados PPOO | 0.875 | 0.859 | $+0.015$ | 0.16 |

**Ninguna lista — ad hoc o tradicional — vota más unida que su vecindad ideológica.** La serie mensual (Figura F10) agrega el matiz dinámico: la Lista del Pueblo se desploma a $R = 0.65$ exactamente en dic-2021/ene-2022 — su fragmentación documentada — y se recompone en la era de normas; la Lista del Apruebo es la menos cohesiva durante todo el proceso.

![**F10.** Cohesión de voto (Rice mensual) por conglomerado de lista. Línea discontinua: inicio de las votaciones de normas bajo 2/3 (15-feb-2022). Nov-2021 se omite (<10 roll-calls).](../results/figures/rice_cohesion_monthly.pdf){width=100%}

**Lectura conjunta.** Las listas compraron **coordinación de patrocinio** ($\lambda$ positivo, comparable entre ad hoc y tradicionales) pero **no disciplina de voto** (premio $\approx 0$ para todas). "Partidos embrionarios" en sentido literal: el aparato de coordinación aparece antes que el aparato de disciplina.

## M1 — Escaños reservados: ¿enclave o puente?

Tres piezas (`code/20-reserved-seats.R` + interacciones del logit; responde también D12 de la revisión):

1. **E-I de Krackhardt** (lazos ponderados de los 17 PPOO): $EI = +0.562$ observado — más de la mitad del peso de sus lazos va hacia afuera — pero el azar (10.000 permutaciones de etiqueta) espera $+0.891$ [p5 0.853, p95 0.920]: **mucho más lazo interno que el azar** ($p < 10^{-4}$; E-I individual mediana 0.27, rango $[-0.57, 0.70]$). Constraint de Burt: mediana PPOO 0.067 vs. resto 0.066 (Wilcoxon $p = 0.53$) — intermediación normal.
2. **Interacciones en el logit condicional**: efecto base PPOO $-0.87$ ($p = 0.0015$; firman menos iniciativas ajenas), interacción PPOO × $\lvert\Delta\theta_1\rvert$ = $+1.28$ ($p = 0.044$), PPOO × $\lvert\Delta\theta_2\rvert$ n.s. La pendiente ideológica clásica de un PPOO es $-2.30$ contra $-3.59$ del resto: **cruzan distancias en $\theta_1$ que otros no cruzan** (el contraste se atenuó al excluir las mega-iniciativas — de $p = 0.005$ a $p = 0.044$ — porque parte de los "puentes largos" visibles pasaba por esos documentos transversales), con la misma sensibilidad a $\theta_2$ que todos.
3. **Veredicto: ni enclave ni bisagra — las dos cosas, por diseño.** Máxima cohesión interna condicional ($\lambda = 2.00$, E-I bajo el azar) *y* puentes más largos sobre el eje izquierda-derecha. Es la firma estructural de la segunda dimensión: su coordenada operativa no es $\theta_1$, así que el eje clásico no les ordena los aliados — coordinan adentro (agenda plurinacional) y contratan afuera a mayor distancia de $\theta_1$ que el resto.

## M1 — Brokerage: H1b con la métrica de Burt

**Qué es la constraint, término a término.** La constraint de Burt mide cuán *encerrado* está un ego en su vecindario:
$$c_i = \sum_{j \in N(i)} \Big(\underbrace{p_{ij}}_{\text{lazo directo}} + \underbrace{\textstyle\sum_{q} p_{iq}\, p_{qj}}_{\text{lazos indirectos vía } q}\Big)^2, \qquad p_{ij} = \frac{w_{ij}}{\sum_k w_{ik}}.$$
$p_{ij}$ es la **proporción de la energía relacional** de $i$ invertida en $j$ (su peso de co-firma normalizado por todo lo que $i$ firma). El paréntesis suma dos caminos por los que $j$ "captura" a $i$: el lazo directo ($p_{ij}$) y los caminos indirectos ($\sum_q p_{iq} p_{qj}$: cuánta de la energía de $i$ pasa por terceros $q$ que a su vez invierten en $j$ — si mis otros socios también son socios de $j$, $j$ me tiene rodeado). Se eleva al cuadrado para castigar la concentración y se suma sobre los vecinos: $c_i$ alto = vecindario denso y redundante (sin agujeros estructurales alrededor de $i$); $c_i$ **bajo = broker** (sus contactos no se conocen entre sí, y ninguno lo monopoliza). Segunda DV: $\log(1 + betweenness_i)$.

**Especificación y resultados** (`code/13`; regresión sobre abogado y experiencia con controles — género, edad, grado, $|\theta_1^{fm}|$ como extremismo, FE de conglomerado; HC1; $N = 154$; `M1_brokerage.csv`). H1b-Burt predice abogados/experimentados con menor constraint y mayor betweenness. Resultado (red $\leq 16$): **ni abogados ni experimentados son brokers** (constraint: $+0.002$, $p = 0.63$ y $+0.010$, $p = 0.10$; betweenness: negativos y n.s.). El único predictor robusto es el **extremismo ideológico**, ahora significativo en *ambas* DV: $|\theta_1|$ sube la constraint en $+0.026$ ($p = 0.003$) y baja el log-betweenness en $-1.16$ ($p = 0.004$) — **los moderados ocupan los agujeros estructurales**, no los expertos. En una asamblea sin partidos, los puentes los hacen los centristas, no los abogados. *(Caveat: la red subyacente es la proyección; se lee como descriptivo robusto, coherente con el clogit.)*

## M1 — Robustez estructural (i): ERGM bipartito, resultado negativo documentado

El ERGM bipartito **no se pudo estimar** sobre estos datos: tres intentos, suspendidos por decisión del autor tras >3 h de cómputo (2026-07-10; bitácora en `code/16-bipartite-ergm.R`):

1. **Con `constraints = ~b2degrees`** (condicionar en el grado de cada iniciativa — el análogo exacto del strata del clogit): el MCMC restringido no mezcla (>1 h detenido en la iteración 2 con paso $4\times10^{-4}$).
2. **Parametrización estándar** (`edges + gwb1degree + gwb2degree + nodematch + b1nodematch`): degeneración desde el arranque MPLE — las estadísticas de grado no varían y el muestreo "did not mix at all" (25 min).
3. **Especificación mínima** (sin términos gw): suspendido sin converger.

El diagnóstico de fondo: con grados de firmante extremadamente sesgados (mediana 42, máximo 157; F9a) y estadísticas `b1nodematch` de conteos enormes, la superficie de verosimilitud del ERGM bipartito es degenerada en la región de los datos — un problema conocido de la familia, no de la implementación. La capa de dependencias estructurales *entre* firmas (repetición, cierre) queda asignada al **RHEM** (`docs/RHEM-intro.pdf`, paquete `amorem`), cuya verosimilitud caso-control evita esta degeneración por diseño.

## M1 — Robustez estructural (ii): inferencia honesta para la red proyectada (bootstrap por iniciativas)

La segunda vía de robustez sí funcionó, y con un dividendo técnico (2026-07-11; `code/23` piloto, `code/25` ejecución). La especificación del ERGM valuado del proyecto (sum + nodematch + absdiff + nodecov, referencia Poisson) es **díado-independiente**: su verosimilitud factoriza y el "ERGM" es exactamente una **regresión de Poisson** $w_{ij} \sim \text{Poisson}(\exp(\theta' x_{ij}))$ sobre las 11.781 díadas — verificado contra el MCMLE archivado (coincidencia a la 3ª decimal; las corridas MCMC de 8 min eran innecesarias para esta spec). Eso vuelve trivial el **bootstrap por iniciativas**: re-muestrear las 487 iniciativas con reemplazo (el *acto* vuelve a ser la unidad muestral — la corrección directa a la pseudo-replicación), reconstruir $W$ y re-ajustar: 0.02 s por réplica, $B = 1000$ en ~20 s. Resultados (red $\leq 16$, spec con ideología 2D; `M1_ergm_bootstrap.csv`):

| Término | $\hat\theta$ | EE Poisson | EE bootstrap | IC 95% | inflación |
|:---|:-:|:-:|:-:|:-:|:-:|
| nodematch afiliación | $+0.267$ | 0.012 | 0.047 | $[+0.18, +0.36]$ | $\times 4.0$ |
| nodematch experiencia | $+0.283$ | 0.014 | 0.046 | $[+0.19, +0.37]$ | $\times 3.3$ |
| nodematch abogado | $+0.101$ | 0.011 | 0.021 | $[+0.06, +0.14]$ | $\times 1.9$ |
| nodematch mujer | $+0.091$ | 0.011 | 0.025 | $[+0.04, +0.14]$ | $\times 2.3$ |
| absdiff edad | $-0.001$ | 0.001 | 0.001 | $[-0.003, +0.001]$ | $\times 2.0$ |
| absdiff grado | $-0.048$ | 0.007 | 0.021 | $[-0.09, -0.01]$ | $\times 2.9$ |
| absdiff $\theta_1$ | $-2.726$ | 0.024 | 0.091 | $[-2.91, -2.54]$ | $\times 3.7$ |
| absdiff $\theta_2$ | $-0.380$ | 0.016 | 0.056 | $[-0.49, -0.27]$ | $\times 3.5$ |

**Lectura.** (i) Los EE honestos son **2--4×** los ingenuos — la cuantificación de cuánta precisión fabricaba tratar las díadas de un clique como independientes. (ii) Aun así, **todas las homofilias sustantivas sobreviven** con IC lejos de cero. (iii) La aparente tensión con el clogit (abogado/experiencia n.s. allí) no es contradicción sino **dos estimandos**: la regresión diádica mide co-ocurrencia *marginal* (los pares de abogados terminan juntos en iniciativas más de lo que el azar de iniciativas predice), el clogit mide la decisión *condicional al menú* (un abogado no elige coaliciones por su densidad de abogados, fijada la iniciativa). Ambos van al paper como complementos con inferencia defendible. (iv) Extensión con dependencia estructural (p. ej. `transitiveweights`): cada réplica exigiría MCMC completo — el piloto de timing (`code/23`, suspendido con el dato en la mano) mostró que un ajuste corto no completa ni su primera iteración MCMLE en 35 min, o sea un bootstrap estructural de **cientos de horas**: impracticable; la dependencia estructural queda para el RHEM.

## M1 — Limitaciones

1. **Definición de la coalición de referencia**: las covariables de $i$ frente a $S_a$ usan leave-one-out para firmantes; los no-firmantes se comparan contra la coalición completa. Variantes (p. ej. primer firmante como "proponente") quedan como robustez futura.
2. **El clogit no modela dependencias entre firmas** (dos iniciativas gemelas cuentan dos veces): esa capa la cubre el ERGM bipartito; la unificación plena (historia + atributos en un solo reloj) es el RHEM (ver `docs/RHEM-intro.pdf`), en cola con el paquete `amorem` (CRAN).
3. **Heterogeneidad por comisión pendiente de re-test**: bajo la especificación proyectada archivada, la homofilia de experiencia aparecía negativa exactamente en C1/C3 (comisiones de diseño institucional); verificar si sobrevive en el diseño honesto agregando interacciones comisión × afinidad al clogit (barato).
4. **21 listas locales `REVISAR`** a la espera del crosswalk de Fábrega para el mapeo fino de conglomerados.

# M2 — Selección vs. influencia en el voto revelado (panel FE)

## M2 — Especificación

Sobre las ondas por comisión (T0 génesis + fechas de informes de indicaciones; Figura F3), la exposición de red de $i$ en la onda $t$ de la comisión $c$ usa la red **acumulada** $W^{c}_{t}$:
$$\text{NetExp}_{i,t} = \frac{\sum_{j \neq i} w_{ij,t}\,\theta_{j,t}}{\sum_{j \neq i} w_{ij,t}},$$
y el modelo principal es el panel con efectos fijos individuales
$$\Delta\theta_{i,t} = \alpha_i + \beta_1\,\theta_{i,t-1} + \beta_3\,\text{NetExp}_{i,t-1} + \varepsilon_{it},$$
con errores agrupados por delegado y test de Hausman contra efectos aleatorios. **Falsificación**: se reemplaza el rezago por el *lead* ($\text{NetExp}_{i,t+1}$); si el efecto contemporáneo fuera influencia causal, el lead debería ser nulo. **Ventanas alternativas de exposición** (robustez): decaimiento exponencial $W^{dec}_{t} = \sum_{s \le t} \lambda^{t-s}\,\Delta W_s$ con $\lambda = 0.5$; solo-última-onda $W^{last}_{t} = \Delta W_t$; y $\Delta\theta$ estandarizado por días transcurridos entre ondas. Los pasos que comparten período emIRT con el anterior ($\Delta\theta \equiv 0$ mecánico) se excluyen (1.078 celdas). *Constructo (decisión 2026-07-10)*: $\theta_{i,t}$ post-agosto se lee como **comportamiento de voto revelado** (§1.3e), no como ideología latente pura.

![**F3.** Construcción de ondas para M2, por comisión (archivo `C#_waves_summary`).](../results/figures/C%23_waves_summary.pdf){width=100%}

## M2 — Motivación descriptiva

La pregunta del modelo ("las posiciones cambian y la red cambia: ¿influencia o selección?") se establece con tres piezas descriptivas.

**(a) La red cambia — ¿cuándo se forman los lazos post-génesis?** La Figura F4 ubica cada evento de indicación multi-autor en su fecha calendario real, apilando las comisiones por niveles.

![**F4.** Eventos de indicación multi-autor por fecha de informe, por comisión (escala compartida 0--145; puntos = informes con cero eventos multi-autor).](../results/figures/indication_events_timeline.pdf){width=100%}

La formación de lazos post-génesis es **sostenida en C1, C3, C4 y C6** (C6 incluso crece hacia el final; C3 es bimodal), **de un solo golpe en C7** (42/54 eventos en el informe del 19-02) y **no observable en C2/C5**. Los eventos de exactamente 2 autores son minoría (2--3% en C1/C3; 26% en C4). *Nota C5*: sus indicaciones **no fueron unipersonales en la realidad** — los informes de C5 registran solo al primer firmante (resto como "y otros"), una **limitación de registro** que impide reconstruir sus lazos post-génesis (M2 — Limitaciones). Comisiones pertinentes para leer dinámica de red: **C1, C3, C4, C6** (C7 con nota).

**(b) Las posiciones cambian.** La Figura F5 (panel superior) traza las 154 trayectorias de $\theta_{i,t}$ sobre las 91 fechas de votación, coloreadas por posición media (rojo = izquierda), con las medias de bloque; el **panel inferior** resume el movimiento onda a onda.

![**F5.** Arriba: trayectorias de punto ideal de los 154 convencionales (color = posición media, rojo = izquierda), con medias de bloque. Abajo: distribución de los desplazamientos por onda de comisión.](../results/figures/m2_positions_dynamics.pdf){width=92%}

El panel inferior de F5 es un **boxplot de $\lvert\Delta\theta_{i,t}\rvert$ por onda de comisión**, *agrupado (pooled) sobre las 7 comisiones*. Cada "onda" es un **informe de indicaciones** de una comisión (la unidad temporal de M2). El eje va de la **onda 2 a la 8** (la onda 1 casi siempre comparte período de votación con el génesis, así que su $\Delta\theta$ es un cero mecánico y se descarta; la onda máxima, 8, solo la alcanza C7). Los **$n$ sobre cada caja** decrecen porque cada vez menos comisiones alcanzan ondas altas: $n = 154 \times \{7, 7, 6, 5, 3, 1, 1\}$ para las ondas $2\ldots 8$. Lectura: la mediana de $\lvert\Delta\theta\rvert$ está entre 0.12 y 0.25 en **todas** las ondas — los bloques son estables en el agregado pero los individuos se mueven de forma no trivial en cada onda.

Vista complementaria por **membresía de comisión** (extraída del campo `integracion_comisiones` de la BCN, 154/154 matcheados; snapshot en `data/raw/commission_membership.csv`):

![**F6.** Trayectorias de punto ideal por comisión de pertenencia (líneas débiles = convencionales, líneas gruesas = media de la comisión).](../results/figures/theta_dynamics_by_commission.pdf){width=92%}

En F6 todas las medias de comisión viven en la franja izquierda (la Convención fue de mayoría izquierda); C5 es la más a la izquierda y C2 la menos.

**Nota sobre la concentración temporal (F5/F6).** La dinámica de posiciones es casi plana entre julio 2021 y enero 2022 y se concentra entre ~febrero y ~junio de 2022. Esto es, ante todo, un artefacto de **densidad de datos de votación, no de quietud política**: la ventana temprana (jul 2021 -- ene 2022) aporta 999 roll-calls (21% de los 4.707), la tardía (feb -- jun 2022) 3.708 (79%). La razón es procesal (verificada contra la normativa, ver `docs/revision-critica.md` IV.D4): en 2021 la Convención se instaló y escribió su reglamento, y recién el **15-02-2022** el Pleno empezó a votar informes de norma bajo 2/3, artículo por artículo, hasta el 14-05-2022. Implicación: los períodos tempranos están débilmente identificados ($\theta$ encogido al prior); toda lectura temporal pondera esta asimetría.

**(c) Selección vs. influencia en una imagen.** La Figura F7 contrasta, por comisión y onda, la correlación transversal exposición–posición con el efecto within.

![**F7.** Correlación transversal exposición–posición por comisión y onda (arriba) vs. efecto within-delegado de la exposición rezagada (caja).](../results/figures/m2_selection_vs_influence_preview.pdf){width=88%}

En cada comisión y onda, la posición ponderada de los co-firmantes sigue de cerca la propia ($r$ transversal 0.83--0.97, estable en el tiempo — la firma de la selección homofílica), mientras el efecto *within* de la exposición rezagada sobre $\Delta\theta$ es cero. La brecha entre ambas cantidades **es** el resultado de M2.

## M2 — Resultados

Panel: **4.224 observaciones** delegado-onda, 154 delegados, 7 comisiones; 1.078 celdas con período emIRT compartido se excluyen.

Coeficiente de exposición ($\hat\beta_3$), EE agrupados por delegado (`M2_panel.csv`):

| Modelo | $\hat\beta_3$ | EE | $p$ |
|:---|---:|---:|:--|
| OLS agrupado | $+0.0707$ | 0.0122 | $7.4\times10^{-9}$ |
| **Efectos fijos (within)** | $+0.0080$ | 0.0048 | $0.095$ |
| OLS + FE de comisión | $+0.0707$ | 0.0121 | $6.3\times10^{-9}$ |
| Falsificación (*lead*) | $+0.0932$ | 0.0142 | $5.4\times10^{-11}$ |
| FE, exposición con decaimiento ($\lambda=0.5$) | $+0.0088$ | 0.0043 | $0.042$ |
| FE, exposición solo-última-onda | $+0.0068$ | 0.0128 | $0.60$ |
| FE, $\Delta\theta$ por día | $+0.0002$ | 0.0006 | $0.71$ |

Coeficientes de los modelos clave (EE agrupados por delegado; \*\*\* $p<0.001$, \*\* $p<0.01$, \* $p<0.05$, $\dagger$ $p<0.1$):

| Término | OLS agrupado | Efectos fijos | Falsificación (lead) |
|:---|:-:|:-:|:-:|
| Intercepto | $0.036$ (0.010)\*\*\* | — | $0.013$ (0.010) |
| $\theta_{t-1}$ | $-0.071$ (0.006)\*\*\* | $-0.656$ (0.023)\*\*\* | $-0.096$ (0.013)\*\*\* |
| NetExp$_{t-1}$ | $+0.071$ (0.007)\*\*\* | $+0.008$ (0.005)$\dagger$ | — |
| NetExp$_{t+1}$ (lead) | — | — | $+0.093$ (0.014)\*\*\* |
| Abogado | $0.027$ (0.011)\* | — | — |
| Experiencia previa | $-0.025$ (0.013)$\dagger$ | — | — |
| Mujer | $-0.022$ (0.010)\* | — | — |

Hausman $\chi^2 = 1810.0$ ($p \approx 0$) $\to$ FE. **La conclusión central — selección, no influencia — se sostiene, ahora con un matiz que la red limpia hizo visible.** Con la exclusión de las mega-iniciativas, el coeficiente de exposición bajo FE deja de ser un cero limpio: $+0.008$ ($p = 0.095$), y la variante con decaimiento cruza el umbral ($+0.0088$, $p = 0.042$). Pero la vara causal sigue siendo la falsificación, y sigue fallando *en grande*: la exposición **futura** "predice" el cambio pasado con un coeficiente **doce veces mayor** ($+0.093$, $p < 10^{-10}$) que el efecto rezagado — la firma inequívoca de selección endógena (elijo co-firmantes hacia cuya posición ya me estoy moviendo). La señal residual bajo FE es un orden de magnitud menor que la firma de selección, no sobrevive las ventanas solo-última-onda ni por-día, y se lee como selección de *timing* fino, no como influencia. H2 confirmada con las 7 comisiones, con el matiz registrado.

## M2 — Influencia como conducta: co-defección de voto

El nulo de H2 dice que la red no mueve las *posiciones*. La contraparte conductual — sugerida por la revisión (P4) — pregunta por los *votos individuales*: **¿rompo la disciplina de mi bloque cuando mis co-firmantes la rompen?** Para el convencional $i$ (bloque $\ell(i)$) y la votación $v$: $D_{iv} = \mathbf{1}\{voto_{iv} \neq voto\ modal\ de\ \ell(i)\ en\ v\}$, y
$$\Pr(D_{iv} = 1) = \Lambda\Big(\eta_i + \mu_v + \phi\, \underbrace{\textstyle\frac{\sum_{j \neq i} w_{ij} D_{jv}}{\sum_{j \neq i} w_{ij}}}_{E_{iv}:\ \text{defección de mis co-firmantes génesis}}\Big),$$
con FE de persona ($\eta_i$) y votación ($\mu_v$), exposición leave-one-out, y — la pieza clave — un **test de permutación dentro de bloque × votación**: se barajan las identidades de los defectores manteniendo exacta la tasa de defección de cada lista en cada votación, lo que preserva todo el co-movimiento mecánico de bloques y solo destruye el alineamiento con la red.

**Resultados** (`code/19-vote-defection.R`; `M_defection.csv`; red $\leq 16$). Defección base: 7.9%. $\hat\phi = 11.5$ (SE 0.57) en la era de normas ($N = 374.039$; período completo: 11.3). El benchmark mecánico de las 200 permutaciones es $\phi_{perm}$ = media 5.93, p95 6.00: la mitad del efecto crudo es co-movimiento de bloque, pero el observado está a decenas de EE del benchmark ($p_{perm} < 0.005$). **La defección viaja por la red de co-firma**: es la primera evidencia del proyecto de coordinación *conductual* que sobrevive un contrafactual duro, y le da contenido positivo al resultado de M2 — la red no mueve $\theta$ (más allá del matiz de timing), pero sí mueve votos en el margen donde la disciplina se rompe. Magnitud: el exceso sobre lo mecánico ($\Delta\phi \approx 5.5$) implica un factor propio de red $\approx e^{0.55} \approx 1.7$ en las odds por cada 10 puntos de exposición.

## M2 — Limitaciones

1. **C2 delgada.** C2 aporta solo 3 ondas con 1 evento de co-firma de indicaciones (52 unipersonales). Decisión del usuario (2026-07-07): se usa de todas formas; su contribución es mínima y así debe leerse.
2. **Ondas planas en C5 (limitación de registro).** Las indicaciones de C5 **sí** fueron colectivas, pero sus informes registran únicamente al primer firmante: con un solo autor recuperable por evento, la red de C5 no puede cambiar después de T0.
3. **Pasos con el mismo período emIRT** se excluyen del panel (1.078 celdas).
4. La incertidumbre de $\theta$ (dynIRT sin SEs, P10) no se propaga.
5. **Ventana densa pendiente (D7).** Las ondas tempranas distan semanas y las tardías días, con identificación de $\theta$ asimétrica (nota de densidad): la robustez de restringir M2 a feb--jun 2022 está diseñada y en cola.

# M3 — Éxito legislativo (Spatial Durbin Model)

## M3 — Especificación

La DV principal es la **retención esperada por artículo presentado** (decisión ART-FALLIDO, P6): con $A_i$ = artículos génesis co-firmados por $i$ y $M_i \subseteq A_i$ los trazados al borrador,
$$y_i' = \frac{1}{|A_i|}\sum_{a \in A_i} \text{sim}(a) = \underbrace{\frac{|M_i|}{|A_i|}}_{\hat{s}_i} \times \underbrace{\overline{\text{sim}}_{M_i}}_{\bar{r}_i}, \qquad \text{sim}(a) = 0 \text{ si } a \text{ fallido/eliminado},$$
donde $\text{sim}(a)$ es el coseno TF-IDF entre el texto génesis de $a$ y su artículo del borrador final (máximo sobre sus pares si mapea a varios; SBERT como medida alternativa). El modelo espacial es
$$y' = \rho\,\tilde{W} y' + X\beta + \tilde{W} X\gamma + \varepsilon,$$
con $\tilde{W}$ = red génesis-iniciativa **row-normalizada** (la estructura precede al resultado, P9), estimado por máxima verosimilitud; se compara con OLS, SAR ($\gamma = 0$) y SEM (error espacial) vía AIC, y la dependencia se diagnostica con el $I$ de Moran.

## M3 — Resultados

Esta sección reporta dos tablas (cifras v3.1, red $\leq 16$). La **Tabla 1** compara el ajuste global de los cuatro modelos por AIC y su parámetro espacial. La **Tabla 2** da los coeficientes del OLS y del SDM, separando en el SDM el efecto **directo** (el atributo del propio delegado) del **término espacial $\tilde{W}X$** (el mismo atributo promediado sobre sus co-firmantes). El diagnóstico previo es Moran's $I = 0.457$ ($p \approx 10^{-177}$; con la red pre-filtro era 0.380 — limpiar las mega-iniciativas *aumentó* la autocorrelación del éxito), que justifica el modelo espacial.

**Tabla 1 — comparación de modelos y parámetro espacial:**

| Modelo | AIC | Parámetro espacial |
|:---|---:|:---|
| OLS | $-454.8$ | — |
| SEM | $-555.1$ | $\lambda = 0.987$ |
| SAR | $-558.6$ | $\rho = 0.977$ |
| **SDM** | $\mathbf{-574.3}$ | $\rho = 0.945$ (0.036)\*\*\* |

**Tabla 2 — coeficientes de OLS y SDM** (`M3_sdm_pivot.csv`, filas OLS/SDM base; \*\*\* $p<0.001$, \*\* $p<0.01$, \* $p<0.05$, $\dagger$ $p<0.1$):

| Término | OLS | SDM (directo) | SDM (lag espacial $\tilde{W}X$) |
|:---|:-:|:-:|:-:|
| Intercepto | $-0.021$ (0.033) | $-0.300$ (0.200) | — |
| Degree | $+0.00145$ (0.0004)\*\*\* | $+0.00007$ (0.0003) | $+0.0062$ (0.0020)\*\* |
| Betweenness | $-0.00033$ (0.0001)\*\* | $-0.00001$ (0.0001) | $-0.0016$ (0.0006)\*\* |
| Abogado | $0.0128$ (0.010) | $0.0051$ (0.006) | $+0.181$ (0.072)\* |
| Experiencia previa | $0.0009$ (0.013) | $0.0152$ (0.008)$\dagger$ | $-0.043$ (0.084) |
| Mujer | $-0.0162$ (0.009)$\dagger$ | $0.0001$ (0.006) | $-0.014$ (0.059) |
| Edad | $0.0000$ (0.0004) | $-0.0001$ (0.0002) | $+0.0007$ (0.0025) |
| Grado académico (0--3) | $0.0070$ (0.006) | $-0.0002$ (0.004) | $-0.072$ (0.044)$\dagger$ |
| $\theta$ medio | $0.0023$ (0.003) | $+0.0108$ (0.006)$\dagger$ | $+0.0444$ (0.020)\* |
| $\theta$ desv. est. | $0.0183$ (0.030) | $-0.0165$ (0.020) | $-0.061$ (0.131) |
| Heterofilia del ego | $0.0109$ (0.017) | $-0.0115$ (0.013) | $-0.069$ (0.072) |

**Lectura.** (i) Hay dependencia de red fuerte y el **SDM domina** (AIC $-574.3$ vs. OLS $-454.8$; $\hat\rho = 0.945$): el éxito de un delegado está acoplado al de sus co-firmantes. (ii) En el OLS, la **posición de red predice el éxito** (`degree` $+$, `betweenness` $-$). (iii) En el SDM, los efectos **directos** individuales se disipan y son los **términos espaciales** los significativos (`lag.degree`, `lag.betweenness`, y ahora `lag.abogado` $+0.18$\*: estar rodeado de abogados ayuda a que *tu* texto sobreviva — el capital jurídico reaparece, pero como recurso *de la coalición*, no del individuo — exactamente el lugar teórico que la revisión D6 le asignó). **H3 confirmada y amplificada**: el éxito es un fenómeno de red, no individual.

## M3 — El rival institucional: la distancia al pívot de 2/3

Toda norma necesitaba 103 votos (§1.1.5): el rival teórico de "la red derrama éxito" es "sobrevive lo que tolera el pívot". Test (revisión D6; `code/18-sdm-pivot.R`): agregar a $X$ la **distancia al pívot** $|\theta^{fm}_{1,i} - \theta_{1,(103)}|$, con $\theta_{1,(103)} = -0.150$ (el estadístico de orden que completa 2/3 de 154, institucionalmente fundado en la normativa verificada — `docs/revision-critica.md` IV.D4).

| Modelo | AIC | $\rho$ | dist. pívot (directo) | $\tilde{W} \times$ dist. pívot (lag) |
|:---|:-:|:-:|:-:|:-:|
| OLS base | $-454.8$ | — | — | — |
| OLS + dist. pívot | $-511.8$ | — | $-0.117$ ($p<10^{-12}$) | — |
| SDM base | $-574.3$ | 0.945 | — | — |
| SDM + dist. pívot | $\mathbf{-584.7}$ | **0.914** | $-0.0003$ ($p=0.99$) | $\mathbf{-0.256}$ ($p=0.0003$) |

**Lectura.** (i) **$\rho$ no colapsa** (0.945 $\to$ 0.914): el derrame de red no era geometría pivotal disfrazada — ambos canales coexisten, y la teoría pivotal *mejora* el modelo (AIC $-584.7$) sin destronar a la red. (ii) El detalle fino: la distancia al pívot **propia** pierde toda su fuerza dentro del SDM ($-0.117$ en OLS $\to \approx 0$), pero la del **entorno de co-firma** ($\tilde{W} \times$ dist. pívot) sigue siendo clara ($-0.26$, $p = 0.0003$): no importa dónde estás tú respecto del pívot — importa dónde está *la compañía que firma contigo*. (iii) Esto reencuadra el mecanismo de H3: el éxito se derrama, en parte, porque co-firmar con gente cercana al pívot te arrastra hacia textos viables. La versión a nivel artículo (M4, diseñada en `docs/revision-critica.md` IV.D6, con la composición de la coalición como bloque SNA) es el siguiente paso natural.

## M3 — Robustez

La dependencia espacial sobrevive todas las variantes (`M3_robustness.csv`). Cada variante aísla una decisión de diseño:

\begin{itemize}
\item \textbf{DV condicional (antigua)} — promedia la similitud solo sobre artículos trazados, sin los fracasos. \emph{Objetivo}: aislar el efecto de la decisión ART-FALLIDO ($y'$ con ceros vs. retención condicional).
\item \textbf{$\tilde{W}$ binaria} — reemplaza el peso (nº de co-firmas) por presencia/ausencia de lazo. \emph{Objetivo}: ver si la dependencia proviene de la intensidad de la co-firma o basta la topología.
\item \textbf{$\tilde{W}$ artículo} — usa la red de unidad artículo en vez de iniciativa. \emph{Objetivo}: comprobar que la conclusión no depende de la definición de lazo (§4.1).
\item \textbf{DV SBERT} — mide la retención con similitud semántica (embeddings multilingües) en vez de léxica (TF-IDF). \emph{Objetivo}: descartar que el resultado sea un artefacto de solapamiento de palabras.
\end{itemize}

| Variante | $N$ | Moran $I$ | $\rho$ (SDM) | AIC OLS | AIC SDM |
|:---|:-:|:-:|:-:|---:|---:|
| $y'$, $\tilde{W}$ iniciativa (**principal**) | 154 | 0.457 | 0.945 | $-454.8$ | $-574.3$ |
| $y$ condicional (DV antigua), $\tilde{W}$ iniciativa | 154 | 0.162 | 0.786 | $-232.3$ | $-238.3$ |
| $y'$, $\tilde{W}$ binaria | 154 | 0.269 | 0.879 | $-454.8$ | $-509.6$ |
| $y'$, $\tilde{W}$ artículo | 154 | 0.487 | 0.945 | $-454.8$ | $-573.2$ |
| $y'$ SBERT, $\tilde{W}$ iniciativa | 154 | 0.478 | 0.937 | $-331.4$ | $-473.2$ |

En las cinco variantes el SDM mejora al OLS y $\rho$ se mantiene alto (0.79--0.95). *(Variantes recalculadas sobre la red $\leq 16$ con `code/24-m3-robustness.R`, portado del antiguo 08.)*

## M3 — Dinámica de la retención sobre las ondas de comisión (7 comisiones, LOCF)

La comparación génesis$\to$final es el extremo de una **trayectoria**: cada artículo pasa por estados intermedios (`content_snapshot` tras cada indicación, **almacenados en el dataset**). El análisis (`code/10-retention-dynamics.py`) mide, para los **359 artículos trazados**, la similitud de su estado vigente en **cada onda de su comisión** contra su artículo del borrador; si un artículo deja de modificarse antes de la última onda, su valor **se propaga hacia adelante** (LOCF). Cobertura de snapshots en artículos trazados: 100% en C3/C4/C5/C6, 97% en C2/C7, 79% en C1.

![**F8.** Trayectorias de las enmiendas hacia el texto final, las 7 comisiones (eje x en fechas calendario; génesis = etiqueta GEN observada más temprana, o primer informe − 10d si no hay ninguna anterior; LOCF; líneas débiles = artículos, gruesas = media por comisión).](../results/figures/retention_dynamics_all_commissions.pdf){width=100%}

Similitud media al borrador, génesis $\to$ última onda: C1 $0.65 \to 0.82$; C2 $0.39 \to 0.88$; C3 $0.50 \to 0.92$; C4 $0.49 \to 0.94$; C5 $0.15 \to 0.95$; C6 $0.40 \to 0.98$; C7 $0.33 \to 0.89$. Tres lecturas: (i) el proceso de indicaciones es **convergente hacia el borrador en las 7 comisiones**; (ii) la heterogeneidad de partida es enorme — C5 nace lejísimos de su forma final (0.15) y se reescribe casi por completo vía indicaciones; C1 nace cerca (0.65) y se mueve poco; (iii) el estado post-última-indicación ($\approx 0.9$) es lo que la v1 medía como "retención" (0.979). **Extensión natural**: el dataset artículo-onda (`retention_dynamics_locf.csv`) es la base del futuro M4 de supervivencia.

## M3 — Limitaciones

1. **Derrame parcialmente mecánico.** $y'$ de co-firmantes comparte artículos por construcción; $\rho$ combina interdependencia real y composición compartida del DV. La extensión artículo-onda (M4) es el camino para separarlas.
2. **Impactos inestables con $\rho \to 1$.** La descomposición directo/indirecto vía $(I-\rho\tilde{W})^{-1}$ explota numéricamente; se reportan coeficientes y $\rho$.
3. **Ancla de validación NLP.** La v1 (0.979 en "idénticos") comparaba el texto post-indicaciones; la v2 usa el génesis verdadero. La validez del emparejamiento se establece por el contraste alineado-vs-barajado (0.554 vs. 0.032) y por la dinámica de F8.

# 4. Decisiones de diseño confirmadas

| Fecha | Decisión |
|:---|:---|
| 2026-07-06 | `coincidencias_comisiones.csv` (UTF-8) es la fuente de verdad del mapeo génesis→borrador final (P7). |
| 2026-07-06 | **M3**: DV principal con ART-FALLIDO = 0 ($y' = \hat{s}\cdot\bar{r}$); DV condicional antigua como robustez (P6). $W$ y centralidades sobre la red génesis (P9). |
| 2026-07-06 | **M2**: exposición acumulada desde T0 hasta $t-1$ como principal; robustez con decaimiento y solo-última-onda. |
| 2026-07-07 | Grado académico en escala ordinal **0--3**. Imputación de covariables: BCN → Wikipedia → base; `manual_validations.json` prevalece. |
| 2026-07-07 | **Unidad de co-firma: INICIATIVA principal, artículo robustez** (§4.1). C2 se usa en M2 pese a ondas delgadas. |
| 2026-07-08 | Motivación descriptiva de M2 (F4--F7); membresía de comisión desde `integracion_comisiones` (BCN). |
| 2026-07-10 | **M1 principal = logit condicional con FE de iniciativa; ERGM bipartito como robustez estructural. El Valued ERGM proyectado se archiva** (`old-version/`) por pseudo-replicación de cliques (revisión D1). |
| 2026-07-10 | Ideología = **2D primer mes (W-NOMINATE, pre-red/pre-comisiones/pre-2/3)** como covariable exógena de M1/M3; dynIRT re-etiquetado **voto revelado** para M2 (revisión D3/D4/D5). |
| 2026-07-10 | Sección "listas como partidos" en M1 ($\lambda_c$ + Rice vs. pseudo-listas). Escaños reservados: E-I + interacciones (Q5/D12). |
| 2026-07-10 | M3 incorpora el **test pivotal** (distancia a $\theta_{(103)}$). M4 artículo-nivel rediseñada con bloque SNA de composición de coalición (revisión D6). |
| 2026-07-10 | Co-defección de voto como complemento conductual de M2 (revisión P4a); adopción de lenguaje **abandonada** (autoría de redacción no observable). |
| 2026-07-10 | Modelo longitudinal futuro: **RHEM** con el paquete `amorem` (CRAN v1.0.0) + capa fina propia; sin implementación paralela propia (ver `docs/RHEM-intro.pdf`). |
| 2026-07-11 | **Exclusión de las 41 iniciativas con >16 firmantes de toda red y menú** (`MAX_SIGNERS = 16` en `code/00`; registro y F9 se conservan completos; el tope no aplica a indicaciones). Toda la cadena re-corrida (v3.1). |
| 2026-07-11 | **Bootstrap por iniciativas** como inferencia honesta del ERGM proyectado (D1): la spec díado-independiente factoriza como Poisson diádico (equivalencia verificada), B=1000 en ~20 s; ejecutado (`code/25`). Extensión con términos estructurales: impracticable por costo MCMC (piloto `code/23`, suspendido); va al RHEM. |

## 4.1 Puntos a revisar (no son problemas P)

1. **Unidad de co-firma iniciativa vs. artículo.** El menú del clogit y $\tilde{W}$ de M3 usan iniciativa; M3 mantiene $\rho \approx 0.93$ con $\tilde{W}$-artículo. Revisitar al escribir el paper.
2. **Narrativa de H1b.** El arco completo (gatekeeping v1 → homofilia positiva v2 proyectada → no-significancia en el diseño honesto + brokers moderados) debe contarse como corrección metodológica en el paper. La heterogeneidad C1/C3 detectada bajo la especificación archivada queda pendiente de re-test (interacciones comisión × afinidad en el clogit).
3. **$\lambda = 0.5$ del decaimiento en M2**: si la robustez de ventana entra al paper, barrer $\lambda \in \{0.25, 0.5, 0.75\}$.
4. **Fechas de génesis (F8)**: los timestamps son rótulos de informe, no fechas legales; alternativa defendible = eje ordinal.
5. **Auditoría de las 41 iniciativas con >16 firmantes** (listado completo en `docs/revision-critica.md` IV.D8): patrón dominante = documentos transversales duplicados por comisión (la 954 con 83 firmantes en C4/C5/C6; la 9-2 en C2/C7); hipótesis de arreglo aguas arriba (CPT): dedupe por `icc_id`/`sources` cruzando comisiones. **Desde v3.1 están excluidas de todos los análisis** (decisión 2026-07-11); cuando CPT resuelva la duplicación, decidir si se reintegran deduplicadas.
6. **Separación de roll-calls por régimen de quórum** (mayoría vs. 2/3, corte 15-02-2022): diseñada y **congelada** — ninguna modificación a las estimaciones de posicionamiento se ejecuta sin notificación previa al autor con el código a la vista. Fuente auxiliar de fechas: `A-data-pleno/sesion_##.xls`.
7. **Ventana densa para M2 (D7)** y **crosswalk de Fábrega** para las 21 listas `REVISAR`: en cola.

# 5. Registro de cambios

- **v3.1 (2026-07-11).** Respuesta a los dos comentarios del autor. **(D8)** Exclusión de las 41 iniciativas con >16 firmantes de toda red/menú (`MAX_SIGNERS = 16` en 00; la red génesis pierde ~35% de su peso — las mega-iniciativas lo fabricaban) y re-corrida completa: la ideología se fortalece en el clogit ($-3.09 \to -3.46$), los $\lambda$ de lista se emparejan (todos $\approx 0.8$--$1.1$ salvo PPOO 2.0), abogado repunta a marginal ($p = 0.06$), el FE de M2 deja de ser cero limpio ($+0.008$, $p = 0.095$; decaimiento $p = 0.042$) pero la falsificación lead sigue fallando 12 a 1 → selección se sostiene; Moran de M3 sube a 0.457 y el test pivotal mantiene su lectura (lag $-0.256$***); defección $\phi = 11.5$ vs. 5.9 mecánico. **(D1)** Bootstrap por iniciativas evaluado y ejecutado: la spec díado-independiente factoriza como Poisson diádico (equivalencia MCMLE$\leftrightarrow$glm verificada a la 3ª decimal), $B = 1000$ en ~20 s; EE honestos 2--4× los ingenuos y todas las homofilias sustantivas sobreviven (nueva sección "M1 — Robustez estructural (ii)"). Scripts nuevos 22--25; `M2_full_models.csv` (huérfano de una versión anterior de 03) eliminado.
- **v3.0 (2026-07-10).** Respuesta a la revisión crítica (`docs/revision-critica.md`, Partes I--IV). **M1 reescrito**: logit condicional con FE de iniciativa (81.312 decisiones) como principal + ERGM bipartito como robustez; el Valued ERGM proyectado y sus 9 robusteces se archivan en `old-version/` (junto con los scripts 01 y 08 y el reporte v2.4). Nuevas secciones: listas como partidos ($\lambda_c$ + Rice + F10), escaños reservados (E-I + interacciones), brokerage de Burt, co-defección de voto (M2), test pivotal (M3). Ideología 2D primer mes como covariable exógena; dynIRT = voto revelado. Nuevas figuras F9 (distribuciones de firma) y F10 (Rice mensual). Documento nuevo: `docs/RHEM-intro.pdf` (modelo longitudinal elegido + decisión de software `amorem`).
- **v2.4 (2026-07-08).** Reestructura editorial; M1/M2/M3 titulados; esquemas visuales (§2.2/2.3); F1--F8 referenciadas; M3 reordenado. *(Historial completo en `docs/report-archive.txt`, fuera de versionado.)*

\clearpage
\thispagestyle{empty}
\mbox{}
\clearpage

# Anexo A — Problemas pendientes

Los problemas resueltos (P1--P7, P9, P11, P12, P14, P15) están archivados con su historial completo en `docs/report-archive.txt` (fuera de versionado). Quedan abiertos:

## P8 — Esquema GENESIS heterogéneo **[ABIERTO — aguas arriba en CPT]**

C1/C2/C3 sin `article_uid`; C4 sin `article`/`sources`; `authors` intermitente. Asignado en CPT con la instrucción de heredar los uids de TRACK/crosswalk. El loader de este repo lo absorbe mientras tanto. *Nuevo insumo para CPT (2026-07-10)*: las 41 iniciativas con >16 firmantes (§4.1.5) sugieren duplicación transversal entre comisiones.

## P10 — dynIRT sin errores estándar **[ABIERTO — prioridad baja]**

No existe `emIRT_bootstrap_output.rds`; `theta_se` queda NA en el pipeline (el script 02 lo tolera). Decidir si (i) bootstrap paramétrico del dynIRT, o (ii) reportar sensibilidad de M2 a la incertidumbre de $\theta$ de otro modo. No bloquea la actualización.

## P13 — Errores y desactualizaciones en los documentos del paper **[ABIERTO]**

1. **Rótulos temáticos de comisiones equivocados.** `docs/extended-abstract.tex` (§Data) describe "C5: Fundamental Rights; C6: Environment". Según la numeración oficial (§1.1), C5 es **Medio Ambiente** y C6 es **Sistemas de Justicia** (Derechos Fundamentales es C4).
2. **"155 delegates" vs. 154** en pasajes del abstract y propuesta (154 es el n tras normalización de nombres; la Convención tuvo 155 escaños).
3. **Cifras a regenerar** tras la actualización, incluyendo ahora el vuelco completo de M1 (logit condicional).
4. **Cambio metodológico no documentado**: la propuesta original prometía TERGM (M1) y SAOM/RSiena (M2); la implementación usa elección discreta + panel FE, con RHEM como extensión longitudinal. Documentar la justificación.

# Anexo B — Fuentes de datos (rutas exactas)

```{=latex}
\begin{landscape}
```

| Fuente | Ubicación | Contenido | N |
|:---|:---|:---|---:|
| Iniciativas génesis | `data/raw/dataverse-final/comision-{1..7}/C{k}_GENESIS_master.json` | Iniciativas originales aprobadas en general | 1.892 |
| Trazabilidad completa | `.../C{k}_TRACK_full.json` | Artículos (incl. fallidos) con `authors`, `history[]` fechado y `final_status` al 100% | 2.019 |
| Trazabilidad filtrada | `.../C{k}_TRACK_articles.json` | Solo artículos presentes en el borrador final | 478 |
| Borrador final | `.../C{k}_BORRADOR_final.json` | Artículos del borrador del 14-05-2022, por comisión | 498 |
| Mapeo maestro | `data/raw/dataverse-final/coincidencias_comisiones.csv` | 498 artículos finales → fuente(s): 298 identical, 146 similar, 54 not\_traced | 498 |
| Codebook | `data/raw/dataverse-final/dataverse-codebook-draft.md` | Semántica de `article_uid`, `final_status`, `timestamp` | — |
| Nombres canónicos | `data/raw/convention_members.json` | Referencia oficial "Apellido, Nombre" | 154 |
| Perfiles curados | `data/raw/conventional-profiles.json` (imputado dual-fuente; ver §2.3) | Covariables: género, afiliación, distrito, abogado, edad, grado (0--3), experiencia | 154 |
| Listas electorales | `data/raw/electoral_lists.csv` | Lista de origen + conglomerado (BCN; = Tabla 1 de Fábrega en los 4 grandes) | 154 |
| Membresía de comisión | `data/raw/commission_membership.csv` | Comisión de cada convencional (campo `integracion_comisiones` BCN) | 154 |
| Puntos ideales dinámicos | `data/raw/emirt/` | Salidas dynIRT: $\Theta$ 154×91, metadata, posiciones resumen | 154×91 |
| Puntos ideales 2D primer mes | `data/processed/ideal_points_2d_firstmonth.csv` | W-NOMINATE 2D, votaciones al 12-08-2021 (réplica Fábrega 2022) | 154 |

Desglose por comisión del snapshot (TRACK_full = artículos + indicaciones sueltas; entre paréntesis, artículos con `authors`, la base de las decisiones de firma):

| Comisión | Nombre oficial (corto) | GENESIS | BORRADOR | TRACK_articles | TRACK_full | Artículos (con authors) | Ind. sueltas |
|:-:|:---|---:|---:|---:|---:|:-:|---:|
| C1 | Sistema Político | 96 | 100 | 131 | 202 | 99 (99) | 103 |
| C2 | Principios Constitucionales | 312 | 41 | 24 | 182 | 182 (149) | 0 |
| C3 | Forma de Estado | 222 | 96 | 72 | 234 | 222 (222) | 12 |
| C4 | Derechos Fundamentales | 167 | 58 | 58 | 175 | 167 (150) | 8 |
| C5 | Medio Ambiente | 464 | 43 | 36 | 484 | 464 (420) | 20 |
| C6 | Sistemas de Justicia | 440 | 119 | 117 | 491 | 447 (418) | 44 |
| C7 | Sistemas de Conocimientos | 191 | 41 | 40 | 251 | 228 (218) | 23 |
| **Total** | | **1.892** | **498** | **478** | **2.019** | **1.809 (1.676)** | **210** |

*(Cifras post-ronda 2 de CPT (`6fac4c4`): `authors` de C2 poblado vía `sources`→firmantes y de C4 vía `icc_id`; el residuo de 134 registros sin autores queda documentado en el codebook.)*

```{=latex}
\end{landscape}
```

# Anexo C — Scripts del pipeline

```{=latex}
\begin{landscape}
```

Pipeline v3: todos los scripts derivan rutas de `code/paths.{py,R}` y nombres de `code/lib_names.{py,R}`; los insumos son el snapshot `data/raw/dataverse-final/` y los perfiles curados (§2.3). Los scripts de la especificación M1 anterior (01 Valued ERGM y 08 robustez proyectada) están en `old-version/scripts/` (fuera de versionado); sus tablas siguen en `results/tables/`.

| # | Script | Rol | Outputs |
|:-:|:---|:---|:---|
| 0a | `code/0a-verify-dataverse-snapshot.py` | Test de aceptación del snapshot | `QA-report.txt` |
| 0b | `code/0b-audit-profiles-dual-source.py` | Auditoría dual-fuente e imputación de perfiles (§2.3) | `conventional-profiles.json`, `profile-audit/*` |
| 00 | `code/00-build_dynamic_networks.py` | Registro de iniciativas; redes génesis; ondas ×7 con dedup | `genesis_network_*.csv`, `initiative_registry.csv`, `commission_waves.csv` |
| 02 | `code/02-extract-emirt-temporal.R` | Alineación temporal $\Theta \leftrightarrow$ ondas de las 7 comisiones | `emirt_*.csv` |
| 03 | `code/03-model-network-influence.R` | **M2**: panel FE/RE/OLS + falsificación *lead* + robusteces | `network_exposure_panel.csv`, `tables/M2_*.csv` |
| 04 | `code/04-build-article-mapping.py` | Lector de `coincidencias` + desenlaces por artículo | `article_mapping_unified.csv`, `track_article_outcomes.csv` |
| 05 | `code/05-nlp-text-similarity.py` | Retención léxica TF-IDF + SBERT; DV $y'$ | `article_similarity_scores.csv`, `author_success_scores.csv` |
| 06 | `code/06-build-integrated-dataset.py` | Merge roster-driven (154 filas) | `integrated_dataset.{csv,json}` |
| 07 | `code/07-model-spatial-durbin.R` | **M3**: Moran; OLS/SAR/SEM/SDM; impactos | `sdm_results.rds`, `tables/M3_*.csv` |
| 09 | `code/09-figures.py` | Figuras F1--F4 | `results/figures/*` |
| 10 | `code/10-retention-dynamics.py` | Dinámica de retención, LOCF (F8) | `retention_dynamics_locf.csv`, figura |
| 11 | `code/11-m2-motivation-figures.py` | Motivación de M2 (F5--F7) | `commission_membership.csv`, figuras |
| 12 | `code/12-ideal-points-2d.R` | Ideología 2D primer mes (W-NOMINATE; réplica Fábrega) | `ideal_points_2d_firstmonth.csv` |
| 13 | `code/13-review-response-models.R` | Brokerage de Burt (constraint/betweenness) + ERGM ± ideología (archivado el uso) | `tables/M1_brokerage.csv` |
| 14 | `code/14-signature-distributions.py` | F9: firmas por convencional / firmantes por iniciativa | `signature_distributions.*` |
| 15 | `code/15-conditional-logit.R` | **M1 principal**: dataset de elección + clogit + $\lambda_c$ + interacciones PPOO | `choice_dataset.csv`, `tables/M1_clogit.csv`, `tables/M1_lambda_lista.csv` |
| 16 | `code/16-bipartite-ergm.R` | **M1 robustez**: ERGM bipartito 154×528 | `tables/M1_bipartite_ergm.csv` |
| 17 | `code/17-listas-rice.R` | Rice por lista + pseudo-listas + serie mensual | `tables/rice_summary.csv`, `tables/rice_monthly.csv` |
| 18 | `code/18-sdm-pivot.R` | **M3**: test pivotal (distancia a $\theta_{(103)}$) | `tables/M3_sdm_pivot.csv` |
| 19 | `code/19-vote-defection.R` | **M2**: co-defección con FE dobles + permutación | `tables/M_defection.csv` |
| 20 | `code/20-reserved-seats.R` | Q5: E-I de Krackhardt + constraint PPOO | `tables/Q5_reserved_seats.csv` |
| 21 | `code/21-rice-figure.py` | F10: Rice mensual por conglomerado | `rice_cohesion_monthly.*` |
| 22 | `code/22-network-metrics.R` | Métricas de red (reemplaza la parte de métricas del antiguo 01) | `network_metrics.csv` |
| 23 | `code/23-ergm-bootstrap-pilot.R` | **D1 piloto**: equivalencia Poisson$\leftrightarrow$MCMLE + timing bootstrap Tier A/B (Tier B suspendido: impracticable) | consola |
| 24 | `code/24-m3-robustness.R` | Variantes de robustez de M3 (portadas del antiguo 08) | `tables/M3_robustness.csv` |
| 25 | `code/25-ergm-initiative-bootstrap.R` | **D1**: bootstrap por iniciativas completo (B=1000) | `tables/M1_ergm_bootstrap.csv` |

```{=latex}
\end{landscape}
```
