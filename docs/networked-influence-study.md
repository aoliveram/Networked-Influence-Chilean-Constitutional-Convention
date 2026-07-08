---
title: "Networked Influence in a Tabula Rasa Legislature — Reporte del estudio"
subtitle: "Redes de co-patrocinio, dinámicas ideológicas y éxito político en la Convención Constitucional de Chile (2021--2022)"
author: "A. Olivera, J. Fábrega"
date: \today
geometry: "margin=2.5cm"
fontsize: 10pt
colorlinks: true
linkcolor: teal
toc: true
toc-depth: 3
header-includes:
  - \usepackage{amsmath}
  - \usepackage{amssymb}
  - \usepackage{booktabs}
  - \usepackage{array}
---

# 1. Propósito de este documento

Este es el **reporte vivo** del proyecto: se actualiza a medida que el estudio avanza y registra, en un solo lugar, (i) los objetivos y el marco teórico, (ii) los datos y su procedencia exacta, (iii) el pipeline y los resultados vigentes, (iv) los **problemas identificados y su estado de resolución**, (v) las decisiones de diseño confirmadas, y (vi) el plan de trabajo. La versión v1 (julio 2026) documenta el estado del proyecto al momento de iniciar la **actualización de 5 a 7 comisiones** con los datos limpios de `dataverse-final` (PR #4 del repositorio de datos).

Convenciones: los problemas se etiquetan `P1, P2, ...` con estado **[ABIERTO]**, **[RESUELTO]** o **[PARCIAL]**. Las rutas relativas refieren a este repositorio; el repositorio de datos se abrevia `CPT` (= `B - constitutional-proposal-tracking`, en `https://github.com/aoliveram/constitutional-proposal-tracking`).

# 2. Objetivos del proyecto

## 2.1 Contexto: la Convención Constitucional como experimento natural

La Convención Constitucional chilena (julio 2021 -- julio 2022) es un caso excepcionalmente limpio para estudiar la **génesis de redes políticas**. A diferencia de una legislatura ordinaria, donde las redes de colaboración observadas son el sedimento de décadas de carreras compartidas, disciplinas de partido y expectativas de reelección, la Convención partió de un cuasi-*tabula rasa* organizacional:

1. **Actores mutuamente desconocidos.** De los 155 escaños, una mayoría inédita correspondió a independientes y outsiders sin trayectoria parlamentaria, electos por listas ad hoc y escaños reservados para pueblos originarios (17). Gran parte de los delegados no se conocía entre sí ni compartía socialización partidaria.
2. **Sin jerarquías heredadas.** No existían comités con seniority, liderazgos de bancada consolidados ni "sombra del futuro" electoral común: el órgano se disolvía al entregar el texto.
3. **Reglas endógenas.** La Convención escribió su propio reglamento, incluyendo el mecanismo central que explota este estudio: toda **iniciativa convencional constituyente** requería el patrocinio de **al menos 8 y no más de 16 convencionales** (Reglamento General, art. 83). El piso obliga a formar coaliciones mínimas para participar del proceso; el techo impide que las firmas se diluyan en mega-bloques y convierte cada patrocinio en un recurso escaso y estratégico.
4. **Horizonte comprimido.** Todo el ciclo — formación de lazos, negociación, producción normativa y resultado (el borrador del 14-05-2022) — ocurre en ~10 meses y está íntegramente documentado, con autoría explícita en cada iniciativa e indicación.

El co-patrocinio es, por tanto, un **proxy observable, costoso y fechado de colaboración política**, y la Convención permite observar el ciclo completo: cómo se forman los lazos entre extraños (RQ1), qué hacen esos lazos con las posiciones de los actores (RQ2), y cómo la posición en la red se traduce en éxito sobre el texto final (RQ3).

## 2.2 Preguntas de investigación

- **RQ1 — Formación.** ¿Qué factores impulsan la formación de la red de co-patrocinio cuando no existen jerarquías previas? ¿Domina la homofilia clásica (afiliación, género) o aparecen estrategias de diversificación?
- **RQ2 — Influencia vs. selección.** ¿La exposición a los co-autores desplaza las posiciones ideológicas de los delegados (influencia social), o los delegados seleccionan co-autores ya afines (selección endógena)?
- **RQ3 — Éxito.** ¿Cómo predice la estructura de red el éxito político individual, operacionalizado como **retención léxica**: cuánto del texto que un delegado patrocinó sobrevive en el borrador constitucional final?

## 2.3 Marco teórico

*(Apartado en construcción: esta sección fija el esqueleto argumental y se irá puliendo junto con la especificación final de los modelos.)*

**(a) Formación de lazos en legislaturas: homofilia y sus límites.** La literatura de co-sponsorship en legislaturas consolidadas (Fowler 2006; Bratton & Rouse 2011; Kirkland 2011) documenta con robustez que los lazos siguen líneas de partido, región y género — homofilia en el sentido de McPherson, Smith-Lovin & Cook (2001). Pero esa literatura no puede separar cuánto de la homofilia observada es preferencia y cuánto es estructura heredada (comités, bancadas, historia). La Convención remueve la estructura heredada: lo que se observe en la formación temprana refleja preferencias y estrategias de actores en un campo casi vacío. **H1a (línea base):** persiste homofilia por afiliación política agrupada, el clivaje más visible incluso entre independientes.

**(b) Gatekeeping estratégico: la homofilia negativa de los "dotados".** Para los actores con recursos escasos y transversalmente valiosos — pericia jurídica (abogados, en una asamblea cuyo producto es un texto legal) y experiencia institucional previa — la teoría de brokerage (Burt 1992; Padgett & Ansell 1993) predice lo contrario de la homofilia: maximizan influencia **dispersándose** entre coaliciones, ocupando agujeros estructurales en vez de agruparse entre sí. Se comportan como *gatekeepers* del conocimiento procedimental. **H1b:** abogados y delegados con experiencia institucional exhiben homofilia *negativa* (co-patrocinan menos entre sí de lo esperado), condicional en los demás términos del modelo.

**(c) Influencia social vs. selección endógena.** Que los conectados se parezcan ideológicamente admite dos mecanismos generativos: influencia (los lazos mueven las posiciones; Friedkin & Johnsen 1990) o selección (las posiciones crean los lazos). Distinguirlos es el problema clásico de la econometría de redes (Shalizi & Thomas 2011; en la tradición SAOM, Steglich, Snijders & Pearson 2010). Nuestra estrategia: panel con efectos fijos individuales (toda heterogeneidad estable del delegado queda absorbida) más un **test de falsificación** con exposición futura (*lead*): si el efecto contemporáneo fuera influencia causal, la exposición futura no debería "predecir" el cambio pasado. **H2:** en un cuerpo con posiciones ideológicas pre-formadas (los delegados llegan con identidades políticas adultas), domina la selección; el efecto de exposición desaparece bajo efectos fijos.

**(d) El éxito legislativo como fenómeno colectivo.** La efectividad legislativa suele tratarse como atributo individual (Volden & Wiseman 2014). Pero si la unidad de producción es la coalición firmante — y el reglamento fuerza que lo sea — el éxito debería *derramarse* por los lazos de co-autoría: mi retención léxica depende de la retención de aquellos con quienes escribo. Econométricamente eso es autocorrelación espacial en la variable de resultado sobre la topología de la red, y el marco natural son los modelos espaciales sobre redes (LeSage & Pace 2009). **H3:** la retención léxica exhibe dependencia de red fuerte; los modelos con rezago espacial (SAR/SDM) dominan al OLS, y el componente indirecto (spillover) de los efectos es sustantivo.

**(e) Anclaje ideológico.** Las posiciones ideológicas se estiman desde el comportamiento de votación nominal con un modelo dinámico de puntos ideales (dynIRT; Martin & Quinn 2002; implementación `emIRT` de Imai, Lo & Olmsted 2016), con prior de caminata aleatoria ($\omega^2 = 0.025$) y anclaje por dos delegados de posición pública inequívoca (Marinovic a la derecha, Baradit a la izquierda). Resultado: matriz $\Theta \in \mathbb{R}^{154 \times 91}$ (delegados × períodos de votación, 2021-07-13 a 2022-06-24).

## 2.4 Diseño: tres modelos

**M1 — Formación (Valued ERGM).** Red pooled de co-patrocinio con pesos $w_{ij}$ = nº de documentos co-firmados. ERGM valuado con referencia Poisson:
$$P(W = w) \propto \exp\{\theta^\top g(w)\}, \qquad g = (\text{sum},\ \text{nodematch}_{\text{afiliación, abogado, experiencia, género}},\ \text{absdiff}_{\text{edad, grado}},\ \text{nodecov}_{\text{edad}})$$
Los términos `nodematch` capturan homofilia (H1a) y su reverso (H1b). El término `absdiff` sobre el **grado académico** (escala ordinal 0--3: sin estudios universitarios terminados / educación superior terminada / magíster / doctorado; decisión 2026-07-07) prueba estratificación por credenciales educativas: un coeficiente negativo indica que la distancia educativa inhibe la co-firma.

**M2 — Dinámica ideológica (panel FE).** Sobre ondas temporales por comisión:
$$\Delta\theta_{i,t} = \alpha_i + \beta_1 \theta_{i,t-1} + \beta_3\, \text{NetExp}_{i,t-1} + \varepsilon_{it}, \qquad \text{NetExp}_{i,t-1} = \frac{\sum_{j \neq i} w_{ij,t-1}\,\theta_{j,t-1}}{\sum_{j \neq i} w_{ij,t-1}}$$
con $w_{ij,t-1}$ = co-firmas acumuladas hasta la onda $t-1$. Comparación OLS agrupado vs. efectos fijos (within) vs. aleatorios (Hausman), errores agrupados por delegado, y falsificación con *lead*.

**M3 — Éxito (Spatial Durbin Model).** Con $y_i$ = retención léxica media del delegado (similitud coseno TF-IDF génesis→final; robustez con Sentence-BERT) y $W$ = matriz de co-autoría row-normalizada:
$$y = \rho W y + X\beta + WX\gamma + \varepsilon$$
Se compara OLS/SAR/SEM/SDM (AIC, Moran's $I$) y se descomponen efectos directos/indirectos/totales.

# 3. Datos

## 3.1 Fuentes primarias (estado objetivo, post-actualización)

Desde 2026-07-07 los datos viven como **snapshot versionado dentro de este repositorio** (`data/raw/dataverse-final/`, copiado de CPT `paper-draft` @ `5852519`; ver P14). El test de aceptación `code/0a-verify-dataverse-snapshot.py` valida cada refresco y escribe `QA-report.txt`.

| Fuente | Ubicación | Contenido | N |
|:---|:---|:---|---:|
| Iniciativas génesis | `data/raw/dataverse-final/comision-{1..7}/C{k}_GENESIS_master.json` | Iniciativas originales aprobadas en general | 1.892 |
| Trazabilidad completa | `.../C{k}_TRACK_full.json` | Artículos (incl. fallidos) con `authors`, `history[]` fechado y `final_status` al 100% | 2.019 |
| Trazabilidad filtrada | `.../C{k}_TRACK_articles.json` | Solo artículos presentes en el borrador final | 478 |
| Borrador final | `.../C{k}_BORRADOR_final.json` | Artículos del borrador del 14-05-2022, por comisión | 498 |
| Mapeo maestro | `data/raw/dataverse-final/coincidencias_comisiones.csv` | 498 artículos finales → fuente(s): 298 identical, 146 similar, 54 not\_traced | 498 |
| Codebook | `data/raw/dataverse-final/dataverse-codebook-draft.md` | Semántica de `article_uid`, `final_status`, `timestamp` | — |
| Nombres canónicos | `data/raw/convention_members.json` | Referencia oficial "Apellido, Nombre" | 154 |
| Perfiles curados | `data/raw/conventional-profiles.json` (imputado dual-fuente; ver P5) | Covariables: género, afiliación, distrito, abogado, edad, grado (0--3), experiencia | 154 |
| Puntos ideales | `data/raw/emirt/` | Salidas dynIRT: $\Theta$ 154×91, metadata, posiciones resumen | 154×91 |

Desglose por comisión del snapshot (TRACK_full = artículos + indicaciones sueltas; entre paréntesis, artículos con `authors`, la base de la red de co-patrocinio):

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

*(Cifras post-ronda 2 de CPT (`6fac4c4`): `authors` de C2 poblado vía `sources`→firmantes de las iniciativas y de C4 vía `icc_id`, reglas validadas 1.415/1.416; el residuo de 134 registros sin autores queda documentado en el codebook — 65 de iniciativas populares/indígenas, 45 de ICC no recuperadas, 24 sin referencia de fuente.)*

Nombres oficiales completos de las comisiones (verificados 2026-07-06 contra el registro oficial `cconstituyente.cl`, Wikipedia y UNESCO; ver P13):

1. Sistema Político, Gobierno, Poder Legislativo y Sistema Electoral
2. Principios Constitucionales, Democracia, Nacionalidad y Ciudadanía
3. Forma de Estado, Ordenamiento, Autonomía, Descentralización, Equidad, Justicia Territorial, Gobiernos Locales y Organización Fiscal
4. Derechos Fundamentales
5. Medio Ambiente, Derechos de la Naturaleza, Bienes Naturales Comunes y Modelo Económico
6. Sistemas de Justicia, Órganos Autónomos de Control y Reforma Constitucional
7. Sistemas de Conocimientos, Culturas, Ciencia, Tecnología, Artes y Patrimonios

## 3.2 Esquema de los datos de trazabilidad

Cada registro de artículo en `TRACK_full` contiene: `article_uid` (identificador único, guiones bajos), `article`, `timestamp` (formato `MM-DD`), `text`, `sources` (iniciativas de origen), `authors` (array `"Apellido, Nombre"`), `final_status` (`"Idéntico a N.- ..."` / `"Similar a N.- ..."` / `"ART-FALLIDO"`), y `history[]`. Cada entrada de `history[]` es una **indicación** fechada: `timestamp`, `step`, `number`, `authors`, `action` (ADD/DELETE/...), `target_scope`, `content`, `content_to_remove`, `placement_instructions`, `content_snapshot`.

Hechos estructurales que el pipeline absorbe (detalle en P8/P15):

- El esquema del GENESIS sigue **heterogéneo** entre comisiones (C1/C2/C3 sin `article_uid`; C4 sin `article`/`sources`; `authors` intermitente) — homogeneización pendiente en CPT; el loader unifica.
- Todos los `TRACK_full` salvo C2 contienen **"indicaciones sueltas"** (210 en total): registros de indicación al nivel superior con `action`, sin anclar a un artículo; 205/210 traen `authors` + `timestamp` y alimentan las ondas de M2. Los títulos de capítulo de C1 fueron eliminados aguas arriba (2026-07-07).
- `step` está normalizado a `"Indicacion"` (2.763 menciones; C2 conserva sus etiquetas de etapa propias). `timestamp` es `MM-DD[-bloque]` con año implícito 2022; hay 66 NA a nivel superior (C1 4, C3 2, C5 5, C6 17, C7 38) y 5 anidados.
- Los `authors` incluyen referencias a **iniciativas populares** ("7-2", "Iniciativa Popular Indígena 21-2"): `code/lib_names.py` las clasifica como no-persona y las excluye de la red. Con esas reglas, las **75.616 menciones de autor-persona resuelven al 100%** contra los 154 canónicos (QA 0a, post-ronda 2).

## 3.3 Datos usados por la versión vigente (pre-actualización)

La versión reflejada en el extended abstract (abril 2026) usa insumos **anteriores** al PR #4 de limpieza, copiados en `data/raw/commissions/` (5 archivos: C1, C3, C5, C6, C7, enriquecidos a mano) y redes pre-construidas en `data/raw/network-visualization/`. Cifras vigentes: red pooled de **7.162 aristas, 154 nodos, peso total 202.765, peso máximo 480**; ondas dinámicas solo para C1 (5), C3 (8) y C5 (6); mapeo génesis→final de **236 artículos** (125 idénticos + 111 similares; C1: 131, C3: 71, C5: 34); panel de **2.926** observaciones delegado-período; SDM sobre **141** casos completos.

## 3.4 Regla de asignación de covariables de perfil

Los perfiles de `data/raw/conventional-profiles.json` (154 filas, insumo de M1--M3) se construyen con la siguiente regla, ejecutada por `code/0b-audit-profiles-dual-source.py --impute` y documentada en `data/raw/profile-audit/README.md`:

1. **Extracción dual e independiente.** Para cada convencional, `gemini-3.5-flash` (con búsqueda web) extrae las características dos veces: desde el texto biográfico BCN ya scrapeado y desde su artículo de Wikipedia en español, con la instrucción de reportar la **situación al momento de ser convencional** (jul-2021 a jul-2022): afiliación vigente entonces, cargos previos al 04-07-2021.
2. **Regla de imputación.** *BCN es la fuente de verdad; si BCN no informa el dato, se usa Wikipedia; si ninguna informa, se conserva el valor base* (la salida del scraper). Cada celda de `profile_audit_table.csv` registra la concordancia entre fuentes ($=$, $\neq$, B, W, $\varnothing$).
3. **Validaciones humanas.** Las entradas de `manual_validations.json` (revisión manual del usuario sobre `discrepancias_pipeline.csv`) **siempre prevalecen** sobre la regla automática (p. ej. Chahin → DC pese a que BCN decía "Independiente"). Nuevas correcciones se agregan ahí y se re-imputa.
4. **Definiciones por campo.** `es_mujer`: género informado por la fuente (no inferido del nombre). `edad_al_asumir`: edad al 04-07-2021 desde la fecha de nacimiento. `afiliacion_agrupada`: partido con militancia vigente durante la Convención, o "Independiente"; los escaños reservados llevan su afiliación partidaria real (o Independiente) y el pueblo queda en `distrito` = "Escaño reservado: \<pueblo\>". `es_abogado`: profesión de abogado/a. `grado_academico_nivel` (escala 0--3): 0 sin estudios universitarios terminados; 1 educación superior terminada (título profesional o licenciatura); 2 magíster/máster (no cuenta diplomado); 3 doctorado. `experiencia_previa_institucional`: 1 si antes de jul-2021 ocupó cargos públicos (parlamentario/a, alcalde/sa, concejal/a, CORE, seremi, intendente/a, ministro/a, subsecretario/a, embajador/a, jefatura de servicio).

Estado vigente: 154/154 perfiles, 0 afiliaciones desconocidas, 0 edades faltantes; grado 0--3 = \{0: 18, 1: 81, 2: 41, 3: 14\}.

# 4. Pipeline y resultados

## 4.1 Scripts

Pipeline v2 (2026-07-07): todos los scripts derivan rutas de `code/paths.{py,R}` y nombres de `code/lib_names.{py,R}`; los insumos son el snapshot `data/raw/dataverse-final/` y los perfiles curados (§3.4).

| # | Script | Rol | Outputs |
|:-:|:---|:---|:---|
| 0a | `code/0a-verify-dataverse-snapshot.py` | Test de aceptación del snapshot | `QA-report.txt` |
| 0b | `code/0b-audit-profiles-dual-source.py` | Auditoría dual-fuente e imputación de perfiles (§3.4) | `conventional-profiles.json`, `profile-audit/*` |
| 00 | `code/00-build_dynamic_networks.py` | Registro de iniciativas; redes génesis (iniciativa y artículo); ondas ×7 con dedup de indicaciones | `genesis_network_{initiative,article}.csv`, `initiative_registry.csv`, `C{k}_dynamic_networks.json`, `commission_waves.csv` |
| 01 | `code/01-model-valued-ergm.R` | **M1**: Valued ERGM Poisson (red iniciativa) + centralidades | `ergm_pooled_results.rds`, `network_metrics.csv`, `tables/M1_ergm_iniciativa.csv` |
| 02 | `code/02-extract-emirt-temporal.R` | Alineación temporal $\Theta \leftrightarrow$ ondas de las 7 comisiones | `emirt_*.csv` |
| 03 | `code/03-model-network-influence.R` | **M2**: panel FE/RE/OLS + falsificación *lead* + robusteces de ventana (decaimiento, última onda, por día) | `network_exposure_panel.csv`, `panel_regression_results.rds`, `tables/M2_panel.csv` |
| 04 | `code/04-build-article-mapping.py` | Lector de `coincidencias` (unión posicional al BORRADOR) + desenlaces por artículo | `article_mapping_unified.csv`, `track_article_outcomes.csv` |
| 05 | `code/05-nlp-text-similarity.py` | Retención léxica TF-IDF + SBERT; DV $y'$ con fracasos = 0 | `article_similarity_scores.csv`, `author_success_scores.csv` |
| 06 | `code/06-build-integrated-dataset.py` | Merge roster-driven (154 filas por construcción) | `integrated_dataset.{csv,json}` |
| 07 | `code/07-model-spatial-durbin.R` | **M3**: $W$ génesis-iniciativa; Moran; OLS/SAR/SEM/SDM; impactos | `sdm_results.rds`, `tables/M3_*.csv` |
| 08 | `code/08-robustness-checks.R` | Robustez M1 (por comisión ×7, unidad artículo, perfiles pre-auditoría) y M3 (DV antigua, W binaria, W artículo, SBERT); **ERGMs en paralelo** (`mclapply`, hasta 8 P-cores) con cronómetro por modelo | `robustness_results.rds`, `tables/M{1,3}_robustness.csv` |
| 09 | `code/09-figures.py` | Figuras F1--F3 (PDF + PNG 300 dpi, inglés) | `results/figures/*` |
| 10 | `code/10-retention-dynamics.py` | Dinámica de retención sobre ondas de comisión, 7 comisiones, con LOCF | `retention_dynamics_locf.csv`, figura |

## 4.2 Resultados de la versión anterior (extended abstract, abril 2026 — 5 comisiones, red mixta, perfiles heurísticos)

- **M1.** `nodematch(afiliación)` $= +0.41$; `nodematch(abogado)` $= -0.09$; `nodematch(experiencia)` $= -0.12$; `nodematch(mujer)` $= +0.08$; `nodecov(edad)` $= -0.024$. Lectura: homofilia positiva de afiliación y género; **homofilia negativa** de abogados y experimentados (gatekeeping estratégico, H1b confirmada).
- **M2.** OLS agrupado: $\hat\beta_3 = +0.033$ ($p<0.001$); efectos fijos: $\hat\beta_3 = +0.0004$ ($p = 0.91$); Hausman $\chi^2 = 784.96$ ($p<0.001$) → FE. Falsificación con *lead* significativa ($p<0.001$) → **selección, no influencia** (H2 confirmada).
- **M3.** Moran's $I = 0.155$ ($p<10^{-6}$). AIC: OLS $-338.5$ < SEM $-354.8$ ($\lambda=0.81$) < SAR $-355.3$ ($\rho=0.89$) < **SDM $-383.4$ ($\hat\rho = 0.997$)**. En OLS: `theta_mean` $+0.012$ ($p=0.008$), `theta_sd` $-0.103$ ($p=0.015$), `ego_heterophily` $-0.063$ ($p=0.027$); centralidades n.s. Validación de medida: artículos "idénticos" puntúan 0.979 en TF-IDF. Robustez: SBERT preserva signos; con $W$ binaria $\rho$ cae a 0.374 manteniendo significancias clave (H3 confirmada).

## 4.3 Insumos construidos (v2 — julio 2026)


![Constitutional initiatives (solid) over genesis articles (faded) per commission (F1) — la brecha entre ambas barras es el nº de artículos por iniciativa.](../results/figures/initiatives_per_commission.pdf){width=88%}

**Registro de iniciativas** (`initiative_registry.csv`): **528 iniciativas** con $\geq 2$ firmantes-persona — C1: 29, C2: 56, C3: 41, C4: 68, C5: 148, C6: 78, C7: 108 (las iniciativas populares/indígenas quedan fuera de la red de personas). **Red génesis-iniciativa** (principal): 154 nodos (sin aislados), **7.731 aristas**, peso total 53.391, peso máximo 76. **Red génesis-artículo** (robustez): 1.676 eventos de co-firma, 8.020 aristas, peso 175.316.

**Ondas para M2** (T0 génesis + fechas observadas de indicaciones; los duplicados de una misma indicación repetida en el `history[]` de varios artículos se colapsan a un acto):

| Comisión | Ondas de indicaciones | Eventos con lazos | Unipersonales | Duplicados colapsados |
|:-:|:-:|:-:|:-:|:-:|
| C1 | 4 | 266 | 99 | 45 |
| C2 | 3 | 1 | 52 | 0 |
| C3 | 6 | 212 | 29 | 166 |
| C4 | 5 | 154 | 241 | 49 |
| C5 | 5 | 0 | 559 | 0 |
| C6 | 6 | 317 | 64 | 291 |
| C7 | 8 | 54 | 278 | 178 |

Las indicaciones son mayoritariamente unipersonales en varias comisiones (C5: todas): no forman lazos pero sí definen la línea temporal; las ondas de C5 son *planas* (la red no cambia tras T0), igual que en la versión anterior.

**Mapeo y DV de éxito**: 484 pares génesis$\to$final (480 vía `coincidencias` + 4 rescatados parseando `final_status`; 2 uids de indicación no encontrados). Desenlaces de los 1.809 artículos: 223 idéntico, 136 similar, 275 ART-FALLIDO, 1.175 eliminado (fracaso = fallido + eliminado, sim = 0). **Validación del emparejamiento**: similitud alineada 0.554 vs. 0.032 con pares barajados; 96.5% de los pares supera su baseline. TF-IDF idéntico 0.570 / similar 0.565; SBERT 0.857 / 0.872. *Nota sobre el ancla de validación*: la v1 reportaba 0.979 en "idénticos" porque comparaba el texto **post-indicaciones** con el borrador; la v2 usa el texto **génesis verdadero** (la operacionalización correcta de "retención de lo que el delegado propuso"), y la etiqueta idéntico/similar — que refiere al estado *final* del artículo — deja de ordenar la similitud génesis$\to$final; la validación pasa a ser el contraste alineado-vs-barajado. Media de $y'$: 0.094; tasa de supervivencia media: 0.211; **154/154 convencionales con score** (el dataset integrado tiene 154 casos completos: por primera vez el roster entero entra a M3).


![Genesis co-sponsorship as a bipartite network, initiative unit (F2a). Documentos arriba (color por comisión); convencionales abajo, ordenados y coloreados por punto ideal — **rojo = izquierda** ($\theta<0$; Baradit $-1.4$), **azul = derecha** ($\theta>0$; Marinovic $+4.3$), con colorbar.](../results/figures/bipartite_initiative.pdf){width=100%}

![Genesis co-sponsorship as a bipartite network, article unit (F2b) — la especificación de robustez, mismas convenciones.](../results/figures/bipartite_article.pdf){width=100%}
## 4.4 Limitaciones de datos (transversales)

1. **Cobertura de autores.** 134/2.019 registros TRACK sin autores (65 de iniciativas populares/indígenas — patrocinio institucional —, 45 de ICC no recuperadas, 24 sin referencia); 54 artículos del borrador final quedaron `not_traced` en `coincidencias`; 66 + 5 registros *undated* se excluyen de las ondas.
2. **dynIRT sin errores estándar** (P10, abierto): la incertidumbre de $\theta$ no se propaga a M2/M3.

Las limitaciones específicas de cada modelo están al final de su sección (§5.4, §6.5, §7.5).

# 5. Modelo 1 — Formación de la red (Valued ERGM)

## 5.1 Especificación

La red observada es $W = [w_{ij}]$, no dirigida, con $w_{ij} \in \{0, 1, 2, \dots\}$ = nº de **iniciativas** co-firmadas por $i$ y $j$ (unidad principal; §8.1). El ERGM valuado con referencia Poisson modela
$$P(W = w) = \frac{\exp\{\theta^\top g(w)\}}{\kappa(\theta)} \prod_{i<j} \frac{1}{w_{ij}!},$$
con estadísticas de cambio
$$g(w) = \Big(\underbrace{\textstyle\sum_{i<j} w_{ij}}_{\text{sum}},\ \underbrace{\textstyle\sum_{i<j} w_{ij}\,\mathbf{1}\{X_i = X_j\}}_{\text{nodematch}_X},\ \underbrace{\textstyle\sum_{i<j} w_{ij}\,|X_i - X_j|}_{\text{absdiff}_X},\ \underbrace{\textstyle\sum_{i<j} w_{ij}(X_i + X_j)}_{\text{nodecov}_X}\Big)$$
para $X \in$ \{afiliación, experiencia previa, abogado, mujer\} (nodematch), \{edad, grado académico 0--3\} (absdiff) y edad (nodecov). Estimación MCMLE (`ergm.count`, semilla 42), 154 nodos (roster completo, sin aislados). Un coeficiente nodematch positivo indica homofilia; `absdiff` negativo indica que la **distancia** en el atributo inhibe la co-firma.

## 5.2 Resultados
| Término | Estimación | EE | $p$ |
|:---|---:|---:|:--|
| sum | $+1.498$ | 0.027 | $<10^{-4}$ |
| nodematch afiliación | $+0.577$ | 0.010 | $<10^{-4}$ |
| nodematch experiencia previa | $+0.435$ | 0.011 | $<10^{-4}$ |
| nodematch abogado | $+0.156$ | 0.009 | $<10^{-4}$ |
| nodematch mujer | $+0.142$ | 0.009 | $<10^{-4}$ |
| absdiff edad | $-0.0005$ | 0.0005 | 0.32 |
| absdiff grado académico (0--3) | $-0.048$ | 0.006 | $<10^{-4}$ |
| nodecov edad | $-0.008$ | 0.0003 | $<10^{-4}$ |

**Cambio sustantivo respecto de la v1: H1b se invierte.** Con la red génesis limpia y los perfiles curados, abogados y delegados con experiencia institucional muestran homofilia **positiva** (antes: $-0.09$ y $-0.12$) — el hallazgo de "gatekeeping estratégico" de la v1 no sobrevive a la corrección de datos. H1a se refuerza (afiliación $+0.577$ vs. $+0.41$). El término nuevo `absdiff(grado)` es negativo y significativo: la **distancia educativa inhibe la co-firma** (estratificación por credenciales), y `nodecov(edad)` negativo indica que los mayores co-firman menos en volumen. *Caveat técnico*: el MCMLE del ERGM pooled converge al 95% pero no al 99% de confianza (p = 0.014 del test de convergencia tras 60 iteraciones); los coeficientes son estables entre corridas pero el caveat queda registrado.


## 5.3 Robustez y descomposición del vuelco de H1b
Los **9 ERGMs de robustez convergieron** (los 7 por comisión — antes C3 y C5 no convergían: P11 resuelto — más las dos variantes pooled). Desde la v2.1 corren **en paralelo** (`mclapply` sobre 8 P-cores del M4 Pro, un modelo por worker, semilla fija por modelo): **10.2 min de pared vs. 24.0 min de suma secuencial**; el techo es el modelo individual más pesado (pooled unidad artículo, 10.2 min), de modo que la ganancia adicional requeriría paralelizar las cadenas MCMC dentro de ese fit. Coeficientes clave (`M1_robustness.csv`; \*\*\* $p<0.001$, \*\* $p<0.01$, \* $p<0.05$):

| Modelo | nodematch afiliación | nodematch experiencia | nodematch abogado | absdiff grado |
|:---|:-:|:-:|:-:|:-:|
| C1 Sistema Político | $+0.511$*** | $-0.175$** | $-0.018$ | $+0.192$*** |
| C2 Principios | $+0.428$*** | $+0.655$*** | $+0.298$*** | $+0.032$ |
| C3 Forma de Estado | $+0.505$*** | $-0.207$*** | $-0.071$* | $-0.027$ |
| C4 Derechos Fundamentales | $+0.502$*** | $+0.530$*** | $+0.267$*** | $+0.105$*** |
| C5 Medio Ambiente | $+0.812$*** | $+0.399$*** | $+0.171$*** | $-0.237$*** |
| C6 Sistemas de Justicia | $+0.673$*** | $+0.405$*** | $+0.001$ | $-0.051$*** |
| C7 Conocimientos | $+0.345$*** | $+0.672$*** | $+0.217$*** | $-0.020$ |
| Pooled unidad artículo | $+0.844$*** | $+0.531$*** | $+0.187$*** | $-0.041$*** |
| Pooled iniciativa, **perfiles pre-auditoría** | $+0.181$*** | $+0.370$*** | $+0.212$*** | $+0.009$ |

**Descomposición del vuelco.** Con los perfiles *antiguos* (pre-auditoría) sobre la red nueva, la homofilia de experiencia y abogados **sigue siendo positiva**: el vuelco de H1b **no** proviene de la corrección de covariables sino del **cambio de red** — génesis puro (sin indicaciones) y cobertura de 7 comisiones. La corrección de perfiles sí explica otra cosa: la homofilia de afiliación salta de $+0.181$ (con 33 "Desconocida" y falsos independientes) a $+0.577$ (curada) — el ruido de medición diluía el clivaje político.

**El patrón fino es heterogéneo y sustantivamente rico**: la homofilia *negativa* de experiencia sobrevive **exactamente en C1 (Sistema Político) y C3 (Forma de Estado)** — las comisiones donde se diseñaba la arquitectura del poder, dominantes en la muestra antigua — mientras en las otras cinco es fuertemente positiva. La lectura de gatekeeping de la v1 no era un fantasma: es un fenómeno **específico de las comisiones de diseño institucional**, no una estrategia general. La unidad iniciativa vs. artículo no altera ninguna conclusión (§8.1).

## 5.4 Limitaciones

1. **Convergencia del ERGM pooled**: MCMLE al 95% pero no al 99% de confianza ($p = 0.014$ del test tras 60 iteraciones); coeficientes estables entre corridas.
2. **Unidad de co-firma**: iniciativa como principal es una decisión interpretativa (§8.1); los resultados son cualitativamente iguales con unidad artículo (§5.3).

# 6. Modelo 2 — Selección vs. influencia (panel con efectos fijos)

## 6.1 Especificación

Sobre las ondas por comisión (T0 génesis + fechas de informes de indicaciones; figura F3), la exposición de red de $i$ en la onda $t$ de la comisión $c$ usa la red **acumulada** $W^{c}_{t}$:
$$\text{NetExp}_{i,t} = \frac{\sum_{j \neq i} w_{ij,t}\,\theta_{j,t}}{\sum_{j \neq i} w_{ij,t}},$$
y el modelo principal es el panel con efectos fijos individuales
$$\Delta\theta_{i,t} = \alpha_i + \beta_1\,\theta_{i,t-1} + \beta_3\,\text{NetExp}_{i,t-1} + \varepsilon_{it},$$
con errores agrupados por delegado y test de Hausman contra efectos aleatorios. **Falsificación**: se reemplaza el rezago por el *lead* ($\text{NetExp}_{i,t+1}$); si el efecto contemporáneo fuera influencia causal, el lead debería ser nulo. **Ventanas alternativas de exposición** (robustez): decaimiento exponencial $W^{dec}_{t} = \sum_{s \le t} \lambda^{t-s}\,\Delta W_s$ con $\lambda = 0.5$; solo-última-onda $W^{last}_{t} = \Delta W_t$; y $\Delta\theta$ estandarizado por días transcurridos entre ondas. Los pasos que comparten período emIRT con el anterior ($\Delta\theta \equiv 0$ mecánico) se excluyen (1.078 celdas).

![Wave construction for Model 2, by commission (F3; archivo `C#_waves_summary`).](../results/figures/C%23_waves_summary.pdf){width=100%}

## 6.2 Motivación descriptiva (diseño en curso)

Para animar la pregunta del modelo ("las posiciones cambian y la red cambia: ¿influencia o selección?") se acordó una batería descriptiva en tres piezas:

**(a) La red cambia — análisis de pertinencia (2026-07-08, ejecutado).** Distribución de los eventos multi-autor por onda (¿lazos nuevos solo al inicio?):

| Comisión | Eventos por onda (fecha: n) | % eventos con 3+ autores |
|:-:|:---|:-:|
| C1 | 03-17: 145 · 04-01: 66 · 04-18: 38 · 04-30: 17 | 97% |
| C2 | 03-02: 1 · resto: 0 | — |
| C3 | 02-14: 81 · 03-01: 15 · 03-14: 4 · 04-04: 4 · 04-06: 80 · 04-26: 28 | 98% |
| C4 | 03-07: 44 · 03-24: 27 · 04-08: 60 · 04-25: 21 · 05-05: 2 | 74% |
| C5 | 0 en todas | — |
| C6 | 02-08: 19 · 02-23: 8 · 03-07: 42 · 04-01: 98 · 04-27: 91 · 05-08: 59 | 88% |
| C7 | 02-19: 42 · resto: 4, 5, 0, 1, 1, 0, 0 | 83% |

Lectura: la formación de lazos post-génesis es **sostenida en C1, C3, C4 y C6** (C6 incluso crece hacia el final; C3 es bimodal), **de un solo golpe en C7** (42/54 en una onda) y **ausente en C2/C5**. Los eventos de exactamente 2 autores — potencialmente problemáticos — son minoría (2--3% en C1/C3; 26% en C4). **Comisiones pertinentes para el panel (a): C1, C3, C4, C6; C7 con nota; C2/C5 se excluyen.**

**(b) Las posiciones cambian (diseño acordado, pendiente de construir).** Dos piezas: la dinámica temporal de las posiciones (spaghetti de $\theta_{i,t}$ con medias por bloque, siguiendo el estilo del proyecto `Z-conv-const-dynamics` que grafica W-Nominate/Ideal por votante y coalición) y la distribución de $|\Delta\theta|$ por onda.

**(c) Preview construido (2026-07-08).** Correlación transversal entre la exposición y la posición propia, por comisión y onda, contra el efecto within:

![Selection, not influence — preview del mini-panel (c).](../results/figures/m2_selection_vs_influence_preview.pdf){width=92%}

Lectura: en **cada comisión y cada onda**, la posición ponderada de los co-firmantes sigue de cerca la propia ($r$ transversal 0.83--0.97, estable en el tiempo — la firma de la selección homofílica), mientras el efecto *within* de la exposición rezagada sobre $\Delta\theta$ es exactamente cero. La brecha entre esas dos cantidades **es** el resultado de M2 en una imagen.

## 6.3 Resultados
Panel: **4.278 observaciones** delegado-onda (antes 2.926), 154 delegados, 7 comisiones; 1.078 celdas cuyo paso comparte período emIRT con el anterior ($\Delta\theta \equiv 0$ mecánico) se excluyen. Coeficiente de exposición ($\hat\beta_3$), EE agrupados por delegado:

| Modelo | $\hat\beta_3$ | EE | $p$ |
|:---|---:|---:|:--|
| OLS agrupado | $+0.0356$ | 0.0099 | $3.1\times10^{-4}$ |
| **Efectos fijos (within)** | $-0.0007$ | 0.0045 | $0.88$ |
| OLS + FE de comisión | $+0.0364$ | 0.0095 | $1.4\times10^{-4}$ |
| Falsificación (*lead*) | $+0.0552$ | 0.0116 | $2.2\times10^{-6}$ |
| FE, exposición con decaimiento ($\lambda=0.5$) | $+0.0017$ | 0.0045 | $0.71$ |
| FE, exposición solo-última-onda | $+0.0068$ | 0.0128 | $0.60$ |
| FE, $\Delta\theta$ por día | $-0.0001$ | 0.0004 | $0.83$ |

Hausman $\chi^2 = 1973.9$ ($p \approx 0$) $\to$ FE. **El resultado central de la v1 se replica y blinda**: la correlación positiva desaparece bajo efectos fijos, la exposición futura "predice" el cambio pasado (falsificación falla $\to$ selección endógena), y el nulo bajo FE sobrevive las tres ventanas alternativas de exposición. H2 confirmada con el doble de observaciones y las 7 comisiones.


Coeficientes completos (`M2_full_models.csv`; EE agrupados por delegado en OLS/FE/lead; \*\*\* $p<0.001$, \*\* $p<0.01$, \* $p<0.05$):

| Término | OLS agrupado | Efectos fijos | Efectos aleatorios | Falsificación (lead) |
|:---|:-:|:-:|:-:|:-:|
| Intercepto | $0.032$ (0.016)\* | — | $0.007$ (0.019) | $0.013$ (0.011) |
| $\theta_{t-1}$ | $-0.038$ (0.008)\*\*\* | $-0.656$ (0.022)\*\*\* | $-0.074$ (0.006)\*\*\* | $-0.059$ (0.009)\*\*\* |
| NetExp$_{t-1}$ | $+0.036$ (0.010)\*\*\* | $-0.001$ (0.005) | $+0.051$ (0.006)\*\*\* | — |
| NetExp$_{t+1}$ (lead) | — | — | — | $+0.055$ (0.012)\*\*\* |
| Abogado | $0.020$ (0.019) | — | $0.032$ (0.021) | — |
| Experiencia previa | $-0.015$ (0.021) | — | $0.037$ (0.026) | — |
| Mujer | $-0.019$ (0.018) | — | $-0.030$ (0.020) | — |

## 6.5 Limitaciones

1. **C2 delgada.** Por la naturaleza distinta de la comisión y de su registro, C2 aporta solo 3 ondas con 1 evento de co-firma de indicaciones (52 unipersonales). Decisión del usuario (2026-07-07): **se usa de todas formas**; su contribución a M2 es mínima y así debe leerse.
2. **Ondas planas en C5.** Todas sus indicaciones con autor son unipersonales: la red de C5 no cambia después de T0 y su aporte proviene de la variación temporal de $\theta$ (igual que en la v1).
3. **Pasos con el mismo período emIRT** (sin votaciones intermedias) se excluyen del panel: 1.078 celdas.
4. La incertidumbre de $\theta$ (dynIRT sin SEs, P10) no se propaga.

# 7. Modelo 3 — Éxito legislativo (Spatial Durbin Model)

## 7.1 Especificación

La DV principal es la **retención esperada por artículo presentado** (decisión ART-FALLIDO, P6): con $A_i$ = artículos génesis co-firmados por $i$ y $M_i \subseteq A_i$ los trazados al borrador,
$$y_i' = \frac{1}{|A_i|}\sum_{a \in A_i} \text{sim}(a) = \underbrace{\frac{|M_i|}{|A_i|}}_{\hat{s}_i} \times \underbrace{\overline{\text{sim}}_{M_i}}_{\bar{r}_i}, \qquad \text{sim}(a) = 0 \text{ si } a \text{ fallido/eliminado},$$
donde $\text{sim}(a)$ es el coseno TF-IDF entre el texto génesis de $a$ y su artículo del borrador final (máximo sobre sus pares si mapea a varios; SBERT como medida alternativa). El modelo espacial es
$$y' = \rho\,\tilde{W} y' + X\beta + \tilde{W} X\gamma + \varepsilon,$$
con $\tilde{W}$ = red génesis-iniciativa **row-normalizada** (la estructura precede al resultado, P9), estimado por máxima verosimilitud; se compara con OLS, SAR ($\gamma = 0$) y SEM (error espacial) vía AIC, y la dependencia se diagnostica con el $I$ de Moran. Los efectos marginales del SDM se descomponen con $(I - \rho \tilde{W})^{-1}(I\beta_k + \tilde{W}\gamma_k)$ en directo/indirecto/total.

## 7.2 Resultados
$N = 154$ (casos completos). Moran's $I = 0.380$ ($p \approx 10^{-194}$; antes 0.155). Comparación (AIC): OLS $-448.9$ < SEM $-529.5$ ($\lambda = 0.981$) < SAR $-539.3$ ($\rho = 0.979$) < **SDM $-575.5$ ($\hat\rho = 0.943$)**. En OLS, con la DV nueva la **posición de red predice éxito**: `degree` $+0.0012$ ($p = 0.002$), `betweenness` $-0.0011$ ($p = 0.004$) — en la v1 las centralidades eran n.s. En el SDM los efectos directos individuales se disipan y los términos **contextuales** son los significativos (`lag.degree` $+0.009$, `lag.betweenness` $-0.008$, `lag.es_mujer` $-0.175$, `lag.theta_mean` $+0.093$): el éxito propio depende de los atributos de los co-firmantes. H3 confirmada y amplificada. *Nota*: con $\rho$ cercano a 1, la descomposición directo/indirecto de los impactos es numéricamente inestable (SEs simuladas explotan); se reportan coeficientes y $\rho$, y los impactos quedan en el `.rds`. Parte del derrame es mecánica — los co-firmantes comparten artículos y por tanto componentes de $y'$ — lo que refuerza la lectura "el éxito es colectivo" pero impide interpretar $\rho$ como influencia pura (ver Limitaciones).

Robustez M3 (tabla `M3_robustness.csv`): la dependencia espacial sobrevive todas las variantes — DV condicional antigua ($I = 0.140$, $\rho = 0.840$), $W$ binaria ($\rho = 0.794$), $W$ artículo ($I = 0.408$, $\rho = 0.934$), DV SBERT ($\rho = 0.937$).


Comparación de modelos y parámetros espaciales:

| Modelo | AIC | Parámetro espacial |
|:---|---:|:---|
| OLS | $-448.9$ | — |
| SEM | $-529.5$ | $\lambda = 0.981$ (0.012)\*\*\* |
| SAR | $-539.3$ | $\rho = 0.979$ (0.014)\*\*\* |
| **SDM** | $\mathbf{-575.5}$ | $\rho = 0.943$ (0.037)\*\*\* |

Coeficientes completos de OLS y SDM (`M3_full_models.csv`; \*\*\* $p<0.001$, \*\* $p<0.01$, \* $p<0.05$, $\dagger$ $p<0.1$):

| Término | OLS | SDM (directo) | SDM (lag espacial $\tilde{W}X$) |
|:---|:-:|:-:|:-:|
| Intercepto | $-0.020$ (0.039) | $-0.532$ (0.253)\* | — |
| Degree | $+0.00125$ (0.0004)\*\* | $+0.00048$ (0.0003) | $+0.0092$ (0.0022)\*\*\* |
| Betweenness | $-0.00110$ (0.0004)\*\* | $-0.00018$ (0.0002) | $-0.0081$ (0.0019)\*\*\* |
| Abogado | $0.0125$ (0.010) | $0.0093$ (0.006) | $0.102$ (0.091) |
| Experiencia previa | $0.0050$ (0.013) | $0.0089$ (0.008) | $-0.078$ (0.109) |
| Mujer | $-0.0163$ (0.009)$\dagger$ | $-0.0013$ (0.006) | $-0.175$ (0.063)\*\* |
| Edad | $-0.0001$ (0.0004) | $-0.0001$ (0.0002) | $-0.0025$ (0.0029) |
| Grado académico (0--3) | $0.0093$ (0.006) | $0.0026$ (0.004) | $-0.0083$ (0.048) |
| $\theta$ medio | $0.0020$ (0.004) | $-0.0034$ (0.004) | $+0.0925$ (0.021)\*\*\* |
| $\theta$ desv. est. | $0.0270$ (0.031) | $0.0088$ (0.019) | $0.158$ (0.164) |
| Heterofilia del ego | $0.0026$ (0.017) | $-0.0081$ (0.012) | $-0.047$ (0.086) |

## 7.3 Robustez

La dependencia espacial sobrevive todas las variantes (`M3_robustness.csv`):

| Variante | $N$ | Moran $I$ | $\rho$ (SDM) | AIC OLS | AIC SDM |
|:---|:-:|:-:|:-:|---:|---:|
| $y'$, $\tilde{W}$ iniciativa (**principal**) | 154 | 0.380 | 0.943 | $-448.9$ | $-575.5$ |
| $y$ condicional (DV antigua), $\tilde{W}$ iniciativa | 154 | 0.140 | 0.840 | $-224.9$ | $-239.5$ |
| $y'$, $\tilde{W}$ binaria | 154 | 0.185 | 0.794 | $-448.9$ | $-512.6$ |
| $y'$, $\tilde{W}$ artículo | 154 | 0.408 | 0.934 | $-448.9$ | $-581.3$ |
| $y'$ SBERT, $\tilde{W}$ iniciativa | 154 | 0.399 | 0.937 | $-329.2$ | $-489.0$ |

## 7.4 Dinámica de la retención sobre las ondas de comisión (7 comisiones, LOCF)

La comparación génesis$\to$final es el extremo de una **trayectoria**: cada artículo pasa por estados intermedios (`content_snapshot` tras cada indicación, almacenados en el dataset — no se reconstruyen). El análisis (`code/10-retention-dynamics.py`, generalización del piloto C3) mide, para los **359 artículos trazados**, la similitud de su estado vigente en **cada onda de su comisión** contra su artículo del borrador; si un artículo deja de modificarse antes de la última onda, su valor **se propaga hacia adelante** (LOCF, decisión 2026-07-08). Cobertura de snapshots en artículos trazados: 100% en C3/C4/C5/C6, 97% en C2/C7, 79% en C1.

![Amendment trajectories toward the final text — all commissions (LOCF).](../results/figures/retention_dynamics_all_commissions.pdf){width=100%}

Similitud media al borrador, génesis $\to$ última onda: C1 $0.65 \to 0.82$; C2 $0.39 \to 0.88$; C3 $0.50 \to 0.92$; C4 $0.49 \to 0.94$; C5 $0.15 \to 0.95$; C6 $0.40 \to 0.98$; C7 $0.33 \to 0.89$. Tres lecturas: (i) el proceso de indicaciones es **convergente hacia el borrador en las 7 comisiones**; (ii) la heterogeneidad de partida es enorme — C5 nace lejísimos de su forma final (0.15) y se reescribe casi por completo vía indicaciones (unipersonales: por eso su red no cambia aunque su texto sí); C1 nace cerca (0.65) y se mueve poco; (iii) el estado post-última-indicación ($\approx 0.9$) es lo que la v1 medía como "retención" (0.979), confirmando la nota del ancla (§7.5). **Extensión natural**: el dataset artículo-onda (`retention_dynamics_locf.csv`) es la base del futuro M4 de supervivencia.

## 7.5 Limitaciones

1. **Derrame parcialmente mecánico.** $y'$ de co-firmantes comparte artículos por construcción; $\rho$ combina interdependencia real y composición compartida del DV. La extensión artículo-onda (M4 futura; §7.4) es el camino para separarlas.
2. **Impactos inestables con $\rho \to 1$.** La descomposición directo/indirecto vía $(I-\rho\tilde{W})^{-1}$ explota numéricamente (SEs simuladas enormes); se reportan coeficientes y $\rho$; los impactos quedan en `sdm_results.rds`.
3. **Ancla de validación NLP.** La v1 (0.979 en "idénticos") comparaba el texto post-indicaciones; la v2 usa el génesis verdadero, y la etiqueta idéntico/similar (que refiere al estado final) deja de ordenar la similitud génesis$\to$final. La validez del emparejamiento se establece por el contraste alineado-vs-barajado (0.554 vs. 0.032; 96.5% de pares sobre su baseline) y por el piloto (§7.4).

# 8. Decisiones de diseño confirmadas

| Fecha | Decisión |
|:---|:---|
| 2026-07-06 | `coincidencias_comisiones.csv` (UTF-8) es la fuente de verdad del mapeo génesis→borrador final (P7). |
| 2026-07-06 | **M1 = red génesis pura**, 7 comisiones. Indicaciones solo en las ondas de M2. Génesis+indicaciones como robustez (P2). |
| 2026-07-06 | **M3**: DV principal con ART-FALLIDO = 0 ($y' = \hat{s}\cdot\bar{r}$); DV condicional antigua como robustez (P6). $W$ y centralidades sobre la red génesis (P9). |
| 2026-07-06 | **M2**: exposición acumulada desde T0 hasta $t-1$ como especificación principal; robustez con decaimiento exponencial y con ventana solo-última-onda; sensibilidad estandarizando $\Delta\theta$ por días entre ondas. |
| 2026-07-06 | Indicaciones sueltas: entran a las ondas de M2 (timestamp+authors), no a M1 ni a M3; títulos se descartan (P8). |
| 2026-07-06 | Numeración oficial de comisiones confirmada; rótulos temáticos del paper a corregir (P13.1). |
| 2026-07-06 | Scraper BCN corregido y re-ejecutado: 154/154 perfiles (P5). |
| 2026-07-06 | Auditoría P5 vía `gemini-3.5-flash` (BCN + Wikipedia independientes, lotes de 5, situación al momento de la CC); símbolos $=$/$\neq$/B/W/$\varnothing$ por celda; validación humana antes de regenerar covariables. |
| 2026-07-06 | Arreglos que corresponden al repo CPT registrados en `CPT-arreglos-pendientes.txt` (raíz); se abordarán en ese repositorio. |
| 2026-07-07 | Grado académico pasa a escala ordinal **0--3** (sin estudios universitarios terminados / educación superior terminada / magíster / doctorado) y entra a M1 como **absdiff** (distancia educativa). |
| 2026-07-07 | Imputación automatizada de covariables: BCN ground truth → Wikipedia → base; `manual_validations.json` siempre prevalece. `data/raw/conventional-profiles.json` es el archivo curado canónico (154). |
| 2026-07-07 | Snapshot `dataverse-final` ingerido (CPT `paper-draft` @ `5852519`) con test de aceptación 0a; regla P14 instaurada. Infraestructura de Fase 0 creada: `paths.py`/`paths.R`, `lib_names.py`/`lib_names.R`. |
| 2026-07-07 | Las referencias a iniciativas populares en `authors` se clasifican como no-persona y quedan fuera de la red de co-patrocinio. |
| 2026-07-07 | ~~Actualización con C1 y C3 a C7~~ → **ronda 2 de CPT resolvió P15: las 7 comisiones entran a la actualización** (snapshot @ `6fac4c4`). |
| 2026-07-07 | Registros sin fecha ("undated") y residuo de 134 sin autores: documentados, no imputados; se excluyen de las ondas / de la red según corresponda. |
| 2026-07-07 | **Unidad de co-firma: INICIATIVA como especificación principal, artículo como robustez** (preferencia del usuario; ver §8.1). |
| 2026-07-07 | **C2 se usa en M2 pese a sus ondas delgadas** (naturaleza distinta de la comisión); documentado como limitación §6.5. |
| 2026-07-07 | Indicaciones repetidas en el `history[]` de varios artículos se colapsan a **un** acto de co-firma (dedup por autores+fecha+contenido). |
| 2026-07-07 | Pipeline v2 ejecutado completo (00--08); resultados en §5--§7 y `results/tables/`. |

## 8.1 Puntos a revisar (no son problemas P)

1. **Unidad de co-firma iniciativa vs. artículo.** La especificación principal usa iniciativa (cada documento co-firmado cuenta una vez); la de artículo pondera por el número de artículos de la iniciativa. Los resultados de M1 son cualitativamente iguales bajo ambas (ver `M1_robustness.csv`) y M3 mantiene $\rho \approx 0.93$ con $W$-artículo, pero conviene revisitar la elección al escribir el paper (interpretación del peso $w_{ij}$).
2. **Interpretación del vuelco de H1b.** Con datos corregidos, la homofilia de abogados/experimentados pasó de negativa a positiva (§5.2): decidir cómo narrarlo en el paper (corrección de un artefacto de medición de la v1) y si mantener el marco de *gatekeeping* como hipótesis rechazada o reformularlo.
3. **$\lambda = 0.5$ del decaimiento en M2** es una elección de conveniencia; si la robustez de ventana entra al paper, barrer $\lambda \in \{0.25, 0.5, 0.75\}$.

# 9. Plan de actualización

- **Fase 0 — Infraestructura**: **completada** (2026-07-07): snapshot `dataverse-final` versionado + QA de aceptación (`0a-verify-dataverse-snapshot.py`); perfiles curados 154; `paths.py`/`paths.R`; `lib_names.py`/`lib_names.R` con validación al 100%. *(P14 cerrado; P3 y P4 quedan en su mitad de recableado de scripts.)*
- **Fase 0b — Calidad de covariables**: **completada** (2026-07-07): auditoría dual-fuente v1+v2, validación manual, e imputación automática con capa de validaciones humanas; `data/raw/conventional-profiles.json` regenerado con 154 perfiles curados. *(P5 cerrado.)*
- **Fase 1 — Loader unificado**: **completada** (2026-07-07) dentro del script 00 v2: lector GENESIS (2 esquemas), clasificación artículo/indicación-suelta, dedup de indicaciones repetidas, parseo de timestamps. *(P8 cerrado en su parte de loader.)*
- **Fase 2 — Redes**: **completada** (2026-07-07): red génesis-iniciativa pooled (528 iniciativas, decisión §6.1) + red artículo de robustez + ondas ×7 con bins observados. *(P1-red y P2 cerrados.)*
- **Fase 3 — Modelos**: **completada** (2026-07-07): M1 ERGM (iniciativa + robusteces), M2 panel de 4.278 obs con falsificación y ventanas, M3 SDM con `coincidencias`, DV $y'$ y $W$-génesis. Resultados en §5--§7. *(P1, P6, P7, P9 cerrados; P11 re-evaluado con la robustez por comisión.)*
- **Fase 4 — Integración y paper**: **parcial**: `integrated_dataset` con exactamente 154 filas, suite de robustez y tablas exportadas a `results/tables/` (hechos); **pendiente**: la actualización del extended abstract y README (cifras nuevas, rótulos oficiales, narrativa del vuelco de H1b, eliminar "ongoing work"). *(P12 cerrado — tablas y figuras F1--F3 exportadas; P13 abierto.)*

# 10. Registro de cambios de este documento

- **v2.2 (2026-07-08).** Iteración de figuras: F1 con artículos translúcidos detrás de las iniciativas; bipartitas con gradiente ideológico **rojo = izquierda** / azul = derecha (verificado: $\theta$ negativo = izquierda; Baradit $-1.4$), colorbar y títulos sin paréntesis; F3 con separador de columnas, subtítulo "$\geq$ 2 delegate signers" y renombrada a `C#_waves_summary`. §6.2 con el **análisis de pertinencia** por onda (panel (a): C1/C3/C4/C6 pertinentes, C7 marginal, C2/C5 excluidas) y el **preview del mini-panel (c)** (correlación transversal 0.83--0.97 vs. within = 0). §7.4 generalizado: dinámica de retención de las 7 comisiones sobre ondas de comisión con **LOCF** (359 artículos; convergencia al borrador en todas; C5 se reescribe casi por completo: $0.15 \to 0.95$); script 10 renombrado a `10-retention-dynamics.py`.
- **v2.1 (2026-07-08).** Reestructura: resultados en secciones independientes por modelo (§5 M1, §6 M2, §7 M3) con especificación matemática, tablas completas de coeficientes (`M2_full_models.csv`, `M3_full_models.csv`) y robustez/limitaciones por sección; "Problemas" pasa a **Anexo** al final (con hoja en blanco previa). Figuras F1--F3 (iniciativas por comisión, bipartitas iniciativa/artículo, construcción de ondas) y **piloto de dinámica de retención en C3** (§7.4: génesis 0.473 $\to$ 0.919 post-indicaciones; 71% monótono). 08 paralelizado (`mclapply`) con cronómetro. Propuesta de motivación descriptiva para M2 en §6.2 (no ejecutada).
- **v2.0 (2026-07-07).** **Re-ejecución completa del estudio con las 7 comisiones** (pipeline v2, scripts 00--08 reescritos sobre paths/lib_names): red génesis-INICIATIVA (528 iniciativas; decisión §6.1), panel M2 de 4.278 obs, M3 con DV $y'$ y N = 154 completos. Nuevas §3.4 (regla de perfiles), §4.3 (resultados v2), §4.4 (limitaciones, incl. C2 delgada) y §6.1 (puntos a revisar). Hallazgos: H1a reforzada; **H1b invertida** (homofilia positiva de abogados/experiencia; el gatekeeping de la v1 no sobrevive la corrección de datos); nueva estratificación educativa (absdiff grado $-0.048$); H2 replicada y blindada (FE nulo + falsificación + 3 ventanas); H3 amplificada (Moran 0.38, $\rho$ 0.94, robusta a 4 variantes). P1--P4, P6, P7, P9 cerrados; P12 parcial.
- **v1.4 (2026-07-07).** Ronda 2 de CPT ingerida (snapshot @ `6fac4c4`): P15 resuelto (authors de C2 vía `sources`→firmantes y C4 vía `icc_id`; 1.676 artículos con $\geq 2$ autores; residuo 134 documentado), timestamps *undated* documentados (P8.4). QA re-ejecutado: 75.616 menciones, 100% resueltas. Las 7 comisiones entran a la actualización; luz verde para reescribir el pipeline.
- **v1.3 (2026-07-07).** Fase 0 completada: snapshot `dataverse-final` ingerido (CPT `paper-draft` @ `5852519`; TRACK_full 2.019, 210 indicaciones sueltas, uid/final_status 100%), test de aceptación 0a, módulos `paths` y `lib_names` (70.043 menciones de autor validadas al 100%, con filtro de iniciativas populares). P14 cerrado; P3/P4/P8 avanzados a parcial; **nuevo P15**: C2 sin `authors` (bloqueado en CPT). §3 reescrita con las cifras del snapshot.
- **v1.2 (2026-07-07).** P5 cerrado: corrida v2 de la auditoría (lotes de 6, grado 0--3, `profiles-batches/`), chequeo de consistencia v1 vs. v2, e imputación automatizada (BCN → Wikipedia → base, con `manual_validations.json` prevaleciendo); `data/raw/conventional-profiles.json` regenerado (154 perfiles curados, 0 "Desconocida"). Decisión de modelo: `absdiff(grado académico 0--3)` se agrega a M1.
- **v1.1 (2026-07-06).** P5: auditoría dual-fuente BCN+Wikipedia ejecutada con `gemini-3.5-flash` (154/154; 135 discrepancias con el pipeline; tablas en `data/raw/profile-audit/`). Nuevo `CPT-arreglos-pendientes.txt` en la raíz con los arreglos que corresponden al repositorio de datos (P8, higiene, documentación).
- **v1 (2026-07-06).** Versión inicial: objetivos y marco teórico (§2), inventario de datos nuevos (§3), pipeline y resultados vigentes (§4), registro detallado de problemas P1--P14 (§5), decisiones confirmadas (§6) y plan por fases (§7). Incluye el fix del scraper BCN (P5) aplicado en esta fecha.

\clearpage
\thispagestyle{empty}
\mbox{}
\clearpage

# Anexo — Problemas a abordar

Esta sección es el inventario completo de defectos, deudas y decisiones pendientes detectados en la auditoría de julio 2026 (previa a la actualización). Es la lista de control contra la cual se ejecutó el plan de la §9.

## P1 — Cobertura incompleta de comisiones **[RESUELTO 2026-07-07 — pipeline v2 con las 7 comisiones; resultados §4.3]**

**Situación.** La versión vigente usa 5 de las 7 comisiones en M1 (C1, C3, C5, C6, C7; C2 y C4 ausentes del repo) y solo **3** en M2 y M3 (C1, C3, C5), porque los archivos antiguos de C6/C7 no tienen `final_status` ni ondas temporales. El propio abstract lo declara como limitación ("Ongoing work").

**Consecuencias.** (i) La red pooled subestima lazos formados vía C2/C4 — con 312 y 167 iniciativas génesis, no son comisiones menores; C4 (Derechos Fundamentales) fue además la comisión políticamente más cargada. (ii) El mapeo de éxito cubre 236 de 498 artículos finales (47%): la DV de M3 ignora más de la mitad del borrador. (iii) El panel de M2 descarta la dinámica de 4 comisiones.

**Solución.** Migrar todo el pipeline a los `dataverse-final` de las 7 comisiones (PR #4 de CPT, mergeado): GENESIS para la red de co-patrocinio, TRACK_full para ondas e indicaciones, `coincidencias_comisiones.csv` + BORRADOR_final para el éxito.

## P2 — Definición de lazo inconsistente entre comisiones **[RESUELTO 2026-07-07 — M1 con red génesis-iniciativa uniforme; indicaciones solo en ondas M2]**

**Situación.** En `code/00-build_dynamic_networks.py`, la red pooled suma la **última onda acumulada** de C1/C3/C5 (= génesis **+ todas las indicaciones**, líneas 175--178) pero solo el génesis de C6/C7 (líneas 194--204). La red que estima M1 — y que M3 usa como $W$ y para las centralidades — mezcla, por tanto, dos definiciones de lazo según la comisión.

**Consecuencias.** Los pesos no son comparables entre comisiones (C5 aporta 4.968 aristas acumuladas vs. C7 solo génesis); los coeficientes del ERGM confunden formación primaria con colaboración posterior.

**Solución (decisión 2026-07-06).** **M1 usará exclusivamente la red génesis** de las 7 comisiones: modela la *formación primaria* de lazos entre desconocidos, y el co-patrocinio de iniciativas es el acto fundacional (además, con umbral reglamentario 8--16 firmas, es un compromiso costoso y acotado). Las indicaciones entran solo a las **ondas acumuladas** de M2 (colaboración posterior, condicionada por la red ya formada). Para M3, ver P9. Robustez: red génesis+indicaciones como variante.

## P3 — Reproducibilidad rota: rutas hardcodeadas a carpetas legacy de otro repositorio **[RESUELTO 2026-07-07 — scripts 00--08 reescritos sobre paths.{py,R}; pipeline corre de extremo a extremo desde este repo]**

**Situación.** Los scripts 02--05 leen desde rutas absolutas del repo CPT, y además desde carpetas **legacy previas a la limpieza**: `02-extract-emirt-temporal.R:10` (`emIRT-analysis/`, escribe a `playground/research-proposal-implementation/data`), `03-model-network-influence.R:30-32` (`conventionals-bcn-webscrapping/`, `network-visualization/`), `04-build-article-mapping.py:15-22` (`comision-N/draft-after-indications-manual/`, `proposals/draft_final_text.json`), `05-nlp-text-similarity.py:23-24`. El archivo `draft_final_text.json` ni siquiera existe en este repositorio. Los scripts 00, 01, 06, 07, 08 sí leen de `data/` local.

**Consecuencias.** El pipeline no corre de extremo a extremo desde este repo; los outputs commiteados en `data/processed/` no son regenerables por terceros; y los insumos legacy quedaron desactualizados respecto de `dataverse-final`.

**Solución (ejecutada 2026-07-07 en su mitad de infraestructura).** Snapshot copiado a `data/raw/dataverse-final/comision-{1..7}/` (CPT `paper-draft` @ `5852519`, con README de procedencia y test de aceptación `code/0a-verify-dataverse-snapshot.py`); módulos centrales `code/paths.py` / `code/paths.R` creados. **Queda**: recablear los scripts 00--08 para que deriven todas sus rutas de esos módulos (se hará al reescribirlos en la actualización de modelos).

## P4 — Normalización de nombres fragmentada (bug de las 155 filas) **[RESUELTO 2026-07-07 — lib_names en todos los scripts; integrated_dataset con exactamente 154 filas por construcción]**

**Situación.** El mapa `NAME_CORRECTIONS` (5 correcciones: Muñoz/Sepúlveda×2/Vargas-typo/Chinga) vive solo en `code/00-build_dynamic_networks.py:35-41`. El pipeline de éxito (04→05→06) corre sin esa normalización, así que `"Sepúlveda, Barbara"` (con tilde, desde los archivos de comisión) no cruza con `"Sepulveda, Barbara"` (canónico) y `integrated_dataset.csv` termina con **155 filas** en vez de 154: la misma persona partida en dos, con métricas de red en una fila y score de éxito en otra.

**Consecuencias.** Una observación duplicada/incompleta en M3; `author_success_scores.csv` registra 149 "autores" con al menos un fantasma.

**Solución (módulos creados 2026-07-07).** `code/lib_names.py` y `code/lib_names.R`: correcciones explícitas (incl. nombres invertidos tipo "Dayyana, Gonzalez"), match normalizado sin tildes, resolución de apellido único ("Roa" → "Roa, Giovanna"), y clasificación de **no-personas** (iniciativas populares registradas como autor). Verificado sobre el snapshot completo: 70.043 menciones, 100% de las personas resuelven a los 154 canónicos; el QA 0a falla si aparece un string nuevo no resuelto. **Queda**: que los scripts del pipeline usen este módulo al reescribirse (elimina el bug Sepúlveda/155 filas por construcción).

## P5 — Perfiles BCN incompletos **[RESUELTO 2026-07-07]**

**Situación original.** `conventional-profiles.json` tenía **147/154** perfiles. Los 7 ausentes (Botto, Castillo M.T., González D., Reyes M.R., Rivera M.M., Tepper, Zúñiga L.A.) recibían valores por defecto (edad 45, flags 0, afiliación "Desconocida") en los scripts.

**Diagnóstico (resuelto).** `match_names()` en `CPT/conventionals-bcn-webscrapping/fetch_convencionales.py` exigía que el campo *nombre de pila completo* fuera substring contiguo del directorio BCN. La BCN registra un solo nombre de pila y a veces **el segundo** ("Ramona" Reyes, "Angélica" Tepper, "Arturo" Zúñiga), y en un caso con ortografía distinta ("Dayyana" González). **Fix aplicado 2026-07-06:** matching en tres niveles (campo completo → cualquier token exacto del nombre de pila → fuzzy `difflib` $\geq 0.85$), verificado: 154/154 matcheados, **0 cambios de URL** en los 147 previos, re-scraping completo ejecutado. `conventional-profiles.json` tiene ahora **154 perfiles**; los 7 recuperados traen género, distrito y edad correctos.

**Pendiente (auditoría manual de covariables).**

1. **Afiliación "Desconocida" (33/154).** Incluye políticos de militancia pública y conocida (Chahin, Monckeberg, Larraín, Celis, Mayol...): es un fallo de extracción de la ficha BCN (la propiedad semántica `bcnbio:ofNamedPoliticalParty` no siempre está), no independencia real. Dado que `nodematch(afiliación)` es un término central de M1, **debe completarse a mano**.
2. **Falsos "Independiente".** La heurística de respaldo de `process_profiles.py` (busca "Independiente" en la trayectoria textual) produce falsos positivos: p. ej. asignó "Independiente" a Arturo Zúñiga (militante UDI). La auditoría debe validar también los "Independiente" de figuras con militancia conocida.
3. **Faltantes enmascarados.** `es_mujer`, `es_abogado`, `grado_academico_nivel` y `experiencia_previa_institucional` no admiten NA: el default 0 significa "no detectado por la heurística de texto", no "negativo confirmado" (p. ej. `es_mujer` se infiere de terminación en "-a" o palabras como "casada"). Riesgo directo sobre H1b (abogados, experiencia). Auditar al menos esos dos campos.
4. **Distrito "Desconocido" (9/154).** Aguilera, Bacián, Chinga, Galleguillos, Godoy, González L., Jiménez, Tirado, Vargas M. — todos **escaños reservados de pueblos originarios no mapuche**: no es dato faltante; recodificar como "Escaño reservado".
5. **Edad (1/154).** Renato Garín sin fecha de nacimiento en BCN; completar a mano.

**Auditoría dual-fuente ejecutada (2026-07-06).** `code/0b-audit-profiles-dual-source.py` consulta `gemini-3.5-flash` (API Gemini con *search grounding*): para cada convencional extrae las características dos veces, de forma independiente — desde el texto BCN ya scrapeado y desde su artículo de Wikipedia en español — referidas al período de la Convención. Outputs en `data/raw/profile-audit/`: tabla de símbolos 154×8 ($=$ coinciden, $\neq$ discrepan, B/W una sola fuente, $\varnothing$ ninguna), valores por fuente, y `discrepancias_pipeline.csv`. Resultados de la corrida v1 (lotes de 5): 154/154 auditados, ~124 con artículo de Wikipedia; **135 discrepancias** con el pipeline: afiliación 62 (resuelve las 33 "Desconocida" — Monckeberg→RN, Larraín→Evópoli, etc. — y detecta falsos "Independiente": Zúñiga→UDI), grado académico 29, experiencia previa 24, distrito 17 (incluye los 9 escaños reservados), género 1, edad 1 (Garín: 35), abogado 1.

**Cierre (2026-07-07).** Tras la validación manual del usuario sobre las primeras ~36 discrepancias (registrada en `manual_validations.json`), se ejecutó la **corrida v2** (lotes de 6, grado académico en escala 0--3; carpeta `profiles-batches/`) y se automatizó la **imputación** con la regla del usuario: *BCN es ground truth; si BCN no informa, Wikipedia; si ninguna, valor base; las validaciones manuales siempre prevalecen*. Chequeo de consistencia v1 vs. v2 (`runs_consistency.csv`): 100% de acuerdo en género, fecha, distrito y experiencia; 99.6% abogado; 97.5% lista; 96.0% grado (mapeando escalas); 93.5% afiliación — los desacuerdos residuales quedan listados para inspección. Las 6 validaciones de grado hechas bajo la escala antigua (1 = magíster) se recodificaron a 2 en la escala nueva (ambas corridas corroboran magíster); los distritos de escaños reservados se estandarizaron a "Escaño reservado: \<pueblo\>". Resultado vigente en `data/raw/conventional-profiles.json`: **154 perfiles** (incluye a Botto y los otros 6 recuperados), **0 afiliaciones "Desconocida"**, 0 edades faltantes, grado 0--3 = \{0: 18, 1: 81, 2: 41, 3: 14\}. Validaciones manuales en conflicto con la regla automática que se preservaron: Chahin→DC (BCN decía "Independiente") y Arrau→Republicano (la v2-BCN leyó UDI). Nuevas correcciones futuras se agregan a `manual_validations.json` y se re-corre `--impute`.

## P6 — Selección sobre la variable dependiente en M3 (ART-FALLIDO) **[RESUELTO 2026-07-07 — DV principal $y'$ con fracasos = 0 implementada; DV condicional como robustez]**

**Situación.** La DV vigente promedia la similitud solo sobre los artículos **mapeados** al borrador final:
$$y_i = \frac{1}{|M_i|}\sum_{a \in M_i} \text{sim}(a), \qquad M_i = \{a \in A_i : \text{final\_status} \in \{\text{Idéntico}, \text{Similar}\}\}$$
Los artículos fallidos no entran ni al numerador ni al denominador. Consecuencias: (i) $y_i$ mide retención *condicional a sobrevivir* — un delegado con 1/10 iniciativas sobrevivientes idénticas puntúa igual que uno con 10/10; (ii) los delegados cuyos artículos fallaron todos salen de la muestra (parte de la caída a $N=141$): selección clásica sobre la DV.

**Solución (decisión confirmada 2026-07-06).** Con `final_status = ART-FALLIDO` disponible en las 7 comisiones, la DV principal pasa a incluir los fallidos con similitud 0:
$$y_i' = \frac{1}{|A_i|}\sum_{a \in A_i} \text{sim}(a) = \underbrace{\frac{|M_i|}{|A_i|}}_{\text{tasa de supervivencia } \hat{s}_i} \times \underbrace{\overline{\text{sim}}_{M_i}}_{\text{retención condicional } \bar{r}_i}$$
es decir, retención esperada por iniciativa presentada. La DV antigua queda como robustez; si la masa de ceros resulta grande, se agrega un modelo en dos partes ($\hat s_i$ y $\bar r_i$ por separado) como chequeo.

## P7 — Mapeo génesis→final recalculado desde insumos legacy **[RESUELTO 2026-07-07 — script 04 v2 lee coincidencias con unión posicional al BORRADOR]**

**Situación.** `04-build-article-mapping.py` reconstruye el mapeo leyendo `draft-after-indications-manual/` (legacy, solo C1/C3/C5) y filtrando `final_status` con matching de strings; produce 236 artículos mapeados.

**Solución (decisión 2026-07-06).** `CPT/coincidencias_comisiones.csv` — curada a mano en el PR #4, en UTF-8, con los **498** artículos del borrador final, su comisión de origen, estatus (`identical`/`similar`), y fuentes múltiples (primaria/secundaria/terciaria) — pasa a ser la **fuente de verdad** del mapeo. El script 04 se reduce a un lector/validador de esa tabla; los textos finales se toman de los `C{k}_BORRADOR_final.json`.

## P8 — Heterogeneidades residuales en `dataverse-final` **[PARCIAL — la mayoría resuelta aguas arriba 2026-07-07]**

Estado tras la revisión del usuario en CPT (`paper-draft` @ `5852519`) y el QA del snapshot:

1. **[RESUELTO aguas arriba]** Títulos de capítulo de C1 eliminados (TRACK_full 230 → 202; typo `"tite"` desapareció con ellos); `article_uid` y `final_status` al **100%** tras la armonización de ids (`f8b9ddc`); `step` normalizado a `"Indicacion"` en todos los niveles (3.205 reemplazos; C2 conserva sus etiquetas propias); nombres de archivo GENESIS unificados a `C{k}_GENESIS_master.json`; semántica de `timestamp` documentada en el codebook (MM-DD, año implícito 2022, sufijo `-{bloque}` = n-ésimo informe del día).
2. **[ABIERTO — CPT]** Esquema GENESIS aún heterogéneo (C1/C2/C3 sin `article_uid`; C4 sin `article`/`sources`; `authors` intermitente); asignado en CPT con la instrucción de heredar los uids de TRACK/crosswalk. El loader de este repo lo absorbe mientras tanto.
3. **[ABIERTO — loader]** Indicaciones sueltas (210): se mantiene la decisión — alimentan las ondas de M2 (205/210 con `authors`+`timestamp`), no entran a M1 ni a M3. Falta la aserción de no-duplicación contra `history[]` en el loader.
4. **[RESUELTO aguas arriba — ronda 2]** Timestamps sin fecha: los 66 top-level + 5 anidados quedaron documentados como *undated* en el codebook (`49e394e`); no se imputan (recuperarlos exigiría volver a los PDF de origen). En M2, los registros sin fecha se reportan y se excluyen de las ondas.

## P9 — $W$ y centralidades de M3 medidas sobre la red posterior al resultado **[RESUELTO 2026-07-07 — $W$ y centralidades sobre la red génesis-iniciativa]**

**Situación.** La $W$ del SDM y las covariables `degree`/`betweenness` se calculan sobre la red pooled acumulada final, que incluye lazos formados durante todo el proceso — en parte *después* de los eventos que determinaron la suerte de los artículos. Simultaneidad estructura $\leftrightarrow$ resultado.

**Solución (decisión 2026-07-06).** Medir $W$ y las centralidades sobre la **red génesis** (coherente con P2): la estructura precede al resultado. La red acumulada queda como robustez. Extensión futura (M4, no compromete el paper actual): modelo de supervivencia a nivel artículo-onda, con la posición de red de los autores en la red vigente en cada onda — los datos lo permiten (timestamps de indicaciones en las 7 comisiones, `voting_result`, `final_status`; el momento de muerte de un fallido se aproxima con su última aparición en el historial).

## P10 — dynIRT sin errores estándar **[ABIERTO — prioridad baja]**

No existe `emIRT_bootstrap_output.rds`; `theta_se` queda NA en el pipeline (el script 02 lo tolera). Decidir si (i) bootstrap paramétrico del dynIRT, o (ii) reportar sensibilidad de M2 a la incertidumbre de $\theta$ de otro modo. No bloquea la actualización.

## P11 — ERGMs por comisión sin convergencia plena **[RESUELTO 2026-07-07 — los 7 ERGMs por comisión convergen sobre las redes génesis-iniciativa (§4.3.5)]**

En la robustez vigente, los Valued ERGM de C3 y C5 no convergieron plenamente (muestras chicas a nivel de comisión). Con 7 comisiones el problema puede repetirse en las nuevas; plan: reintentar con los datos nuevos y, si persiste, reportar solo el pooled + los que converjan, documentándolo.

## P12 — Sin exportación de figuras ni tablas **[RESUELTO 2026-07-08 — tablas en results/tables/ y figuras F1--F3 + piloto en results/figures/ (script 09/10)]**

`results/figures/` y `results/tables/` están vacíos; las tablas del extended abstract están escritas a mano en el `.tex`. Riesgo de divergencia silenciosa entre resultados y paper (ya ocurrió: el abstract dice "max weight > 300" cuando el máximo real es 480; ver P13.3). Plan: cada script de modelo exporta sus tablas (CSV + LaTeX) y figuras a `results/`, y el `.tex` las incluye en vez de duplicarlas.

## P13 — Errores y desactualizaciones en los documentos del paper **[ABIERTO]**

1. **Rótulos temáticos de comisiones equivocados.** `docs/extended-abstract.tex` (§Data) describe "C5: Fundamental Rights; C6: Environment". Según la numeración oficial (verificada; §3.1), C5 es **Medio Ambiente** y C6 es **Sistemas de Justicia** (Derechos Fundamentales es C4, que no estaba en la muestra). La numeración de los archivos es consistente con la oficial; son los rótulos temáticos del texto (y del README) los que están corridos.
2. **"155 delegates" vs. 154** en pasajes del abstract y propuesta (154 es el n tras normalización de nombres; la Convención tuvo 155 escaños — precisar la distinción donde corresponda).
3. **Cifras a regenerar** tras la actualización: todas las tablas, el "max weight > 300" (real: 480), N de panel (2.926), N del SDM (141), artículos mapeados (236), y el párrafo "Ongoing work" sobre comisiones pendientes (desaparece).
4. **Cambio metodológico no documentado**: la propuesta original (`docs/research-proposal.tex`) prometía TERGM (M1) y SAOM/RSiena (M2); la implementación usa Valued ERGM y panel FE. Documentar la justificación (tamaño/estructura de los datos; identificación de selección vs. influencia vía FE + falsificación) en el README y en la próxima versión del paper.

## P14 — Copias de insumos desincronizadas entre repos **[RESUELTO 2026-07-07]**

Regla instaurada: **este repo consume snapshots versionados de CPT; ningún script lee de CPT en runtime.** El snapshot `data/raw/dataverse-final/` (CPT `paper-draft` @ `5852519`) incluye README de procedencia, codebook y `QA-report.txt`; se refresca copiando desde CPT y corriendo `code/0a-verify-dataverse-snapshot.py`. Los perfiles curados viven en `data/raw/conventional-profiles.json` (P5). Las carpetas `data/raw/commissions/` y `data/raw/network-visualization/` quedan como legacy de solo-lectura para comparación con los resultados pre-actualización.

## P15 — C2 sin autores: no podía entrar a la red de co-patrocinio **[RESUELTO aguas arriba 2026-07-07, ronda 2]**

**Situación original (QA 2026-07-07 a.m.).** C2 no tenía `authors` en ninguna parte utilizable (0/312 GENESIS, 0/182 artículos TRACK_full); C4 tenía un hueco parcial (135/167, sin `sources` para el join).

**Resolución (CPT `paper-draft` 55fae5d → 6fac4c4).** `authors` poblado con la misma regla con que se construyó el resto del dataset — autores = firmantes de la(s) iniciativa(s) en `sources` (C2, 401 registros; regla validada 1.252/1.253 contra C3/C5/C6/C7) y vía `icc_id` (C4, 39 registros; validada 163/163). Verificado en el snapshot refrescado: C2 149/182 y C4 150/167 artículos con autores; **1.676 artículos con $\geq 2$ autores-persona** para la red génesis, con las 7 comisiones aportando; las 75.616 menciones siguen resolviendo al 100% (los nuevos autores vienen en formato canónico). Residuo irrecuperable documentado en el codebook: 134/2.019 (65 populares/indígenas — patrocinio institucional, no de convencionales —, 45 de ICC ausentes del pool de PDF, 24 sin referencia de fuente).

**Nota de alcance para M2.** El `history[]` de C2 sigue delgado (33/54 entradas utilizables) y C2 no tiene indicaciones sueltas: C2 entra plenamente a M1 (génesis) y M3 (éxito), pero sus ondas en M2 aportarán poca variación intra-comisión — esperable y no bloqueante.

