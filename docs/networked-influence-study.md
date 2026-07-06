---
title: "Networked Influence in a Tabula Rasa Legislature — Reporte del estudio v1"
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
$$P(W = w) \propto \exp\{\theta^\top g(w)\}, \qquad g = (\text{sum},\ \text{nodematch}_{\text{afiliación, abogado, experiencia, género}},\ \text{absdiff}_{\text{edad}},\ \text{nodecov}_{\text{edad}})$$
Los términos `nodematch` capturan homofilia (H1a) y su reverso (H1b).

**M2 — Dinámica ideológica (panel FE).** Sobre ondas temporales por comisión:
$$\Delta\theta_{i,t} = \alpha_i + \beta_1 \theta_{i,t-1} + \beta_3\, \text{NetExp}_{i,t-1} + \varepsilon_{it}, \qquad \text{NetExp}_{i,t-1} = \frac{\sum_{j \neq i} w_{ij,t-1}\,\theta_{j,t-1}}{\sum_{j \neq i} w_{ij,t-1}}$$
con $w_{ij,t-1}$ = co-firmas acumuladas hasta la onda $t-1$. Comparación OLS agrupado vs. efectos fijos (within) vs. aleatorios (Hausman), errores agrupados por delegado, y falsificación con *lead*.

**M3 — Éxito (Spatial Durbin Model).** Con $y_i$ = retención léxica media del delegado (similitud coseno TF-IDF génesis→final; robustez con Sentence-BERT) y $W$ = matriz de co-autoría row-normalizada:
$$y = \rho W y + X\beta + WX\gamma + \varepsilon$$
Se compara OLS/SAR/SEM/SDM (AIC, Moran's $I$) y se descomponen efectos directos/indirectos/totales.

# 3. Datos

## 3.1 Fuentes primarias (estado objetivo, post-actualización)

| Fuente | Ubicación | Contenido | N |
|:---|:---|:---|---:|
| Iniciativas génesis | `CPT/comision-{1..7}/dataverse-final/C{k}_GENESIS_master[_merged].json` | Iniciativas originales aprobadas en general, con `authors` | 1.892 |
| Trazabilidad completa | `.../C{k}_TRACK_full.json` | Todos los artículos (incl. fallidos) con `history[]` de indicaciones fechadas y `final_status` | 2.047 |
| Trazabilidad filtrada | `.../C{k}_TRACK_articles.json` | Solo artículos presentes en el borrador final | 478 |
| Borrador final | `.../C{k}_BORRADOR_final.json` | Artículos del borrador del 14-05-2022, por comisión | 498 |
| Mapeo maestro | `CPT/coincidencias_comisiones.csv` (UTF-8) | 498 artículos finales → fuente(s) por comisión, estatus identical/similar, fuentes secundarias/terciarias | 498 |
| Nombres canónicos | `data/raw/convention_members.json` | Referencia oficial "Apellido, Nombre" | 154 |
| Perfiles BCN | `CPT/conventionals-bcn-webscrapping/conventional-profiles.json` | Covariables: género, afiliación, distrito, abogado, edad, grado, experiencia | 154 |
| Puntos ideales | `data/raw/emirt/` | Salidas dynIRT: $\Theta$ 154×91, metadata, posiciones resumen | 154×91 |

Desglose por comisión de los `dataverse-final` (conteo de registros):

| Comisión | Nombre oficial (corto) | GENESIS | BORRADOR | TRACK_articles | TRACK_full |
|:-:|:---|---:|---:|---:|---:|
| C1 | Sistema Político | 96 | 100 | 131 | 230 |
| C2 | Principios Constitucionales | 312 | 41 | 24 | 182 |
| C3 | Forma de Estado | 222 | 96 | 72 | 234 |
| C4 | Derechos Fundamentales | 167 | 58 | 58 | 175 |
| C5 | Medio Ambiente | 464 | 43 | 36 | 484 |
| C6 | Sistemas de Justicia | 440 | 119 | 117 | 491 |
| C7 | Sistemas de Conocimientos | 191 | 41 | 40 | 251 |
| **Total** | | **1.892** | **498** | **478** | **2.047** |

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

Dos hechos estructurales importan para el pipeline (detalle en P8):

- El esquema del GENESIS tiene **dos variantes** (C1/C2/C3/C6 vs. C4/C5/C7).
- Todos los `TRACK_full` salvo C2 contienen **"indicaciones sueltas"**: registros de indicación al nivel superior, no anidados en `history[]` (C1: 103; C3: 12; C4: 8; C5: 20; C6: 44; C7: 23). C1 contiene además 28 registros de títulos de capítulo. Regla mecánica de clasificación: `titleuid` presente → título; `action` presente al nivel superior → indicación suelta; en otro caso → artículo.

## 3.3 Datos usados por la versión vigente (pre-actualización)

La versión reflejada en el extended abstract (abril 2026) usa insumos **anteriores** al PR #4 de limpieza, copiados en `data/raw/commissions/` (5 archivos: C1, C3, C5, C6, C7, enriquecidos a mano) y redes pre-construidas en `data/raw/network-visualization/`. Cifras vigentes: red pooled de **7.162 aristas, 154 nodos, peso total 202.765, peso máximo 480**; ondas dinámicas solo para C1 (5), C3 (8) y C5 (6); mapeo génesis→final de **236 artículos** (125 idénticos + 111 similares; C1: 131, C3: 71, C5: 34); panel de **2.926** observaciones delegado-período; SDM sobre **141** casos completos.

# 4. Pipeline y resultados vigentes

## 4.1 Scripts

| # | Script | Rol | Outputs |
|:-:|:---|:---|:---|
| 00 | `code/00-build_dynamic_networks.py` | Redes de co-autoría (pooled + ondas C1/C3/C5) | `pooled_cumulative_network.csv`, `{C}_dynamic_networks.json` |
| 01 | `code/01-model-valued-ergm.R` | **M1**: Valued ERGM Poisson + centralidades | `ergm_pooled_results.rds`, `network_metrics.csv` |
| 02 | `code/02-extract-emirt-temporal.R` | Alineación temporal $\Theta \leftrightarrow$ ondas de comisión | `emirt_*.csv` |
| 03 | `code/03-model-network-influence.R` | **M2**: exposición de red + panel FE/RE/OLS | `network_exposure_panel.csv`, `panel_regression_results.rds` |
| 04 | `code/04-build-article-mapping.py` | Mapeo artículos comisión → borrador final | `article_mapping_unified.{json,csv}` |
| 05 | `code/05-nlp-text-similarity.py` | Retención léxica TF-IDF + SBERT | `article_similarity_scores.csv`, `author_success_scores.csv` |
| 06 | `code/06-build-integrated-dataset.py` | Merge éxito+red+ideología+perfiles | `integrated_dataset.{csv,json}` |
| 07 | `code/07-model-spatial-durbin.R` | **M3**: Moran, OLS/SAR/SEM/SDM, impactos | `sdm_results.rds`, `sdm_impacts.csv` |
| 08 | `code/08-robustness-checks.R` | Robustez de M1--M3 | `robustness_results.rds` |

## 4.2 Resultados vigentes (extended abstract, abril 2026)

- **M1.** `nodematch(afiliación)` $= +0.41$; `nodematch(abogado)` $= -0.09$; `nodematch(experiencia)` $= -0.12$; `nodematch(mujer)` $= +0.08$; `nodecov(edad)` $= -0.024$. Lectura: homofilia positiva de afiliación y género; **homofilia negativa** de abogados y experimentados (gatekeeping estratégico, H1b confirmada).
- **M2.** OLS agrupado: $\hat\beta_3 = +0.033$ ($p<0.001$); efectos fijos: $\hat\beta_3 = +0.0004$ ($p = 0.91$); Hausman $\chi^2 = 784.96$ ($p<0.001$) → FE. Falsificación con *lead* significativa ($p<0.001$) → **selección, no influencia** (H2 confirmada).
- **M3.** Moran's $I = 0.155$ ($p<10^{-6}$). AIC: OLS $-338.5$ < SEM $-354.8$ ($\lambda=0.81$) < SAR $-355.3$ ($\rho=0.89$) < **SDM $-383.4$ ($\hat\rho = 0.997$)**. En OLS: `theta_mean` $+0.012$ ($p=0.008$), `theta_sd` $-0.103$ ($p=0.015$), `ego_heterophily` $-0.063$ ($p=0.027$); centralidades n.s. Validación de medida: artículos "idénticos" puntúan 0.979 en TF-IDF. Robustez: SBERT preserva signos; con $W$ binaria $\rho$ cae a 0.374 manteniendo significancias clave (H3 confirmada).

# 5. Problemas identificados (registro detallado)

Esta sección es el inventario completo de defectos, deudas y decisiones pendientes detectados en la auditoría de julio 2026 (previa a la actualización). Es la lista de control contra la cual se ejecutará el plan de la §7.

## P1 — Cobertura incompleta de comisiones **[ABIERTO — motivo central de la actualización]**

**Situación.** La versión vigente usa 5 de las 7 comisiones en M1 (C1, C3, C5, C6, C7; C2 y C4 ausentes del repo) y solo **3** en M2 y M3 (C1, C3, C5), porque los archivos antiguos de C6/C7 no tienen `final_status` ni ondas temporales. El propio abstract lo declara como limitación ("Ongoing work").

**Consecuencias.** (i) La red pooled subestima lazos formados vía C2/C4 — con 312 y 167 iniciativas génesis, no son comisiones menores; C4 (Derechos Fundamentales) fue además la comisión políticamente más cargada. (ii) El mapeo de éxito cubre 236 de 498 artículos finales (47%): la DV de M3 ignora más de la mitad del borrador. (iii) El panel de M2 descarta la dinámica de 4 comisiones.

**Solución.** Migrar todo el pipeline a los `dataverse-final` de las 7 comisiones (PR #4 de CPT, mergeado): GENESIS para la red de co-patrocinio, TRACK_full para ondas e indicaciones, `coincidencias_comisiones.csv` + BORRADOR_final para el éxito.

## P2 — Definición de lazo inconsistente entre comisiones **[ABIERTO — decisión tomada]**

**Situación.** En `code/00-build_dynamic_networks.py`, la red pooled suma la **última onda acumulada** de C1/C3/C5 (= génesis **+ todas las indicaciones**, líneas 175--178) pero solo el génesis de C6/C7 (líneas 194--204). La red que estima M1 — y que M3 usa como $W$ y para las centralidades — mezcla, por tanto, dos definiciones de lazo según la comisión.

**Consecuencias.** Los pesos no son comparables entre comisiones (C5 aporta 4.968 aristas acumuladas vs. C7 solo génesis); los coeficientes del ERGM confunden formación primaria con colaboración posterior.

**Solución (decisión 2026-07-06).** **M1 usará exclusivamente la red génesis** de las 7 comisiones: modela la *formación primaria* de lazos entre desconocidos, y el co-patrocinio de iniciativas es el acto fundacional (además, con umbral reglamentario 8--16 firmas, es un compromiso costoso y acotado). Las indicaciones entran solo a las **ondas acumuladas** de M2 (colaboración posterior, condicionada por la red ya formada). Para M3, ver P9. Robustez: red génesis+indicaciones como variante.

## P3 — Reproducibilidad rota: rutas hardcodeadas a carpetas legacy de otro repositorio **[ABIERTO]**

**Situación.** Los scripts 02--05 leen desde rutas absolutas del repo CPT, y además desde carpetas **legacy previas a la limpieza**: `02-extract-emirt-temporal.R:10` (`emIRT-analysis/`, escribe a `playground/research-proposal-implementation/data`), `03-model-network-influence.R:30-32` (`conventionals-bcn-webscrapping/`, `network-visualization/`), `04-build-article-mapping.py:15-22` (`comision-N/draft-after-indications-manual/`, `proposals/draft_final_text.json`), `05-nlp-text-similarity.py:23-24`. El archivo `draft_final_text.json` ni siquiera existe en este repositorio. Los scripts 00, 01, 06, 07, 08 sí leen de `data/` local.

**Consecuencias.** El pipeline no corre de extremo a extremo desde este repo; los outputs commiteados en `data/processed/` no son regenerables por terceros; y los insumos legacy quedaron desactualizados respecto de `dataverse-final`.

**Solución.** Copiar los `dataverse-final` a `data/raw/dataverse-final/comision-{1..7}/` + un módulo único de configuración de rutas (`code/config`), del que todos los scripts derivan sus paths. Ningún script vuelve a referenciar CPT.

## P4 — Normalización de nombres fragmentada (bug de las 155 filas) **[ABIERTO]**

**Situación.** El mapa `NAME_CORRECTIONS` (5 correcciones: Muñoz/Sepúlveda×2/Vargas-typo/Chinga) vive solo en `code/00-build_dynamic_networks.py:35-41`. El pipeline de éxito (04→05→06) corre sin esa normalización, así que `"Sepúlveda, Barbara"` (con tilde, desde los archivos de comisión) no cruza con `"Sepulveda, Barbara"` (canónico) y `integrated_dataset.csv` termina con **155 filas** en vez de 154: la misma persona partida en dos, con métricas de red en una fila y score de éxito en otra.

**Consecuencias.** Una observación duplicada/incompleta en M3; `author_success_scores.csv` registra 149 "autores" con al menos un fantasma.

**Solución.** Módulo compartido de normalización (Python + R) que (i) aplique correcciones, (ii) **valide todo autor contra los 154 canónicos** de `data/raw/convention_members.json`, y (iii) emita reporte de no-matcheados en cada corrida. Con los datos nuevos (autores ya estandarizados por el PR #4) el mapa de correcciones debería encogerse, pero la validación queda como invariante del pipeline.

## P5 — Perfiles BCN incompletos **[PARCIAL — scraper resuelto 2026-07-06; auditoría pendiente]**

**Situación original.** `conventional-profiles.json` tenía **147/154** perfiles. Los 7 ausentes (Botto, Castillo M.T., González D., Reyes M.R., Rivera M.M., Tepper, Zúñiga L.A.) recibían valores por defecto (edad 45, flags 0, afiliación "Desconocida") en los scripts.

**Diagnóstico (resuelto).** `match_names()` en `CPT/conventionals-bcn-webscrapping/fetch_convencionales.py` exigía que el campo *nombre de pila completo* fuera substring contiguo del directorio BCN. La BCN registra un solo nombre de pila y a veces **el segundo** ("Ramona" Reyes, "Angélica" Tepper, "Arturo" Zúñiga), y en un caso con ortografía distinta ("Dayyana" González). **Fix aplicado 2026-07-06:** matching en tres niveles (campo completo → cualquier token exacto del nombre de pila → fuzzy `difflib` $\geq 0.85$), verificado: 154/154 matcheados, **0 cambios de URL** en los 147 previos, re-scraping completo ejecutado. `conventional-profiles.json` tiene ahora **154 perfiles**; los 7 recuperados traen género, distrito y edad correctos.

**Pendiente (auditoría manual de covariables).**

1. **Afiliación "Desconocida" (33/154).** Incluye políticos de militancia pública y conocida (Chahin, Monckeberg, Larraín, Celis, Mayol...): es un fallo de extracción de la ficha BCN (la propiedad semántica `bcnbio:ofNamedPoliticalParty` no siempre está), no independencia real. Dado que `nodematch(afiliación)` es un término central de M1, **debe completarse a mano**.
2. **Falsos "Independiente".** La heurística de respaldo de `process_profiles.py` (busca "Independiente" en la trayectoria textual) produce falsos positivos: p. ej. asignó "Independiente" a Arturo Zúñiga (militante UDI). La auditoría debe validar también los "Independiente" de figuras con militancia conocida.
3. **Faltantes enmascarados.** `es_mujer`, `es_abogado`, `grado_academico_nivel` y `experiencia_previa_institucional` no admiten NA: el default 0 significa "no detectado por la heurística de texto", no "negativo confirmado" (p. ej. `es_mujer` se infiere de terminación en "-a" o palabras como "casada"). Riesgo directo sobre H1b (abogados, experiencia). Auditar al menos esos dos campos.
4. **Distrito "Desconocido" (9/154).** Aguilera, Bacián, Chinga, Galleguillos, Godoy, González L., Jiménez, Tirado, Vargas M. — todos **escaños reservados de pueblos originarios no mapuche**: no es dato faltante; recodificar como "Escaño reservado".
5. **Edad (1/154).** Renato Garín sin fecha de nacimiento en BCN; completar a mano.

## P6 — Selección sobre la variable dependiente en M3 (ART-FALLIDO) **[ABIERTO — decisión tomada]**

**Situación.** La DV vigente promedia la similitud solo sobre los artículos **mapeados** al borrador final:
$$y_i = \frac{1}{|M_i|}\sum_{a \in M_i} \text{sim}(a), \qquad M_i = \{a \in A_i : \text{final\_status} \in \{\text{Idéntico}, \text{Similar}\}\}$$
Los artículos fallidos no entran ni al numerador ni al denominador. Consecuencias: (i) $y_i$ mide retención *condicional a sobrevivir* — un delegado con 1/10 iniciativas sobrevivientes idénticas puntúa igual que uno con 10/10; (ii) los delegados cuyos artículos fallaron todos salen de la muestra (parte de la caída a $N=141$): selección clásica sobre la DV.

**Solución (decisión confirmada 2026-07-06).** Con `final_status = ART-FALLIDO` disponible en las 7 comisiones, la DV principal pasa a incluir los fallidos con similitud 0:
$$y_i' = \frac{1}{|A_i|}\sum_{a \in A_i} \text{sim}(a) = \underbrace{\frac{|M_i|}{|A_i|}}_{\text{tasa de supervivencia } \hat{s}_i} \times \underbrace{\overline{\text{sim}}_{M_i}}_{\text{retención condicional } \bar{r}_i}$$
es decir, retención esperada por iniciativa presentada. La DV antigua queda como robustez; si la masa de ceros resulta grande, se agrega un modelo en dos partes ($\hat s_i$ y $\bar r_i$ por separado) como chequeo.

## P7 — Mapeo génesis→final recalculado desde insumos legacy **[ABIERTO — decisión tomada]**

**Situación.** `04-build-article-mapping.py` reconstruye el mapeo leyendo `draft-after-indications-manual/` (legacy, solo C1/C3/C5) y filtrando `final_status` con matching de strings; produce 236 artículos mapeados.

**Solución (decisión 2026-07-06).** `CPT/coincidencias_comisiones.csv` — curada a mano en el PR #4, en UTF-8, con los **498** artículos del borrador final, su comisión de origen, estatus (`identical`/`similar`), y fuentes múltiples (primaria/secundaria/terciaria) — pasa a ser la **fuente de verdad** del mapeo. El script 04 se reduce a un lector/validador de esa tabla; los textos finales se toman de los `C{k}_BORRADOR_final.json`.

## P8 — Heterogeneidades residuales en `dataverse-final` **[ABIERTO]**

Defectos de forma de los datos limpios que el loader debe absorber (ninguno requiere re-limpiar aguas arriba):

1. **GENESIS con dos esquemas y dos nombres de archivo.** C1/C2/C3/C6: campos `article/text/sources[/authors]`, archivo `*_master.json` (C2, C3, C6) o `*_master_merged.json` (C1). C4/C5/C7: campos `voting_result/authors/text/icc_id/article_uid`, archivo `*_master_merged.json`. El loader unifica a un esquema interno único.
2. **Indicaciones sueltas al nivel superior** en todos los TRACK_full salvo C2 (conteos en §3.2). Regla de clasificación: `titleuid` → título (descartar; solo C1, 28 registros); `action` al nivel superior → indicación suelta; resto → artículo. Las sueltas **no tienen `sources`** y su uid codifica el artículo destino de forma poco fiable (capítulos placeholder `CH*`): no se intentará anclarlas a artículos. Sí aportan lazos a las ondas de M2 (tienen `timestamp` y `authors`). No entran a la red génesis de M1 ni al éxito de M3 (que usa `coincidencias`).
3. **Riesgo de doble conteo.** Verificar programáticamente (hash de `authors`+`timestamp`+`content`) que ninguna indicación suelta duplique una entrada de `history[]`; en las inspecciones manuales no hubo solape, pero debe quedar como aserción del loader.
4. **Typos y variantes menores.** Un registro con clave `"tite"` en C1; `step` = "Indicacion" (sin tilde, C1) vs. "Indicación" (C3/C6). No usar `step` como discriminador.
5. **Timestamps irregulares.** Formato `MM-DD` sin año, con variantes como `"04-01-2"` (sufijo de segunda sesión). Regla de parseo: prefijo `MM-DD` (matching por `startswith` contra los bins, como ya hace el script 00); año implícito 2022 para las ondas de indicaciones (la Convención sesionó jul-2021 → jul-2022 y todos los informes de comisión son de 2022). Definir los bins de ondas por comisión a partir de los timestamps observados en TRACK_full (hoy solo existen para C1/C3/C5, hardcodeados en `00-build_dynamic_networks.py:44-48`).

## P9 — $W$ y centralidades de M3 medidas sobre la red posterior al resultado **[ABIERTO — decisión tomada]**

**Situación.** La $W$ del SDM y las covariables `degree`/`betweenness` se calculan sobre la red pooled acumulada final, que incluye lazos formados durante todo el proceso — en parte *después* de los eventos que determinaron la suerte de los artículos. Simultaneidad estructura $\leftrightarrow$ resultado.

**Solución (decisión 2026-07-06).** Medir $W$ y las centralidades sobre la **red génesis** (coherente con P2): la estructura precede al resultado. La red acumulada queda como robustez. Extensión futura (M4, no compromete el paper actual): modelo de supervivencia a nivel artículo-onda, con la posición de red de los autores en la red vigente en cada onda — los datos lo permiten (timestamps de indicaciones en las 7 comisiones, `voting_result`, `final_status`; el momento de muerte de un fallido se aproxima con su última aparición en el historial).

## P10 — dynIRT sin errores estándar **[ABIERTO — prioridad baja]**

No existe `emIRT_bootstrap_output.rds`; `theta_se` queda NA en el pipeline (el script 02 lo tolera). Decidir si (i) bootstrap paramétrico del dynIRT, o (ii) reportar sensibilidad de M2 a la incertidumbre de $\theta$ de otro modo. No bloquea la actualización.

## P11 — ERGMs por comisión sin convergencia plena **[ABIERTO]**

En la robustez vigente, los Valued ERGM de C3 y C5 no convergieron plenamente (muestras chicas a nivel de comisión). Con 7 comisiones el problema puede repetirse en las nuevas; plan: reintentar con los datos nuevos y, si persiste, reportar solo el pooled + los que converjan, documentándolo.

## P12 — Sin exportación de figuras ni tablas **[ABIERTO]**

`results/figures/` y `results/tables/` están vacíos; las tablas del extended abstract están escritas a mano en el `.tex`. Riesgo de divergencia silenciosa entre resultados y paper (ya ocurrió: el abstract dice "max weight > 300" cuando el máximo real es 480; ver P13.3). Plan: cada script de modelo exporta sus tablas (CSV + LaTeX) y figuras a `results/`, y el `.tex` las incluye en vez de duplicarlas.

## P13 — Errores y desactualizaciones en los documentos del paper **[ABIERTO]**

1. **Rótulos temáticos de comisiones equivocados.** `docs/extended-abstract.tex` (§Data) describe "C5: Fundamental Rights; C6: Environment". Según la numeración oficial (verificada; §3.1), C5 es **Medio Ambiente** y C6 es **Sistemas de Justicia** (Derechos Fundamentales es C4, que no estaba en la muestra). La numeración de los archivos es consistente con la oficial; son los rótulos temáticos del texto (y del README) los que están corridos.
2. **"155 delegates" vs. 154** en pasajes del abstract y propuesta (154 es el n tras normalización de nombres; la Convención tuvo 155 escaños — precisar la distinción donde corresponda).
3. **Cifras a regenerar** tras la actualización: todas las tablas, el "max weight > 300" (real: 480), N de panel (2.926), N del SDM (141), artículos mapeados (236), y el párrafo "Ongoing work" sobre comisiones pendientes (desaparece).
4. **Cambio metodológico no documentado**: la propuesta original (`docs/research-proposal.tex`) prometía TERGM (M1) y SAOM/RSiena (M2); la implementación usa Valued ERGM y panel FE. Documentar la justificación (tamaño/estructura de los datos; identificación de selección vs. influencia vía FE + falsificación) en el README y en la próxima versión del paper.

## P14 — Copias de insumos desincronizadas entre repos **[ABIERTO]**

`data/raw/conventional-profiles.json` (este repo) es una copia de la salida del scraper en CPT, hoy desactualizada (147 vs. 154 tras el fix de P5); `data/raw/commissions/` y `data/raw/network-visualization/` son copias de insumos legacy. Al ejecutar la Fase 0 (§7), las copias se reemplazan por los `dataverse-final` + perfiles regenerados, y se define una regla única: **este repo consume snapshots versionados de CPT; ningún script lee de CPT en runtime** (cierra también P3).

# 6. Decisiones de diseño confirmadas

| Fecha | Decisión |
|:---|:---|
| 2026-07-06 | `coincidencias_comisiones.csv` (UTF-8) es la fuente de verdad del mapeo génesis→borrador final (P7). |
| 2026-07-06 | **M1 = red génesis pura**, 7 comisiones. Indicaciones solo en las ondas de M2. Génesis+indicaciones como robustez (P2). |
| 2026-07-06 | **M3**: DV principal con ART-FALLIDO = 0 ($y' = \hat{s}\cdot\bar{r}$); DV condicional antigua como robustez (P6). $W$ y centralidades sobre la red génesis (P9). |
| 2026-07-06 | **M2**: exposición acumulada desde T0 hasta $t-1$ como especificación principal; robustez con decaimiento exponencial y con ventana solo-última-onda; sensibilidad estandarizando $\Delta\theta$ por días entre ondas. |
| 2026-07-06 | Indicaciones sueltas: entran a las ondas de M2 (timestamp+authors), no a M1 ni a M3; títulos se descartan (P8). |
| 2026-07-06 | Numeración oficial de comisiones confirmada; rótulos temáticos del paper a corregir (P13.1). |
| 2026-07-06 | Scraper BCN corregido y re-ejecutado: 154/154 perfiles (P5). |

# 7. Plan de actualización

- **Fase 0 — Infraestructura** *(branch dedicado)*: copiar `dataverse-final` a `data/raw/dataverse-final/`; snapshot de perfiles 154; módulo de configuración de rutas; módulo compartido de normalización/validación de nombres. *(Cierra P3, P4, P14.)*
- **Fase 0b — Calidad de covariables**: planilla de auditoría de afiliación (33 "Desconocida" + "Independiente" sospechosos) pre-llenada para validación manual; recodificación "Escaño reservado"; edad de Garín; auditoría de `es_abogado`/`experiencia_previa`. *(Cierra P5.)*
- **Fase 1 — Loader unificado**: lector GENESIS (2 esquemas), lector TRACK_full (clasificación artículo/indicación-suelta/título, dedup, timestamps), tests de conteos contra §3.1. *(Cierra P8.)*
- **Fase 2 — Redes**: red génesis pooled 7 comisiones (M1); ondas acumuladas por comisión ×7 (M2); variantes de robustez. *(Cierra P1-red, P2.)*
- **Fase 3 — Modelos**: M1 ERGM (pooled + por comisión); M2 realineación emIRT + panel ampliado + robusteces de ventana; M3 con `coincidencias` + BORRADOR_final + DV nueva + $W$-génesis. *(Cierra P1, P6, P7, P9; revisita P11.)*
- **Fase 4 — Integración y paper**: `integrated_dataset` de exactamente 154 filas; suite de robustez; exportación automatizada a `results/`; actualización del extended abstract (cifras, rótulos, "ongoing work") y README (métodos). *(Cierra P12, P13.)*

# 8. Registro de cambios de este documento

- **v1 (2026-07-06).** Versión inicial: objetivos y marco teórico (§2), inventario de datos nuevos (§3), pipeline y resultados vigentes (§4), registro detallado de problemas P1--P14 (§5), decisiones confirmadas (§6) y plan por fases (§7). Incluye el fix del scraper BCN (P5) aplicado en esta fecha.
