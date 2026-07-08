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
  - \usepackage{pdflscape}
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

Las tablas con las rutas exactas de cada fuente y el desglose por comisión están en el **Anexo B** (orientación horizontal). Desde 2026-07-07 los datos viven como **snapshot versionado dentro de este repositorio** (`data/raw/dataverse-final/`, copiado de CPT `paper-draft` @ `5852519`; ver P14). El test de aceptación `code/0a-verify-dataverse-snapshot.py` valida cada refresco y escribe `QA-report.txt`.

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
| 11 | `code/11-m2-motivation-figures.py` | Motivación descriptiva de M2: trayectorias de $\theta$, $|\Delta\theta|$ por onda, medias por comisión | `commission_membership.csv`, figuras FB1/FB2 |

## 4.2 Insumos construidos (v2 — julio 2026)


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
## 4.3 Limitaciones de datos (transversales)

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

## 6.2 Motivación descriptiva

La pregunta del modelo ("las posiciones cambian y la red cambia: ¿influencia o selección?") se establece con tres piezas descriptivas.

**(a) La red cambia — ¿cuándo se forman los lazos post-génesis?**

![Multi-author indication events per report date, by commission.](../results/figures/indication_events_timeline.pdf){width=100%}

La formación de lazos post-génesis es **sostenida en C1, C3, C4 y C6** (C6 incluso crece hacia el final; C3 es bimodal), **de un solo golpe en C7** (42/54 eventos en el informe del 19-02) y **no observable en C2/C5**. Los eventos de exactamente 2 autores son minoría (2--3% en C1/C3; 26% en C4). *Nota C5*: sus indicaciones **no fueron unipersonales en la realidad** — los informes de C5 registran solo al primer firmante (resto como "y otros"), una **limitación de registro** que impide reconstruir sus lazos post-génesis (§6.5). Comisiones pertinentes para leer dinámica de red: **C1, C3, C4, C6** (C7 con nota).

**(b) Las posiciones cambian.**

![Ideal-point trajectories and per-wave shifts (FB1).](../results/figures/m2_positions_dynamics.pdf){width=96%}

Los bloques son estables en el agregado, pero los individuos se mueven en cada onda (mediana de $|\Delta\theta|$ entre 0.12 y 0.25 según la onda). Vista complementaria por **membresía de comisión** (extraída del campo `integracion_comisiones` de la BCN, 154/154 matcheados; snapshot en `data/raw/commission_membership.csv`):

![Ideal-point trajectories by commission membership (FB2).](../results/figures/theta_dynamics_by_commission.pdf){width=96%}

Todas las medias de comisión viven en la franja izquierda (la Convención fue de mayoría izquierda); C5 es la más a la izquierda y C2 la menos — contexto útil para comparar las correlaciones del panel (c) entre comisiones.

**(c) Selección vs. influencia en una imagen.**

![Cross-sectional exposure–position correlation vs. within-delegate effect.](../results/figures/m2_selection_vs_influence_preview.pdf){width=92%}

En cada comisión y cada onda, la posición ponderada de los co-firmantes sigue de cerca la propia ($r$ transversal 0.83--0.97, estable en el tiempo — la firma de la selección homofílica), mientras el efecto *within* de la exposición rezagada sobre $\Delta\theta$ es cero. La brecha entre ambas cantidades **es** el resultado de M2.

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
2. **Ondas planas en C5 (limitación de registro).** Las indicaciones de C5 **sí** fueron colectivas, pero sus informes registran únicamente al primer firmante (el resto queda como "y otros"): con un solo autor recuperable por evento, la red de C5 no puede cambiar después de T0 y su aporte a M2 proviene solo de la variación temporal de $\theta$.
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

![Amendment trajectories toward the final text — all commissions (eje x en fechas calendario; génesis nominal 10 días antes del primer informe; LOCF).](../results/figures/retention_dynamics_all_commissions.pdf){width=100%}

Similitud media al borrador, génesis $\to$ última onda: C1 $0.65 \to 0.82$; C2 $0.39 \to 0.88$; C3 $0.50 \to 0.92$; C4 $0.49 \to 0.94$; C5 $0.15 \to 0.95$; C6 $0.40 \to 0.98$; C7 $0.33 \to 0.89$. Tres lecturas: (i) el proceso de indicaciones es **convergente hacia el borrador en las 7 comisiones**; (ii) la heterogeneidad de partida es enorme — C5 nace lejísimos de su forma final (0.15) y se reescribe casi por completo vía indicaciones — cuyos informes registran solo al primer firmante, por eso su red no cambia aunque su texto sí (§6.5); C1 nace cerca (0.65) y se mueve poco; (iii) el estado post-última-indicación ($\approx 0.9$) es lo que la v1 medía como "retención" (0.979), confirmando la nota del ancla (§7.5). **Extensión natural**: el dataset artículo-onda (`retention_dynamics_locf.csv`) es la base del futuro M4 de supervivencia.

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

# 9. Registro de cambios

- **v2.3 (2026-07-08).** Limpieza del reporte (resultados antiguos, plan y problemas resueltos movidos a `docs/report-archive.txt`, fuera de versionado); tablas de fuentes al Anexo B en orientación horizontal. Iteración de figuras: F1 con anchos iguales; 2×2 con columnas separadas (sin línea); **timeline de eventos por fecha de informe** (reemplaza la tabla de pertinencia); retention dynamics con **eje de fechas calendario** y génesis nominal; figuras (b) de M2 ejecutadas — trayectorias de $\theta$ con medias de bloque + $|\Delta\theta|$ por onda, y variante con **medias por comisión** (membresía BCN 154/154, `commission_membership.csv`). Nota correctiva C5: indicaciones colectivas registradas solo con el primer firmante (limitación de registro, no unipersonales). El historial completo de versiones está en `docs/report-archive.txt`.

\clearpage
\thispagestyle{empty}
\mbox{}
\clearpage

# Anexo A — Problemas pendientes

Los problemas resueltos (P1--P7, P9, P11, P12, P14, P15) están archivados con su historial completo en `docs/report-archive.txt` (fuera de versionado). Quedan abiertos:

## P8 — Esquema GENESIS heterogéneo **[ABIERTO — aguas arriba en CPT]**

C1/C2/C3 sin `article_uid`; C4 sin `article`/`sources`; `authors` intermitente. Asignado en CPT con la instrucción de heredar los uids de TRACK/crosswalk. El loader de este repo lo absorbe mientras tanto (los demás ítems de P8 — títulos, `step`, timestamps documentados, dedup de indicaciones — quedaron resueltos).

## P10 — dynIRT sin errores estándar **[ABIERTO — prioridad baja]**

No existe `emIRT_bootstrap_output.rds`; `theta_se` queda NA en el pipeline (el script 02 lo tolera). Decidir si (i) bootstrap paramétrico del dynIRT, o (ii) reportar sensibilidad de M2 a la incertidumbre de $\theta$ de otro modo. No bloquea la actualización.

## P13 — Errores y desactualizaciones en los documentos del paper **[ABIERTO]**

1. **Rótulos temáticos de comisiones equivocados.** `docs/extended-abstract.tex` (§Data) describe "C5: Fundamental Rights; C6: Environment". Según la numeración oficial (verificada; §3.1), C5 es **Medio Ambiente** y C6 es **Sistemas de Justicia** (Derechos Fundamentales es C4, que no estaba en la muestra). La numeración de los archivos es consistente con la oficial; son los rótulos temáticos del texto (y del README) los que están corridos.
2. **"155 delegates" vs. 154** en pasajes del abstract y propuesta (154 es el n tras normalización de nombres; la Convención tuvo 155 escaños — precisar la distinción donde corresponda).
3. **Cifras a regenerar** tras la actualización: todas las tablas, el "max weight > 300" (real: 480), N de panel (2.926), N del SDM (141), artículos mapeados (236), y el párrafo "Ongoing work" sobre comisiones pendientes (desaparece).
4. **Cambio metodológico no documentado**: la propuesta original (`docs/research-proposal.tex`) prometía TERGM (M1) y SAOM/RSiena (M2); la implementación usa Valued ERGM y panel FE. Documentar la justificación (tamaño/estructura de los datos; identificación de selección vs. influencia vía FE + falsificación) en el README y en la próxima versión del paper.

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


```{=latex}
\end{landscape}
```
