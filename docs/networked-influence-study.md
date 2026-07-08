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

- **RQ1 — Formación.** ¿Qué factores impulsan la formación de la red de co-patrocinio cuando no existen jerarquías previas? ¿Domina la homofilia clásica (afiliación, género) o aparecen estrategias de diversificación?
- **RQ2 — Influencia vs. selección.** ¿La exposición a los co-autores desplaza las posiciones ideológicas de los delegados (influencia social), o los delegados seleccionan co-autores ya afines (selección endógena)?
- **RQ3 — Éxito.** ¿Cómo predice la estructura de red el éxito político individual, operacionalizado como **retención léxica**: cuánto del texto que un delegado patrocinó sobrevive en el borrador constitucional final?

## 1.3 Marco teórico

*(Apartado en construcción: esta sección fija el esqueleto argumental y se irá puliendo junto con la especificación final de los modelos.)*

**(a) Formación de lazos en legislaturas: homofilia y sus límites.** La literatura de co-sponsorship en legislaturas consolidadas (Fowler 2006; Bratton & Rouse 2011; Kirkland 2011) documenta con robustez que los lazos siguen líneas de partido, región y género — homofilia en el sentido de McPherson, Smith-Lovin & Cook (2001). Pero esa literatura no puede separar cuánto de la homofilia observada es preferencia y cuánto es estructura heredada (comités, bancadas, historia). La Convención remueve la estructura heredada: lo que se observe en la formación temprana refleja preferencias y estrategias de actores en un campo casi vacío. **H1a (línea base):** persiste homofilia por afiliación política agrupada, el clivaje más visible incluso entre independientes.

**(b) Gatekeeping estratégico: la homofilia negativa de los "dotados".** Para los actores con recursos escasos y transversalmente valiosos — pericia jurídica (abogados, en una asamblea cuyo producto es un texto legal) y experiencia institucional previa — la teoría de brokerage (Burt 1992; Padgett & Ansell 1993) predice lo contrario de la homofilia: maximizan influencia **dispersándose** entre coaliciones, ocupando agujeros estructurales en vez de agruparse entre sí. Se comportan como *gatekeepers* del conocimiento procedimental. **H1b:** abogados y delegados con experiencia institucional exhiben homofilia *negativa* (co-patrocinan menos entre sí de lo esperado), condicional en los demás términos del modelo.

**(c) Influencia social vs. selección endógena.** Que los conectados se parezcan ideológicamente admite dos mecanismos generativos: influencia (los lazos mueven las posiciones; Friedkin & Johnsen 1990) o selección (las posiciones crean los lazos). Distinguirlos es el problema clásico de la econometría de redes (Shalizi & Thomas 2011; en la tradición SAOM, Steglich, Snijders & Pearson 2010). Nuestra estrategia: panel con efectos fijos individuales (toda heterogeneidad estable del delegado queda absorbida) más un **test de falsificación** con exposición futura (*lead*): si el efecto contemporáneo fuera influencia causal, la exposición futura no debería "predecir" el cambio pasado. **H2:** en un cuerpo con posiciones ideológicas pre-formadas (los delegados llegan con identidades políticas adultas), domina la selección; el efecto de exposición desaparece bajo efectos fijos.

**(d) El éxito legislativo como fenómeno colectivo.** La efectividad legislativa suele tratarse como atributo individual (Volden & Wiseman 2014). Pero si la unidad de producción es la coalición firmante — y el reglamento fuerza que lo sea — el éxito debería *derramarse* por los lazos de co-autoría: mi retención léxica depende de la retención de aquellos con quienes escribo. Econométricamente eso es autocorrelación espacial en la variable de resultado sobre la topología de la red, y el marco natural son los modelos espaciales sobre redes (LeSage & Pace 2009). **H3:** la retención léxica exhibe dependencia de red fuerte; los modelos con rezago espacial (SAR/SDM) dominan al OLS, y el componente indirecto (spillover) de los efectos es sustantivo.

**(e) Anclaje ideológico.** Las posiciones ideológicas se estiman desde el comportamiento de votación nominal con un modelo dinámico de puntos ideales (dynIRT; Martin & Quinn 2002; implementación `emIRT` de Imai, Lo & Olmsted 2016), con prior de caminata aleatoria ($\omega^2 = 0.025$) y anclaje por dos delegados de posición pública inequívoca (Marinovic a la derecha, Baradit a la izquierda). Resultado: matriz $\Theta \in \mathbb{R}^{154 \times 91}$ (delegados × períodos de votación, 2021-07-13 a 2022-06-24).

## 1.4 Diseño: tres modelos

**M1 — Formación (Valued ERGM).** Red pooled de co-patrocinio con pesos $w_{ij}$ = nº de documentos co-firmados. ERGM valuado con referencia Poisson:
$$P(W = w) \propto \exp\{\theta^\top g(w)\}, \qquad g = (\text{sum},\ \text{nodematch}_{\text{afiliación, abogado, experiencia, género}},\ \text{absdiff}_{\text{edad, grado}},\ \text{nodecov}_{\text{edad}})$$
Los términos `nodematch` capturan homofilia (H1a) y su reverso (H1b). El término `absdiff` sobre el **grado académico** (escala ordinal 0--3: sin estudios universitarios terminados / educación superior terminada / magíster / doctorado) prueba estratificación por credenciales educativas: un coeficiente negativo indica que la distancia educativa inhibe la co-firma.

**M2 — Dinámica ideológica (panel FE).** Sobre ondas temporales por comisión:
$$\Delta\theta_{i,t} = \alpha_i + \beta_1 \theta_{i,t-1} + \beta_3\, \text{NetExp}_{i,t-1} + \varepsilon_{it}, \qquad \text{NetExp}_{i,t-1} = \frac{\sum_{j \neq i} w_{ij,t-1}\,\theta_{j,t-1}}{\sum_{j \neq i} w_{ij,t-1}}$$
con $w_{ij,t-1}$ = co-firmas acumuladas hasta la onda $t-1$. Comparación OLS agrupado vs. efectos fijos (within) vs. aleatorios (Hausman), errores agrupados por delegado, y falsificación con *lead*.

**M3 — Éxito (Spatial Durbin Model).** Con $y_i$ = retención léxica media del delegado (similitud coseno TF-IDF génesis→final; robustez con Sentence-BERT) y $W$ = matriz de co-autoría row-normalizada:
$$y = \rho W y + X\beta + WX\gamma + \varepsilon$$
Se compara OLS/SAR/SEM/SDM (AIC, Moran's $I$) y se descomponen efectos directos/indirectos/totales.

# 2. Datos

## 2.1 Fuentes primarias

Las tablas con las rutas exactas de cada fuente y el desglose por comisión están en el **Anexo B** (orientación horizontal). Desde 2026-07-07 los datos viven como **snapshot versionado dentro de este repositorio** (`data/raw/dataverse-final/`, copiado de CPT `paper-draft` @ `6fac4c4`; ver P8/Anexo A). El test de aceptación `code/0a-verify-dataverse-snapshot.py` valida cada refresco y escribe `QA-report.txt`. En resumen: **1.892 iniciativas génesis**, **2.019 registros de trazabilidad** (`TRACK_full`), **498 artículos** en el borrador final del 14-05-2022, **154 convencionales** con perfil curado y matriz de puntos ideales $\Theta \in \mathbb{R}^{154\times 91}$.

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
│    ├─ authors       ["Uribe, Cesar", …]     FIRMANTES DEL GÉNESIS  → red de M1
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

En palabras: **los autores de la iniciativa original** están en `authors` a nivel de artículo y definen la red de co-patrocinio de **M1**; **los autores de cada enmienda** están en `authors` dentro de cada entrada de `history[]` (o en el registro suelto) y, junto con su `timestamp`, definen las ondas de **M2**; el desenlace de cada artículo (`final_status`) y su `text` génesis definen la variable de éxito de **M3**, y los `content_snapshot` permiten reconstruir la trayectoria textual (§M3 — Dinámica).

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

Estado vigente: **154/154 perfiles**, 0 afiliaciones desconocidas, 0 edades faltantes; grado 0--3 = \{0: 18, 1: 81, 2: 41, 3: 14\}.

# 3. Exploración de datos

*(El detalle de los 12 scripts del pipeline está en el **Anexo C**.)*

## 3.1 Insumos construidos

**Registro de iniciativas** (`initiative_registry.csv`): **528 iniciativas** con $\geq 2$ firmantes-persona — C1: 29, C2: 56, C3: 41, C4: 68, C5: 148, C6: 78, C7: 108 (las iniciativas populares/indígenas quedan fuera de la red de personas). La Figura F1 contrasta ese conteo de iniciativas con el de artículos génesis por comisión: la brecha entre ambas barras es el número de artículos por iniciativa (C5 es el caso extremo: 148 iniciativas → 420 artículos).

![**F1.** Iniciativas constitucionales (barra sólida) sobre artículos génesis (barra translúcida) por comisión, ambos con $\geq 2$ firmantes.](../results/figures/initiatives_per_commission.pdf){width=80%}

**Red génesis-iniciativa** (principal): 154 nodos (sin aislados), **7.731 aristas**, peso total 53.391, peso máximo 76. **Red génesis-artículo** (robustez): 1.676 eventos de co-firma, 8.020 aristas, peso 175.316. La Figura F2 muestra ambas redes como grafos bipartitos documento–convencional, con los convencionales ordenados por punto ideal (rojo = izquierda, azul = derecha): se ve de inmediato qué comisiones firmaron transversalmente y cuáles en bloque.

![**F2a.** Co-patrocinio génesis como red bipartita, **unidad iniciativa** (principal). Documentos arriba (color por comisión); convencionales abajo, ordenados y coloreados por punto ideal — **rojo = izquierda** ($\theta<0$; Baradit $-1.4$), **azul = derecha** ($\theta>0$; Marinovic $+4.3$), con colorbar.](../results/figures/bipartite_initiative.pdf){width=100%}

![**F2b.** Co-patrocinio génesis como red bipartita, **unidad artículo** (robustez), mismas convenciones.](../results/figures/bipartite_article.pdf){width=100%}

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

**Mapeo y DV de éxito**: 484 pares génesis$\to$final (480 vía `coincidencias` + 4 rescatados parseando `final_status`; 2 uids de indicación no encontrados). Desenlaces de los 1.809 artículos: 223 idéntico, 136 similar, 275 ART-FALLIDO, 1.175 eliminado (fracaso = fallido + eliminado, sim = 0). **Validación del emparejamiento**: similitud alineada 0.554 vs. 0.032 con pares barajados; 96.5% de los pares supera su baseline. TF-IDF idéntico 0.570 / similar 0.565; SBERT 0.857 / 0.872. *Nota sobre el ancla de validación*: la v1 reportaba 0.979 en "idénticos" porque comparaba el texto **post-indicaciones** con el borrador; la v2 usa el texto **génesis verdadero** (la operacionalización correcta de "retención de lo que el delegado propuso"), y la etiqueta idéntico/similar — que refiere al estado *final* del artículo — deja de ordenar la similitud génesis$\to$final; la validación pasa a ser el contraste alineado-vs-barajado. Media de $y'$: 0.094; tasa de supervivencia media: 0.211; **154/154 convencionales con score** (por primera vez el roster entero entra a M3).

## 3.2 Limitaciones de datos (transversales)

1. **Cobertura de autores.** 134/2.019 registros TRACK sin autores (65 de iniciativas populares/indígenas — patrocinio institucional —, 45 de ICC no recuperadas, 24 sin referencia); 54 artículos del borrador final quedaron `not_traced` en `coincidencias`; 66 + 5 registros *undated* se excluyen de las ondas.
2. **dynIRT sin errores estándar** (P10, abierto): la incertidumbre de $\theta$ no se propaga a M2/M3.

Las limitaciones específicas de cada modelo están al final de su sección (M1/M2/M3 — Limitaciones).

# M1 — Formación de la red (Valued ERGM)

## M1 — Especificación

La red observada es $W = [w_{ij}]$, no dirigida, con $w_{ij} \in \{0, 1, 2, \dots\}$ = nº de **iniciativas** co-firmadas por $i$ y $j$ (unidad principal; §4.1). El ERGM valuado con referencia Poisson modela
$$P(W = w) = \frac{\exp\{\theta^\top g(w)\}}{\kappa(\theta)} \prod_{i<j} \frac{1}{w_{ij}!},$$
con estadísticas de cambio
$$g(w) = \Big(\underbrace{\textstyle\sum_{i<j} w_{ij}}_{\text{sum}},\ \underbrace{\textstyle\sum_{i<j} w_{ij}\,\mathbf{1}\{X_i = X_j\}}_{\text{nodematch}_X},\ \underbrace{\textstyle\sum_{i<j} w_{ij}\,|X_i - X_j|}_{\text{absdiff}_X},\ \underbrace{\textstyle\sum_{i<j} w_{ij}(X_i + X_j)}_{\text{nodecov}_X}\Big)$$
para $X \in$ \{afiliación, experiencia previa, abogado, mujer\} (nodematch), \{edad, grado académico 0--3\} (absdiff) y edad (nodecov). Estimación MCMLE (`ergm.count`, semilla 42), 154 nodos (roster completo, sin aislados). Un coeficiente nodematch positivo indica homofilia; `absdiff` negativo indica que la **distancia** en el atributo inhibe la co-firma.

## M1 — Resultados

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

## M1 — Robustez y descomposición del vuelco de H1b

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

**El patrón fino es heterogéneo y sustantivamente rico**: la homofilia *negativa* de experiencia sobrevive **exactamente en C1 (Sistema Político) y C3 (Forma de Estado)** — las comisiones donde se diseñaba la arquitectura del poder — mientras en las otras cinco es fuertemente positiva. La lectura de gatekeeping de la v1 no era un fantasma: es un fenómeno **específico de las comisiones de diseño institucional**, no una estrategia general. La unidad iniciativa vs. artículo no altera ninguna conclusión (§4.1).

## M1 — Diseño: ¿se sostiene H1b en comisiones específicas? (por implementar)

La sección anterior ajusta un ERGM por comisión y *sugiere* que la homofilia de experiencia es negativa en C1/C3 y positiva en el resto. Pero **comparar coeficientes entre siete modelos ajustados por separado no es un test formal**: cada comisión tiene su propia densidad y su propio término `sum`, de modo que las escalas no son estrictamente comparables, y no existe un contraste de significancia para "¿difiere C1 de C4?". El diseño en tres niveles (a ejecutar; **nada se corre aún**):

- **Nivel 1 — ¿Se sostiene H1b *dentro* de una comisión?** El ERGM de cada comisión $k$ ya entrega el coeficiente de `nodematch(experiencia)` con su EE. H1b se sostiene en $k$ $\iff$ ese coeficiente es significativamente **negativo** (test de Wald unilateral $H_0:\beta^{\text{exp}}_k \geq 0$). Con lo ya corrido: se sostiene en **C1** ($-0.175$, $p<0.01$) y **C3** ($-0.207$, $p<0.001$); se rechaza (positivo y significativo) en C2, C4, C5, C6, C7. Este es el test que responde directamente la pregunta. Coste: los 7 ajustes ya corren en paralelo (`mclapply`, 8 P-cores, ~10 min).
- **Nivel 2 — ¿Difieren las comisiones entre sí? (heterogeneidad).** Meta-análisis sobre los 7 estimados: tratar cada $(\hat\beta^{\text{exp}}_k, \widehat{\text{SE}}_k)$ como un "estudio" y ajustar un modelo de efectos aleatorios $\hat\beta_k = \bar\beta + u_k + \varepsilon_k$ (`metafor::rma` en R). La $Q$ de Cochran y el $I^2$ dan un $p$-valor formal para "el término de experiencia varía entre comisiones", y $\tau^2$ cuantifica esa variación. Barato (7 números por término), sin MCMC.
- **Nivel 3 — Test dentro de un solo modelo (opcional, riguroso).** ERGM valuado sobre la red **block-diagonal multicapa** (una capa por comisión, sin aristas entre capas), con términos de homofilia específicos por capa. Un test de razón de verosimilitud (o AIC) entre el modelo **homogéneo** (un término de experiencia común) y el **heterogéneo** (7 términos por capa) formaliza la heterogeneidad dentro de una única verosimilitud. Es el más caro (un MCMC sobre la red apilada de ~7.700 aristas); se paraleliza repartiendo las cadenas entre los 8 P-cores con `control.ergm(parallel = 8, parallel.type = "PSOCK")`.

Recomendación: reportar **Nivel 1** (responde la pregunta del usuario) y **Nivel 2** (formaliza "difieren") como principales, y **Nivel 3** como robustez si un revisor exige el test dentro de un modelo.

## M1 — Limitaciones

1. **Convergencia del ERGM pooled**: MCMLE al 95% pero no al 99% de confianza ($p = 0.014$ del test tras 60 iteraciones); coeficientes estables entre corridas.
2. **Unidad de co-firma**: iniciativa como principal es una decisión interpretativa (§4.1); los resultados son cualitativamente iguales con unidad artículo.

# M2 — Selección vs. influencia (panel con efectos fijos)

## M2 — Especificación

Sobre las ondas por comisión (T0 génesis + fechas de informes de indicaciones; Figura F3), la exposición de red de $i$ en la onda $t$ de la comisión $c$ usa la red **acumulada** $W^{c}_{t}$:
$$\text{NetExp}_{i,t} = \frac{\sum_{j \neq i} w_{ij,t}\,\theta_{j,t}}{\sum_{j \neq i} w_{ij,t}},$$
y el modelo principal es el panel con efectos fijos individuales
$$\Delta\theta_{i,t} = \alpha_i + \beta_1\,\theta_{i,t-1} + \beta_3\,\text{NetExp}_{i,t-1} + \varepsilon_{it},$$
con errores agrupados por delegado y test de Hausman contra efectos aleatorios. **Falsificación**: se reemplaza el rezago por el *lead* ($\text{NetExp}_{i,t+1}$); si el efecto contemporáneo fuera influencia causal, el lead debería ser nulo. **Ventanas alternativas de exposición** (robustez): decaimiento exponencial $W^{dec}_{t} = \sum_{s \le t} \lambda^{t-s}\,\Delta W_s$ con $\lambda = 0.5$; solo-última-onda $W^{last}_{t} = \Delta W_t$; y $\Delta\theta$ estandarizado por días transcurridos entre ondas. Los pasos que comparten período emIRT con el anterior ($\Delta\theta \equiv 0$ mecánico) se excluyen (1.078 celdas).

![**F3.** Construcción de ondas para M2, por comisión (archivo `C#_waves_summary`).](../results/figures/C%23_waves_summary.pdf){width=100%}

## M2 — Motivación descriptiva

La pregunta del modelo ("las posiciones cambian y la red cambia: ¿influencia o selección?") se establece con tres piezas descriptivas.

**(a) La red cambia — ¿cuándo se forman los lazos post-génesis?** La Figura F4 ubica cada evento de indicación multi-autor en su fecha calendario real, apilando las comisiones por niveles.

![**F4.** Eventos de indicación multi-autor por fecha de informe, por comisión (escala compartida 0--145; puntos = informes con cero eventos multi-autor).](../results/figures/indication_events_timeline.pdf){width=100%}

La formación de lazos post-génesis es **sostenida en C1, C3, C4 y C6** (C6 incluso crece hacia el final; C3 es bimodal), **de un solo golpe en C7** (42/54 eventos en el informe del 19-02) y **no observable en C2/C5**. Los eventos de exactamente 2 autores son minoría (2--3% en C1/C3; 26% en C4). *Nota C5*: sus indicaciones **no fueron unipersonales en la realidad** — los informes de C5 registran solo al primer firmante (resto como "y otros"), una **limitación de registro** que impide reconstruir sus lazos post-génesis (M2 — Limitaciones). Comisiones pertinentes para leer dinámica de red: **C1, C3, C4, C6** (C7 con nota).

**(b) Las posiciones cambian.** La Figura F5 (panel superior) traza las 154 trayectorias de $\theta_{i,t}$ sobre las 91 fechas de votación, coloreadas por posición media (rojo = izquierda), con las medias de bloque; el **panel inferior** resume el movimiento onda a onda.

![**F5.** Arriba: trayectorias de punto ideal de los 154 convencionales (color = posición media, rojo = izquierda), con medias de bloque. Abajo: distribución de los desplazamientos ideológicos por onda de comisión.](../results/figures/m2_positions_dynamics.pdf){width=92%}

El panel inferior de F5 es un **boxplot de $\lvert\Delta\theta_{i,t}\rvert$ por onda de comisión**, *agrupado (pooled) sobre las 7 comisiones*. Cada "onda" es un **informe de indicaciones** de una comisión (la unidad temporal de M2): la onda 1 es el primer informe, la onda 2 el segundo, etc. $\lvert\Delta\theta_{i,t}\rvert$ es el tamaño del cambio de posición del convencional $i$ entre los períodos de votación (emIRT) alineados a las ondas $t-1$ y $t$ de su comisión. El eje va de la **onda 2 a la 8** (la onda 1 casi siempre comparte período de votación con el génesis, así que su $\Delta\theta$ es un cero mecánico y se descarta; la onda máxima, 8, solo la alcanza C7). Los **$n$ sobre cada caja** son el número de observaciones convencional×comisión que contribuyen a esa onda, y decrecen porque cada vez menos comisiones alcanzan ondas altas: $n = 154 \times \{7, 7, 6, 5, 3, 1, 1\}$ para las ondas $2\ldots 8$ (todas las comisiones llegan a la onda 2--3; solo C7 llega a la 7--8). Lectura: la mediana de $\lvert\Delta\theta\rvert$ está entre 0.12 y 0.25 en **todas** las ondas — los bloques son estables en el agregado pero los individuos se mueven de forma no trivial en cada onda.

Vista complementaria por **membresía de comisión** (extraída del campo `integracion_comisiones` de la BCN, 154/154 matcheados; snapshot en `data/raw/commission_membership.csv`):

![**F6.** Trayectorias de punto ideal por comisión de pertenencia (líneas débiles = convencionales, líneas gruesas = media de la comisión).](../results/figures/theta_dynamics_by_commission.pdf){width=92%}

En F6 todas las medias de comisión viven en la franja izquierda (la Convención fue de mayoría izquierda); C5 es la más a la izquierda y C2 la menos — contexto útil para comparar entre comisiones las correlaciones del panel (c).

**Nota sobre la concentración temporal (F5/F6).** La dinámica de posiciones es casi plana entre julio 2021 y enero 2022 y se concentra fuertemente entre ~febrero y ~junio de 2022. Esto es, ante todo, un artefacto de **densidad de datos de votación, no de quietud política**. El dynIRT se alimenta de votaciones nominales: la ventana temprana (jul 2021 -- ene 2022) aporta 35 fechas de sesión y **999 roll-calls (21% de los 4.707 totales)**, mientras la tardía (feb -- jun 2022) aporta 56 fechas y **3.708 roll-calls (79%)** — 1.6× más fechas pero 3.7× más votaciones, porque además es más densa por sesión (66 vs. 28 roll-calls/sesión). Meses como noviembre 2021 aportan 2 votaciones; mayo 2022 aporta 1.138. La razón es procesal: en 2021 la Convención se instaló y escribió su reglamento (votaciones procedimentales, poco discriminantes) y el trabajo sustantivo estaba en las comisiones (votaciones fragmentadas, no del Pleno completo); recién el **15-02-2022 el Pleno empezó a votar los informes de norma** artículo por artículo (con reposiciones de las que no alcanzaban 2/3), y esas votaciones nominales masivas terminaron el **14-05-2022**, con una cola de armonización/transitorias hasta junio. Implicación para M2: los períodos tempranos están débilmente identificados ($\theta$ encogido hacia el prior), así que parte del "movimiento" de feb--jun refleja **más información, no solo más movimiento real** — toda lectura de la dinámica temporal debe ponderar esta asimetría.

**(c) Selección vs. influencia en una imagen.** La Figura F7 contrasta, por comisión y onda, la correlación transversal exposición–posición con el efecto within.

![**F7.** Correlación transversal exposición–posición por comisión y onda (arriba) vs. efecto within-delegado de la exposición rezagada (caja).](../results/figures/m2_selection_vs_influence_preview.pdf){width=88%}

En cada comisión y cada onda, la posición ponderada de los co-firmantes sigue de cerca la propia ($r$ transversal 0.83--0.97, estable en el tiempo — la firma de la selección homofílica), mientras el efecto *within* de la exposición rezagada sobre $\Delta\theta$ es cero. La brecha entre ambas cantidades **es** el resultado de M2.

## M2 — Resultados

Panel: **4.278 observaciones** delegado-onda (antes 2.926), 154 delegados, 7 comisiones; 1.078 celdas con período emIRT compartido se excluyen. La primera tabla resume el coeficiente de exposición $\hat\beta_3$ a través de las especificaciones (principal, alternativas y falsificación); la segunda da los coeficientes completos de los cuatro modelos base.

Coeficiente de exposición ($\hat\beta_3$), EE agrupados por delegado:

| Modelo | $\hat\beta_3$ | EE | $p$ |
|:---|---:|---:|:--|
| OLS agrupado | $+0.0356$ | 0.0099 | $3.1\times10^{-4}$ |
| **Efectos fijos (within)** | $-0.0007$ | 0.0045 | $0.88$ |
| OLS + FE de comisión | $+0.0364$ | 0.0095 | $1.4\times10^{-4}$ |
| Falsificación (*lead*) | $+0.0552$ | 0.0116 | $2.2\times10^{-6}$ |
| FE, exposición con decaimiento ($\lambda=0.5$) | $+0.0017$ | 0.0045 | $0.71$ |
| FE, exposición solo-última-onda | $+0.0068$ | 0.0128 | $0.60$ |
| FE, $\Delta\theta$ por día | $-0.0001$ | 0.0004 | $0.83$ |

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

Hausman $\chi^2 = 1973.9$ ($p \approx 0$) $\to$ FE. **El resultado central de la v1 se replica y blinda**: la correlación positiva desaparece bajo efectos fijos, la exposición futura "predice" el cambio pasado (falsificación falla $\to$ selección endógena), y el nulo bajo FE sobrevive las tres ventanas alternativas de exposición. H2 confirmada con el doble de observaciones y las 7 comisiones.

## M2 — Limitaciones

1. **C2 delgada.** Por la naturaleza distinta de la comisión y de su registro, C2 aporta solo 3 ondas con 1 evento de co-firma de indicaciones (52 unipersonales). Decisión del usuario (2026-07-07): **se usa de todas formas**; su contribución a M2 es mínima y así debe leerse.
2. **Ondas planas en C5 (limitación de registro).** Las indicaciones de C5 **sí** fueron colectivas, pero sus informes registran únicamente al primer firmante (el resto queda como "y otros"): con un solo autor recuperable por evento, la red de C5 no puede cambiar después de T0 y su aporte a M2 proviene solo de la variación temporal de $\theta$.
3. **Pasos con el mismo período emIRT** (sin votaciones intermedias) se excluyen del panel: 1.078 celdas.
4. La incertidumbre de $\theta$ (dynIRT sin SEs, P10) no se propaga.

# M3 — Éxito legislativo (Spatial Durbin Model)

## M3 — Especificación

La DV principal es la **retención esperada por artículo presentado** (decisión ART-FALLIDO, P6): con $A_i$ = artículos génesis co-firmados por $i$ y $M_i \subseteq A_i$ los trazados al borrador,
$$y_i' = \frac{1}{|A_i|}\sum_{a \in A_i} \text{sim}(a) = \underbrace{\frac{|M_i|}{|A_i|}}_{\hat{s}_i} \times \underbrace{\overline{\text{sim}}_{M_i}}_{\bar{r}_i}, \qquad \text{sim}(a) = 0 \text{ si } a \text{ fallido/eliminado},$$
donde $\text{sim}(a)$ es el coseno TF-IDF entre el texto génesis de $a$ y su artículo del borrador final (máximo sobre sus pares si mapea a varios; SBERT como medida alternativa). El modelo espacial es
$$y' = \rho\,\tilde{W} y' + X\beta + \tilde{W} X\gamma + \varepsilon,$$
con $\tilde{W}$ = red génesis-iniciativa **row-normalizada** (la estructura precede al resultado, P9), estimado por máxima verosimilitud; se compara con OLS, SAR ($\gamma = 0$) y SEM (error espacial) vía AIC, y la dependencia se diagnostica con el $I$ de Moran. Los efectos marginales del SDM se descomponen con $(I - \rho \tilde{W})^{-1}(I\beta_k + \tilde{W}\gamma_k)$ en directo/indirecto/total.

## M3 — Resultados

Esta sección reporta dos tablas. La **Tabla 1** compara el ajuste global de los cuatro modelos (OLS, SEM, SAR, SDM) por AIC y su parámetro espacial: responde "¿hay dependencia de red y qué modelo la captura mejor?". La **Tabla 2** da los coeficientes del OLS y del SDM, y en el SDM **separa el efecto directo** de cada covariable (el atributo del propio delegado) del **término espacial $\tilde{W}X$** (el mismo atributo promediado sobre sus co-firmantes): responde "¿el éxito depende de los atributos de uno o de los de la red?". El diagnóstico previo es Moran's $I = 0.380$ ($p \approx 10^{-194}$; antes 0.155), que justifica el modelo espacial.

**Tabla 1 — comparación de modelos y parámetro espacial:**

| Modelo | AIC | Parámetro espacial |
|:---|---:|:---|
| OLS | $-448.9$ | — |
| SEM | $-529.5$ | $\lambda = 0.981$ (0.012)\*\*\* |
| SAR | $-539.3$ | $\rho = 0.979$ (0.014)\*\*\* |
| **SDM** | $\mathbf{-575.5}$ | $\rho = 0.943$ (0.037)\*\*\* |

**Tabla 2 — coeficientes de OLS y SDM** (`M3_full_models.csv`; \*\*\* $p<0.001$, \*\* $p<0.01$, \* $p<0.05$, $\dagger$ $p<0.1$):

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

**Lectura.** (i) Hay dependencia de red fuerte y el **SDM domina** (AIC $-575.5$, muy por debajo de OLS $-448.9$; $\hat\rho = 0.943$): el éxito de un delegado está acoplado al de sus co-firmantes. (ii) En el OLS, la **posición de red predice el éxito** (`degree` $+$, `betweenness` $-$, ambos $p<0.01$) — en la v1 las centralidades eran no significativas; incorporar los fracasos como ceros (DV $y'$) hizo visible ese efecto. (iii) En el SDM, los efectos **directos** individuales se disipan y son los **términos espaciales** los significativos (`lag.degree` $+0.009$, `lag.betweenness` $-0.008$, `lag.mujer` $-0.175$, `lag.`$\theta$ medio $+0.093$): tu éxito depende menos de tus atributos que de los de tu vecindario de co-firma. **H3 confirmada y amplificada**: el éxito es un fenómeno de red, no individual.

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
| $y'$, $\tilde{W}$ iniciativa (**principal**) | 154 | 0.380 | 0.943 | $-448.9$ | $-575.5$ |
| $y$ condicional (DV antigua), $\tilde{W}$ iniciativa | 154 | 0.140 | 0.840 | $-224.9$ | $-239.5$ |
| $y'$, $\tilde{W}$ binaria | 154 | 0.185 | 0.794 | $-448.9$ | $-512.6$ |
| $y'$, $\tilde{W}$ artículo | 154 | 0.408 | 0.934 | $-448.9$ | $-581.3$ |
| $y'$ SBERT, $\tilde{W}$ iniciativa | 154 | 0.399 | 0.937 | $-329.2$ | $-489.0$ |

En las cinco variantes el SDM mejora al OLS y $\rho$ se mantiene alto (0.79--0.94): la dependencia de red no es un artefacto de la DV, del peso ni de la definición de lazo.

## M3 — Dinámica de la retención sobre las ondas de comisión (7 comisiones, LOCF)

La comparación génesis$\to$final es el extremo de una **trayectoria**: cada artículo pasa por estados intermedios (`content_snapshot` tras cada indicación, **almacenados en el dataset** — no se reconstruyen). El análisis (`code/10-retention-dynamics.py`) mide, para los **359 artículos trazados**, la similitud de su estado vigente en **cada onda de su comisión** contra su artículo del borrador; si un artículo deja de modificarse antes de la última onda, su valor **se propaga hacia adelante** (LOCF). Cobertura de snapshots en artículos trazados: 100% en C3/C4/C5/C6, 97% en C2/C7, 79% en C1.

![**F8.** Trayectorias de las enmiendas hacia el texto final, las 7 comisiones (eje x en fechas calendario; génesis = etiqueta GEN observada más temprana, o primer informe − 10d si no hay ninguna anterior; LOCF; líneas débiles = artículos, gruesas = media por comisión).](../results/figures/retention_dynamics_all_commissions.pdf){width=100%}

Similitud media al borrador, génesis $\to$ última onda: C1 $0.65 \to 0.82$; C2 $0.39 \to 0.88$; C3 $0.50 \to 0.92$; C4 $0.49 \to 0.94$; C5 $0.15 \to 0.95$; C6 $0.40 \to 0.98$; C7 $0.33 \to 0.89$. Tres lecturas: (i) el proceso de indicaciones es **convergente hacia el borrador en las 7 comisiones**; (ii) la heterogeneidad de partida es enorme — C5 nace lejísimos de su forma final (0.15) y se reescribe casi por completo vía indicaciones (cuyos informes registran solo al primer firmante, por eso su red no cambia aunque su texto sí; ver M2 — Limitaciones); C1 nace cerca (0.65) y se mueve poco; (iii) el estado post-última-indicación ($\approx 0.9$) es lo que la v1 medía como "retención" (0.979), confirmando la nota del ancla (M3 — Limitaciones). **Extensión natural**: el dataset artículo-onda (`retention_dynamics_locf.csv`) es la base del futuro M4 de supervivencia.

## M3 — Limitaciones

1. **Derrame parcialmente mecánico.** $y'$ de co-firmantes comparte artículos por construcción; $\rho$ combina interdependencia real y composición compartida del DV. La extensión artículo-onda (M4 futura) es el camino para separarlas.
2. **Impactos inestables con $\rho \to 1$.** La descomposición directo/indirecto vía $(I-\rho\tilde{W})^{-1}$ explota numéricamente (SEs simuladas enormes); se reportan coeficientes y $\rho$; los impactos quedan en `sdm_results.rds`.
3. **Ancla de validación NLP.** La v1 (0.979 en "idénticos") comparaba el texto post-indicaciones; la v2 usa el génesis verdadero, y la etiqueta idéntico/similar (que refiere al estado final) deja de ordenar la similitud génesis$\to$final. La validez del emparejamiento se establece por el contraste alineado-vs-barajado (0.554 vs. 0.032; 96.5% de pares sobre su baseline) y por la dinámica de F8.

# 4. Decisiones de diseño confirmadas

| Fecha | Decisión |
|:---|:---|
| 2026-07-06 | `coincidencias_comisiones.csv` (UTF-8) es la fuente de verdad del mapeo génesis→borrador final (P7). |
| 2026-07-06 | **M1 = red génesis pura**, 7 comisiones. Indicaciones solo en las ondas de M2. Génesis+indicaciones como robustez (P2). |
| 2026-07-06 | **M3**: DV principal con ART-FALLIDO = 0 ($y' = \hat{s}\cdot\bar{r}$); DV condicional antigua como robustez (P6). $W$ y centralidades sobre la red génesis (P9). |
| 2026-07-06 | **M2**: exposición acumulada desde T0 hasta $t-1$ como especificación principal; robustez con decaimiento exponencial y con ventana solo-última-onda; sensibilidad estandarizando $\Delta\theta$ por días entre ondas. |
| 2026-07-06 | Indicaciones sueltas: entran a las ondas de M2 (timestamp+authors), no a M1 ni a M3; títulos se descartan (P8). |
| 2026-07-06 | Numeración oficial de comisiones confirmada; rótulos temáticos del paper a corregir (P13). |
| 2026-07-07 | Grado académico en escala ordinal **0--3**; entra a M1 como **absdiff** (distancia educativa). |
| 2026-07-07 | Imputación automatizada de covariables: BCN → Wikipedia → base; `manual_validations.json` siempre prevalece. |
| 2026-07-07 | Iniciativas populares en `authors` = no-persona; fuera de la red. |
| 2026-07-07 | **Unidad de co-firma: INICIATIVA principal, artículo robustez** (§4.1). |
| 2026-07-07 | **C2 se usa en M2 pese a ondas delgadas**; documentado como limitación. |
| 2026-07-07 | Indicaciones repetidas en varios `history[]` se colapsan a **un** acto (dedup por autores+fecha+contenido). |
| 2026-07-08 | Motivación descriptiva de M2 (F4--F7) ejecutada; membresía de comisión desde `integracion_comisiones` (BCN). |

## 4.1 Puntos a revisar (no son problemas P)

1. **Unidad de co-firma iniciativa vs. artículo.** La especificación principal usa iniciativa (cada documento co-firmado cuenta una vez); la de artículo pondera por el número de artículos de la iniciativa. Los resultados de M1 son cualitativamente iguales bajo ambas y M3 mantiene $\rho \approx 0.93$ con $\tilde{W}$-artículo, pero conviene revisitar la elección al escribir el paper (interpretación del peso $w_{ij}$).
2. **Interpretación del vuelco de H1b.** Con datos corregidos, la homofilia de abogados/experimentados pasó de negativa a positiva (M1 — Resultados): decidir cómo narrarlo en el paper (corrección de un artefacto de la v1) y si mantener el marco de *gatekeeping* como hipótesis rechazada o reformularlo (heterogeneidad C1/C3; ver M1 — Diseño por comisión).
3. **$\lambda = 0.5$ del decaimiento en M2** es una elección de conveniencia; si la robustez de ventana entra al paper, barrer $\lambda \in \{0.25, 0.5, 0.75\}$.
4. **Fechas de inicio ("génesis") de las comisiones (Figura F8).** *Verificado y corregido (2026-07-08).* La regla anterior ("primer informe − 10 días") resultó factualmente incorrecta para tres comisiones: los datos traen una etiqueta de informe génesis **anterior** a esa marca en C2 (02-16), C3 (01-27) y C6 (01-25); el resto (C1/C4/C5/C7) no tiene etiqueta génesis previa a su primera onda, así que ahí la marca −10d es un supuesto conservador inofensivo. F8 usa ahora `génesis = min(etiqueta GEN más temprana observada, primera onda − 10d)`, lo que muestra la genuina variación entre comisiones (el génesis observado más temprano va de 01-25 en C6 a 03-17 en C1). **Importante**: esta fecha solo posiciona el punto de génesis en el eje calendario — no entra en ninguna similitud ni en el LOCF —, de modo que el cambio es cosmético y no altera ningún resultado. **Queda por revisar** para el paper: (i) los timestamps son rótulos de informe, no fechas legales de envío (las reales son aún más tempranas y no están en los datos); (ii) C3/C5/C6/C7 tienen 2--3 *lotes* de génesis con etiquetas distintas (p. ej. C3: 01-27 y 04-04). Si se prioriza rigor sobre el eje calendario, la alternativa plenamente defendible es un **eje ordinal** (génesis → onda 1 → … → onda n), que honra que los timestamps son ordinales y evita la ambigüedad de múltiples lotes.

# 5. Registro de cambios

- **v2.4 (2026-07-08).** Reestructura editorial: se elimina "Propósito"; los nombres oficiales de las comisiones pasan al Contexto (§1.1); §4 se renombra "Exploración de datos"; la tabla de scripts pasa al Anexo C (landscape); se elimina "datos pre-actualización". El esquema de trazabilidad (§2.2) y la regla de covariables (§2.3) pasan a diagramas visuales. Secciones de modelos tituladas **M1/M2/M3** con subtítulos homónimos. Figuras numeradas **F1--F8** y referenciadas en el texto. Nueva subsección **M1 — Diseño por comisión** (test de H1b por comisión: Wald por comisión + meta-análisis + ERGM multicapa; sin correr). **M3 — Resultados** reescrito (explicación → tablas → lectura) e **itemize** con el objetivo de cada robustez. Nuevo punto a revisar (§4.1.4): verificar fechas de génesis por comisión (F8). Explicación del subplot inferior de F5.
- **v2.3 (2026-07-08).** Iteración de figuras (fechas calendario, gradiente ideológico rojo=izquierda, timeline de eventos); motivación de M2 (b) ejecutada; nota correctiva C5. El historial completo de versiones (v1--v2.3) está en `docs/report-archive.txt` (fuera de versionado).

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

1. **Rótulos temáticos de comisiones equivocados.** `docs/extended-abstract.tex` (§Data) describe "C5: Fundamental Rights; C6: Environment". Según la numeración oficial (§1.1), C5 es **Medio Ambiente** y C6 es **Sistemas de Justicia** (Derechos Fundamentales es C4). La numeración de los archivos es consistente con la oficial; son los rótulos temáticos del texto (y del README) los que están corridos.
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
| Perfiles curados | `data/raw/conventional-profiles.json` (imputado dual-fuente; ver §2.3) | Covariables: género, afiliación, distrito, abogado, edad, grado (0--3), experiencia | 154 |
| Membresía de comisión | `data/raw/commission_membership.csv` | Comisión de cada convencional (campo `integracion_comisiones` BCN) | 154 |
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

# Anexo C — Scripts del pipeline

```{=latex}
\begin{landscape}
```

Pipeline v2: todos los scripts derivan rutas de `code/paths.{py,R}` y nombres de `code/lib_names.{py,R}`; los insumos son el snapshot `data/raw/dataverse-final/` y los perfiles curados (§2.3).

| # | Script | Rol | Outputs |
|:-:|:---|:---|:---|
| 0a | `code/0a-verify-dataverse-snapshot.py` | Test de aceptación del snapshot | `QA-report.txt` |
| 0b | `code/0b-audit-profiles-dual-source.py` | Auditoría dual-fuente e imputación de perfiles (§2.3) | `conventional-profiles.json`, `profile-audit/*` |
| 00 | `code/00-build_dynamic_networks.py` | Registro de iniciativas; redes génesis (iniciativa y artículo); ondas ×7 con dedup de indicaciones | `genesis_network_{initiative,article}.csv`, `initiative_registry.csv`, `C{k}_dynamic_networks.json`, `commission_waves.csv` |
| 01 | `code/01-model-valued-ergm.R` | **M1**: Valued ERGM Poisson (red iniciativa) + centralidades | `ergm_pooled_results.rds`, `network_metrics.csv`, `tables/M1_ergm_iniciativa.csv` |
| 02 | `code/02-extract-emirt-temporal.R` | Alineación temporal $\Theta \leftrightarrow$ ondas de las 7 comisiones | `emirt_*.csv` |
| 03 | `code/03-model-network-influence.R` | **M2**: panel FE/RE/OLS + falsificación *lead* + robusteces de ventana | `network_exposure_panel.csv`, `panel_regression_results.rds`, `tables/M2_*.csv` |
| 04 | `code/04-build-article-mapping.py` | Lector de `coincidencias` (unión posicional al BORRADOR) + desenlaces por artículo | `article_mapping_unified.csv`, `track_article_outcomes.csv` |
| 05 | `code/05-nlp-text-similarity.py` | Retención léxica TF-IDF + SBERT; DV $y'$ con fracasos = 0 | `article_similarity_scores.csv`, `author_success_scores.csv` |
| 06 | `code/06-build-integrated-dataset.py` | Merge roster-driven (154 filas por construcción) | `integrated_dataset.{csv,json}` |
| 07 | `code/07-model-spatial-durbin.R` | **M3**: $\tilde{W}$ génesis-iniciativa; Moran; OLS/SAR/SEM/SDM; impactos | `sdm_results.rds`, `tables/M3_*.csv` |
| 08 | `code/08-robustness-checks.R` | Robustez M1 (por comisión ×7, unidad artículo, perfiles pre-auditoría) y M3; **ERGMs en paralelo** (`mclapply`, 8 P-cores) con cronómetro | `robustness_results.rds`, `tables/M{1,3}_robustness.csv` |
| 09 | `code/09-figures.py` | Figuras F1--F4 (PDF + PNG 300 dpi, inglés) | `results/figures/*` |
| 10 | `code/10-retention-dynamics.py` | Dinámica de retención sobre ondas de comisión, 7 comisiones, LOCF (F8) | `retention_dynamics_locf.csv`, figura |
| 11 | `code/11-m2-motivation-figures.py` | Motivación descriptiva de M2 (F5--F6): trayectorias de $\theta$, $\lvert\Delta\theta\rvert$ por onda, medias por comisión | `commission_membership.csv`, figuras |

```{=latex}
\end{landscape}
```
