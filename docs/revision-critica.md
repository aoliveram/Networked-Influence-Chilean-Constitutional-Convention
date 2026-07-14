---
title: "Informe de revisión crítica — Networked Influence in a Tabula Rasa Legislature"
subtitle: "Debilidades, cambios de perspectiva y preguntas nuevas, desde la mirada de un revisor externo"
author: "Revisor simulado (ciencia política / política comparada / ciencias sociales computacionales)"
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

# 0. Alcance y veredicto general

Este informe adopta el rol de un revisor que ve el estudio por primera vez, con formación en ciencia política (legislaturas, teoría espacial del voto), política comparada (América Latina, procesos constituyentes) y ciencias sociales computacionales (redes, texto-como-dato). Insumos revisados: el reporte del estudio (v2.4), sus figuras y tablas, y Fábrega (2022), *Ordenamiento Ideológico en la Convención Constitucional Chilena* (RCP 42(1)), que analizó el ordenamiento ideológico del mismo cuerpo con las votaciones del primer mes.

**Veredicto general.** El proyecto tiene tres activos raros de encontrar juntos: (i) una base de datos genuinamente nueva y de trazabilidad completa (iniciativa → artículo → enmienda → texto final, todo fechado y con autoría); (ii) un caso teóricamente privilegiado (un cuerpo colegiado sin inercia institucional, el punto que Fábrega 2022 establece con cuidado); y (iii) una infraestructura de reproducibilidad muy superior al estándar del área. Ninguna de las debilidades que siguen es una objeción a los datos: casi todas son objeciones a **cómo se está haciendo la pregunta** — la unidad de análisis, la variable política que organiza el espacio, y el mecanismo institucional (la regla de 2/3) que hoy está ausente de los modelos. La mayoría se puede resolver con los datos ya construidos.

Triage de severidad, para orientar la lectura:

| # | Debilidad | Severidad | ¿Reparable con los datos actuales? |
|:-:|:---|:---|:---|
| D1 | Pseudo-replicación por cliques (inferencia del ERGM) | **Letal** | Sí (bipartito / clúster por iniciativa) |
| D2 | "Independiente" como categoría de homofilia (102/154) | **Letal** | Sí (usar lista electoral) |
| D3 | M1 omite la distancia ideológica — el driver ya demostrado | **Mayor** | Sí ($\theta$ ya estimado) |
| D4 | La ventana de $\theta$ contradice a Fábrega (2022) | **Mayor** | Sí (re-especificar/robustez) |
| D5 | Unidimensionalidad ideológica; la acción está en la 2ª dimensión | **Mayor** | Sí (re-estimar 2D) |
| D6 | M3 sin la regla de 2/3: la explicación pivotal no está descartada | **Mayor** | Sí (controlar pívot; nivel artículo) |
| D7 | El "nulo" de M2: poder, reflexión y shocks comunes | **Mayor** | Parcial (reportar MDE; reencuadrar) |
| D8 | "Tabula rasa" y "firma costosa" sobrevendidos | Mayor (teoría) | Sí (reencuadre + evidencia descriptiva) |
| D9 | H1b: brokerage operacionalizado como homofilia; riesgo de HARKing | Mayor | Sí (medidas de brokerage; honestidad expositiva) |
| D10 | Estructura de oportunidad ausente (comisión) y covariables LLM | Media | Sí (nodematch comisión; análisis de error) |
| D11 | Validez de constructo del "éxito" (léxico, borrador rechazado) | Media (encuadre) | Sí (encuadre + robustez semántica ya hecha) |
| D12 | Escaños reservados aplanados | Media | Sí (dummy/nodematch pueblo originario) |

---

# Parte I — Debilidades del proyecto, como revisor

## D1. Las firmas vienen en paquetes, pero el modelo las cuenta como si fueran pares independientes

**Intuición.** Cuando 16 personas firman juntas una iniciativa, ese único acto crea de golpe 120 "lazos". El ERGM actual mira esos 120 lazos como si fueran 120 decisiones separadas, cuando en realidad hubo *una* decisión colectiva. Es como encuestar a una persona y anotar su respuesta 120 veces: los errores estándar se encogen artificialmente y todo parece hiper-significativo.

**Nota pedagógica — cómo detectar este problema en proyectos futuros.** Cuatro señales de alarma, en orden de uso: (1) *Pregúntese cuál es el acto que genera la observación.* Si un solo acto (una firma colectiva, una foto grupal, una co-asistencia) genera muchas filas de datos a la vez, el N efectivo es el número de **actos**, no el de filas. La prueba mental: "si borro un evento del mundo, ¿cuántas observaciones desaparecen de mi matriz?" — si la respuesta es "decenas", hay pseudo-replicación. (2) *Desconfíe de los $z$ enormes.* En redes sociales reales, coeficientes con $|z| > 10$ casi nunca sobreviven a una inferencia honesta; $|z|$ de 20--60 son la huella dactilar del problema. (3) *Identifique la proyección.* Si sus datos nacen bipartitos (personas × eventos/documentos/organizaciones) y su matriz de análisis es persona × persona, hubo una proyección — y toda proyección de eventos multi-actor fabrica cliques con dependencia perfecta dentro de cada evento. (4) *Test barato de una tarde:* re-estime con bootstrap remuestreando **eventos** (no díadas) — si los errores estándar se multiplican por 3 o más, el problema es real. Regla de diseño general: cuando los datos son "quién participó en qué", la representación por defecto debe ser bipartita o de hipergrafo, y la proyección a díadas requiere justificar explícitamente su inferencia.

**Detalle técnico.** La red génesis-iniciativa se construye proyectando cada iniciativa (8--16 firmantes) a su clique de pares: una iniciativa de $s$ firmantes genera $\binom{s}{2} \in [28, 120]$ díadas simultáneas y perfectamente dependientes. El ERGM valuado con términos exclusivamente diádico-independientes (sum, nodematch, absdiff, nodecov) trata las 11.781 díadas como observaciones; el N efectivo, sin embargo, está mucho más cerca de las **528 iniciativas** que de las 11.781 díadas. Esto explica los $z$ de 17--60 en la tabla principal de M1 — magnitudes implausibles en cualquier red social real — y contamina también los ERGMs por comisión y, aguas abajo, la $\tilde W$ de M3. Nótese además que, sin términos endógenos (transitividad, distribución de grados), el "ERGM" es matemáticamente una regresión de Poisson sobre díadas: el nombre promete una estructura que el modelo no usa.

**Qué haría yo.** Tres opciones en orden de preferencia: (1) modelar la red **bipartita** delegado × iniciativa directamente (la Figura F2 del reporte ya la dibuja; el modelo debe seguir a la figura) — ERGM bipartito o, más simple y robusto, un modelo de elección discreta: cada delegado decide firmar o no cada iniciativa, con efectos fijos por iniciativa y errores agrupados por iniciativa; la homofilia entra como distancia delegado–grupo firmante. (2) Mantener la proyección pero con inferencia honesta: bootstrap **por iniciativa** (remuestrear iniciativas, no díadas) o MRQAP. (3) Como mínimo, un test de placebo: reasignar aleatoriamente los conjuntos de firmantes preservando tamaños y volumen por delegado (modelo de configuración bipartito) y mostrar dónde caen los estadísticos observados respecto de ese nulo — en el espíritu del ejercicio de permutación que el propio Fábrega (2022, §VII) usó para los copatrocinios de julio 2021.

## D2. La categoría "Independiente" no es un grupo: es la ausencia de un dato

**Intuición.** Dos tercios de los convencionales están codificados con la misma etiqueta de afiliación: "Independiente". Cuando el modelo encuentra que "los de igual afiliación co-firman más", en buena parte está diciendo que *los independientes co-firman con independientes* — lo cual es casi inevitable si son 102 de 154. El coeficiente estrella de M1 descansa sobre una categoría que no distingue a un militante de la Lista del Pueblo de un ex-ministro de derecha electo como independiente.

**Detalle técnico.** `nodematch(afiliación)` con distribución \{Independiente: 102, resto: partidos chicos\} es dominado por el bloque residual: la mayoría de las díadas "match" son Independiente–Independiente, y ese "grupo" mezcla izquierda radical, centroizquierda y derecha (Arrau y Cozzi comparten celda con la Lista del Pueblo). El clivaje operativo de la Convención no era la militancia sino la **lista/pacto electoral**, exactamente como lo tabula Fábrega (2022, Tabla 1): Vamos por Chile (37), Apruebo Dignidad (28), Lista del Pueblo (27), Lista del Apruebo (25), PPOO (17), INN (11), Otros (10) — siete categorías políticamente interpretables que cubren el roster completo, sin residuo. La auditoría de perfiles del propio proyecto ya **recolectó `lista_electoral`** y no la está usando: es la variable correcta esperando en el cajón.

**Qué haría yo.** Reemplazar (o al menos acompañar) `nodematch(afiliación)` por `nodematch(lista_electoral)` con las 7 categorías de Fábrega; reportar ambas para mostrar cuánto del +0.577 era artefacto del residuo. Bono: con listas, la homofilia se puede descomponer en intra-bloque (izquierda con izquierda) vs. intra-lista, que es la pregunta comparada interesante (¿las listas ad hoc funcionaron como partidos?).

## D3. El modelo de formación omite la variable que ya sabemos que forma los grupos: la ideología

**Intuición.** El propio coautor del proyecto demostró en 2022 que los grupos de copatrocinio de la Convención se forman a menor distancia ideológica que el azar — inequívocamente. El M1 actual modela la formación de la red con género, edad, profesión y educación... pero sin ideología. Un revisor que conozca ese paper preguntará en la primera página: ¿dónde está $|\theta_i - \theta_j|$?

**Detalle técnico.** Fábrega (2022, §VII) muestra con 10.000 permutaciones por grupo que la distancia ideológica media entre co-firmantes es sistemáticamente menor que la de grupos aleatorios del mismo tamaño. Omitir `absdiff`($\theta$) en M1 tiene dos costos: (i) los coeficientes de homofilia demográfica quedan sesgados por variable omitida — abogados y experimentados pueden co-firmar entre sí simplemente porque están cerca en el espacio ideológico (la derecha de la Convención concentra abogados y ex-autoridades), de modo que el "vuelco de H1b" podría reflejar sorting ideológico, no afinidad profesional; (ii) se renuncia a la pregunta más informativa: ¿cuánta estructura queda **después** de la ideología? Ese residuo (lo que la ideología no explica) es donde viven el gatekeeping, la pericia y el capital social — el aporte diferencial del proyecto sobre Fábrega (2022).

**Qué haría yo.** Añadir `absdiff`($\theta_{dim1}$) — y `absdiff`($\theta_{dim2}$), ver D5 — a todas las especificaciones de M1, con los $\theta$ del primer mes (ver D4) para que la ideología sea pre-red. Reportar M1 con y sin ideología como par principal: "la homofilia profesional sobrevive/no sobrevive a la distancia ideológica" es un titular en cualquiera de las dos direcciones.

## D4. El estudio contradice la advertencia metodológica de su propio antecedente: qué mide $\theta$ después de agosto de 2021

**Intuición.** Fábrega (2022) usa deliberadamente *solo el primer mes* de votaciones para estimar ideología, y explica por qué: después, las votaciones se contaminan de estrategia — quórums de 2/3, reglas de insistencia, disciplina de grupo emergente. El estudio actual estima $\theta$ dinámico sobre los 12 meses y trata sus movimientos como cambios de *preferencia* (la variable dependiente de M2). Pero según el antecedente del propio equipo, buena parte de ese movimiento tardío es comportamiento estratégico bajo reglas supermayoritarias, no convicción.

**Detalle técnico.** El dynIRT (91 períodos, jul-2021 a jun-2022) toma el 79% de su información de feb--jun 2022 (3.708 de 4.707 roll-calls, ya documentado en el reporte), exactamente la fase que Fábrega (2022, §I y §III) describe como estratégicamente compleja ("comisiones, subcomisiones, debates de primera o segunda instancia, procesos de insistencia, quórums supramayoritarios... requieren una consideración más detallada del comportamiento estratégico"). Esto redefine qué es $\Delta\theta_{i,t}$ en M2: bajo quórum de 2/3, un delegado de izquierda que modera su voto para sumar al centro *se mueve en $\theta$ sin cambiar de preferencia*. La ironía es que esto no debilita el hallazgo de M2 — el nulo bajo efectos fijos — sino su *interpretación*: "la exposición no mueve las preferencias" debería ser "la exposición no mueve el comportamiento de voto revelado", y el test de falsificación con *lead* (la exposición futura correlaciona con el cambio pasado) es tan consistente con selección como con coordinación estratégica anticipada dentro de bloques que negocian paquetes de votos.

**Qué haría yo.** (1) Fijar la ideología *pre-red* con la estimación del primer mes (los datos de Fábrega están publicados: github.com/jfabregalacoa/rcp_convencion) y usarla como covariable de M1/M3 — inmune a la contaminación estratégica. (2) En M2, re-etiquetar el constructo (comportamiento de voto revelado, no "ideología") y añadir una robustez que excluya las votaciones bajo quórum de 2/3 o que distinga votaciones en general vs. en particular. (3) Citar la tensión explícitamente: es más elegante anticiparla que recibirla de un árbitro.

## D5. La Convención tenía dos dimensiones y el estudio mira solo una — justo donde ocurre la acción

**Intuición.** El eje izquierda–derecha ordena bien a la Convención completa, pero la Convención era dos tercios izquierda: la política real ocurrió *dentro* de la izquierda — entre la Lista del Pueblo, Apruebo Dignidad y la ex-Concertación. Fábrega (2022) encontró que esa diferenciación interna vive en una segunda dimensión (plurinacionalidad, pueblos originarios, anti-establishment). Un $\theta$ unidimensional ve a toda esa mitad del órgano como un bloque homogéneo, y entonces ni la homofilia ni la "influencia" pueden medirse bien donde más varianza política hay.

**Detalle técnico.** Fábrega (2022, §V--VI): la 1ª dimensión predice el 89% de los votos, pero la 2ª "muestra dispersión significativa en la izquierda" y es individualmente significativa en las preguntas de vivienda, pueblos originarios, plurinacionalidad y quórums (Tabla A.1); el test de Pillai da 0.63 para la 2ª dimensión — no es ruido. Consecuencias: (i) en M2, la exposición se computa sobre $\theta$-1D; si la convergencia/divergencia entre co-firmantes de izquierda ocurre en la dimensión plurinacional, el modelo la proyecta a cero → sesgo hacia el nulo; (ii) en M1, `absdiff`($\theta_1$) — cuando se añada, D3 — subestimará la homofilia ideológica dentro de la izquierda; (iii) el hallazgo heterogéneo de H1b (negativa solo en C1/C3) podría reflejar que en las comisiones de arquitectura del poder el conflicto corría por la dimensión 2 (establishment vs. anti-establishment), que el modelo no ve.

**Qué haría yo.** Re-estimar puntos ideales en 2D (el pipeline emIRT lo permite; o usar directamente los 2D de Fábrega para el primer mes) y: añadir `absdiff` en ambas dimensiones a M1; construir la exposición de M2 como vector 2D (o distancia euclidiana en el plano); y describir la red bipartita F2 coloreando por dim2 dentro de la izquierda. Mi expectativa: la dimensión 2 reorganiza de manera visible los resultados intra-izquierda.

## D6. El "éxito se derrama por la red" tiene un rival institucional que el estudio aún no descarta: la regla de los 2/3

**Intuición.** En la Convención, un artículo sobrevivía si juntaba dos tercios del Pleno. Fábrega (2022, Fig. 2) muestra que ese umbral se alcanzaba juntando a la izquierda y el centro, sin la derecha. Entonces hay una explicación aburrida y poderosa para el $\rho = 0.94$ de M3: mis co-firmantes y yo tenemos éxito juntos *porque estamos del mismo lado del pívot de 2/3*, no porque el éxito viaje por los lazos. Mientras esa explicación no esté sobre la mesa y controlada, el hallazgo de red no está identificado.

**Detalle técnico.** El SDM interpreta $\rho$ como interdependencia de resultados sobre la topología de $\tilde W$. Pero el DGP institucional es espacial-pivotal (Shepsle & Weingast 1981, citados por Fábrega): $P(\text{sobrevivir}) = f(\text{posición del artículo respecto del pívot de 2/3})$, y como la red de co-firma es fuertemente homofílica en ideología (D3), $\tilde W$ está correlacionada con "mismo lado del pívot" *por construcción*. Señales de esto en las propias tablas del reporte: los efectos directos individuales se disipan y el término espacial significativo más interpretable es `lag.`$\theta$ medio ($+0.093$, $p<10^{-4}$): el éxito sube con la *ideología media de tus co-firmantes* — exactamente lo que predice la historia pivotal (co-firmantes a la izquierda del pívot $\Rightarrow$ artículos que alcanzan 2/3). A esto se suma el derrame mecánico ya reconocido en el reporte (co-firmantes comparten los artículos que componen $y'$), que también infla $\rho$.

**Qué haría yo.** (1) Añadir a $X$ la distancia del delegado (y del artículo, vía media de firmantes) al **pívot de 2/3** ($\theta_{(103)}$ en el orden de 154) y una partición por bloques; si $\rho$ colapsa, la historia es pivotal; si sobrevive, el argumento de red queda mucho más fuerte. (2) Pasar el análisis principal al **nivel artículo** (supervivencia de los 1.809 con desenlace conocido — el M4 ya previsto), donde se puede separar posición espacial del artículo, tamaño y composición de su coalición firmante, y centralidad de sus autores; el nivel delegado con $y'$ promediado debería quedar como estadística descriptiva, no como el modelo causal. (3) Mientras tanto, reencuadrar M3: "el éxito está estructurado en coaliciones" (defendible hoy) en lugar de "el éxito se derrama por lazos" (aún no identificado).

## D7. Un resultado nulo necesita mostrar su potencia; y la "exposición" tiene un problema de espejo

**Intuición.** M2 concluye "selección, no influencia" a partir de un coeficiente nulo. Pero un nulo solo informa si el diseño tenía capacidad real de detectar el efecto: si la exposición casi no cambia dentro de cada persona (las redes acumuladas apenas se mueven entre ondas), el test estaba condenado al cero desde el inicio. Y hay un segundo problema clásico: la "exposición" es el promedio de los $\theta$ de tus co-firmantes, que se estiman con los mismos votos que el tuyo — el espejo se mira al espejo.

**Detalle técnico.** Tres frentes. (i) *Poder*: reportar la desviación estándar within de $\text{NetExp}_{i,t}$ (con redes acumuladas y C5 plana, sospecho que es una fracción pequeña de la between) y el efecto mínimo detectable al 80% de poder; si el MDE es mayor que cualquier efecto plausible de influencia en 2--4 semanas, el titular correcto es "no identificable a esta frecuencia", no "no hay influencia". (ii) *Reflexión* (Manski 1993): $\theta_{j,t}$ y $\theta_{i,t}$ se estiman de las mismas votaciones plenarias; shocks comunes por bloque (una votación polarizante mueve a todo un sector) inducen correlación mecánica entre exposición y $\Delta\theta$ que los efectos fijos individuales no absorben — absorben heterogeneidad estable, no shocks tiempo-bloque. Añadir efectos fijos onda×bloque, o al menos onda. (iii) *Error de medición*: sin errores estándar del dynIRT (P10 del propio reporte), $\Delta\theta$ es ruidoso y el coeficiente se atenúa hacia cero; el prior de caminata aleatoria ($\omega^2 = 0.025$) además *suaviza* $\theta$ por construcción, comprimiendo la varianza que M2 intenta explicar. Un mínimo honesto: sensibilidad a $\omega^2$ y una nota de atenuación.

**Qué haría yo.** Publicar el nulo — es creíble y consistente con la literatura de socialización adulta — pero blindado: MDE explícito, FE de onda(×bloque), sensibilidad a $\omega^2$, y el reencuadre de D4 (comportamiento revelado). Así el nulo pasa de "no encontramos" a "no está, y teníamos cómo verlo".

**Respuesta del autor (2026-07-08) y ajuste del revisor.** El autor propone concentrar el análisis en la ventana de votación densa, con esta preocupación: el panel actual trata como pasos equivalentes a dos ondas separadas por dos semanas y a dos separadas por un día, cuando la información (y la oportunidad de "moverse") difiere. El revisor coincide plenamente y lo convierte en prescripción: (i) **robustez de ventana densa** — re-estimar M2 solo con ondas de feb--may 2022, donde los períodos emIRT entre ondas están bien identificados (79% de los roll-calls) y los intervalos son cortos y homogéneos; (ii) el control por días transcurridos ya existente no basta, porque corrige la *magnitud* esperada de $\Delta\theta$ pero no la *identificación desigual* de $\theta$ entre períodos ralos y densos; (iii) idealmente, ponderar cada transición por la información disponible (nº de roll-calls entre las dos ondas) o excluir transiciones con menos de $k$ votaciones intermedias. Con eso, el nulo queda testeado donde el instrumento de medición realmente ve.

## D8. "Tabula rasa" y "firma costosa": dos premisas del marco que la propia evidencia matiza

**Intuición.** El encuadre dice que los convencionales eran extraños sin estructuras previas y que cada firma era un recurso escaso. Pero llegaron electos en *listas* — 79 listas, y las relevantes funcionaron como proto-partidos con marca, negociación de cupos y campaña conjunta — y un convencional podía firmar cuantas iniciativas quisiera: lo escaso era conseguir 8 firmas para *tu* iniciativa, no gastar la tuya. Las dos premisas están sobrevendidas, y son reparables sin drama.

**Detalle técnico.** (i) Fábrega (2022, §II) documenta las listas y su estructura; los conglomerados (LdP, INN) coordinaban antes del 4 de julio. La "tabula rasa" defendible es institucional (sin reglamento, sin comités, sin seniority), no relacional-completa. El propio dato del proyecto lo confirma: la homofilia por lista/afiliación es el término más grande de M1 — si fuera tabula rasa relacional, no habría nada que "matchear" el día uno. (ii) El costo de la firma: la restricción 8--16 opera del lado de la *demanda* de la iniciativa; del lado del delegado no hay presupuesto. La distribución de firmas por delegado (grado ponderado máx. 76 sobre 528 iniciativas) permitiría mostrar si existe *de facto* un costo (tiempo, reputación) — hay delegados que firman poco y selectivamente vs. firmantes seriales. Ese contraste es teóricamente interesante en sí mismo (position-taking vs. compromiso; cf. Kessler & Krehbiel 1996) y hoy está invisible.

**Qué haría yo.** Reescribir §1.1 del reporte: tabula rasa *institucional* + estructura relacional embrionaria (listas) como punto de partida explícito — lo que además convierte a las listas en la categoría natural de homofilia (D2). Añadir al reporte la distribución de firmas por delegado, y considerar `nodecov(propensión a firmar)` o efectos de actividad en el modelo bipartito (D1), que separan sociabilidad de selectividad.

**Respuesta del autor (2026-07-08) y nombre del fenómeno.** El autor acepta el matiz pero re-centra el argumento en algo mejor: aun con listas y firmas gratuitas, este es el caso más cercano posible a un experimento natural, **porque la Convención se disolvía sin reelección ni reencuentro** — a diferencia de toda legislatura ordinaria, el costo futuro del comportamiento presente era mínimo. El revisor suscribe y le pone nombre: es la **"sombra del futuro"** (*shadow of the future*, Axelrod 1984) — la cooperación y la disciplina en cuerpos colegiados se sostienen porque el juego es repetido (teorema folk); cuando el juego es de **una sola ronda**, desaparece el castigo futuro y aparecen los **efectos de última jugada** (*end-game / last-period effects*), emparentados con la literatura de *term limits* y *lame ducks* (legisladores sin incentivo de reelección se comportan sistemáticamente distinto). La Convención es un cuerpo en "último período desde el día uno": eso — más que la ausencia de lazos — es lo que la vuelve un laboratorio limpio de preferencias. Este reencuadre debe entrar al marco teórico (§1.1 y §1.3 del reporte del estudio). La distribución de firmas está en la Parte IV (IV.D8).

## D9. El gatekeeping se está midiendo con la herramienta equivocada, y su historia se reescribió después de ver los datos

**Intuición.** La teoría de brokerage dice que los actores con recursos valiosos se ubican en los *huecos* de la red — entre grupos que no se hablan. Eso es una propiedad de la posición estructural de cada ego, no de si dos abogados se firman entre sí. Medir gatekeeping con homofilia negativa es como medir si alguien es puente contando cuántos amigos suyos se le parecen. Además, la narrativa pasó de "gatekeeping general" (v1) a "gatekeeping solo en C1/C3" (v2) después de ver los resultados — eso, sin marcarlo como exploratorio, es contar la historia hacia atrás.

**Detalle técnico.** (i) Operacionalización: Burt (1992) se testea con *constraint*, tamaño efectivo, o betweenness del ego — no con `nodematch`. Un abogado puede tener homofilia positiva (firma con otros abogados) *y* ser broker (conecta bloques). Las dos cosas se confunden en el término actual. Con la red ya construida: regresar constraint/betweenness sobre abogado/experiencia (con controles) responde H1b como Burt la formuló. (ii) El hallazgo por comisión (negativa en C1/C3) es hoy la parte más interesante de M1, pero nació post hoc; el paper debe: declararlo exploratorio, pre-especificar el test confirmatorio (el diseño de tres niveles ya redactado en el reporte: Wald por comisión + meta-análisis + ERGM multicapa) y, deseablemente, proponer el mecanismo *ex ante* comprobable — p. ej., si el gatekeeping aparece donde los stakes de poder son mayores, debería correlacionar con la saliencia mediática o la tasa de rechazo en el Pleno de cada comisión, no solo con la etiqueta "C1/C3".

## D10. El modelo pooled ignora dónde se conocen las personas: la comisión

**Intuición.** Dos convencionales de la misma comisión se ven todas las semanas, discuten los mismos artículos y firman las mismas iniciativas casi por gravedad. Si además las comisiones tienen colores ideológicos distintos (la F6 del propio reporte lo muestra), parte de lo que el modelo llama "homofilia" es simplemente "compartían sala".

**Matiz del autor y verificación (2026-07-08).** El autor observa que si el ordenamiento ideológico se construyó con votaciones *anteriores* a la conformación de las comisiones, la ideología queda limpia de ese confusor. Verificado: la ventana de Fábrega (4-jul a 12-ago-2021) es **íntegramente anterior** a las comisiones temáticas (creadas con el Reglamento General, publicado el 13-10-2021; inicio del trabajo temático el 18-10-2021). Por tanto: la ideología del primer mes (IV.D3) es exógena a las comisiones; la crítica de este punto queda acotada a la **red** — las iniciativas se firmaron en ene--mar 2022, cuando las comisiones llevaban meses operando — donde `nodematch(comisión)` sigue siendo necesaria.

**Detalle técnico.** M1 pooled carece de `nodematch(comisión)` — la estructura de oportunidad más obvia del caso. Dado que la membresía ya está construida (`commission_membership.csv`, 154/154), añadirla es trivial y probablemente reduce varios nodematch demográficos (las comisiones difieren en composición profesional y de género). En el modelo bipartito (D1) esto se vuelve más limpio aún: efecto "iniciativa de mi comisión" a nivel de la elección de firmar. Relacionado: las covariables curadas por LLM son un aporte metodológico del proyecto (auditoría dual-fuente, validaciones humanas), pero para publicación conviene un párrafo de *análisis de error orientado a coeficientes*: dado el 6.5% de celdas con discrepancia entre fuentes, simular la reasignación de esas celdas y reportar la banda de variación de los nodematch (un "sensitivity to measurement" de dos líneas que desarma la objeción antes de que ocurra).

## D11. "Éxito" es retención léxica en un borrador que la ciudadanía rechazó — hay que decirlo y encuadrarlo — [CERRADA por decisión del autor: éxito $\equiv$ influencia intra-proceso (agenda-setting y drafting power), independiente de la ratificación]

**Intuición.** El estudio mide éxito como cuánto de tu texto sobrevivió en el borrador del 14 de mayo. Pero ese borrador perdió el plebiscito 62/38. Nada de esto invalida la pregunta (influencia *dentro* del órgano), pero un revisor de política comparada lo va a señalar en la primera lectura, y también notará que "retención léxica" premia lo boilerplate y castiga la idea que sobrevive reescrita.

**Detalle técnico.** Dos frentes menores pero visibles. (i) *Encuadre*: definir éxito explícitamente como influencia intra-proceso sobre el texto (agenda-setting y drafting power), independiente de la ratificación; una nota que discuta si los "exitosos" internos fueron los "responsables" del fracaso externo convertiría la debilidad en una pregunta (ver Q7). (ii) *Constructo*: TF-IDF es léxico; el reporte ya corre SBERT como robustez (bien), pero conviene mostrar 2--3 casos cualitativos (artículo retenido verbatim trivial vs. idea retenida reescrita) y, mejor, la variante a nivel artículo del M4 donde el desenlace es supervivencia (binaria/categórica), menos sensible a la métrica textual.

## D12. Los 17 escaños reservados son teóricamente centrales y están estadísticamente invisibles

**Intuición.** La Convención fue el primer órgano constituyente con escaños indígenas reservados y paridad de género. En los modelos actuales, los convencionales de pueblos originarios quedan absorbidos como "independientes" con un distrito especial. Su comportamiento de red (¿enclave o puente?) es de las preguntas más distintivas del caso, y la segunda dimensión de Fábrega (plurinacionalidad) sugiere que su política no corre por el eje izquierda-derecha.

**Detalle técnico.** Mínimo: dummy/nodematch de escaño reservado en M1 y covariable en M3. Deseable: la pregunta Q5 de la Parte III.

---

# Parte II — ¿Qué cambio de perspectiva tomaría para las mismas preguntas?

## P1. Dejar de proyectar: el objeto es bipartito (personas × documentos), y la teoría también

**Intuición.** Todo el pipeline convierte "quién firmó qué" en "quién está conectado con quién", y en esa proyección se pierde la institución: en la Convención no se elige un amigo, se decide *sumarse a un documento*. La figura más informativa del reporte (F2) es bipartita; los modelos deberían serlo también.

*(Confirmación al autor, 2026-07-08: sí — el "modelo de elección discreta" es un **logit condicional** à la McFadden; detalle pedagógico en IV.D1.)*

**Técnico.** RQ1 se convierte en un problema de **ensamblaje de equipos** (Guimerà et al. 2005): cada iniciativa es un equipo de 8--16; las preguntas son de composición (¿equipos de repetidores o de sangre nueva? ¿diversos o homogéneos en qué dimensiones?) y de elección (modelo de utilidad del delegado sobre iniciativas disponibles, estimable como logit condicional con FE de iniciativa). Resuelve D1 de raíz, absorbe D8 (actividad del delegado como margen propio) y da parámetros con interpretación conductual directa ("la probabilidad de firmar cae X% por unidad de distancia ideológica al grupo ya firmante").

## P2. Un solo reloj: modelar firmas y enmiendas como historia de eventos, no como tres modelos

**Intuición.** Hoy M1 (formación) y M2 (dinámica) miran el mismo proceso con dos cámaras distintas y sin hablarse. Pero los datos son una *secuencia fechada de eventos de co-firma* (iniciativas génesis, luego indicaciones). Existe una familia de modelos hecha exactamente para eso.

**Técnico.** Modelos de eventos relacionales (REM, Butts 2008; DyNAM, Stadtfeld & Block 2017) sobre la secuencia {evento = (fecha, conjunto de firmantes, documento)}: permiten estimar simultáneamente homofilia (selección), inercia/repetición (incumbencia de equipo), cierre triádico y efectos de actividad, con el tiempo real como reloj y sin proyección. La versión "hiperevento" (múltiples actores por evento; Lerner & Lomi 2023 sobre hiperREM) calza uno-a-uno con iniciativas de 8--16 firmantes. Esto también rehabilita la promesa original del proyecto (co-evolución tipo SAOM) en una forma computacionalmente viable con 37 ondas y ~700 eventos multi-autor.

## P3. Poner la regla de 2/3 en el centro: de "redes que derraman éxito" a "coaliciones bajo supermayoría"

**Intuición.** La institución más determinante de la Convención era el quórum de 2/3, y hoy no aparece en ningún modelo. La pregunta de éxito (RQ3) formulada como teoría espacial — ¿qué sobrevive cuando necesitas dos tercios? — es más precisa, más chilena y más publicable que la versión "spillover genérico".

**Técnico.** Es, literalmente, la agenda futura que Fábrega (2022, §VIII) deja escrita: usar las métricas para "testear modelos formales en el marco de la teoría espacial del voto que permitan entender el rol estratégico de actores pivotales". Operativamente: estimar la posición espacial de cada *artículo* (por la media/mediana de sus firmantes, o mejor, escalándolo con los votos que recibió), computar distancia al pívot de 2/3, y modelar supervivencia artículo-nivel con esa distancia + composición de la coalición + red de autores (M4). El rol de la red queda identificado como lo que agrega *sobre* la geometría espacial — que es la pregunta bien formada.

## P4. Menos theta, más comportamiento observable: votos y palabras

**Intuición.** "¿La red te influye?" no necesita medirse en un espacio latente suavizado; puede medirse en actos: ¿votaste distinto de tu bloque cuando tus co-firmantes lo hicieron? ¿empezaste a usar las palabras de tus co-autores?

**Técnico.** Dos DVs directamente construibles: (i) *defección de voto*: para cada roll-call, defección = votar contra la mayoría del propio bloque/lista; testear si la tasa de defección conjunta entre co-firmantes excede la esperada (condicional en bloque) — influencia sobre comportamiento sin pasar por $\theta$; (ii) *adopción de lenguaje*: con los `content_snapshot` y las indicaciones fechadas, medir si el vocabulario distintivo de un delegado aparece en indicaciones posteriores de sus co-firmantes (text reuse dirigido y fechado). Esto es CSS en sentido propio y esquiva simultáneamente D4, D5 y D7.

## P5. Convertir el caso en comparación: la Convención 2021--22 contra el Consejo 2023 — [DESCARTADA por el autor: el costo de datos del Consejo 2023 excede el alcance]

**Intuición.** Chile hizo el experimento dos veces, con reglas casi opuestas: una Convención de independientes sin inercia institucional, y un Consejo 2023 dominado por partidos (y por la derecha), con expertos y otro reglamento. La misma tubería de datos aplicada al segundo proceso convertiría cada hallazgo de este proyecto en una comparación institucional dentro del mismo país.

**Técnico.** El pipeline (iniciativas→enmiendas→texto final; puntos ideales; perfiles) es replicable sobre el proceso 2023 con costo marginal. Diseño de dos casos con variación en (a) presencia de partidos, (b) quórum, (c) composición ideológica: si la homofilia de lista domina en 2021 pero la disciplina partidaria domina en 2023, la tesis "las listas ad hoc funcionaron como partidos embrionarios" gana identificación comparada. Para política comparada, este es el salto de "estudio de caso rico" a "diseño".

---

# Parte III — ¿Qué preguntas me haría yo con esta base de datos?

Las siete comisiones con trazabilidad completa (iniciativa → artículo → enmienda fechada y firmada → snapshot textual → desenlace) constituyen, hasta donde alcanzo a ver, el registro más granular disponible de un proceso constituyente. Las preguntas de abajo usan piezas de la base que los modelos actuales no tocan.

## Q1. ¿Quién enmienda a quién — y para ayudar o para desarmar?

**Intuición.** Cada indicación es un acto dirigido: alguien interviene el artículo de otros. Esa red dirigida (enmendador → autores del artículo) no existe todavía en el proyecto, y trae su propio signo: la enmienda que acerca el texto al borrador final es cooperativa; la que lo vacía es hostil.

**Matiz del autor (2026-07-08), aceptado.** "La que lo vacía es hostil" es demasiado directo: quien simpatiza con el espíritu de un artículo puede *recortarlo* precisamente para hacerlo viable ante el 2/3. La clasificación no puede leerse de la intención sino del **resultado y el contexto**: (a) *poda estratégica* — la enmienda reduce el texto y el artículo sobrevive (o aumenta su similitud al borrador final); (b) *vaciamiento* — la enmienda reduce el texto y el artículo muere o pierde su núcleo semántico (similitud SBERT al génesis cae bajo un umbral); (c) como covariable de contexto, la distancia ideológica enmendador–autores separa la poda amiga de la intervención adversaria. La tipología se valida con una muestra codificada a mano.

**Técnico.** Con `history[]` + autores del artículo: red dirigida fechada de ~2.900 indicaciones. El signo se mide con los snapshots ya computados (Δ similitud al estado previo / al final). Preguntas: ¿la hostilidad cruza bloques o disciplina internamente? ¿los "reparadores" (enmiendas que salvan artículos ajenos) ganan centralidad y éxito posterior? Es un paper completo y nadie tiene estos datos.

## Q2. El lavado de ideas: ¿a dónde va el texto de los artículos muertos?

**Intuición.** Murieron 1.450 artículos. ¿Murieron sus ideas? Si frases de un artículo eliminado reaparecen —con otra firma— en un artículo que sí llegó al borrador, alguien perdió el crédito y otro capturó el contenido. Eso es influencia política en estado puro, y es medible con reuso de texto.

**Técnico.** Similitud (n-gramas/embeddings, umbral + validación manual) entre artículos fallidos/eliminados y todo artículo posterior (misma u otra comisión). Grafo de "resurrección": muertos → herederos. Métricas por delegado: tasa de captura (ideas ajenas que adopto) vs. tasa de expropiación (ideas mías que sobreviven sin mí) — una descomposición del éxito de M3 en crédito vs. contenido.

## Q3. La regla 8--16 como microscopio de coaliciones: bunching y equipos mínimos

**Intuición.** Si una iniciativa aparece con exactamente 8 firmas, es un equipo mínimo que apenas cruzó el umbral; con 16, es una demostración de fuerza que llenó el cupo. La distribución de tamaños de firma — y quién puebla cada extremo — cuenta la historia de cómo se administraba el capital político.

**Técnico.** Histograma de firmantes por iniciativa (predicción: masas en 8 y 16); composición ideológica de equipos-8 vs. equipos-16; y el vínculo con desenlace: ¿los equipos mínimos mueren más? Descriptivo + un modelo de supervivencia con tamaño instrumentado por la congestión temporal de la ventana de presentación. Barato, visual, y habla directo a la literatura de coaliciones mínimas ganadoras.

## Q4. El primer órgano constituyente paritario de la historia: ¿cómo colaboran y cuánto cosechan las mujeres? — [DESCARTADA por el autor: fuera del foco relacional/SNA del proyecto]

**Intuición.** La paridad de género de la Convención es un experimento mundial sin precedente, y el estudio la tiene reducida a una covariable. El propio SDM del proyecto asoma algo incómodo: tener co-firmantes mujeres *reduce* el éxito predicho (lag.mujer $-0.175$, $p<0.01$). Eso merece ser pregunta, no control.

**Técnico.** Con paridad forzada en la composición, cualquier desigualdad de resultados es de *proceso*: comparar redes ego (¿las mujeres firman en equipos más grandes/diversos?), acceso a posiciones de intermediación, tasa de supervivencia de artículos por composición de género del equipo (artículo-nivel, M4), y hostilidad de enmiendas recibidas (Q1) por género. Literatura receptora enorme (gender & legislative collaboration: Barnes 2016) con un caso único.

## Q5. Escaños reservados: ¿enclave, bisagra o vanguardia de la segunda dimensión?

**Intuición.** Los 17 convencionales de pueblos originarios podían encapsularse entre sí o tejer hacia afuera. La segunda dimensión de Fábrega sugiere que su agenda (plurinacionalidad) cortaba transversalmente a la izquierda. ¿Su red de firmas refleja el enclave o el puente?

**Técnico.** E-I index y brokerage de los PPOO en la bipartita; éxito de sus artículos (la plurinacionalidad llegó al borrador — ¿con qué coaliciones firmantes?); y posición en dim2 como moderador. Conecta escaños reservados (institución comparada en expansión: Bolivia, Colombia, NZ) con resultados de proceso, no solo de representación descriptiva.

## Q6. Forense del fracaso: ¿qué mata a un artículo? — [ANOTADA para el futuro (M4); no se aborda ahora]

**Intuición.** El proyecto mira a los sobrevivientes; los 1.450 muertos son el grupo de control que nadie interroga. ¿Mueren por extremos, por huérfanos de red, por llegar tarde, o por caer en la comisión equivocada?

**Técnico.** Es el M4 ya previsto, formulado como pregunta propia: supervivencia artículo-onda con riesgo proporcional o discreto, covariables = posición espacial del artículo (P3), tamaño/composición de coalición, centralidad de autores, comisión, timing, y — con Q1 — presión de enmiendas hostiles recibidas. El dataset (`retention_dynamics_locf.csv` + desenlaces) ya existe; falta solo el modelo.

## Q7. ¿De qué color quedó el borrador — y quién lo tiñó?

**Intuición.** Si escalamos ideológicamente cada artículo por quiénes lo firmaron, podemos preguntar cuán a la izquierda del convencional mediano quedó el texto final, artículo por artículo — y qué delegados sistemáticamente corrieron el texto hacia sí. Es la conexión natural entre este proyecto y el plebiscito perdido: ¿el borrador reflejó al pívot de 2/3 o se le escapó?

**Técnico.** Ideología de artículo = agregado de firmantes (o ideal points de artículos vía votos del Pleno, que están en los roll-calls); comparar la distribución de los 498 artículos finales contra mediana y pívot de la Convención; medir "arrastre" individual (cuánto se mueve el texto hacia $\theta_i$ por unidad de participación de i). Cierra el arco: formación (M1) → dinámica (M2) → éxito (M3) → *contenido* — y ofrece la nota comparada sobre por qué el éxito interno pudo fabricar el fracaso externo.

## Q8. Especialistas y generalistas: la división del trabajo constituyente — [DESCARTADA por el autor]

**Intuición.** ¿Conviene concentrar todas tus firmas en tu comisión o repartirlas por todo el texto? La base permite medir el portafolio temático de cada convencional y preguntar cuál estrategia cosechó más texto sobreviviente.

**Técnico.** Entropía del portafolio de firmas por comisión/tema por delegado; asociación con $y'$ y con supervivencia artículo-nivel, controlando actividad total y posición espacial. Dialoga con especialización legislativa (Volden & Wiseman) en un cuerpo sin comités heredados — donde la especialización es puramente electiva.

---

# Cierre — Qué haría primero

Si tuviera que ordenar el trabajo para blindar el paper actual (sin abrir aún las preguntas nuevas): **(1)** D1 + D2 juntas — reestimar M1 como modelo bipartito/elección con lista electoral y clúster por iniciativa; casi toda la tabla de M1 cambia de estatus inferencial con esos dos movimientos. **(2)** D3 + D4 — incorporar la ideología del primer mes (2D si es posible, D5) como covariable pre-red; decide si el vuelco de H1b es sorting ideológico o afinidad profesional. **(3)** D6 — el control pivotal en M3 y el paso del análisis principal al nivel artículo (M4), que además habilita Q6. **(4)** D7 — el kit de honestidad del nulo de M2 (MDE, FE de onda, sensibilidad a $\omega^2$). Todo lo anterior reutiliza datos ya construidos; nada requiere recolección nueva.

Lo que no debe perderse de vista: las debilidades D1--D12 son las de un *paper*; las preguntas Q1--Q8 son las de un *programa*. La base de datos soporta el programa.

---

# Parte IV — Respuesta a la revisión (bitácora en curso)

Esta parte registra, punto por punto, cómo el proyecto responde a la revisión: qué se verificó, qué se ejecutó, qué se diseñó y qué queda en cola. Se irá completando a medida que avance la implementación. Convención: **[Hecho]** = ejecutado y con resultados; **[Diseñado]** = especificado, listo para implementar; **[En cola]**.

## IV.D1 — Del clique proyectado al modelo bipartito **[Hecho: logit condicional + bootstrap por iniciativas; ERGM bipartito = negativo documentado]**

Vamos a re-estimar la formación de la red con dos modelos que respetan la unidad real de observación (la iniciativa), en lugar de las díadas proyectadas. Antes de implementarlos, qué son y por qué responden al problema.

**Qué es un ERGM bipartito.** Un ERGM ordinario modela una red de un solo tipo de nodo (personas) preguntando: ¿qué configuraciones locales (lazos homófilos, triángulos) hacen más probable el grafo observado? Un ERGM **bipartito** modela una red con **dos tipos de nodos** — aquí, convencionales e iniciativas — donde los lazos solo existen *entre* tipos: "el convencional $i$ firmó la iniciativa $a$". La matriz deja de ser $154 \times 154$ y pasa a ser $154 \times 528$, y cada celda es **una** decisión de firma, no un subproducto de un paquete. Los términos cambian de nombre pero la lógica es la misma: efectos de grado para cada modo (¿hay firmantes seriales? ¿iniciativas-imán?), y homofilia vía configuraciones de dos pasos ("dos abogados firmando la misma iniciativa" = *two-star* bipartito sobre atributo). La dependencia dentro de cada iniciativa deja de ser un artefacto: está representada por el nodo-iniciativa mismo.

**Qué es el logit condicional (McFadden).** Más simple todavía: tratar cada iniciativa como un "menú" y modelar la decisión de cada convencional de sumarse o no. La utilidad latente de que $i$ firme $a$ es
$$U_{ia} = \beta_1\, d^{\theta}_{ia} + \beta_2\, d^{lista}_{ia} + \beta_3\, d^{prof}_{ia} + \alpha_a + \varepsilon_{ia},$$
donde $d^{\theta}_{ia} = |\theta_i - \bar\theta_{a}|$ es la distancia del convencional al grupo firmante (y análogos para lista y perfil profesional), y $\alpha_a$ es un **efecto fijo por iniciativa** que absorbe todo lo que hace atractivo al documento en sí (tema, redactor, momento). Con $\alpha_a$ dentro, $\beta$ se identifica solo con la variación *entre convencionales frente a la misma iniciativa* — la comparación correcta — y los errores se agrupan por iniciativa (el acto que genera las observaciones). Es exactamente el mismo insumo de datos (la matriz de firmas), sin proyección y con el N honesto.

**Por qué los dos.** El logit condicional es transparente, rápido y de inferencia estándar — será la especificación principal. El ERGM bipartito añade lo que el logit no ve: dependencias estructurales *entre* firmas (grados, anidamiento), y sirve de robustez estructural. Si ambos cuentan la misma historia de homofilia que el ERGM proyectado, los hallazgos de M1 sobreviven con inferencia defendible; si no, habremos aprendido exactamente cuánto compraba la pseudo-replicación.

**Respuesta a la duda del autor (2026-07-10): ¿se puede meter el efecto fijo de iniciativa en el ERGM persona-persona?** No. En la red proyectada la iniciativa ya no existe como objeto: sus 8--16 firmas quedaron trituradas en $\binom{|S_a|}{2}$ díadas que además se *suman* con las de otras iniciativas en el peso $w_{ij}$ — no hay nada sobre lo cual anclar un $\alpha_a$. Las dos maneras de "controlar por iniciativa" son exactamente los dos modelos elegidos: el **logit condicional** absorbe $\alpha_a$ por strata (equivalente exacto del efecto fijo), y el **ERGM bipartito** representa la iniciativa como nodo y además se estima **condicionando en el grado de cada iniciativa** (`constraints = ~b2degrees`): el tamaño 8--16 de cada coalición se trata como regla, no como resultado — el análogo estructural del strata.

**Resultados del logit condicional (2026-07-10; script `code/15-conditional-logit.R`; 81.312 decisiones = 528 menús $\times$ 154; SE cluster por convencional).** Covariables = distancias/afinidades de $i$ a la coalición $S_a$ (leave-one-out para firmantes):

| Término | $\hat\beta$ | OR | $p$ |
|:---|:-:|:-:|:-:|
| $\lvert\theta_{1,i} - \bar\theta_{1,S_a}\rvert$ | $-3.09$ | 0.05 | $<10^{-70}$ |
| $\lvert\theta_{2,i} - \bar\theta_{2,S_a}\rvert$ | $-1.23$ | 0.29 | $<10^{-22}$ |
| misma comisión (D10) | $+1.21$ | 3.36 | $<10^{-125}$ |
| $\lambda_{lista}$ (media entre conglomerados) | $\approx +1.1$ | $\approx 3$ | ver IV.D2 |
| afinidad abogado ($es\_ab_i \times$ prop. abogados en $S_a$) | $+0.27$ | 1.31 | 0.19 |
| afinidad experiencia | $+0.27$ | 1.32 | 0.27 |
| afinidad mujer | $+0.44$ | 1.56 | 0.010 |
| $\lvert grado_i - \overline{grado}_{S_a}\rvert$ | $-0.08$ | 0.92 | 0.25 |

**La lección de la pseudo-replicación, cuantificada.** El ERGM proyectado (IV.D3) decía que las homofilias de abogado y experiencia "sobrevivían casi intactas" al condicionar en ideología ($p<10^{-50}$). Con la unidad de observación honesta, **ninguna de las dos es distinguible de cero** — eran significancia comprada por el conteo de díadas dentro de cliques. Lo que sí sobrevive con el N honesto: la **ideología** (el mayor efecto del modelo, y $\theta_2$ con efecto propio), la **estructura de oportunidad** (misma comisión, OR 3.4), la **coordinación de lista** (IV.D2) y — único rasgo demográfico en pie — la **afinidad de género** (OR 1.56). El vuelco de H1b queda re-escrito otra vez: ni gatekeeping (v1), ni homofilia profesional robusta (v2 proyectada); en el modelo correcto, el capital profesional *no organiza la co-firma*. *(Con la muestra $\leq 16$ de la decisión D8, la afinidad de abogado repunta a marginal: $+0.37$, $p = 0.060$ — ver la re-corrida en el reporte v3.1.)*

**El ERGM bipartito: resultado negativo.** Tres intentos (bitácora en `code/16-bipartite-ergm.R`): con `constraints = ~b2degrees` el MCMC no mezcla (>1 h en la iteración 2); la parametrización estándar con `gwb1degree + gwb2degree` degenera desde el arranque MPLE ("did not mix at all"); la especificación mínima fue suspendida por el autor a las 3 h. Diagnóstico: con grados de firmante extremadamente sesgados (mediana 42, máx 157) y estadísticas `b1nodematch` de conteos enormes, la superficie de verosimilitud es degenerada en la región de los datos — problema conocido de la familia.

**Bootstrap por iniciativas — reconsideración a petición del autor (2026-07-11) y ejecución.** La evaluación de pertinencia dio un hallazgo que ordena todo: la especificación del ERGM valuado del proyecto (sum + nodematch + absdiff + nodecov, referencia Poisson) es **díado-independiente**, así que su verosimilitud **factoriza sobre las díadas** — el "ERGM" es *exactamente* una regresión de Poisson $w_{ij} \sim \text{Poisson}(\exp(\theta' x_{ij}))$ sobre las 11.781 díadas. Verificado empíricamente (`code/23-ergm-bootstrap-pilot.R`): los coeficientes del glm coinciden con los del MCMLE archivado hasta la 3ª decimal (max $|\Delta| = 0.0017$) — las corridas MCMC de 8 minutos eran innecesarias para esta especificación. Consecuencias:

1. **Tier A (especificación actual): el bootstrap es pertinente y prácticamente gratis.** Re-muestrear las 487 iniciativas con reemplazo, reconstruir $W$ y re-ajustar el glm cuesta **0.02 s por réplica** ($B = 1000$ en ~20 s). Como era gratis, se ejecutó completo (`code/25-ergm-initiative-bootstrap.R`, red $\leq 16$, spec con ideología 2D):

   | Término | $\hat\theta$ | EE Poisson | EE bootstrap | IC 95% percentil | inflación |
   |:---|:-:|:-:|:-:|:-:|:-:|
   | nodematch afiliación | $+0.267$ | 0.012 | 0.047 | $[+0.18, +0.36]$ | $\times 4.0$ |
   | nodematch experiencia | $+0.283$ | 0.014 | 0.046 | $[+0.19, +0.37]$ | $\times 3.3$ |
   | nodematch abogado | $+0.101$ | 0.011 | 0.021 | $[+0.06, +0.14]$ | $\times 1.9$ |
   | nodematch mujer | $+0.091$ | 0.011 | 0.025 | $[+0.04, +0.14]$ | $\times 2.3$ |
   | absdiff grado | $-0.048$ | 0.007 | 0.021 | $[-0.09, -0.01]$ | $\times 2.9$ |
   | absdiff $\theta_1$ | $-2.726$ | 0.024 | 0.091 | $[-2.91, -2.54]$ | $\times 3.7$ |
   | absdiff $\theta_2$ | $-0.380$ | 0.016 | 0.056 | $[-0.49, -0.27]$ | $\times 3.5$ |

   Doble lectura. (i) Los EE honestos son **2--4 veces** los ingenuos — la cuantificación exacta de cuánta precisión fabricaba la pseudo-replicación (los $|z|$ de 30--130 caen a 5--30). (ii) Aun así, **todas las homofilias sustantivas sobreviven** con IC lejos de cero. La aparente tensión con el clogit (donde abogado/experiencia no son significativas) no es contradicción sino **dos estimandos distintos**: la regresión diádica mide co-ocurrencia *marginal* (¿cuánto terminan juntos en iniciativas los pares parecidos?), el clogit mide la decisión *condicional al menú* (¿me sumo a esta coalición dada su composición?). Los pares de abogados co-ocurren más de lo que el azar de iniciativas predice (marginal), pero un abogado no elige coaliciones por su densidad de abogados una vez fijada la iniciativa (condicional). Ambos, con inferencia ahora defendible, van al paper como complementos.

2. **Tier B ("más información de red": términos estructurales como `transitiveweights`)**: al agregar dependencia diádica la factorización se pierde y cada réplica exige MCMC completo. El piloto de timing (`code/23`, ajuste corto con `MCMLE.maxit = 3`) fue **suspendido con el dato ya en la mano**: la fase CD tomó 14 iteraciones y la *primera* iteración MCMLE no terminó en 35 minutos — es decir, un solo ajuste completo (~25 iteraciones) costaría varias horas, y un bootstrap $B = 200$ del orden de **cientos de horas incluso en 8 cores**. Veredicto: el bootstrap estructural es impracticable en esta máquina; la dependencia estructural (repetición, cierre) queda asignada al **RHEM/amorem** de IV.P2, que la maneja por diseño, sin MCMC y sin degeneración.

## IV.D2 — Listas electorales: verificación del mapeo y la pregunta "¿listas como partidos?" **[Hecho: verificación + puntos 1 y 2 ejecutados]**

**Verificación (2026-07-08).** El campo `lista_electoral` de la auditoría dual-fuente está completo: **154/154 con dato desde la BCN** (123 también en Wikipedia; 0 faltantes). La distribución calza *exactamente* con la Tabla 1 de Fábrega (2022) en las categorías grandes: **Vamos por Chile 37/37, Apruebo Dignidad 28/28, Lista del Apruebo 25/25, PPOO 17/17**. El matiz: la BCN registra la **lista local de origen** (74 listas de independientes distritales), no el conglomerado nacional — por eso "Independientes No Neutrales" aparece con 3 (la marca nacional) y 21 convencionales quedan en listas locales ("Independientes por la Nueva Constitución", "Coordinadora Social de Magallanes", etc.) que Fábrega agrupó en P/NN/O. Está materializado en `data/raw/electoral_lists.csv` con `needs_review = SI` en esos 21; el cierre correcto es el **crosswalk del repositorio de replicación de Fábrega** (github.com/jfabregalacoa/rcp_convencion), que ya asignó cada lista local a su conglomerado. La Lista del Pueblo suma 23 + parte de esos 21 (esperado: 26, dado que el roster de 154 excluye a Rojas Vade).

**"¿Las listas ad hoc funcionaron como partidos?" — cómo introducirla al estudio.** Un partido, operacionalmente, hace tres cosas por sus miembros: los ayuda a *coordinar lazos*, los hace *votar juntos*, y hace que esas dos cosas *persistan*. Las tres son medibles y separables de la mera afinidad ideológica:

1. **Coordinación de lazos más allá de la ideología.** En el modelo de formación (logit condicional de IV.D1), incluir simultáneamente
$$U_{ia} = \beta_{\theta}\, |\theta_i - \bar\theta_a| + \lambda_{lista}\, \mathbf{1}\{lista_i = lista\ modal\ de\ a\} + \alpha_a + \cdots$$
Si $\hat\lambda_{lista} > 0$ *condicional en la distancia ideológica*, la lista aporta coordinación que la ideología no explica — el sello partidario. El test comparado: $\hat\lambda_{lista}$ de las listas ad hoc (LdP, INN) vs. el de los pactos de partidos tradicionales (CV, A): "funcionaron como partidos" $\iff \hat\lambda_{LdP} \approx \hat\lambda_{CV}$, con intervalos que se solapan.
2. **Cohesión de voto (unidad).** Para cada lista $\ell$ y votación $v$, el índice de Rice:
$$R_{\ell v} = \frac{|Y_{\ell v} - N_{\ell v}|}{Y_{\ell v} + N_{\ell v}}, \qquad \bar R_{\ell} = \tfrac{1}{V}\sum_v R_{\ell v}.$$
Comparar $\bar R$ de listas ad hoc vs. pactos tradicionales, contra el benchmark de pseudo-listas (grupos aleatorios del mismo tamaño e ideología media, mismo espíritu del test de permutación de Fábrega §VII). Y su **trayectoria**: un partido sostiene $R$ en el tiempo; una etiqueta electoral sin organización lo ve decaer — la serie mensual $\bar R_{\ell,t}$ (jul-2021 a jun-2022) distingue ambos mundos (la fragmentación documentada de la Lista del Pueblo hacia fines de 2021 es la predicción de "no-partido").
3. **Persistencia relacional.** En el modelo de eventos (IV.P2), el efecto de lista sobre la *repetición* de co-firma: los partidos generan colaboración repetida por encima de la homofilia; las etiquetas efímeras, no.

La pregunta entra al paper como una sección propia de M1 ("las listas como partidos embrionarios"), con la tabla $\hat\lambda$ por conglomerado y la serie $\bar R_{\ell,t}$ como figura. El punto 3 (persistencia relacional) espera al modelo de eventos de IV.P2.

**Resultados (2026-07-10; puntos 1 y 2 ejecutados).**

*Punto 1 — coordinación de lazos* (dentro del logit condicional de IV.D1, condicional en distancia ideológica 2D, comisión y afinidades profesionales; `results/tables/M1_lambda_lista.csv`):

| Conglomerado | $\hat\lambda$ | IC 95% |
|:---|:-:|:-:|
| Escaños Reservados PPOO | **1.79** | [1.42, 2.16] |
| Vamos por Chile | **1.28** | [0.95, 1.60] |
| Otras listas locales | 1.08 | [0.78, 1.37] |
| Lista del Pueblo | **0.99** | [0.71, 1.28] |
| Lista del Apruebo | 0.99 | [0.67, 1.30] |
| Apruebo Dignidad | 0.61 | [0.36, 0.85] |
| Independientes No Neutrales | — | (n=3, nunca lista modal) |

La respuesta a "¿funcionaron como partidos?" en el eje *coordinación*: **sí** — el $\hat\lambda$ de la Lista del Pueblo (0.99) es indistinguible del pacto tradicional de centro-izquierda (Lista del Apruebo, 0.99) y su IC se solapa con el de Vamos por Chile (1.28). Los dos extremos son elocuentes: los **PPOO** son el grupo con mayor coordinación interna condicional en ideología (1.79 — consistente con IV.Q5), y **Apruebo Dignidad** — la coalición con partidos *reales* detrás — es la que menos coordinación aporta sobre la ideología (0.61): sus miembros ya están pegados por $\theta$, la etiqueta agrega poco.

*Punto 2 — cohesión de voto* (`code/17-listas-rice.R`; 4.707 roll-calls; benchmark = 500 pseudo-listas del mismo tamaño y vecindad ideológica; figura `results/figures/rice_cohesion_monthly.pdf`):

| Conglomerado | $\bar R$ real | $\bar R$ pseudo | premio | $p$ perm. |
|:---|:-:|:-:|:-:|:-:|
| Vamos por Chile | 0.855 | 0.833 | $+0.023$ | 0.32 |
| Apruebo Dignidad | 0.816 | 0.808 | $+0.008$ | 0.32 |
| Lista del Apruebo | 0.713 | 0.740 | $-0.027$ | 0.91 |
| Lista del Pueblo | 0.873 | 0.858 | $+0.015$ | 0.10 |
| Escaños Reservados PPOO | 0.875 | 0.859 | $+0.015$ | 0.16 |

**Ninguna lista tiene premio de cohesión de voto** sobre su vecindad ideológica — ni siquiera los pactos con partidos reales. La serie mensual añade el matiz dinámico: la Lista del Pueblo se desploma a $R = 0.65$ exactamente en dic-2021/ene-2022 (su fragmentación documentada) y se recompone en la era de normas; la Lista del Apruebo es la menos cohesiva *todo* el proceso (incluso menos que sus pseudo-listas).

**Lectura conjunta, que es la respuesta fina a la pregunta:** las listas ad hoc funcionaron como partidos en el **eje organizacional** (coordinar con quién firmas: $\lambda$ positivo y comparable a los pactos tradicionales) pero **no** en el eje de **disciplina** (votar unidos más allá de lo que la ideología ya predice: premio $\approx 0$ para todas). En la Convención, *ninguna* etiqueta — ad hoc o tradicional — compró disciplina de voto; lo que las etiquetas compraron fue coordinación de patrocinio. Eso es exactamente "partidos embrionarios": el aparato de coordinación aparece antes que el aparato de disciplina.

## IV.D3 — Dos dimensiones: verificación y ERGM con ideología **[Hecho]**

**Verificación de dimensionalidad.** El dynIRT del pipeline (`emIRT`) es **unidimensional** — la respuesta a la duda del autor es que nuestro método actual *no* tiene dos dimensiones. Solución ejecutada: estimación propia en 2D con **W-NOMINATE sobre las votaciones del primer mes** (147 roll-calls, 2021-07-13 a 2021-08-12; Fábrega usa 146 de la misma ventana), solo votos (ver IV.D5), polaridad dim1 = Marinovic. Réplica casi exacta del original: **clasificación correcta 89.4% (dim1) y 91.6% (2D)** vs. 89.25%/91.43% de Fábrega; 154/154 estimados; $r(\theta_1^{fm}, \theta^{dynIRT}) = 0.979$; los signos calzan (Marinovic $+0.79$, Cubillos $+0.80$, Baradit $-0.24$, Atria $-0.34$; Linconao $-0.68$ con $\theta_2 = +0.32$ frente al $\theta_2 \approx -0.93$ de la izquierda tradicional — la segunda dimensión separa a los PPOO del eje clásico, como en la Fig. 3 de Fábrega). Archivo: `data/processed/ideal_points_2d_firstmonth.csv` (script `code/12-ideal-points-2d.R`).

**ERGM actual ± ideología** (script `code/13-review-response-models.R`; se mantiene la especificación proyectada *a sabiendas* de D1 — el objetivo es medir cuánto se mueven las homofilias demográficas al condicionar en ideología):

| Término | Base (sin ideología) | Base + $\lvert\Delta\theta_1\rvert$ + $\lvert\Delta\theta_2\rvert$ | Cambio |
|:---|:-:|:-:|:---|
| sum | $+1.498$ (0.027)\*\*\* | $+2.468$ (0.029)\*\*\* | — |
| nodematch afiliación | $+0.577$ (0.010)\*\*\* | $+0.304$ (0.010)\*\*\* | **cae 47%** |
| nodematch experiencia previa | $+0.435$ (0.011)\*\*\* | $+0.338$ (0.012)\*\*\* | sobrevive (78%) |
| nodematch abogado | $+0.156$ (0.009)\*\*\* | $+0.136$ (0.009)\*\*\* | sobrevive (87%) |
| nodematch mujer | $+0.142$ (0.009)\*\*\* | $+0.082$ (0.009)\*\*\* | cae 42% |
| absdiff edad | $-0.0005$ (n.s.) | $+0.0001$ (n.s.) | — |
| absdiff grado académico | $-0.048$ (0.006)\*\*\* | $-0.052$ (0.006)\*\*\* | estable |
| nodecov edad | $-0.0082$ (0.0003)\*\*\* | $-0.0052$ (0.0003)\*\*\* | cae 37% |
| **absdiff $\theta_1$ (primer mes)** | — | $\mathbf{-1.915}$ (0.015)\*\*\* | el mayor del modelo |
| **absdiff $\theta_2$ (primer mes)** | — | $\mathbf{-0.319}$ (0.012)\*\*\* | la 2ª dim. importa |

*(Tiempos: 478s y 321s en paralelo — 8.0 min de pared. La tabla de arriba corresponde a la corrida pre-filtro; tras la decisión D8 el CSV `results/tables/M1_ergm_ideologia.csv` se regeneró con la red $\leq 16$ — mismos signos, ideología más fuerte: $\theta_1 = -2.73$ — y el bootstrap por iniciativas de IV.D1 provee ahora la inferencia honesta para esta especificación. Nota técnica descubierta en ese ejercicio: por ser díado-independiente, esta especificación factoriza como Poisson diádico y no necesitaba MCMC.)*

**Lectura.** (i) La distancia ideológica es, por un orden de magnitud, **el mayor inhibidor de la co-firma** — exactamente lo que D3 anticipaba desde Fábrega (2022) — y la **segunda dimensión aporta efecto propio** ($-0.32$) por encima de la primera: la homofilia intra-izquierda corre también por el eje plurinacional (valida D5). (ii) El hallazgo importante para el proyecto: las homofilias de **experiencia y abogado sobreviven casi intactas** al condicionamiento ideológico (78% y 87% del coeficiente) — *no* eran sorting ideológico disfrazado; hay afinidad profesional genuina en la co-firma. (iii) La homofilia de afiliación **cae a la mitad**: esa mitad era ideología; la mitad restante es coordinación de grupo más allá de las preferencias — precisamente la cantidad que la pregunta "¿listas como partidos?" (IV.D2) va a medir con la variable correcta (lista, no militancia). (iv) Todo esto con la advertencia de D1 vigente: los EE siguen siendo optimistas; las *magnitudes relativas* y sus cambios son el dato interpretable aquí, no los $p$-valores.

## IV.D4 — Reglas de votación del Pleno: qué dice la normativa **[Investigado]**

Decisiones adoptadas: (1) la ideología del primer mes (IV.D3) entra como covariable pre-red; (3) la tensión con Fábrega (2022) se cita explícitamente y el constructo de M2 se re-etiqueta como **comportamiento de voto revelado**. Para el punto (2) — separar votaciones por quórum — se investigó la normativa con verificación cruzada (dos búsquedas independientes reconciliadas):

**Hallazgos (2026-07-09; dos investigaciones independientes reconciliadas — una sobre los textos legales, otra con lente académico — con los textos oficiales del Diario Oficial contrastados en copias separadas; coincidencias = alta confianza).**

| Decisión | Quórum | Fuente |
|:---|:---|:---|
| Normas constitucionales y su reglamento de votación | **2/3 de los miembros en ejercicio** | Art. 133 inc. 3 CPR (Ley 21.200, D.O. 24-12-2019) |
| Elección de la Mesa (4-jul-2021) | Mayoría absoluta en ejercicio, rondas sucesivas | Art. 133 inc. 2 CPR |
| Pleno provisional (jul--oct 2021, incluye toda la ventana de Fábrega) | **Mayoría — nunca 2/3**; formalización exacta disputada: "mayoría absoluta en ejercicio" (propuesta de la Mesa, prensa 13-07-2021) vs. mayoría simple de facto (afirmación textual de Fábrega 2022) | Normas Básicas 14-07-2021, que **no** contienen cláusula general de quórum |
| Acuerdos generales (Pleno, comisiones) bajo el Reglamento General | Mayoría de los **presentes**, salvo regla especial | Arts. 18--19 RG (D.O. 13-10-2021) |
| Propuestas de normas e indicaciones **en comisión temática** | **Mayoría simple de la comisión** | Art. 92 RG |
| Pleno, votación **en general** de informes | 2/3 en ejercicio (103/154) — aplicado por práctica uniforme desde el 15-02-2022; **no está cifrado en el art. 94**, deriva del art. 133 CPR | Art. 94 RG + art. 133 inc. 3 CPR |
| Pleno, votación **en particular** de cada norma | **2/3 en ejercicio, texto expreso** (103/154) | Art. 96 RG |
| Insistencia (una sola oportunidad por nivel) | En general: devolución a comisión, 15 días para informe de reemplazo, segundo rechazo = definitivo. En particular: devolución **solo si** la norma obtuvo mayoría de los presentes; la segunda propuesta va directa a Tabla; sin 2/3, rechazo definitivo | Arts. 94 inc. 3 y 97 RG |
| Modificación de los arts. 96/97/103 del RG | 2/3 en ejercicio (blindaje); el resto del RG, por mayoría | Art. 103 RG |

*(El plebiscito dirimente — $\geq 3/5$ en segunda votación — quedó reglamentado pero **nunca operó** por falta de reforma constitucional habilitante: sin consecuencias para los datos. Único detalle no verificado en fuente oficial: el cómputo "103 = 2/3 de 154 en ejercicio" tras la vacante de Rojas Vade es uso uniforme de la prensa, no un acto formal localizado — nota al pie en el paper.)*

**Cuatro consecuencias para el estudio.**

1. **La ideología del primer mes es doblemente exógena.** Las 146 votaciones de la ventana de Fábrega (4-jul a 12-ago-2021) operaron **todas** bajo regla de mayoría — el 2/3 solo se volvió operativo en el Pleno el 15-02-2022. Además de ser pre-comisiones (D10), $\theta^{fm}$ está libre del voto estratégico que induce un umbral supermayoritario: refuerza su uso como covariable limpia en IV.D3/D5.
2. **El pívot de IV.D6 queda institucionalmente bien definido:** $\theta_{(103)}$, el estadístico de orden que completa 2/3 de 154, operativo desde el primer informe votado (Sistemas de Justicia, 15--16 feb 2022: 14 de 16 artículos superaron los 103 votos).
3. **La asimetría mayoría-en-comisión / 2/3-en-Pleno es el mecanismo estructural de M4.** Los artículos nacen por mayoría simple (art. 92) y mueren a 103 votos (art. 96); la insistencia da una sola segunda vida, y solo a los que lograron mayoría de presentes (art. 97). Eso crea el rango $[\text{mayoría}, 2/3)$ donde la distancia al pívot decide — exactamente lo que $\gamma_1$ mide. Consecuencia documentada por ambas investigaciones: muchas normas aprobadas cómodamente en comisión caían en el Pleno; lo no consensuado simplemente desaparecía del texto (la "hoja en blanco" por defecto).
4. **Separar por quórum en M2 (el punto 2 de D4) es factible y binario:** antes del 15-02-2022 todo el Pleno es régimen de mayoría; desde entonces, las votaciones de *normas* son 2/3 y las de *procedimiento* siguen por mayoría de presentes (arts. 18--19). El dynIRT de 91 períodos mezcla ambos regímenes: la robustez concreta es etiquetar cada roll-call por régimen y re-estimar sobre feb--jun 2022 (coincide con la extensión opcional de IV.D5).

*Fuentes primarias: Ley 21.200 (D.O. 24-12-2019, CVE 1703893); Reglamento General de la CC (D.O. 13-10-2021, Núm. 43.076, CVE 2024421; copia CEP cotejada); Normas Básicas para el Funcionamiento Provisional (14-07-2021, consolidadas 24-08-2021); Fábrega (2022, RCP); prensa de la primera votación de informes (La Tercera y Pauta, 15--16 feb 2022). Lectura académica conectada: Larraín, Negretto y Voigt (2023) sobre cómo el mecanismo circular norma-por-norma, sin votación final del texto completo, moldeó el borrador.*

**Reglas operativas fijadas por el autor (2026-07-10).** (i) **Ninguna modificación a las estimaciones de posicionamiento político** (dynIRT, W-NOMINATE) se ejecuta sin notificarle antes exactamente qué se hará, revisando el código concreto — la separación por régimen de quórum (consecuencia 4) queda *diseñada pero congelada* hasta esa conversación. (ii) Fuente adicional disponible para fechar votaciones cuando el roll-call no traiga fecha: los excel originales de sesión en `B - Convención Constitucional - Data/A-data-pleno/sesion_##.xls` (muchas sesiones tempranas no tienen votaciones; las últimas concentran la mayoría — consistente con la densidad 21%/79% documentada en M2).

## IV.D5 — En limpio: qué haremos con la medición ideológica **[Decidido]**

**La duda del autor, resuelta:** Fábrega (2022) estimó las dos dimensiones **usando únicamente las votaciones plenarias** (WD-NOMINATE e IDEAL sobre roll-calls); las encuestas a candidatos ("Conoce tu candidato", "Tu Match Constituyente") se usaron *solo como validación externa* de las estimaciones ya hechas (§VI del paper), nunca como insumo. Nuestra réplica (IV.D3) siguió el mismo protocolo: solo votos.

**El plan en limpio:** (i) $\theta_1^{fm}, \theta_2^{fm}$ (primer mes, 2D, pre-red y pre-comisiones) son las covariables ideológicas de **M1** (absdiff) y **M3** (distancia al pívot, IV.D6); (ii) el dynIRT 1D de 91 períodos se conserva para **M2**, re-etiquetado como *voto revelado* y con la robustez de ventana densa (respuesta en D7); (iii) queda como extensión opcional un dynIRT estimado solo sobre feb--jun 2022, donde la identificación es fuerte.

## IV.D6 — Por qué el nivel artículo, y la especificación pivotal **[Test SDM ejecutado; especificación artículo refinada]**

**La intuición que faltaba.** La pregunta de M3 es "¿qué hace que *lo que uno propone* sobreviva?". Pero lo que sobrevive o muere no es el convencional: es el **artículo**. Al promediar los desenlaces de ~12 artículos por delegado en un solo número ($y_i'$), pasan tres cosas malas a la vez: (a) se mezclan artículos con coaliciones firmantes distintas — el "tratamiento" (con quién firmaste) varía artículo a artículo y el promedio lo borra; (b) dos delegados que co-firman comparten los mismos artículos, así que sus $y'$ comparten componentes *por construcción* — este es exactamente el derrame mecánico que infla $\rho$; (c) se desperdicia información: hay 1.809 artículos con desenlace observado y solo 154 promedios. Modelar a nivel artículo deshace (a) y (b) y multiplica (c). El nivel delegado no desaparece: queda como estadística descriptiva y como robustez.

**Especificación propuesta.** Para el artículo $a$ de la comisión $c$, con coalición firmante $S_a$:
$$P(\text{sobrevive}_a) = \Lambda\Big( \alpha_c + \gamma_1\, \underbrace{|\bar\theta_{S_a} - \theta^{pivote}|}_{\text{distancia al pívot de 2/3}} + \gamma_2\, |S_a| + \gamma_3\, \overline{grado}_{S_a} + \gamma_4\, sd(\theta_{S_a}) + X_a'\delta \Big),$$
con $\theta^{pivote} = \theta_{(103)}$ (el estadístico de orden que completa 2/3 de 154), $\bar\theta_{S_a}$ la posición media de la coalición, $sd(\theta_{S_a})$ su heterogeneidad (¿las coaliciones transversales sobreviven más?), $\overline{grado}_{S_a}$ la centralidad media de los firmantes (el efecto de red *neto de* la geometría espacial), $X_a$ controles del artículo (extensión, timing, nº de enmiendas recibidas), y errores agrupados por **iniciativa** de origen. La hipótesis pivotal es $\gamma_1 < 0$; la hipótesis de red del proyecto es $\gamma_3 > 0$ *condicional* en $\gamma_1$ — así quedan por fin en la misma cancha. En el SDM actual (nivel delegado), el gemelo del control es añadir $|\theta_{1,i}^{fm} - \theta^{pivote}|$ a $X$: si $\rho$ colapsa, la historia era pivotal.

**El test SDM, ejecutado (2026-07-10; `code/18-sdm-pivot.R`, `results/tables/M3_sdm_pivot.csv`).** Pívot institucional: $\theta_{1,(103)} = -0.150$ (IV.D4 fija el 103). Resultados:

| Modelo | AIC | $\rho$ | dist. pívot (directo) | W $\times$ dist. pívot (lag) |
|:---|:-:|:-:|:-:|:-:|
| OLS base | $-448.9$ | — | — | — |
| OLS + dist. pívot | $-509.3$ | — | $-0.118$ ($p<10^{-13}$) | — |
| SDM base | $-575.5$ | 0.943 | — | — |
| SDM + dist. pívot | $-594.1$ | **0.915** | $-0.0004$ ($p=0.99$) | $\mathbf{-0.418}$ ($p<10^{-5}$) |

Tres lecturas. (i) **$\rho$ no colapsa** (0.943 $\to$ 0.915): el derrame de red *no era* geometría pivotal disfrazada — ambos canales coexisten, y la teoría pivotal *mejora* el modelo (AIC $-575 \to -594$) sin destronar a la red. (ii) El detalle fino es exactamente **la intuición del autor sobre el contexto de coalición**: la distancia al pívot *propia* pierde toda su fuerza dentro del SDM ($-0.118$ en OLS $\to$ $-0.0004$), pero la distancia al pívot **del entorno de co-firma** ($W \times$ dist. pívot, el término Durbin) es grande y significativa ($-0.42$): no importa dónde estás tú respecto del pívot — importa dónde está *la compañía que firma contigo*. (iii) Esto reencuadra el mecanismo de M3: el "éxito que se derrama" opera, al menos en parte, porque co-firmar con gente cercana al pívot te arrastra hacia textos viables.

**Refinamiento del modelo artículo-nivel: el contexto de red como composición de la coalición (petición del autor, 2026-07-10).** El autor pide que la sobrevivencia dependa explícitamente del *contexto* que rodea al artículo — "si firmo con gente más educada, más experta, mejor conectada, mi artículo vive distinto" — que es lo que el SDM capturaba bien pero a oscuras, comprimido dentro de $\rho$. La versión SNA, sin forzar el aparato: reemplazar el escalar $\overline{grado}_{S_a}$ por un **bloque de composición de la coalición** $C_{S_a}$, y separar sus tres canales teóricos:

$$P(\text{sobrevive}_a) = \Lambda\Big( \alpha_c + \gamma_1 |\bar\theta_{S_a} - \theta^{pivote}| + \gamma_2 |S_a| + \boldsymbol{\gamma_3}' C_{S_a} + \gamma_4\, sd(\theta_{S_a}) + X_a'\delta \Big),$$

$$C_{S_a} = \Big(\underbrace{\overline{grado}_{S_a},\ \overline{betweenness}_{S_a}}_{\text{capital posicional}},\ \underbrace{\overline{constraint}_{S_a}}_{\text{cohesión vs. puentes}},\ \underbrace{\overline{grado\ acad.}_{S_a},\ \overline{exper.}_{S_a}}_{\text{capital humano}},\ \underbrace{densidad(S_a)}_{\text{historia interna}}\Big),$$

donde $densidad(S_a)$ = proporción de pares de $S_a$ que ya habían co-firmado antes de $t_a$ (la "coalición consolidada" del RHEM, IV.P2 — es la misma estadística $sub.rep^{(2)}$ restringida al pasado). Las preguntas quedan separables y todas son SNA de manual: ¿sobrevive el artículo porque sus firmantes están *bien posicionados* (capital posicional), porque son *expertos* (capital humano — el regreso de H1b, ahora en el lugar teóricamente correcto: no en formar lazos sino en ganar con ellos), porque son un *equipo consolidado* (densidad interna), o porque la coalición *puentea* (constraint bajo = coalición de brokers)? El SDM de nivel delegado queda como el techo agregado del mismo mecanismo; este modelo lo abre en piezas interpretables. Nota de disciplina inferencial: los errores se agrupan por iniciativa *y* los $C_{S_a}$ se estandarizan; la colinealidad entre canales se reporta (VIF) y, si es alta, se muestran los canales de a uno.

## IV.D8 — La distribución de firmas **[Hecho]**

![**F9.** (a) Iniciativas firmadas por convencional; (b) firmantes-persona por iniciativa, con la regla 8--16.](../results/figures/signature_distributions.pdf){width=100%}

Dos lecturas y una anomalía útil. (a) **La firma no tiene presupuesto**: mediana 42 iniciativas firmadas por convencional, media 45, máximo **157** (de 528 posibles) — hay firmantes seriales y firmantes selectivos, exactamente la heterogeneidad de "costo revelado" que D8 pedía documentar; esto justifica efectos de actividad en el modelo bipartito. (b) **Bunching en el tope**: la moda es 16 firmantes (108 iniciativas exactamente en el máximo legal) con masa secundaria en 9--10 — el cupo de 16 se llenaba como demostración de fuerza, y el mínimo de 8 se cruzaba con poco margen. La anomalía: **41 iniciativas registran >16 firmantes** (hasta 83) — imposible bajo la regla ICC; son candidatas a artefactos del registro (uniones multi-fuente al construir `initiative_registry`, o documentos de otro régimen de patrocinio). **Queda anotado para auditoría del registro**: decidir si se fusionan, se separan o se excluyen de la red principal.

**El listado completo de las 41 (id (nº de firmantes-persona), agrupadas por comisión):**

- **C1** (2): 94-1 (21); 754-1 (18)
- **C2** (5): 40-2 (28); 21-2 (25); 9-2 (20); 587-2 (18); 652 (18)
- **C3** (3): 151-3 (18); 471-3 (17); 898-3 (17)
- **C4** (9): 954-5 (83); 28-4 (32); 272 (21); 274 (21); 274-4 (21); 681-4 (18); 278 (17); 303 (17); 458-4 (17)
- **C5** (6): 954-5 (83); 315-5 (24); 875-5 (23); 390-5 (22); 790-5 (19); 617-5 (17)
- **C6** (3): 954 (83); 370 (22); 242-6 (17)
- **C7** (13): 295-07 (49); 435-7 (23); 147-7 (20); 24-7 (20); 9-2 (20); 339-07 (19); 429-7 (19); 112 (18); 112-7 (18); 186-7 (18); 483 (18); 518-7 (18); 187-7 (17)

Dos pistas de auditoría saltan del propio listado: (i) la **954** aparece con 83 firmantes *en tres comisiones a la vez* (C4, C5 y C6) y la **9-2** en C2 y C7 — el patrón dominante no es "demasiadas firmas" sino **documentos transversales duplicados por comisión**, probablemente iniciativas cuyo texto tocaba materias de varias comisiones y cuyo registro consolidó la unión de patrocinantes; (ii) hay pares sospechosos de ser el mismo documento con y sin sufijo (272/274/274-4 en C4; 112/112-7 en C7). La hipótesis de trabajo para el arreglo aguas arriba (CPT): dedupe por `icc_id`/`sources` cruzando comisiones antes de contar firmantes.

**Decisión del autor (2026-07-11): las 41 quedan FUERA de todos los análisis** mientras se resuelve la auditoría (regla `MAX_SIGNERS = 16` en `code/00`; el registro CSV se conserva completo y F9 sigue documentando la anomalía; el tope **no** se aplica a las indicaciones, que legítimamente podían llevar >16 firmas — art. 95). El impacto no era cosmético: la red génesis pasa de 7.731 aristas y peso 53.391 a **5.870 aristas y peso 34.857** — las 41 mega-iniciativas fabricaban ~35% del peso total y una cuarta parte de las aristas. Toda la cadena se re-corrió (reporte v3.1); los titulares: la distancia ideológica se *fortalece* (clogit: $-3.09 \to -3.46$; las coaliciones de 83 firmantes eran ideológicamente anchas y diluían el efecto), los $\lambda$ de lista se *emparejan* (VC baja de 1.28 a 1.04; AD sube de 0.61 a 0.78 — todos los conglomerados en $\approx 0.8$--$1.1$ salvo los PPOO en 2.0), la afinidad de abogado repunta a marginal ($p = 0.060$), y M2/M3/P4a/Q5 mantienen sus conclusiones con matices (detalle en el reporte).

## IV.D9 — H1b a la Burt: brokerage sobre atributos **[Hecho]**

**Especificación.** Brokerage del ego medido con la *constraint* de Burt sobre la red génesis-iniciativa ponderada (menor constraint = más agujeros estructurales alrededor del ego):
$$c_i = \sum_{j \in N(i)} \Big(p_{ij} + \sum_{q} p_{iq}\, p_{qj}\Big)^2, \qquad p_{ij} = \frac{w_{ij}}{\sum_k w_{ik}},$$
y, como segunda DV, $\log(1+betweenness_i)$. Regresión sobre abogado y experiencia con controles (género, edad, grado, $|\theta_1^{fm}|$ como extremismo, y efectos fijos de conglomerado de lista); errores HC1. H1b-Burt predice: abogados/experimentados con **menor** constraint y **mayor** betweenness.

**Resultados (N = 154).** Ni abogados ni experimentados son brokers: sobre constraint, $\hat\beta_{abogado} = +0.003$ ($p = 0.40$) y $\hat\beta_{experiencia} = +0.005$ ($p = 0.34$); sobre betweenness, ambos negativos y no significativos. El único predictor robusto es el **extremismo ideológico**: $|\theta_1|$ sube la constraint en $+0.017$ ($p = 0.02$) — es decir, **los moderados ocupan los agujeros estructurales**, no los expertos. Conclusión sustantiva: H1b queda rechazada también en su operacionalización correcta; el brokerage en la Convención fue una propiedad de la *posición ideológica* (estar entre los bloques), no del *capital profesional*. Coherente con el vuelco de M1 y con la lectura pivotal de D6 — y una frase para el paper: *en una asamblea sin partidos, los puentes los hacen los centristas, no los abogados*. Caveat: la red subyacente es la proyección (D1); estos resultados se tratan como descriptivos robustos y se re-validarán sobre el modelo bipartito. Tabla: `results/tables/M1_brokerage.csv`.

## IV.P2 — El modelo longitudinal elegido: eventos relacionales de hiperevento **[Diseñado; software decidido: amorem (CRAN); intro en docs/RHEM-intro.pdf]**

**El menú, en una tabla mental.** Para redes que cambian en el tiempo hay cuatro familias: (1) **TERGM** — fotografías de la red en paneles, modeladas como ERGMs encadenados: hereda intacto el problema D1 (paneles de la proyección) y desecha el tiempo fino; (2) **SAOM/RSiena** — actores que *mantienen* lazos y los revisan entre olas: su objeto es el lazo persistente (amistad), no el **evento** (una firma ocurre y queda en el pasado; no se "disuelve"); además con 3--8 ondas por comisión está al límite; (3) **DyNAM** (Stadtfeld) — eventos de coordinación en tiempo continuo, pariente directo de lo que necesitamos; (4) **REM — modelo de eventos relacionales** (Butts 2008) y su extensión a **hipereventos** (Lerner & Lomi 2023): la secuencia observada es $\{(t_1, S_1), (t_2, S_2), \dots\}$ — en el instante $t_k$ el conjunto de actores $S_k$ (los 8--16 firmantes) protagoniza un evento — y el modelo pregunta *por qué ese conjunto y no otro*.

**Elección: REM de hipereventos, implementado como logit condicional por casos-control.** Es la única familia cuya unidad nativa es exactamente nuestro dato (evento fechado multi-actor, sin proyección ni disolución), y su estimación práctica es una extensión directa del modelo de IV.D1: para cada evento observado $(t_k, S_k)$ se muestrean $m$ conjuntos contrafactuales $\tilde S$ (mismo tamaño, actores en riesgo), y se estima
$$\Pr(S_k \mid \text{historia}_{t_k}) = \frac{\exp\{\beta's(S_k, H_{t_k})\}}{\sum_{\tilde S} \exp\{\beta's(\tilde S, H_{t_k})\}},$$
donde las estadísticas $s(\cdot)$ ahora pueden **depender del pasado**: *repetición* (¿cuántas veces los miembros de $S$ ya co-firmaron?), *cierre* (¿amigos de amigos?), homofilia de lista e ideología, actividad previa. Eso unifica M1 y M2 en un solo reloj: la homofilia es el efecto de los atributos, la selección es el efecto de la historia, y ambos se estiman juntos. Pedagógicamente: *el logit condicional de IV.D1 es este mismo modelo con $\beta_{historia} = 0$* — primero estimamos la versión estática (M1 corregido), luego se liberan las estadísticas históricas (M1+M2 unificados).

**Software — decisión revisada (2026-07-10, a petición del autor).** La sugerencia original de "construcción propia del muestreo + clogit" queda **descartada**: investigación con verificación cruzada (instalación y benchmark locales incluidos) confirmó que el paquete **`amorem`** ("Augmented Modelling of Relational Events"; Richter, Boschi, Wit & Lembo) **sí está en CRAN** — v1.0.0, publicada el 2026-06-29, checks limpios — y viene del mismo grupo de la literatura RHEM. Soporta hipereventos nativamente (`hyperedge_log`, `hyperedge_features` con `subrep_1/subrep_2/activity`) y estima por caso-control con `rem(method = "clogit")`. Benchmark con la forma exacta de nuestros datos (528 eventos, 8--16 de 154): **41 s en un solo core** con 5 controles/evento; con 50 controles, ~5--10 min secuencial y ~1 min con `mclapply(mc.cores = 8)` sobre los estratos. **No hay ganancia que justifique una implementación paralela propia** — el plan es `amorem` + una capa fina (~100 líneas: muestreador de coaliciones control y covariables de composición), versión fijada con renv y sanity check de `subrep` contra cálculo manual. Introducción pedagógica completa, con las ecuaciones término a término y el mapa de lecturas arXiv: **`docs/RHEM-intro.pdf`**. Plan B si un revisor exige el catálogo completo de estadísticas endógenas: `eventnet` (Java) como preprocesador, mismo estimador.

## IV.P4 — Influencia como comportamiento: defección de voto **[(a) Hecho; (b) abandonado]**

**(a) Defección de voto.** La pregunta "¿votaste distinto de tu bloque cuando tus co-firmantes lo hicieron?" se formaliza así. Para el convencional $i$, lista $\ell(i)$ y votación $v$: $D_{iv} = \mathbf{1}\{voto_{iv} \neq voto\ modal\ de\ \ell(i)\ en\ v\}$ (defección). El modelo:
$$\Pr(D_{iv} = 1) = \Lambda\Big(\eta_i + \mu_v + \phi\, \underbrace{\frac{\sum_{j \neq i} w_{ij}\, D_{jv}}{\sum_{j \neq i} w_{ij}}}_{\text{defección de mis co-firmantes en } v}\Big),$$
con efectos fijos de persona ($\eta_i$: propensión individual a defeccionar) y de votación ($\mu_v$: votaciones que rompen a todos). $\phi > 0$ = mis co-firmantes y yo rompemos disciplina *juntos* — influencia (o coordinación) sobre comportamiento observable, sin pasar por $\theta$. Dos precauciones: la exposición se calcula **leave-one-out** (excluye a $i$), y la inferencia se acompaña de un test de permutación *dentro de lista × votación* (se permutan las identidades de los defectores manteniendo la tasa de defección de la lista en esa votación): si el $\hat\phi$ observado supera el 95% de las permutaciones, la co-defección excede lo mecánico. Insumo: los 4.707 roll-calls ya en `emIRT_data_input.rds` + la lista de IV.D2.

**Resultados (2026-07-10; `code/19-vote-defection.R`, `results/tables/M_defection.csv`).** Tasa base de defección: 7.9% de las celdas persona-votación válidas. Con FE de persona y votación (fixest, SE cluster por convencional): $\hat\phi = 14.0$ (SE 0.86) en la era de normas ($\geq$ 15-feb-2022, $N = 374.044$) y $\hat\phi = 13.8$ en el período completo ($N = 486.021$). El número crudo es enorme pero **engañoso por diseño** — la exposición y la defección comparten el co-movimiento del bloque — y para eso estaba el test de permutación: las 200 permutaciones dentro de bloque × votación (que preservan *exactamente* la tasa de defección de cada lista en cada votación) producen $\phi_{perm}$ con media **6.55** y p95 **6.62**. Es decir: la mitad del efecto crudo es mecánica de bloque, pero el $\hat\phi$ observado (14.0) está a *decenas* de desviaciones del benchmark mecánico ($p_{perm} < 0.005$). **La defección viaja por la red de co-firma**: cuando rompo la disciplina de mi lista, mis co-firmantes génesis rompen la suya en la *misma* votación, mucho más de lo que el co-movimiento de bloques explica. Es la primera evidencia del proyecto de coordinación de *comportamiento* (no de afinidad) que sobrevive un contrafactual duro — y es exactamente el tipo de resultado que D7 pedía para darle contenido positivo al nulo de M2: la red no mueve $\theta$ (posiciones), pero sí mueve *votos* en el margen donde la disciplina se rompe. Magnitud interpretable: pasar de 0% a 10% de co-firmantes defeccionando multiplica las odds propias por $e^{1.40} \approx 4$; el exceso sobre lo mecánico ($\Delta\phi \approx 7.5$) implica un factor propio de red $\approx e^{0.75} \approx 2.1$ por cada 10 puntos de exposición.

**(b) Adopción de lenguaje — [ABANDONADO por decisión del autor, 2026-07-10].** Razón registrada: queda fuera del alcance del estudio y el vínculo de atribución es débil — **nadie observa quién *escribió* un artículo**, solo quién lo firmó; el texto pudo redactarlo un asesor, por lo que "el lenguaje de $j$ se movió hacia el de $i$" no identifica influencia interpersonal entre convencionales. El diseño dif-en-dif queda archivado arriba por si un trabajo futuro (con datos de autoría de redacción) lo rehabilita.

## IV.Q2 — El lavado de ideas: diseño **[NO SE EJECUTARÁ en este estudio — decisión del autor, 2026-07-10: buena idea, pero diversificaría demasiado el paper; queda documentada para un trabajo posterior]**

**Objeto.** Grafo dirigido de "resurrección": $d \to a$ si el artículo muerto $d$ (fallido/eliminado, con fecha de muerte $t_d$ = su última aparición) y un texto posterior $a$ (artículo o indicación con $t_a > t_d$) comparten contenido por encima de un umbral: $sim(d, a) > \tau$ con autores esencialmente distintos ($|S_d \cap S_a| / |S_d| < 0.5$, para excluir el auto-reciclaje legítimo).

**Medición.** Doble filtro: candidatos por TF-IDF de caracteres (3--5-gramas, robusto a ediciones menores; producto punto disperso sobre $1.450 \times \sim 2.900$ pares — trivial computacionalmente) y confirmación por SBERT (semántica) + validación manual de una muestra estratificada. El umbral $\tau$ se calibra con los baselines que ya tenemos: pares alineados verdaderos (0.554 de media) vs. barajados (0.032) — $\tau$ se fija donde la tasa de falsos positivos estimada en pares barajados cae bajo 1%.

**Métricas por convencional.** Tasa de *captura* $C_i = \#\{d \to a: i \in S_a, i \notin S_d\}$ (ideas ajenas que $i$ adopta y hace sobrevivir) y tasa de *expropiación* $E_i = \#\{d \to a: i \in S_d, i \notin S_a\}$ (ideas de $i$ que sobreviven sin él). El cociente $C_i / E_i$ descompone el éxito de M3 en **crédito** (mi texto con mi firma) vs. **contenido** (mi texto, cualquier firma) — y la asimetría entre bloques (¿quién captura de quién?) es la versión textual de la pregunta pivotal.

**Qué concepto de economía política estaría en juego (detalle pedido por el autor para el estudio posterior).** El fenómeno tiene nombre en dos literaturas que convergen. (i) La distinción de **Mayhew (1974)** entre *credit claiming* y *policy making*: un legislador puede querer la *política* o querer el *crédito* por ella. El grafo de resurrección separa empíricamente ambas monedas — algo que casi ningún dato legislativo permite — porque observa el destino del *contenido* independiente del destino de la *firma*. En ese lenguaje, $E_i$ alto con éxito personal bajo describe a un *policy maker* sin *credit claiming* (sus ideas viven, su nombre no), y $C_i$ alto describe al *credit claimer* que cosecha contenido ajeno. (ii) La literatura de **derechos de propiedad sobre las ideas de política** (*legislative entrepreneurship*: Wawro 2000; *policy entrepreneurship*: Kingdon 1984): en una legislatura sin partidos consolidados, los derechos de propiedad sobre una propuesta son débiles — no hay marca partidaria que "registre" la autoría — y la teoría predice exactamente lo que el grafo mediría: **expropiación sistemática de ideas viables desde los proponentes débiles en votos hacia las coaliciones capaces de juntar los 103**. La versión institucional fina: bajo supermajoría, dejar morir un texto y *re-firmarlo* con otra coalición puede ser la única vía de que la idea cruce el pívot — un "lavado" que es socialmente eficiente (la idea sobrevive) pero distributivamente regresivo (el crédito migra hacia el centro pivotal). Esa tensión eficiencia/distribución del crédito — Coase aplicado al mercado de ideas constituyentes, con el pívot de 2/3 como precio de entrada — es la contribución teórica que el estudio posterior podría reclamar.

## IV.Q5 — Escaños reservados: ¿enclave, bisagra o vanguardia de la 2ª dimensión? **[Hecho (1)-(3); el desenlace artículo-nivel espera a M4]**

Tres mediciones encadenadas. (1) **¿Enclave o puente?** El índice E-I de Krackhardt sobre los lazos ponderados de los 17 PPOO: $EI_i = (E_i - I_i)/(E_i + I_i)$ con $E_i$ = peso de lazos hacia no-PPOO e $I_i$ hacia PPOO; $EI \to -1$ enclave, $EI \to +1$ integración; benchmark por permutación (mismo grado, etiquetas al azar). Complemento: constraint de Burt de los PPOO vs. el resto (IV.D9 ya tiene la maquinaria). (2) **¿Con quién tejen?** En el logit condicional (IV.D1), interacción PPOO × distancia ideológica: si los PPOO firman *cruzando* distancias en $\theta_1$ que otros no cruzan, son estructuralmente distintos. (3) **¿Su política corre por la 2ª dimensión?** Repetir (2) con $\theta_2$: la hipótesis fina es que los PPOO son *cercanos en $\theta_2$ y dispersos en $\theta_1$* — puentean el eje izquierda-derecha precisamente porque su coordenada operativa es la plurinacional. El desenlace (¿la plurinacionalidad llegó al borrador con coaliciones firmantes más anchas que el promedio?) se lee en el modelo artículo-nivel de IV.D6 con un dummy de contenido plurinacional. Esto absorbe y responde también D12.

**Resultados (2026-07-10; `code/20-reserved-seats.R` + modelo 2 de `code/15-conditional-logit.R`).**

1. **E-I:** el grupal PPOO es $+0.673$ — en términos absolutos, dos tercios del peso de sus lazos van *hacia afuera* — pero el benchmark de permutación (10.000 barajados de la etiqueta, misma red) espera $+0.891$ [p5: 0.858, p95: 0.917]: los PPOO tienen **mucho más lazo interno que el azar** ($p < 10^{-4}$). E-I individual: mediana 0.41, y *los 17* por debajo de la media permutada. Constraint de Burt: mediana PPOO 0.049 vs. resto 0.051 (Wilcoxon $p = 0.35$) — su posición de intermediación es *normal*.
2. **Interacciones en el logit condicional:** el efecto base PPOO es $-0.79$ ($p = 0.001$: firman *menos* iniciativas ajenas), la interacción con $\lvert\Delta\theta_1\rvert$ es $\mathbf{+1.38}$ ($p = 0.005$) y con $\lvert\Delta\theta_2\rvert$ es $-0.33$ (n.s.). Traducción: para el convencional promedio la pendiente de la distancia ideológica clásica es $-3.22$; para un PPOO es $-3.22 + 1.38 = -1.84$ — **cruzan distancias en $\theta_1$ que nadie más cruza** (casi la mitad de la penalización), mientras su sensibilidad a $\theta_2$ es igual a la de todos. Y recuérdese IV.D2: su $\lambda_{lista}$ interno es el mayor de la Convención (1.79).
3. **El veredicto en una frase: ni enclave ni bisagra — las dos cosas, por diseño.** Los PPOO son el grupo con *mayor* cohesión interna condicional (E-I bajo el azar, $\lambda$ máximo) **y a la vez** el que más largo puentea sobre el eje izquierda-derecha (interacción $+1.38$), con brokerage posicional normal. Es exactamente la firma estructural que predice la hipótesis de la 2ª dimensión: su coordenada operativa no es $\theta_1$, así que el eje clásico no les ordena los aliados — coordinan *dentro* (agenda plurinacional) y contratan *afuera* a cualquier distancia de $\theta_1$ que sirva. La pieza que falta (¿esa estrategia hizo llegar la plurinacionalidad al borrador con coaliciones más anchas?) queda para el modelo artículo-nivel (M4).

## IV.Q7 — ¿De qué color quedó el borrador? **[Diseñado y refinado: serie temporal con dynIRT + rol de los cutting points]**

**Medición del color de un artículo.** Dos vías complementarias: (a) *por firmantes* — $x_a = \bar\theta_{1,S_a}$ (y $\bar\theta_{2,S_a}$), disponible para los 1.809 artículos; (b) *por votos* — para los artículos votados en el Pleno, el punto de corte (cutting point) de su votación estima la posición del artículo directamente de la conducta, independiente de sus autores. La correlación entre (a) y (b) es en sí un resultado (¿los artículos "heredan" la posición de sus firmantes?).

**Las dos preguntas.** (1) *Distribucional*: comparar la densidad de $x_a$ de los 498 artículos del borrador contra la de los 1.311 muertos, y ambas contra dos referencias verticales: la mediana de la Convención y el pívot de 2/3. Predicción pivotal: el borrador se concentra a la izquierda del pívot pero *no* más allá de lo que el pívot tolera; los muertos pueblan las colas. (2) *Individual ("arrastre")*: para cada convencional, $drag_i = \bar x_{(borrador)} - \bar x_{(borrador \setminus \text{artículos de } i)}$ — cuánto movería el color del texto quitar la participación de $i$; un leave-one-out barato que rankea quién *tiñó* el borrador. Cierra el arco del proyecto (formación → dinámica → éxito → **contenido**) y conecta con la pregunta comparada del plebiscito sin salirse del alcance intra-proceso (D11).

**Refinamiento (a): el color del borrador como serie de tiempo (idea del autor, 2026-07-10).** Respuesta primero a la pregunta directa: sí — tal como estaba diseñada, la vía (a) usaba $\theta^{fm}$, las posiciones del **primer mes** (fijas, pre-red). La propuesta del autor la mejora: usando el dynIRT ($\theta_{i,t}$, 91 períodos), se puede medir el color del borrador **en cada momento**, bajo el contrafactual "si la Convención terminara hoy". Formalmente, sea $D_t$ el conjunto de artículos *vivos* en $t$ (la trazabilidad artículo-onda con LOCF ya lo reconstruye — es el mismo objeto de F5/F8) y $x_a(t) = \frac{1}{|S_a|}\sum_{i \in S_a} \theta_{i,t}$ el color del artículo evaluado con las posiciones *de ese momento*. El color del borrador-contrafactual es
$$X_t = \frac{1}{|D_t|} \sum_{a \in D_t} x_a(t),$$
y su cambio entre períodos se descompone de forma exacta (shift-share) en los dos canales que la pregunta quiere separar:
$$\Delta X_t = \underbrace{\Big[X(D_t, \theta_{t-1}) - X(D_{t-1}, \theta_{t-1})\Big]}_{\text{composición: qué textos entran/mueren}} + \underbrace{\Big[X(D_t, \theta_t) - X(D_t, \theta_{t-1})\Big]}_{\text{drift: los firmantes se mueven}}.$$
El término de *composición* es la selección institucional (el pívot matando colas — conecta con IV.D6); el de *drift* es el movimiento de posiciones (el objeto de M2 — y dado el nulo de H2, la predicción es que casi todo $\Delta X_t$ sea composición: **el borrador cambia de color porque cambian los textos, no porque cambien las personas**; si saliera al revés, sería noticia). Dos disciplinas al usarlo: el dynIRT temprano está encogido al prior (densidad de votación 21% pre-feb-2022 — nota de M2), así que la serie se lee con confianza desde feb-2022; y $\theta$ es 1D (ver la respuesta D3 en el chat: para *drift* comparable en el tiempo la base común del dynIRT es la herramienta correcta, al costo de perder $\theta_2$ — el color plurinacional del borrador solo se puede seguir con la variante estática 2D).

**Refinamiento (b): qué es exactamente el cutting point, y por qué la duda del autor es razonable pero no fatal.** En el modelo espacial 1D, cada votación $v$ tiene un **punto de corte** $c_v$: el lugar del eje donde la asamblea se parte (idealmente, todos los $\theta_i < c_v$ votan de un lado y los $\theta_i > c_v$ del otro). La intuición del autor — "es como si el artículo votara una sola vez" — apunta a algo cierto pero al parámetro equivocado: lo que se estima con *una* votación no es la posición de un *votante* (eso sí exigiría muchas votaciones por votante), sino **uno o dos parámetros ($c_v$ y su orientación) con 154 observaciones binarias** — los 154 votos de esa única votación. Es la dualidad votante/votación: los votantes necesitan muchas votaciones; una votación necesita muchos votantes, y tenemos todos. En IRT es explícito: el ítem $v$ tiene discriminación $\beta_v$ y dificultad $\alpha_v$, $\Pr(\text{sí}_{iv}) = \Lambda(\beta_v \theta_i - \alpha_v)$, y el corte es $c_v = \alpha_v / \beta_v$ — estimable por votación con IC por bootstrap, *tomando los $\theta_i$ como dados* (ya estimados con las 4.707). Las limitaciones honestas son otras tres: (i) el corte es una **frontera de coalición**, no la "posición del contenido" — identifica dónde se partió la sala, y leerlo como posición del artículo exige un supuesto sobre el status quo (en la Convención el status quo es "hoja en blanco": no aprobar = no hay norma, lo que hace la lectura *más* limpia que en una legislatura ordinaria, pero hay que decirlo); (ii) **las votaciones aplastantes no identifican nada**: si un artículo pasó 140-8, el corte solo está acotado por una cola — y en la era del 2/3 los artículos que llegaron al borrador tienden precisamente al consenso, así que la vía (b) sufre **selección hacia lo disputado**; (iii) por eso su rol correcto es **validación sobre el subconjunto contestado** (márgenes entre 60-40 y 75-25, digamos), no medida principal: la correlación (a)-(b) se reporta ahí, con la advertencia de selección a la vista. Con (i)-(iii) dichas, la vía (b) deja de ser oscura: es barata, es conducta revelada, y en el subconjunto donde está identificada es el mejor chequeo externo de que $\bar\theta_{S_a}$ no es un artefacto de la autoría.

