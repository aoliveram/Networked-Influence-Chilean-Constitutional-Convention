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

## D8. "Tabula rasa" y "firma costosa": dos premisas del marco que la propia evidencia matiza

**Intuición.** El encuadre dice que los convencionales eran extraños sin estructuras previas y que cada firma era un recurso escaso. Pero llegaron electos en *listas* — 79 listas, y las relevantes funcionaron como proto-partidos con marca, negociación de cupos y campaña conjunta — y un convencional podía firmar cuantas iniciativas quisiera: lo escaso era conseguir 8 firmas para *tu* iniciativa, no gastar la tuya. Las dos premisas están sobrevendidas, y son reparables sin drama.

**Detalle técnico.** (i) Fábrega (2022, §II) documenta las listas y su estructura; los conglomerados (LdP, INN) coordinaban antes del 4 de julio. La "tabula rasa" defendible es institucional (sin reglamento, sin comités, sin seniority), no relacional-completa. El propio dato del proyecto lo confirma: la homofilia por lista/afiliación es el término más grande de M1 — si fuera tabula rasa relacional, no habría nada que "matchear" el día uno. (ii) El costo de la firma: la restricción 8--16 opera del lado de la *demanda* de la iniciativa; del lado del delegado no hay presupuesto. La distribución de firmas por delegado (grado ponderado máx. 76 sobre 528 iniciativas) permitiría mostrar si existe *de facto* un costo (tiempo, reputación) — hay delegados que firman poco y selectivamente vs. firmantes seriales. Ese contraste es teóricamente interesante en sí mismo (position-taking vs. compromiso; cf. Kessler & Krehbiel 1996) y hoy está invisible.

**Qué haría yo.** Reescribir §1.1 del reporte: tabula rasa *institucional* + estructura relacional embrionaria (listas) como punto de partida explícito — lo que además convierte a las listas en la categoría natural de homofilia (D2). Añadir al reporte la distribución de firmas por delegado, y considerar `nodecov(propensión a firmar)` o efectos de actividad en el modelo bipartito (D1), que separan sociabilidad de selectividad.

## D9. El gatekeeping se está midiendo con la herramienta equivocada, y su historia se reescribió después de ver los datos

**Intuición.** La teoría de brokerage dice que los actores con recursos valiosos se ubican en los *huecos* de la red — entre grupos que no se hablan. Eso es una propiedad de la posición estructural de cada ego, no de si dos abogados se firman entre sí. Medir gatekeeping con homofilia negativa es como medir si alguien es puente contando cuántos amigos suyos se le parecen. Además, la narrativa pasó de "gatekeeping general" (v1) a "gatekeeping solo en C1/C3" (v2) después de ver los resultados — eso, sin marcarlo como exploratorio, es contar la historia hacia atrás.

**Detalle técnico.** (i) Operacionalización: Burt (1992) se testea con *constraint*, tamaño efectivo, o betweenness del ego — no con `nodematch`. Un abogado puede tener homofilia positiva (firma con otros abogados) *y* ser broker (conecta bloques). Las dos cosas se confunden en el término actual. Con la red ya construida: regresar constraint/betweenness sobre abogado/experiencia (con controles) responde H1b como Burt la formuló. (ii) El hallazgo por comisión (negativa en C1/C3) es hoy la parte más interesante de M1, pero nació post hoc; el paper debe: declararlo exploratorio, pre-especificar el test confirmatorio (el diseño de tres niveles ya redactado en el reporte: Wald por comisión + meta-análisis + ERGM multicapa) y, deseablemente, proponer el mecanismo *ex ante* comprobable — p. ej., si el gatekeeping aparece donde los stakes de poder son mayores, debería correlacionar con la saliencia mediática o la tasa de rechazo en el Pleno de cada comisión, no solo con la etiqueta "C1/C3".

## D10. El modelo pooled ignora dónde se conocen las personas: la comisión

**Intuición.** Dos convencionales de la misma comisión se ven todas las semanas, discuten los mismos artículos y firman las mismas iniciativas casi por gravedad. Si además las comisiones tienen colores ideológicos distintos (la F6 del propio reporte lo muestra), parte de lo que el modelo llama "homofilia" es simplemente "compartían sala".

**Detalle técnico.** M1 pooled carece de `nodematch(comisión)` — la estructura de oportunidad más obvia del caso. Dado que la membresía ya está construida (`commission_membership.csv`, 154/154), añadirla es trivial y probablemente reduce varios nodematch demográficos (las comisiones difieren en composición profesional y de género). En el modelo bipartito (D1) esto se vuelve más limpio aún: efecto "iniciativa de mi comisión" a nivel de la elección de firmar. Relacionado: las covariables curadas por LLM son un aporte metodológico del proyecto (auditoría dual-fuente, validaciones humanas), pero para publicación conviene un párrafo de *análisis de error orientado a coeficientes*: dado el 6.5% de celdas con discrepancia entre fuentes, simular la reasignación de esas celdas y reportar la banda de variación de los nodematch (un "sensitivity to measurement" de dos líneas que desarma la objeción antes de que ocurra).

## D11. "Éxito" es retención léxica en un borrador que la ciudadanía rechazó — hay que decirlo y encuadrarlo

**Intuición.** El estudio mide éxito como cuánto de tu texto sobrevivió en el borrador del 14 de mayo. Pero ese borrador perdió el plebiscito 62/38. Nada de esto invalida la pregunta (influencia *dentro* del órgano), pero un revisor de política comparada lo va a señalar en la primera lectura, y también notará que "retención léxica" premia lo boilerplate y castiga la idea que sobrevive reescrita.

**Detalle técnico.** Dos frentes menores pero visibles. (i) *Encuadre*: definir éxito explícitamente como influencia intra-proceso sobre el texto (agenda-setting y drafting power), independiente de la ratificación; una nota que discuta si los "exitosos" internos fueron los "responsables" del fracaso externo convertiría la debilidad en una pregunta (ver Q7). (ii) *Constructo*: TF-IDF es léxico; el reporte ya corre SBERT como robustez (bien), pero conviene mostrar 2--3 casos cualitativos (artículo retenido verbatim trivial vs. idea retenida reescrita) y, mejor, la variante a nivel artículo del M4 donde el desenlace es supervivencia (binaria/categórica), menos sensible a la métrica textual.

## D12. Los 17 escaños reservados son teóricamente centrales y están estadísticamente invisibles

**Intuición.** La Convención fue el primer órgano constituyente con escaños indígenas reservados y paridad de género. En los modelos actuales, los convencionales de pueblos originarios quedan absorbidos como "independientes" con un distrito especial. Su comportamiento de red (¿enclave o puente?) es de las preguntas más distintivas del caso, y la segunda dimensión de Fábrega (plurinacionalidad) sugiere que su política no corre por el eje izquierda-derecha.

**Detalle técnico.** Mínimo: dummy/nodematch de escaño reservado en M1 y covariable en M3. Deseable: la pregunta Q5 de la Parte III.

---

# Parte II — ¿Qué cambio de perspectiva tomaría para las mismas preguntas?

## P1. Dejar de proyectar: el objeto es bipartito (personas × documentos), y la teoría también

**Intuición.** Todo el pipeline convierte "quién firmó qué" en "quién está conectado con quién", y en esa proyección se pierde la institución: en la Convención no se elige un amigo, se decide *sumarse a un documento*. La figura más informativa del reporte (F2) es bipartita; los modelos deberían serlo también.

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

## P5. Convertir el caso en comparación: la Convención 2021--22 contra el Consejo 2023

**Intuición.** Chile hizo el experimento dos veces, con reglas casi opuestas: una Convención de independientes sin inercia institucional, y un Consejo 2023 dominado por partidos (y por la derecha), con expertos y otro reglamento. La misma tubería de datos aplicada al segundo proceso convertiría cada hallazgo de este proyecto en una comparación institucional dentro del mismo país.

**Técnico.** El pipeline (iniciativas→enmiendas→texto final; puntos ideales; perfiles) es replicable sobre el proceso 2023 con costo marginal. Diseño de dos casos con variación en (a) presencia de partidos, (b) quórum, (c) composición ideológica: si la homofilia de lista domina en 2021 pero la disciplina partidaria domina en 2023, la tesis "las listas ad hoc funcionaron como partidos embrionarios" gana identificación comparada. Para política comparada, este es el salto de "estudio de caso rico" a "diseño".

---

# Parte III — ¿Qué preguntas me haría yo con esta base de datos?

Las siete comisiones con trazabilidad completa (iniciativa → artículo → enmienda fechada y firmada → snapshot textual → desenlace) constituyen, hasta donde alcanzo a ver, el registro más granular disponible de un proceso constituyente. Las preguntas de abajo usan piezas de la base que los modelos actuales no tocan.

## Q1. ¿Quién enmienda a quién — y para ayudar o para desarmar?

**Intuición.** Cada indicación es un acto dirigido: alguien interviene el artículo de otros. Esa red dirigida (enmendador → autores del artículo) no existe todavía en el proyecto, y trae su propio signo: la enmienda que acerca el texto al borrador final es cooperativa; la que lo vacía es hostil.

**Técnico.** Con `history[]` + autores del artículo: red dirigida fechada de ~2.900 indicaciones. El signo se mide con los snapshots ya computados (Δ similitud al estado previo / al final). Preguntas: ¿la hostilidad cruza bloques o disciplina internamente? ¿los "reparadores" (enmiendas que salvan artículos ajenos) ganan centralidad y éxito posterior? Es un paper completo y nadie tiene estos datos.

## Q2. El lavado de ideas: ¿a dónde va el texto de los artículos muertos?

**Intuición.** Murieron 1.450 artículos. ¿Murieron sus ideas? Si frases de un artículo eliminado reaparecen —con otra firma— en un artículo que sí llegó al borrador, alguien perdió el crédito y otro capturó el contenido. Eso es influencia política en estado puro, y es medible con reuso de texto.

**Técnico.** Similitud (n-gramas/embeddings, umbral + validación manual) entre artículos fallidos/eliminados y todo artículo posterior (misma u otra comisión). Grafo de "resurrección": muertos → herederos. Métricas por delegado: tasa de captura (ideas ajenas que adopto) vs. tasa de expropiación (ideas mías que sobreviven sin mí) — una descomposición del éxito de M3 en crédito vs. contenido.

## Q3. La regla 8--16 como microscopio de coaliciones: bunching y equipos mínimos

**Intuición.** Si una iniciativa aparece con exactamente 8 firmas, es un equipo mínimo que apenas cruzó el umbral; con 16, es una demostración de fuerza que llenó el cupo. La distribución de tamaños de firma — y quién puebla cada extremo — cuenta la historia de cómo se administraba el capital político.

**Técnico.** Histograma de firmantes por iniciativa (predicción: masas en 8 y 16); composición ideológica de equipos-8 vs. equipos-16; y el vínculo con desenlace: ¿los equipos mínimos mueren más? Descriptivo + un modelo de supervivencia con tamaño instrumentado por la congestión temporal de la ventana de presentación. Barato, visual, y habla directo a la literatura de coaliciones mínimas ganadoras.

## Q4. El primer órgano constituyente paritario de la historia: ¿cómo colaboran y cuánto cosechan las mujeres?

**Intuición.** La paridad de género de la Convención es un experimento mundial sin precedente, y el estudio la tiene reducida a una covariable. El propio SDM del proyecto asoma algo incómodo: tener co-firmantes mujeres *reduce* el éxito predicho (lag.mujer $-0.175$, $p<0.01$). Eso merece ser pregunta, no control.

**Técnico.** Con paridad forzada en la composición, cualquier desigualdad de resultados es de *proceso*: comparar redes ego (¿las mujeres firman en equipos más grandes/diversos?), acceso a posiciones de intermediación, tasa de supervivencia de artículos por composición de género del equipo (artículo-nivel, M4), y hostilidad de enmiendas recibidas (Q1) por género. Literatura receptora enorme (gender & legislative collaboration: Barnes 2016) con un caso único.

## Q5. Escaños reservados: ¿enclave, bisagra o vanguardia de la segunda dimensión?

**Intuición.** Los 17 convencionales de pueblos originarios podían encapsularse entre sí o tejer hacia afuera. La segunda dimensión de Fábrega sugiere que su agenda (plurinacionalidad) cortaba transversalmente a la izquierda. ¿Su red de firmas refleja el enclave o el puente?

**Técnico.** E-I index y brokerage de los PPOO en la bipartita; éxito de sus artículos (la plurinacionalidad llegó al borrador — ¿con qué coaliciones firmantes?); y posición en dim2 como moderador. Conecta escaños reservados (institución comparada en expansión: Bolivia, Colombia, NZ) con resultados de proceso, no solo de representación descriptiva.

## Q6. Forense del fracaso: ¿qué mata a un artículo?

**Intuición.** El proyecto mira a los sobrevivientes; los 1.450 muertos son el grupo de control que nadie interroga. ¿Mueren por extremos, por huérfanos de red, por llegar tarde, o por caer en la comisión equivocada?

**Técnico.** Es el M4 ya previsto, formulado como pregunta propia: supervivencia artículo-onda con riesgo proporcional o discreto, covariables = posición espacial del artículo (P3), tamaño/composición de coalición, centralidad de autores, comisión, timing, y — con Q1 — presión de enmiendas hostiles recibidas. El dataset (`retention_dynamics_locf.csv` + desenlaces) ya existe; falta solo el modelo.

## Q7. ¿De qué color quedó el borrador — y quién lo tiñó?

**Intuición.** Si escalamos ideológicamente cada artículo por quiénes lo firmaron, podemos preguntar cuán a la izquierda del convencional mediano quedó el texto final, artículo por artículo — y qué delegados sistemáticamente corrieron el texto hacia sí. Es la conexión natural entre este proyecto y el plebiscito perdido: ¿el borrador reflejó al pívot de 2/3 o se le escapó?

**Técnico.** Ideología de artículo = agregado de firmantes (o ideal points de artículos vía votos del Pleno, que están en los roll-calls); comparar la distribución de los 498 artículos finales contra mediana y pívot de la Convención; medir "arrastre" individual (cuánto se mueve el texto hacia $\theta_i$ por unidad de participación de i). Cierra el arco: formación (M1) → dinámica (M2) → éxito (M3) → *contenido* — y ofrece la nota comparada sobre por qué el éxito interno pudo fabricar el fracaso externo.

## Q8. Especialistas y generalistas: la división del trabajo constituyente

**Intuición.** ¿Conviene concentrar todas tus firmas en tu comisión o repartirlas por todo el texto? La base permite medir el portafolio temático de cada convencional y preguntar cuál estrategia cosechó más texto sobreviviente.

**Técnico.** Entropía del portafolio de firmas por comisión/tema por delegado; asociación con $y'$ y con supervivencia artículo-nivel, controlando actividad total y posición espacial. Dialoga con especialización legislativa (Volden & Wiseman) en un cuerpo sin comités heredados — donde la especialización es puramente electiva.

---

# Cierre — Qué haría primero

Si tuviera que ordenar el trabajo para blindar el paper actual (sin abrir aún las preguntas nuevas): **(1)** D1 + D2 juntas — reestimar M1 como modelo bipartito/elección con lista electoral y clúster por iniciativa; casi toda la tabla de M1 cambia de estatus inferencial con esos dos movimientos. **(2)** D3 + D4 — incorporar la ideología del primer mes (2D si es posible, D5) como covariable pre-red; decide si el vuelco de H1b es sorting ideológico o afinidad profesional. **(3)** D6 — el control pivotal en M3 y el paso del análisis principal al nivel artículo (M4), que además habilita Q6. **(4)** D7 — el kit de honestidad del nulo de M2 (MDE, FE de onda, sensibilidad a $\omega^2$). Todo lo anterior reutiliza datos ya construidos; nada requiere recolección nueva.

Lo que no debe perderse de vista: las debilidades D1--D12 son las de un *paper*; las preguntas Q1--Q8 son las de un *programa*. La base de datos soporta el programa.
