---
title: "ERGM con bootstrap por iniciativas"
subtitle: "Introducción pedagógica para el proyecto Networked Influence — qué hace el bootstrap, por qué responde al problema de los cliques, y en qué se diferencia (y por qué) del logit condicional"
author: "Documento de trabajo del proyecto (M1); acompaña a code/23 y code/25"
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

# 1. El problema que ambos modelos intentan resolver

Nuestros datos de formación de red no son díadas: son **eventos**. Cada iniciativa $a$ es un acto político único en que un conjunto $S_a$ de 8--16 convencionales firmó junto. La red de co-patrocinio que analizamos,

$$w_{ij} = \sum_{a} \mathbf{1}\{i \in S_a \wedge j \in S_a\},$$

es una **proyección**: cada evento de tamaño $|S_a|$ se tritura en $\binom{|S_a|}{2}$ díadas simultáneas (una iniciativa de 16 firmantes fabrica 120), y esas díadas después se suman entre iniciativas. Dos consecuencias:

1. **Las díadas de un mismo evento no son observaciones independientes.** Los 120 pares de la iniciativa de 16 no aportan 120 datos: aportan *uno* (ese grupo firmó ese texto). Cualquier verosimilitud que trate cada celda $w_{ij}$ como independiente — el ERGM valuado incluido — divide la varianza real entre un $N$ inflado y entrega errores estándar de fantasía. Es el diagnóstico D1 de la revisión: los $|z|$ de 30--130 de nuestras tablas v2 eran ese artefacto.
2. **La dependencia no es "por grupos" ordenados sino solapada.** Cada díada $(i,j)$ participa en *muchas* iniciativas, y cada iniciativa toca a *muchas* díadas. No existe una partición de las 11.781 díadas en clusters disjuntos, así que el remedio econométrico estándar — errores agrupados por cluster — **no está bien definido aquí**: ¿a qué cluster pertenece la díada (Atria, Bassa), que co-firmó en 40 iniciativas distintas?

Frente a esto hay dos salidas. Cambiar la unidad de análisis a la decisión de firma (el **logit condicional**, §4), o quedarse con la red proyectada — el objeto clásico de la literatura de co-sponsorship — y reparar su inferencia re-muestreando la unidad que sí es independiente: la iniciativa (el **bootstrap por iniciativas**, §3).

# 2. Qué es exactamente "el ERGM" en nuestro caso

El ERGM valuado del proyecto modela la matriz de pesos completa:

$$P(W = w) = \frac{\exp\{\theta^\top g(w)\}}{\kappa(\theta)} \prod_{i<j} \frac{1}{w_{ij}!},$$

con estadísticas $g(w)$ del tipo sum, nodematch, absdiff y nodecov, y referencia Poisson. La constante $\kappa(\theta)$ suma sobre todas las redes posibles — de ahí, en general, el MCMC.

**El detalle que lo cambia todo:** todas nuestras estadísticas son **díado-independientes** — cada una se escribe como $\sum_{i<j} w_{ij}\, x_{ij}$, donde $x_{ij}$ depende solo de atributos de $i$ y $j$ (¿misma afiliación?, $|edad_i - edad_j|$, ...). En ese caso la verosimilitud **factoriza** celda a celda y el modelo es exactamente

$$w_{ij} \sim \text{Poisson}\big(\exp(\theta^\top x_{ij})\big) \quad \text{independientes},$$

una regresión de Poisson sobre las 11.781 díadas. Lo verificamos empíricamente (`code/23`): los coeficientes del `glm` coinciden con el MCMLE archivado hasta la tercera decimal (max $|\Delta| = 0.0017$). El aparato MCMC no aportaba nada para esta especificación — solo se vuelve necesario con términos **estructurales** (transitividad, grados), que hacen que la probabilidad de un lazo dependa de otros lazos (§7).

Esto deja el diagnóstico en limpio: el problema del ERGM proyectado nunca fue el estimador puntual (es un MLE razonable de tasas de co-ocurrencia), sino su **matriz de varianza**, calculada como si las celdas fueran independientes cuando la iniciativa las fabrica en bloques.

# 3. El bootstrap por iniciativas, en términos matemáticos

## 3.1 La idea: re-muestrear el proceso generador, no las celdas

Pensemos el conjunto de iniciativas como una muestra $\mathcal{A} = \{S_1, \dots, S_m\}$ ($m = 487$) de un proceso de formación de coaliciones $F$ que es el que realmente queremos caracterizar. Todo lo que calculamos es un **funcional de esa muestra**:

$$\hat\theta = T(\mathcal{A}) = \arg\max_\theta \; \ell\big(\theta;\, W(\mathcal{A})\big), \qquad W(\mathcal{A}) = \sum_{a \in \mathcal{A}} \text{clique}(S_a).$$

Término a término: $W(\mathcal{A})$ dice que la red *entera* es una suma de cliques de eventos — la proyección hecha explícita; $T(\cdot)$ es el pipeline completo (construir la red, ajustar la Poisson diádica); $\hat\theta$ es lo que reportamos. La pregunta inferencial correcta es: **¿cuánto variaría $\hat\theta$ si el proceso político hubiera producido otro conjunto de iniciativas del mismo tipo?** — no "¿cuánto variaría si cada celda $w_{ij}$ se re-sorteara Poisson-independiente?", que es lo que responden los EE ingenuos.

El bootstrap responde la pregunta correcta por el **principio plug-in** (Efron): no conocemos $F$, pero la mejor estimación no-paramétrica de $F$ es la distribución empírica $\hat F$ que pone masa $1/m$ en cada iniciativa observada. Entonces:

$$\mathcal{A}^{*(b)} = \{S^*_1, \dots, S^*_m\} \;\overset{iid}{\sim}\; \hat F \quad \text{(re-muestreo con reemplazo)}, \qquad \hat\theta^{*(b)} = T(\mathcal{A}^{*(b)}), \quad b = 1, \dots, B.$$

Cada réplica $b$ es un "mundo contrafactual" en que la Convención produjo $m$ iniciativas sorteadas (con reemplazo) de las reales: algunas aparecen dos veces, otras ninguna. Se reconstruye la red **desde los eventos** y se re-estima todo. La distribución empírica de $\{\hat\theta^{*(1)}, \dots, \hat\theta^{*(B)}\}$ aproxima la distribución muestral de $\hat\theta$ bajo el proceso real:

$$\widehat{\text{EE}}_{boot}(\hat\theta_k) = \sqrt{\tfrac{1}{B-1}\textstyle\sum_b \big(\hat\theta^{*(b)}_k - \bar{\theta}^*_k\big)^2}, \qquad \text{IC}_{95\%} = \big[\hat\theta^{*}_{(0.025)},\; \hat\theta^{*}_{(0.975)}\big]$$

(EE = desviación estándar entre réplicas; IC = percentiles 2.5 y 97.5 de las réplicas — el intervalo percentil, que no exige normalidad ni fórmula de varianza).

## 3.2 Por qué esto responde exactamente al problema de los cliques

- **La unidad de re-muestreo coincide con la unidad de independencia.** Al sortear iniciativas completas, los $\binom{|S_a|}{2}$ pares de cada evento viajan siempre juntos: entran juntos o salen juntos de cada réplica. El bootstrap *nunca* trata las díadas de un clique como información separable — que es precisamente el error de la verosimilitud ingenua. Formalmente, la varianza bootstrap de $w_{ij}$ y la covarianza entre $w_{ij}$ y $w_{kl}$ heredan la estructura del proceso de eventos: dos díadas que comparten iniciativas quedan correlacionadas exactamente en la medida en que las comparten.
- **Resuelve lo que el cluster-robusto no puede.** Como las "pertenencias a cluster" se solapan (una díada vive en muchas iniciativas), no hay agrupación válida de filas; re-muestrear el *generador* (los eventos) es la generalización natural del cluster bootstrap a dependencia solapada.
- **Es agnóstico al modelo de varianza.** No usa la varianza Poisson para nada: si los pesos están sobredispersos, subdispersos o correlacionados por bloques, el bootstrap lo captura, porque solo re-usa los eventos observados.
- **El precio quedó medido.** En nuestros datos los EE honestos son **2 a 4 veces** los ingenuos (`code/25`, $B = 1000$, ~20 s): la afiliación pasa de EE 0.012 a 0.047, la distancia ideológica de 0.024 a 0.091. Los $|z|$ bajan de 30--130 a 5--30 — y aun así **todas las homofilias sustantivas conservan IC lejos de cero**. La corrección era necesaria; los hallazgos la sobreviven.

## 3.3 Qué supone (y qué no)

El supuesto de fondo es que las **iniciativas son intercambiables** como realizaciones del proceso de coalición — la versión evento de "iid". Es un supuesto fuerte pero explícito y criticable: ignora dependencia *entre* iniciativas (la iniciativa de hoy puede responder a la de ayer; los mismos equipos se repiten — exactamente la dependencia histórica que el RHEM modela como objeto de interés). En la práctica eso significa que el bootstrap por iniciativas es honesto sobre la varianza *dentro del proceso observado*, pero no convierte a la red proyectada en un modelo *de* ese proceso secuencial.

# 4. El logit condicional: qué es y por qué se llama "condicional"

El otro camino no repara la proyección: la evita. Cada iniciativa se trata como un **menú** y cada convencional decide sumarse o no:

$$U_{ia} = \beta^\top x_{ia} + \alpha_a + \varepsilon_{ia},$$

con $\alpha_a$ un efecto fijo del documento (su tema, su redactor, su momento, su "atractivo" total) y $x_{ia}$ covariables que varían por par convencional-iniciativa (distancia de $i$ a la coalición, misma comisión, misma lista...).

**"Condicional" tiene aquí un significado matemático preciso**, heredado de McFadden. La verosimilitud no modela la probabilidad *incondicional* de que $i$ firme $a$ (eso exigiría conocer $\alpha_a$), sino la probabilidad del patrón de firmas observado **condicional a cuántos firmaron cada iniciativa**:

$$P\Big(\text{firmaron exactamente } S_a \;\Big|\; \textstyle\sum_i y_{ia} = |S_a|\Big) = \frac{\exp\big(\sum_{i \in S_a} \beta^\top x_{ia}\big)}{\sum\limits_{R \subseteq \{1..154\},\, |R| = |S_a|} \exp\big(\sum_{i \in R} \beta^\top x_{ia}\big)}.$$

Término a término: el numerador es el "puntaje" de la coalición observada; el denominador suma sobre **todas las coaliciones alternativas del mismo tamaño**. Al condicionar en el total $|S_a|$, el término $\alpha_a$ — que es común a todos los candidatos de la iniciativa $a$ — aparece $|S_a|$ veces en numerador y en cada término del denominador, y **se cancela algebraicamente**. Eso es lo que compra la palabra "condicional": elimina *todo* lo que hace atractiva a la iniciativa en sí, sin estimarlo, y $\beta$ queda identificado solo por la comparación *entre convencionales frente al mismo menú*. (En la práctica, `survival::clogit` implementa esta verosimilitud como una parcial de Cox con empates a la Efron.)

# 5. Qué carece el logit condicional

Ser el diseño de identificación más limpio tiene costos, y conviene tenerlos a la vista:

1. **Independencia entre menús.** Condicional a las covariables, las 487 iniciativas se tratan como decisiones independientes. No hay lugar para "estos tres ya firmaron juntos tres veces" ni "los co-firmantes de mis co-firmantes": cero dependencia estructural *entre* eventos. (Esa capa es el RHEM.)
2. **Lo condicionado desaparece del análisis.** El $\alpha_a$ absorbe — y por tanto *prohíbe estudiar* — todo lo que varía a nivel iniciativa: por qué unas atraen 16 firmas y otras 8, qué temas movilizan, el efecto del timing. Para RQ1 eso es una virtud (es el confusor); para otras preguntas es información tirada.
3. **El estimando es conditional-on-the-menu, no estructural de red.** El clogit no dice nada sobre la *forma* de la red resultante (distribución de grados, clustering, componentes); habla de decisiones marginales de adhesión. Un lector SNA puede legítimamente decir que responde una pregunta de micro-elección, no de estructura.
4. **Simultaneidad de la coalición.** Nuestras covariables miden la afinidad de $i$ con la coalición $S_a$ (leave-one-out), como si $i$ decidiera frente a un grupo ya formado. En realidad la coalición se forma *conjuntamente* — nadie es el último en llegar. El leave-one-out mitiga la parte mecánica, pero la lectura causal fina ("me sumo *porque* el grupo es afín") es una aproximación.
5. **Heterogeneidad del firmante no absorbida (en la spec actual).** El strata absorbe la iniciativa, no al convencional: la propensión individual a firmar (mediana 42, máximo 157 — F9a) entra solo vía los EE cluster. Es agregable (dummies de convencional), a costo de interpretación.

# 6. Por qué dan resultados distintos (y por qué eso no es un error)

La comparación empírica del proyecto lo muestra con nitidez: la afinidad de abogados es claramente positiva en la Poisson diádica con bootstrap (IC $[+0.06, +0.14]$) y solo marginal en el clogit ($p = 0.06$); la experiencia es positiva allá y nula acá. No es que uno esté "malo": estiman **cantidades distintas**.

- La **Poisson diádica** estima la asociación **marginal**: ¿cuánto más co-ocurren en iniciativas los pares de abogados que los pares mixtos, *en total, sumando sobre todas las iniciativas*? En ese total entran varios canales a la vez: (i) preferencia de afiliación directa, (ii) composición — hay iniciativas "jurídicas" que reclutan muchos abogados, y toda esa co-ocurrencia dentro de esas iniciativas cuenta —, (iii) actividad correlacionada — si los abogados firman más en general, co-ocurren más por pura exposición.
- El **clogit** estima el efecto **condicional al menú**: fijada la iniciativa (su tema, su atractivo, su tamaño), ¿el convencional abogado se inclina más hacia coaliciones densas en abogados que un no-abogado? El $\alpha_a$ **apaga el canal (ii)** por construcción, y buena parte del (iii).

En forma compacta: si $\alpha_a$ se correlaciona con la composición del menú, entonces

$$\underbrace{\beta_{marginal}}_{\text{Poisson diádica}} \;=\; \underbrace{\beta_{condicional}}_{\text{clogit}} \;+\; \underbrace{\text{sesgo de composición entre iniciativas}}_{\text{lo que } \alpha_a \text{ absorbe}},$$

la misma aritmética del contraste OLS-agrupado vs. efectos-fijos de M2. La lectura sustantiva para el paper: *los abogados terminan juntos en iniciativas más de lo que el azar predice* (hecho de red, marginal), *pero no porque, puestos frente al mismo menú, elijan coaliciones por su densidad de abogados* (mecanismo de elección, condicional): terminan juntos sobre todo porque ciertos textos los convocan en bloque. Los dos números juntos dicen más que cualquiera solo.

# 7. ¿Es el ERGM-boot esencialmente más flexible para agregar términos de red?

**En principio sí; en la práctica, en estos datos, no — y lo medimos.**

- *La promesa.* La gracia del ERGM es que $g(w)$ puede incluir términos **endógenos**: transitividad (`transitiveweights`), heterogeneidad de grados, estrellas — "la red explicando la red". Eso es lo que ninguna regresión diádica ni ningún clogit de menús puede expresar.
- *El costo.* Con cualquier término endógeno la verosimilitud **deja de factorizar** ($\kappa(\theta)$ vuelve a acoplar todas las celdas) y cada ajuste exige MCMC. El bootstrap multiplica ese costo por $B$: nuestro piloto (`code/23`) mostró que un ajuste *corto* (3 iteraciones MCMLE) con `transitiveweights` no completa ni su primera iteración en 35 minutos — un bootstrap estructural de $B = 200$ costaría **cientos de horas** incluso en 8 núcleos. A eso se suma el riesgo documentado de **degeneración** (los tres intentos del ERGM bipartito fallaron por eso), que dentro de un loop de bootstrap se vuelve inmanejable: réplicas que no convergen contaminan silenciosamente la distribución.
- *La flexibilidad que sí es real y barata.* El bootstrap por iniciativas no está casado con el glm: **cualquier funcional de los eventos** se puede re-muestrear con el mismo esquema y costo trivial — centralidades, constraint de Burt, el índice E-I de Q5, la regresión de brokerage de D9. Es una *máquina general de inferencia honesta* para todos los descriptivos de la red proyectada, y esa es probablemente su contribución más útil al paper.
- *El lugar correcto de la dependencia estructural.* Para "más términos de red" en serio (repetición, cierre, actividad endógena), la ruta viable en estos datos no es el ERGM-boot sino el **RHEM** (`docs/RHEM-intro.pdf`): su verosimilitud caso-control estima efectos estructurales *sin* constante de normalización global, sin MCMC y sin degeneración, sobre la unidad nativa (el evento fechado).

# 8. Síntesis comparativa

| Criterio | Poisson diádica + bootstrap por iniciativas | Logit condicional |
|:---|:---|:---|
| Unidad de análisis | díada proyectada (evento como unidad de *inferencia*) | decisión de firma (evento como unidad de *análisis*) |
| Estimando | asociación **marginal** de co-ocurrencia | efecto **condicional al menú** |
| Confusor de iniciativa ($\alpha_a$) | no controlado (entra al estimando) | eliminado por condicionamiento |
| Dependencia intra-evento | corregida en EE (réplicas por evento) | eliminada por diseño (strata) |
| Dependencia entre eventos | supuesta ausente (intercambiabilidad) | supuesta ausente (independencia de menús) |
| Heterogeneidad del firmante | no controlada (sin términos de grado) | no absorbida en spec actual (agregable) |
| Objeto SNA (estructura global) | la red proyectada, con inferencia honesta | ninguno (decisiones) |
| Términos estructurales endógenos | posibles en teoría; impracticables aquí (MCMC × B; degeneración) | imposibles por construcción; van al RHEM |
| Costo computacional | ~20 s ($B = 1000$) | ~2 s |
| Continuidad con la literatura co-sponsorship | alta (Fowler 2006 y sucesores usan la proyección) | media (elección discreta, más econométrica) |

La decisión de cuál de los dos encabeza M1 es editorial, no técnica — depende de qué estimando responde mejor la pregunta RQ1 del paper y de qué audiencia lo revisará. Los criterios están en la tabla; la discusión de esa decisión se lleva por separado.
