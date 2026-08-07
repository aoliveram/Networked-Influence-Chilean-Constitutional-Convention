# Speaker Script — Political Networks 2026, Manchester (v2)

Target: 18 minutes of talk + 7 of questions. Slide numbers are *logical* slides (section dividers get a one-liner in passing).

---

## Slide 1 — Title (0:00–0:30)

> Hi everyone, thanks for being here. I'm Aníbal Olivera, PhD student in Social Complexity Sciences at Universidad del Desarrollo, in Chile.
> And in this work we explore the Chilean Constitutional Convention as a "tabula rasa legislature".

---

## Slide 1b — The hook: October 2019 (0:30–1:30)

*(three photos appear one by one — advance twice)*

> First of all, let me remind you of the context.
> *(photo 1)* In October 2019, the largest social uprising in Chile's modern history took place, with millions of people on the street across the whole nation.
> The political answer was unique. *(photo 2)* A cross-party agreement to write a **new constitution from scratch**, through a brand-new institution, the Constitutional Convention, where almost *any* citizen could potentially get elected.
> *(photo 3)* And a year later, this draft of the constitution was delivered.
> And that institution left behind a great dataset that was useless because it was a big mess, with documents in different formats, each commission had its own way to report their work, and it was very hard to reconstruct the path towards the first draft of the new constitution.

---

## Slide 1c — The data release (1:30–1:50)

> But hopefully, all of this data will be **publicly available soon**, since we're preparing with my team a *Data in Brief* paper to announce the release of all this data in a structured format. We have a GitHub repository with the work, 
> but don't worry if you can't scan it now, we'll come back to it at the end.

---

## Slide 2 — A natural quasi-experiment (1:50–2:40)

> So, why was this institution particularly interesting? In any normal institution, the network you observe today is the sediment of decades of favors, committees, careers, etc.
>
> But the Chilean Constitutional Convention started from zero relational stock. Most members were newcomers and independents, there were seventeen reserved indigenous seats, and the body *dissolved* when it delivered the draft.
>
> And they set their own rules.
> every constitutional initiative needed 8 to sixteen sponsors to be accepted.
> and, every norm needed 2/3 of the floor to be approved.

---

## Slide 3 — The data (2:00–3:30) ← slow down here

> This is an infographic of the data.
>
> On the top we have the convention's temporal map, where the light hatch is when conventionals **voted the rules** of the next months (so we can measure the ideology *before* the network existed).
> The dense hatch is the **votes on norms**, so that's where the constitution actually got written.
> 7 Commissions worked from October to May, making initiatives that contained one or more articles, that were modified by amendment when the articles were not approved by the plenary.
>
> The bottom is the pipeline:
> We had 154 members,
> 947 initiatives,
> which gave us 18-hundred potential articles, which,
> after more than 4,000 roll calls,
> resulted in nearly 500 articles that survive into the draft.

---

## Slide 3b — What we know about each member

> Also, we have the profile of each member *before* the convention: their electoral list, their district,
> age, gender, whether they held a law degree, their education level,
> and whether they had *prior institutional experience* (that is if they had held public office before: former members of Congress, mayors, etc...) only 35 of them had experience.
> And we have the ideology, calculated using the first month of roll calls; before the initiatives showed the collaboration network.

---

## Slide 4 — The seven commissions (3:30–4:15)

> The Convention worked in seven thematic commissions. Quickly:
> C1 political system (branches of government, the presidential regime, etc).
> C2 constitutional principles.
> C3 form of the state (the territorial organization).
> C4 fundamental rights (that shows the lower Educational Degree, basically batchelor degree, And was the most productive with 283 initiatives).
> C5 environment and the economic model.
> C6 the justice systemS (in plural, because the Indigenous people were going to have their own justice system, AND most of them were lawyers).
> C7 knowledge systems (science, culture, education, WHERE no one had political experience)

---

## Slide 5 — Positions over time, by commission (4:15–5:15)

> These are the revealed positions of all members over the year, commission by commission, estimated with the *dynamic ItemResponseTheory R package*.
>
> On the left side the lines look very stable — but that's mostly because there are *few votes* in those months,
> and there is one window with a *lot of dynamic* — and that's where the plenary voted the articles under the two-thirds rule.
> And you can see how the Environment commission holds the most left-wing positions of all commissions.
>

---

## Slide 6 — Research questions (5:15–6:00)

> Ok. As you can see, there are lots of possible research questions you can address using these data.
> I'll show you some of the first research questions I've been working on, but it is still a work in progress, and it's not meant to be included in a single paper since they address more than one particular topic.
> 
> Our questions come in three families.
> Formation: can the co-sponsorship network be *predicted* from what people brought with them — their district, their profession, their profile?
> Behavior, in two parts: does exposure to your co-signers *move* your ideological position? And does voting *defection* travel along co-sponsorship ties?
> And success, also in two parts: what makes an *article* survive into the draft? And does the *context* an initiative is born into matter for its authors' success?

*(RQ1 divider: "First: could we predict the network?")*

---

## Slide 7 — Bipartite ERGM (6:00–7:15)

> Here it is worth noticing that the real units of analysis we have are the person-document ties, so we have a bipartite network.
>
> So, we fit an ERGM on that bipartite network to see what variables explain that network.
> 
> We are expecting that the ideology (measured using the first month of votes) is relevant, but what about the age, the education, or the repeated pairs?
> We ran seven models, one per commission,
> using maximum pseudo-likelihood as the estimator with standard errors from an initiative bootstrap.

---

## Slides 8–11 — Profile homophily, four passes (7:15–9:30)

*Pass 0 — :*

> Here are the results. They look intimidating, but it's just because we have 7 models and each variable is decomposed in 6 political sub-groups.
> What each coefficient tells us is how much one more co-signer of that same group changes the probability that I join it too.

*Pass I — Lawyer:*

> Let's see the law degree. In a body drafting a constitution, you'd expect lawyers to seek each other; but that pattern is present only in some cases, as the "Right" lawyers seek each other in C6 Justice system, but they repelled each other in C2 Constitutional principles, so the lawyers had their own niche in that commission.

*Pass II — Experience (7:45–8:15):*

> In prior political experience. We spot that in the Left, the experienced members cluster together.

*Pass IV — District (8:45–9:30) ← the star of this table:*

> And in district, 
> We have positive coefficients in "Across blocs" in all seven commissions, so *territory is the main bridge* over the political divide.
> 
> But more interesting is that sharing a district organizes co-signing **inside the Right**, but inside the Left it's the *opposite*.
> And this fits well with the notion that the right-wing is mainly concentrated in two specific regions in Chile, but, during the 2020 election, the left was well spread across the whole nation.

*Pass V — The tabula rasa signature (10:00–10:30):*

> And my favorite structural one, the *repeated pairs* is negative and significant in all seven commissions.
> So repeated pairs are **under-used**: when a new initiative forms, the previous coalition tends to not be re-used.
> And that's exactly the opposite you'd expect from an old body, so that could be the structural signature of a tabula rasa.

*(RQ2 divider: "Does the network move positions, or behavior?")*

---

## Slide 14 — RQ2a design: the norms era, and a careful clock (10:30–11:45)

> Next question: Does the network move positions, or behavior?
> 
> First, the positions. We focus on the *norms era* (that window with a lot of votes and dynamics).
>
> And we tried to explain that dynamic from your exposure, that "is the weighted mean position of your co-signers, where network weights decay over time"
> 
> The model is within-person, and answers if my *moves* follow the moves my neighborhood did.
>
> We also added a future exposure to see if the "influence" was selection. (That is, if this term is significant, the dynamics are predicted by the future, so that's selection). 

---

## Slide 15 — The result (11:45–13:15) ← the freshest result, slow down

> This table shows the same model with three different memories, very similar results in all three.
> 
> The lagged exposure is significant across all memories,
> And the future predicts nothing, so there is no selection (the opposite of what one could think about a new institution)
>
> So the influence component survives every test; but latent homophily (unobserved trait that also predicts how I will move) cannot be excluded by design.

---

## Slide 17 — RQ2b: defection travels (13:40–14:50)

**Slide 17a — The design:**

> Now a very similar approach to another type of behaviour: bloc discipline.
> The question: when someone breaks ranks, do they break alone — or with the people they wrote initiatives with?
> Again, the exposure here is the weighted share of your co-signers defecting in the *same* roll call.
> 
> We have Person fixed effects to absorb the born rebels, another for "the divided votes" (if the defection is general, it is not relevant to us),
> We add a control for *marginality* (because the outsiders break ranks more),
> and **Phi** is the parameter of interest: do I defect more when the people I wrote initiatives with defect?

**Slide 17b — The result:**

> In the results table we have two models, so we can see how, even when we control for marginality, Phi is high and significant.
> So yes, the members whose co-signers defect, defect far more.
> Marginality predicts defection strongly but it's an independent channel.
> But the crucial result is that defection travels along co-sponsorship ties, so the relational ties are very informative here.

*(RQ3a divider: "What makes an article survive?")*

---

## Slide 18 — What survives? (15:10–15:40)

> Ok, we have 15-hundred articles, and only 20% of them reach the draft — and not evenly.
> In this plot we see the Article survival rate, sorted by the average position of the team that signed them — left to right. 
> So you can see how the peak is very close to the 2/3 pivot (the dashed line).
> 
> Therefore where your coalition stands matters for whether your text survives. 
> What else matters to explain the success rate?

---

## Slide 19 — The survival model (15:30–16:20)

> To answer we fit a logistic regression with 1) commission intercepts, 2) how close is it to the 2/3, 3) how wide is the coalition where the article was born, 4) the size of the coalition, 5) some network properties, 6) and human capital.
>
> And, as the plot showed us, the distance to the pivot is negative and strong.
> But ideological *heterogeneity* is positive (wide coalitions survive more),
> And, in terms of relational capital, internal density (pairs with more common history) actually helps to explain why some articles stay alive.
> 
> But, maybe more interestingly, human capital — lawyers, experience, degrees — are all null.
> The articles do not win by credentials.
>

*(RQ3b divider: "Does the context an initiative is born into matter?")*

**Slide 20a — retention plot (16:20–16:50):**

> Last question: does the context an initiative is born into matter?
>
> In this plot you can see how the similarity between the original articles and the final draft just grows as the original articles are modified by the amendments.
> 
> So we define "Success" as the mean textual retention of my articles calculated via TF-IDF.

**Slide 20b — Moran, and the model:**

> But the success is *not individual*: it clusters on the co-sponsorship network (we have a positive Moran's I), so some clusters are more successful than others.
> To test whether the context an initiative is born into matters, we relate each member’s success to the *average success of their co-signers* via a spatial autocorrelation model,
> so **rho** answers whether the company an initiative is born into matters.

**Slide 21 — The full model (16:50–17:20):**

> And here are the results.
> Own attributes predict almost nothing.
> Your co-signers' attributes do. Especially the *coalition's* distance to the pivot.
> And the rho value is huge and significant.
> 
> And, *up to an identification test currently in progress*, rho is the coalition-effectiveness spillover (so the effectiveness of your coalition spills over into your effectiveness).
> 
> So the evidence supports the *network view of success*.

> **The context is the coalition you stand in.**

---

## Slide 23 — Conclusions (18:00–18:40)

> Time to draft some conclusions.
> The network was predictable from what people brought with them — their political bloc and their district. Credentials organized nothing.
> Repeated pairs are under-used everywhere — the structural signature of a tabula rasa.
> In the norms era, positions drift toward the *recent* neighborhood — about two percent of the distance per wave — and the strictly-future placebo shows nothing: an influence component, not just selection.
> Defection travels along co-sponsorship ties — same bloc, same vote, same pressure — carried by newcomers at both ends.
> And articles win by geometry and team history; for their authors, the context is the coalition — success is a coalition good.

---

## Slide 24 — Thanks

> Thanks — the QR is the project repository, everything is reproducible. Happy to take questions.

---

--------------------------------------------------------------------------------

### Timing cheat-sheet (target ~18:40)

| Block | Slides | Target |
|---|---|---|
| Setup + standpoint | 2 | 0:30–2:00 |
| Data + commissions + ideology | 3–5 | 2:00–5:15 |
| RQs | 6 | 5:15–6:00 |
| ERGM tool | 7 | 6:00–7:15 |
| Profile table (4 passes, top→bottom) | 8–11 | 7:15–9:30 |
| Structure: repeated pairs (1 pass) | 12 | 9:30–10:00 |
| RQ2a: design, result, decay | 14–16 | 10:30–13:40 |
| RQ2b: defection | 17 | 13:40–14:50 |
| RQ3: survival + pivot picture | 18–19 | 14:50–16:20 |
| SDM: question + plot, model | 20a–21 | 16:20–18:00 |
| Takeaways + thanks | 23–24 | 18:00 onward |

**Notes for delivery:**
- The two spines: **counters, not cuts** (slide 7 → tables 8–12), and **the careful clock** (slide 14 → result 15). If the audience remembers two moves, those.
- **Release valves if running long:** compress passes I+II of the profile table to one sentence each (saves ~45s); slide 16 (decay) can be one sentence ("with decaying memory it sharpens — p below ten to the minus four"); slide 19 can be one sentence over the figure.
- Slide 15 is the freshest result — slow down. Keep verbatim: "an influence component that survives every test this panel supports."
- Slide 21 (full model): never say "influence" or "contagion" for rho without the qualifier; the scripted qualifier is "up to an identification test currently in progress" (now in the table's footnote).
- If a political-reading question goes deep: "that's exactly the part my advisor and I are working on — what the model pins down is X."

### Anticipated questions

- *"Why not one ERGM for the whole Convention?"* → Pooling with a single intercept flips homophily signs — Simpson's paradox, verified numerically. The commissions are different arenas (slide 4).
- *"Is MPLE valid here?"* → Points are standard for bipartite networks of this density; the known problem is optimistic SEs, which the initiative bootstrap fixes (measured inflation 1.2–3.6×). Full MCMC was computationally infeasible (documented). Our implementation is certified against `ergm` to 1e-11.
- *"Why t+2 for the innovation and t−1 for the lag?"* → Positions at t+1 are estimated from votes cast at t (and the dynamic IRT smooths two-sidedly), so t±1 is contaminated by the outcome window. With a contemporaneous exposure regressor the coefficients explode to mechanically contaminated values — we checked.
- *"Isn't the norms-era result just latent homophily?"* → It can never be fully excluded in observational networks (Shalizi–Thomas). What we can say: it's not common shocks (date FE), and the selection signature — the future predicting the present — is gone under the strict clock.
- *"Why not SAOM/Siena?"* → Right tool conceptually; our waves are report-dated and irregular, and positions carry estimated measurement error that SAOM doesn't propagate. On the roadmap for the paper.
- *"Rho = 0.89 — common shocks? mechanical coupling?"* → Partly mechanical (co-signers share articles, hence outcome components) — which is why the causal language waits for the instrument. The substantive content that doesn't depend on rho: the covariate migration (own vs co-signers' distance to the pivot).
- *"What's the instrument?"* → Pre-Convention ties: an alumni network (university × cohort; 88% of members have university education, covering ~78% of dyads) plus prior co-service in Congress as a complement. Battaglini-style two-step with a control function. In progress.
- *"The lawyer null — measurement?"* → It's a triple null: choice model, hybrid ERGM (in every bloc), and article survival (share of lawyers). Three designs, same answer.

### Pending before the talk
- Author's review of the data infographic (slide 3) — v2 just rebuilt.
- Any trimming to fit 18:00 flat (current script ~18:40; use the release valves).

--------------------------------------------------------------------------------

### Vocabulary to practice (term — approx. pronunciation — meaning here)

| Term | Pronunciation | Meaning in this talk |
|---|---|---|
| roll call | *ROL-col* | votación nominal (cada voto queda registrado) |
| co-sponsorship | *cou-SPON-sor-ship* | co-patrocinio (firmar juntos una iniciativa) |
| ideal point | *ai-DÍL point* | posición ideológica estimada de los votos |
| lagged / lag | *lagd* | rezagado; "the lag" = la exposición pasada |
| decayed exposure | *di-KEID ex-POU-yer* | exposición con memoria que se desvanece |
| bounded memory | *BAUN-did MÉ-mo-ri* | memoria acotada (la cota temporal) |
| within-person | *ui-DÍN PER-son* | dentro de cada persona (efectos fijos) |
| clustered standard errors | *CLÁS-terd* | EE agrupados por convencional |
| referee's test | *RE-fe-rís test* | el test del árbitro (la innovación futura) |
| quasi-experiment | *KUA-sai ex-PÉ-ri-ment* | cuasi-experimento |
| homophily | *jo-MÓ-fi-li* (acento en MO) | juntarse con los parecidos |
| bipartite | *bai-PAR-tait* | red de dos tipos de nodos |
| pseudo-likelihood | *SU-dou LAIK-li-jud* | pseudo-verosimilitud (MPLE) |
| bootstrap / resample | *BUT-strap / ri-SÁM-pel* | re-sortear iniciativas para los EE |
| breaks ranks / off script | *breiks ranks* | romper filas / salirse del libreto |
| shuffle | *SHÁ-fel* | barajar (la permutación) |
| marginality / outsider | *mar-yi-NÁ-li-ti* | el periférico ideológico del bloque |
| spillover | *SPIL-ou-ver* | derrame (el rho como externalidad) |
| draft | *draft* | el borrador constitucional |
| retention | *ri-TÉN-shon* | cuánto texto sobrevivió |
| pivot / quorum | *PÍ-vot / KUÓ-rum* | el pívot de 2/3 / el quórum |
| breadth / widening | *bredz / UAI-de-ning* | anchura ideológica / ensancharse |
| credentials | *cri-DÉN-shals* | títulos y experiencia |
| newcomers / old guard | *NIU-ca-mers / ould gard* | novatos / vieja guardia |
| tie / dyad | *tai / DAI-ad* | lazo / par de personas |
| amendments | *a-MÉND-ments* | indicaciones (las modificaciones a los artículos) |
| former MPs | *FOR-mer em-PÍS* | exparlamentarios (exdiputados y exsenadores) |
| light hatch |
| dense hatch |

**Full sentences to rehearse** (they carry the talk):
- "closing about two percent of the distance to their recent neighborhood, per wave"
- "the strictly-future innovation predicts nothing"
- "same bloc, same vote, same pressure"
- "coalitions re-form with fresh pairs"
- "the context is the coalition you stand in"
