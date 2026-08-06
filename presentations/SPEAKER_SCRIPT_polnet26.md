# Speaker Script — Political Networks 2026, Manchester (v2)

Target: 18 minutes of talk + 7 of questions. Slide numbers are *logical* slides (section dividers get a one-liner in passing).

---

## Slide 1 — Title (0:00–0:30)

> Hi everyone, thanks for being here. I'm Aníbal Olivera, PhD student in Social Complexity Sciences at Universidad del Desarrollo, in Chile.
> This is joint work with my advisor, Jorge Fábrega.
> The title says "tabula rasa legislature", and that's the whole point: we got to watch a legislature's collaboration network being born from zero.

---

## Slide 1b — The hook: October 2019 (0:30–1:30)

*(three photos appear one by one — advance twice)*

> Before any model, let me show you where this all comes from.
> *(photo 1)* In October 2019, a metro fare hike in Santiago turned into the largest protests in Chile's modern history, with more than a million people in this square. This photo became the icon of that social outburst.
> The political answer was unique. *(photo 2)* A cross-party agreement to write a **new constitution from scratch**, through a brand-new institution where almost *any* citizen could potentially get elected: full gender parity, seventeen reserved indigenous seats, and independents running on equal footing with parties.
> *(photo 3)* And this is the draft it delivered, one year later.
> So Chile went from the street to a draft constitution in under three years — and left behind a great dataset that was useless because it was a big mess, with documents in different formats, each commission had its own way to report their work, and it was very hard to reconstruct the path towards the first draft of the new constitution.

---

## Slide 1c — The data release (1:30–1:50)

> One practical thing before we start: all of this data — every initiative, article, and vote, cleaned and linked — will be **publicly available soon**. That QR points to the data-cleaning repository, and we're preparing a *Data in Brief* paper to announce the release.
> Don't worry if you can't scan it now — we'll come back to it at the end.

---

## Slide 2 — A natural quasi-experiment (1:50–2:40)

> So, why was this institution particularly interesting? In any normal parliament, the network you observe today is the sediment of decades of favors, committees, careers, etc.
>
> The Chilean Constitutional Convention is the cleanest exception I know of. It ran for exactly one year. Most members were newcomers and independents, there were seventeen reserved indigenous seats, and the body *dissolved* when it delivered the draft.
>
> They set their own rules.
> First: every constitutional initiative needed 8 to sixteen sponsors. So signing was forming a visible, dated coalition.
> Second: every norm needed two thirds of the floor — **103 out of 154 votes**. Being born was cheap; surviving was expensive.

---

## Slide 3 — The data (2:00–3:30) ← slow down here

> Let me walk you through the data.
>
> On the top we have the convention's temporal map, where the light hatch at the start is when conventionals **voted the rules** of the next months, and we use that first month to measure ideology *before the network existed*.
> The dense hatch is the **votes on norms** under the two-thirds rule. That's where the constitution actually got written.
> Commissions worked from October to May, and they started making initiatives that contained one or more articles, and then their amendments to modify them when the articles were not approved in the plenary.
>
> The bottom is the pipeline:
> We had 154 members (most of them without political experience),
> 947 initiatives (all signed by up to 16 conventionals),
> which gave us 18-hundred genesis articles, which,
> after more than 4,000 roll calls,
> resulted in nearly 500 articles that survive into the draft.
>
> And all this information is fully mapped, and it will be available to anyone soon.

---

## Slide 3b — What we know about each member

> Beyond the documents and the votes, we have the profile of each member *before* day one: their electoral list, their district,
> age, gender, whether they held a law degree, their education level,
> and whether they had **prior institutional experience** — that is if they had held public office before: former members of Congress, mayors, etc... only thirty-five of the 154 had.
> We calculated the 2D ideology using the first month of roll calls, before the initiatives showed the collaboration network.

---

## Slide 4 — The seven commissions (3:30–4:15)

> The Convention worked in seven thematic commissions. Quickly:
> C1 political system — branches of government, the presidential regime, etc.
> C2 writes the constitutional principles.
> C3 form of the state — the territorial organization.
> C4 is fundamental rights.
> C5 is environment and the economic model.
> C6 is the justice systemS (because the Indigenous people were going to have their own justice system).
> C7 is knowledge systems — science, culture, education.
> And the last column is the payoff: how many of each commission's genesis articles made it into the draft. Justice placed ninety-eight; Environment wrote a lot and placed only thirty — the harshest filter in the Convention.
> Every model I'll show is estimated within commission — these worlds are too different to pool.

---

## Slide 5 — Positions over time, by commission (4:15–5:15)

> These are the revealed positions of all members over the year, commission by commission — estimated with the *dynamic ItemResponseTheory R package*.
> 
> On the left side the lines look very stable — but that's mostly because there are *few votes* in those months,
> and there is one window with a *lot of movement* — and that's exactly where the plenary voted the articles, under the two-thirds rule.
> And the Environment commission holds the most left-wing positions of all commissions.
>
> Ok. As you can see, there are lots of possible research questions you can address using these data,
> and this is just a glimpse of the research you can do with this new rich dataset.

---

## Slide 6 — Research questions (5:15–6:00)

> I'll show you some of the first research questions I've been working on, but it is still a work in progress, and it's not meant to be included in a single paper since they address more than one particular topic.
> That's why your comments are so welcome.
> 
> Our questions come in three families.
> Formation: can the co-sponsorship network be *predicted* from what people brought with them — their district, their profession, their profile?
> Behavior, in two parts: does exposure to your co-signers *move* your ideological position? And does voting *defection* travel along co-sponsorship ties?
> And success, also in two parts: what makes an *article* survive into the draft? And does the *context* an initiative is born into matter for its authors' success?
>
> Before moving on, I just want to comment that I'm not a sociologist, I'm more interested in the methodological aspects, so I'll report what the models find, and comment on some of it. But the deeper politological reading is an ongoing work with my advisor.


*(RQ1 divider: "First: could we predict the network?")*

---

## Slide 7 — Bipartite ERGM (6:00–7:15)

> Here it is worth noticing that the real units of analysis we have are the person-document ties, so we have a bipartite network.
>
> The model we fit is an ERGM on that bipartite network.
> 
> So the continuous variables enter as per-document *ranges* — that is how diverse the signers are in ideology, age, education — plus two structural terms I'll come back to.
> We ran seven models, one per commission,
> using maximum pseudo-likelihood as the estimator with standard errors from an initiative bootstrap.

---

## Slides 8–11 — Profile homophily, four passes (7:15–9:30)

*Pass I — Lawyers (7:15–7:45):*

> Here are the results. They look intimidating, but it's just because we have 7 models and each variable is decomposed in 6 political sub-groups.
> What each coefficient tells us is this: how much one more co-signer of that same group changes the probability that I join it too;
> positive values mean the pattern is over-used, negative means under-used, always compared against comparable random networks.
> 
> I'll walk it top to bottom with these red boxes.
>
> The first group is the law degree. In a body drafting a constitution, you'd expect lawyers to seek lawyers; but that pattern is present only in some cases, as the "Center-left" and "Reserved seats" in Environment, and the "Right" lawyers seek each other in Justice system, but they repelled each other in Constitutional principles, so they had their own niche in that commission.

*Pass II — Experience (7:45–8:15):*

> Second: prior political experience. The only bloc where experienced members cluster together is the left — around plus point one, significant in the two biggest commissions, positive in five. Everywhere else, nothing.

*Pass III — Gender (8:15–8:45):*

> Third: gender — and here I'll point you only at the across-blocs row: positive and consistent, significant in five of the seven commissions. Gender helps signatures *cross* the political divide.

*Pass IV — District (8:45–9:30) ← the star of this table:*

> And fourth, district, the strongest result. 
> We have positive coefficients "Across blocs", in all seven commissions, so *territory is the main bridge* over the political divide.
> But more interesting is that sharing a district organizes co-signing **inside the Right**, but inside the Left it's the *opposite*.
> And this fits well with the notion that the right-wing is mainly concentrated in two specific regions in Chile, but, during the 2020 election, the left was well spread across the whole nation.

---

## Slides 12–13 — Discipline and structure, two passes (9:30–10:30)

*Pass V — Compact contingents (9:30–10:00):*

> And just to finish the first model. When someone joins a document where their bloc is already present, do they make the delegation's ideological range *wider*, or do they fit inside it?
> Where it's significant, it's negative — the left in the two biggest commissions, around minus three point seven. Blocs add signers that fit inside what the delegation already covers. Contingents are built compact.

*Pass VI — The tabula rasa signature (10:00–10:30):*

> And my favorite structural result, the bottom row: — repeated pairs — negative and significant in *all seven* commissions.
> Conditional on everything else, repeated pairs are **under-used**: when a new initiative forms, the previous coalition (the group who signed the initiative) is not re-used, but they tend to use fresh pairs.
> In an old legislature you'd expect the opposite, so that could be the structural signature of a tabula rasa.

*(RQ2 divider: "Does the network move positions, or behavior?")*

---

## Slide 14 — RQ2a design: the norms era, and a careful clock (10:30–11:45)

> First, influence on positions. Everything here uses only the *norms era* — that window with a lot of votes and dynamics.
>
> Here we use the exposure to your neighborhood to explain your dynamic.
> Your exposure is the weighted mean position of your co-signers, where
> network weights decay over time, so the effective windows are 2, 4, or 6 weeks.

> The model is within-person, and answers if my *change* follows where my recent neighborhood *was*.
>
> To see if the "influence" was selection, we also added a future exposure, as the future exposure should predict the dynamics and the lag should die. 

---

## Slide 15 — The result (11:45–13:15) ← the freshest result, slow down

> This table shows the same model with three different memories. And the result is the same in all three.
> Movement follows *past* exposure with a coefficient of point zero two, so, if we aggregate all the waves, this could explain ~12% of the total dynamic. 
> And future innovation predicts nothing.
> That's great because, if this were really *selection* (I sign with people I'm already moving toward), then where my network is *heading* should predict my change today, and the past should die.
> So there is no selection here, the opposite of what one could think about a new institution…
>
> So the influence component survives every test; but latent homophily (unobserved trait that also predicts how I will move) cannot be excluded by design.

---

## Slide 17 — RQ2b: defection travels (13:40–14:50)

**Slide 17a — The design:**

> Now another type of behaviour: bloc discipline.
> The question: when someone breaks ranks, do they break alone — or with the people they wrote initiatives with?
> Again, the exposure here is the weighted share of your co-signers defecting in the *same* roll call.
> 
> We have here three controls, each one with a job.
> Person fixed effects absorb the born rebels, and this mu absorbs "the divided votes" (if the defection is general, it is not relevant to us),
> *marginality* is the distance to your own bloc's median position (because the ideological outsiders break ranks more),
> and **Phi** is the parameter of interest: do I defect more when the people I wrote initiatives with defect?

**Slide 17b — The result:**

> In the results table we have two models, so we can see how, even when we control for marginality, Phi is high and significant.
> So yes, the members whose co-signers defect, defect far more.
> Marginality predicts defection strongly but it's an independent channel.
> But the crucial result is that defection travels along co-sponsorship ties.

*(RQ3a divider: "What makes an article survive?")*

---

## Slide 18 — What survives? (15:10–15:40)

> Now the texts themselves. We have 1,565 articles, and only 20% of them reach the draft — and not evenly.
> Each dot is a group of articles, ordered by the average position of the team that signed them — left to right. The height is the share that survived. And the peak is close to the 2/3 pivot — the dashed line.
> So where your coalition stands matters for whether your text survives. What else matters?

---

## Slide 19 — The survival model (15:30–16:20)

> To answer we fit a logistic regression with 1) commission intercepts, 2) how close is it to the 2/3, 3) how wide is the coalition where the article was born, 4) the size of the coalition, 5) some network properties, 6) and human capital.
>
> And, as the plot showed us, the distance to the pivot is negative and strong.
> But ideological *heterogeneity* is positive (wide coalitions survive more),
> And internal density (pairs with more common history) predicts better chances to keep their articles alive, *so the coalition's relational capital saves the articles*.
> Interestingly, human capital — lawyers, experience, degrees — are all null. The articles do not win by credentials.
>

*(RQ3b divider: "Whose success is it?")*

**Slide 20a — The question: success, and the retention plot (16:20–16:50):**

> Last model, and the one I'm most excited about.
> We will define Success as the mean textual retention of my articles calculated via TF-IDF.
> The plot shows what that means: with text similarity — TF-IDF over each article's wording — we follow every article through the year. And you can see it: as the process advances, the surviving texts look *more and more like the draft*. "Retention" is exactly that: how much of your text made it to the end.

**Slide 20b — Moran, and the model:**

> But the success is *not individual*: it clusters on the co-sponsorship network (we have a positive Moran's I), so some clusters are more successful than others.
> To test whether the context an initiative is born into matters, we relate each member’s success to the *average success of their co-signers* via a spatial autocorrelation model,
> so **rho** answers whether the company an initiative is born into matters.

**Slide 21 — The full model (16:50–17:20):**

> Here's the whole thing. Two patterns.
> Own attributes (the left columns) predict almost nothing.
> Your co-signers' attributes (the right columns) do. See for example the *coalition's* distance to the pivot.
> And the rho value is huge and significant.
> 
> And, *up to an identification test currently in progress*, rho is the coalition-effectiveness spillover: connected members' effectiveness spills over into yours.
> So the evidence supports the network view of success. Where you stand shapes how well you do, beyond who you are.
> **The context is the coalition you stand in.**

---

## Slide 23 — Conclusions (18:00–18:40)

> Five things.
> The network was predictable from what people brought with them — their political bloc and, conditionally, their district. Credentials organized nothing, in any bloc.
> Territory is the right's internal glue and the system's bridge; gender crosses blocs; and repeated pairs are under-used everywhere — the structural signature of a tabula rasa.
> In the norms era, positions drift toward the *recent* neighborhood — about two percent of the distance per wave — and the strictly-future placebo shows nothing: an influence component, not just selection.
> Defection travels along co-sponsorship ties — same bloc, same vote, same pressure — carried by newcomers at both ends.
> And articles win by geometry and team history; for their authors, the context is the coalition — success is a coalition good.
> The political interpretation is ongoing work with my advisor. Thank you.

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
| Discipline + structure (2 passes) | 12–13 | 9:30–10:30 |
| RQ2a: design, result, decay | 14–16 | 10:30–13:40 |
| RQ2b: defection | 17 | 13:40–14:50 |
| RQ3: survival + pivot picture | 18–19 | 14:50–16:20 |
| SDM: question + plot, model | 20a–21 | 16:20–18:00 |
| Takeaways + thanks | 23–24 | 18:00 onward |

**Notes for delivery:**
- The two spines: **counters, not cuts** (slide 7 → tables 8–13), and **the careful clock** (slide 14 → result 15). If the audience remembers two moves, those.
- **Release valves if running long:** compress passes II+III of the profile table to one sentence each (saves ~45s); slide 16 (decay) can be one sentence ("with decaying memory it sharpens — p below ten to the minus four"); slide 19 can be one sentence over the figure.
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

**Full sentences to rehearse** (they carry the talk):
- "closing about two percent of the distance to their recent neighborhood, per wave"
- "the strictly-future innovation predicts nothing"
- "same bloc, same vote, same pressure"
- "coalitions re-form with fresh pairs"
- "the context is the coalition you stand in"
