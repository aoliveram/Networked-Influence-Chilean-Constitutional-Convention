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
> *(photo 1)* October 2019. A thirty-peso metro fare hike in Santiago turned into the largest protests in Chile's modern history — more than a million people in this square. This photo — a protester on top of the statue — became the icon of what Chileans call the *estallido social*, the social outburst.
> The political answer was unique. Not a cabinet change, not a reform package — a cross-party agreement to write a **new constitution from scratch**, through a brand-new institution where almost *any* citizen could potentially get elected: full gender parity — the first constituent body in the world with it —, seventeen reserved indigenous seats, and independents running on equal footing with parties.
> *(photo 2)* This is that institution at work. *(photo 3)* And this is the draft it delivered, one year later.
> So Chile went from the street to a draft constitution in under three years — and left behind a perfect dataset for network science. That's this talk.

---

## Slide 2 — A natural quasi-experiment (1:30–2:30)

> Here's the problem with studying collaboration in any normal parliament: the network you observe today is the sediment of decades. Careers, committees, party discipline, old favors.
>
> The Chilean Constitutional Convention is the cleanest exception I know of. It ran for exactly one year. Most members were newcomers and independents, there were seventeen reserved indigenous seats, and the body *dissolved* when it delivered the draft.
>
> Two rules structure everything. First: every constitutional initiative needed **eight to sixteen sponsors**. So signing was forming a visible, dated coalition. Second: every norm needed **two thirds of the floor — 103 out of 154 votes**. Being born was cheap; surviving was expensive.
>
> A quick note: I'm not a sociologist, so I'll report what the models find, and comment on some of it. But the deeper politological reading is an ongoing work with my Advisor.

---

## Slide 3 — The data (2:00–3:30) ← slow down here

> Let me walk you through the data.
>
> The top is the year: The light hatch at the start is when conventionals **voted the rules** for the construction of the final draft, and we use that first month to measure ideology *before the network existed*. The dense hatch is the **votes on norms**, under the two-thirds rule: that's where the constitution actually got written.
> Commissions worked from October to May, and they started making initiatives that contained one or more articles, and then their amendments, when the articles were not approved in the plenary.
>
> The bottom is the pipeline:
> We had 154 members (some of them with political experience, some not),
> 947 initiatives (all signed by up to 16 conventionals),
> which gave us 18-hundred genesis articles, which,
> after more than 4,000 roll calls (from which we calculated the political positions of each member),
> resulted in 498 articles that survive into the draft.
>
> And all this information is fully mapped, and it will be available to anyone soon.

---

## Slide 3b — What we know about each member

> Beyond the documents and the votes, we know who each member *was* before day one: their list and political bloc, their district,
> age, gender, whether they held a law degree, their education level,
> and whether they had **prior institutional experience** — meaning they had held public office before: former members of Congress, mayors, officials. Thirty-five of the 154 had.
> We calculated the 2D ideology using the first month of roll calls, so these votes are before the collaboration network existed.

---

## Slide 4 — The seven commissions (3:30–4:15)

> The Convention worked in seven thematic commissions. Quickly, what each one is about: C1 designs the political system — branches of government, the regime. C2 writes the constitutional principles. C3 is the form of the state — the territorial organization. C4 is fundamental rights. C5 is environment and the economic model. C6 is the justice system. And C7 is knowledge systems — science, culture, education.
> And they are *very* different worlds. Look at the numbers: in Justice, fifteen of the seventeen members are lawyers — almost all of them. In Principles, only four. Fundamental Rights is a monster with 283 initiatives, and its education level is basically a bachelor's degree — one point zero on our scale. Knowledge Systems is the oldest commission, average age fifty-two, and has *zero* members with prior political experience.
> Every model I'll show is estimated within commission — these worlds are too different to pool.

> So this is just a glimpse of the research you can do with this rich new dataset.
> I'll show you some of the first research questions I've been working on, but it is still a work in progress, and it's not meant to be included in a single paper since they address more that one particular topic.
> That's why your comments are so welcomed.

---

## Slide 5 — Positions over time, by commission (4:15–5:15)

> These are the revealed positions of all 154 members over the year, commission by commission — estimated from every roll call with a dynamic IRT.
> Three things to see here. First, on the left side the lines look very stable — but that's mostly because there are *few votes* in those months, so the estimates barely move. Second, there is one window with a lot of movement — and that's exactly where the plenary voted the articles, under the two-thirds rule. That window is where the dynamics live, and it's the window we'll model.
> And third, notice C5 — environment: it holds the most left-leaning positions of all commissions.

---

## Slide 6 — Research questions (5:15–6:00)

> Our questions come in three families.
> Formation: can the co-sponsorship network be *predicted* from what people brought with them — their district, their profession, their profile?
> Behavior, in two parts: does exposure to your co-signers *move* your ideological position? And does voting *defection* travel along co-sponsorship ties?
> And success, also in two parts: what makes an *article* survive into the draft? And does the *context* an initiative is born into matter for its authors' success?

*(RQ1 divider: "First: could we predict the network?")*

---

## Slide 7 — Bipartite ERGM (6:00–7:15)

> The real unit of analysis we have are the ties person-document, in the sense that a signature is one tie in a bipartite network.
>
> The model is an ERGM on that network.
> The continuous variables enter as per-document *ranges* — how spread out a document's signers are in ideology, age, education — plus two structural terms I'll come back to.
> We ran seven models, one per commission.
> Estimation is maximum pseudo-likelihood — a logistic on change statistics — with standard errors from an initiative bootstrap.

---

## Slides 8–11 — Profile homophily, four passes (7:15–9:30)

**Pass I — Lawyers (7:15–7:45):**

> Here are the results. They look intimidating, but is just because we have 7 models and each variable is decomposed in 6 political sub-groups.
> What each coefficient tells us is this: how much one more co-signer of that same group changes the chances that I join it too
> positive values means the pattern is over-used, negative means under-used, always compared against comparable random networks.
> I'll walk it top to bottom with these red boxes.
>
> The first group is the law degree — and it's a null, with one honest nuance. A handful of cells do reach significance — five of thirty-five, slightly more than chance — but their signs contradict each other: the right is *negative* in one commission and *positive* in another, and nothing replicates across commissions. So: no bloc shows a *consistent* pattern. In a body drafting a constitution, you'd expect lawyers to seek lawyers; there is no consistent trace of that in any political sector.

**Pass II — Experience (7:45–8:15):**

> Second: prior political experience. The only bloc where experienced members cluster together is the left — around plus point one, significant in the two biggest commissions, positive in five. Everywhere else, nothing.

**Pass III — Gender (8:15–8:45):**

> Third: gender — and here I'll point you only at the across-blocs row: small, positive, and consistent, significant in five of the seven commissions. Gender helps signatures *cross* the political divide. Within blocs there's nothing to see; the action is between them.

**Pass IV — District (8:45–9:30) ← the star of this table:**

> And fourth, the strongest story: district. Sharing a district organizes co-signing **inside the right** — plus point four to plus one point four. Inside the left it's the *opposite*: negative, significant.
> This fits well with the notion that the right-wing is mainly concentrated in two specific regions in Chile, but, during the 2020 election, the left was well spreaded across the whole nation.
> And across blocs, positive in all seven — territory is the main bridge over the political divide.

---

## Slides 12–13 — Discipline and structure, two passes (9:30–10:30)

**Pass I — Compact contingents (9:30–10:00):**

> Second table. The top block is the controls; the story is in the middle block: when someone joins a document where their bloc is already present, do they *stretch* the delegation's ideological range, or fit inside it?
> Where it's significant, it's negative — the left in the two biggest commissions, around minus three point seven. Blocs add signers that do not stretch their delegation. Contingents are built compact.

**Pass II — The tabula rasa signature (10:00–10:30):**

> And my favorite structural result, the bottom row: gwdsp — repeated pairs — negative and significant in *all seven* commissions.
> Conditional on everything else, repeated pairs are **under-used**: when a new initiative forms, the previous coalition is not re-used — it re-forms with fresh pairs. In an old legislature you'd expect the opposite sign, entrenched partnerships. Here there was no inherited stock of partnerships — and the model detects its *absence*. That's the structural signature of a tabula rasa.

*(RQ2 divider: "Does the network move positions, or behavior?")*

---

## Slide 14 — RQ2a design: the norms era, and a careful clock (10:30–11:45)

> Now, influence on positions. Everything here uses only the *norms era* — the three months when the constitution was actually voted, under the two-thirds rule — and positions re-estimated with era votes only, so the thermometer doesn't mix two different political games.
>
> The design: your exposure is the weighted mean position of your co-signers — and it has **bounded memory**. Network weights decay over each wave's new co-signing, so the effective windows are two weeks, a month, and six weeks.
> The model is within-person: does my *change* follow where my recent neighborhood *was*?
>
> Also we added a future exposure. If "influence" were selection, the future should predict and the lag should die.

---

## Slide 15 — The result (11:45–13:15) ← the freshest result, slow down

> One table, three columns — the same model at the three memories: two weeks, a month, six weeks.
> The result is the same in all three. Movement follows *recent* exposure: plus point zero two two — in words, each wave people close about two percent of the distance to their recent neighborhood. That's with date fixed effects, so it's not the common shocks of the era — not the polls, not the big deals.
> And the referee's test: if this were really *selection* — I sign with people I'm already moving toward — then where my network is *heading* should predict my change today, and the past should die. We build the strictly-future exposure — also decayed, nearest future weighted most — race them… and the future predicts **nothing**, at every memory, while the past exposure stands.
> Nobody picked the memory by looking at results — the three columns are the proof.
>
> Honest label: an influence *component* that survives every test this panel supports. Latent homophily can never be fully excluded in observational networks — but the selection signature, the future predicting the present, is gone.

---

## Slide 17 — RQ2b: defection travels (13:40–14:50)

**Slide 17a — The design:**

> Now another type of behaviour: bloc discipline. Defection is defined here as voting against your own bloc's majority in that roll call.
> The question: when someone breaks ranks, do they break alone — or together with the people they wrote initiatives with at the start?
> The exposure is the weighted share of your co-signers defecting in the *same* roll call.
> And three controls, each with a job. Person fixed effects absorb the born rebels. **Bloc-times-vote** fixed effects absorb "this vote split this bloc" — the mechanical source of co-defection — so we compare two members of the *same bloc in the same vote*, under exactly the same pressure. And *marginality* — your distance to your own bloc's median — because the bloc's ideological outsider breaks ranks more, and phi must not steal that.
> **Phi is the parameter of interest: do I defect more when my people defect?**

**Slide 17b — The result:**

> Here's the final model. Phi is eight point seven five — huge. In words: same bloc, same vote, same pressure — the members whose co-signers defect, defect far more.
> Marginality also matters: the bloc's ideological outsider breaks ranks more. But notice — it takes *nothing* from phi. Two independent channels.
> And it's carried by newcomers at both ends of the tie: novice receivers at twelve point two versus nine point five for the experienced, and the same on the sender side. The new majority moves together; the old guard neither drags nor gets dragged.
> The footnote is the validation: if you shuffle *who* defected within each bloc-and-vote — keeping how many — pure mechanics gives six point zero, far below what we see.

*(RQ3a divider: "What makes an article survive?")*

---

## Slide 18 — What survives? (14:50–15:30)

> Now the texts themselves. We have 1,565 articles, and only twenty percent of them reach the draft — and not evenly.
> This is survival by where the signing coalition stands ideologically: it varies a lot, and it peaks close to the two-thirds pivot — the dashed line. Where your coalition stands matters for whether your text lives.

---

## Slide 19 — The survival model (15:30–16:20)

> Here's the model behind that picture: a logistic with commission intercepts and three families of coalition traits — ideological geometry, network properties, and human capital. The bold p-values are the survivors.
> Three results. Distance to the two-thirds pivot: negative, strong — coalitions far from the pivot die. Ideological *heterogeneity*: positive — wide coalitions survive more. And internal density — pairs with a common history — positive and significant: teams with shared history keep their articles alive.
> Human capital — lawyers, experience, degrees — all null. Articles win by geometry and team history, not by credentials.

---

## Slides 20–22 — Does the birth context matter? (16:20–18:00) ← sell it

*(RQ3b divider: "Whose success is it?")*

**Slide 20 — The question (16:20–16:50):**

> Last model, and the one I'm most excited about going forward.
> Each member's success: of all the article text you co-signed, how much made it into the draft? Simple number, zero to one.
> First fact: that number is *not individual*. It clusters on the co-sponsorship network — Moran's I of point four four. Your success looks like your co-signers' success.
> So we fit the model that takes this seriously: your success related to the *average success of your co-signers*, your own attributes, and your co-signers' attributes. One number — rho — answers the question in the title: does the company an initiative is born into matter?

**Slide 21 — The full model (16:50–17:20):**

> Here's the whole thing. Two patterns.
> Own attributes — the left columns — predict almost nothing. Your co-signers' attributes — the right columns — do.
> Look at distance to the pivot: your own doesn't matter, p point nine five. Your *co-signers'* distance does — minus point two seven. Even for individual success, what matters is the company's position.
> And rho: point eight nine, robust between point six three and point nine five across network definitions.

**Slide 22 — Reading rho (17:20–18:00):**

> How do we read that number? Here's the part I like. This equation is not ad hoc — it's the *equilibrium of an effort game*, from Battaglini and coauthors in AJPS 2020: if my effectiveness rises with my allies' effectiveness, the unique equilibrium makes everyone's success equal their Katz–Bonacich centrality — and the reduced form is exactly this model.
> Under that reading — *up to an identification test currently in progress*; we're instrumenting the network with pre-Convention ties, the design is ready — rho is the **coalition-effectiveness spillover**: connected members' effectiveness spills over into yours.
> So let me state our position plainly: the evidence supports the network view of success. Where you stand shapes how well you do, beyond who you are. **The context is the coalition you stand in.**

---

## Slide 23 — Takeaways (18:00–18:40)

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
| SDM: question, model, reading | 20–22 | 16:20–18:00 |
| Takeaways + thanks | 23–24 | 18:00 onward |

**Notes for delivery:**
- The two spines: **counters, not cuts** (slide 7 → tables 8–13), and **the careful clock** (slide 14 → result 15). If the audience remembers two moves, those.
- **Release valves if running long:** compress passes II+III of the profile table to one sentence each (saves ~45s); slide 16 (decay) can be one sentence ("with decaying memory it sharpens — p below ten to the minus four"); slide 19 can be one sentence over the figure.
- Slide 15 is the freshest result — slow down. Keep verbatim: "an influence component that survives every test this panel supports."
- Slide 22: never say "influence" or "contagion" for rho without the qualifier; the scripted qualifier is "up to an identification test currently in progress."
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

**Full sentences to rehearse** (they carry the talk):
- "closing about two percent of the distance to their recent neighborhood, per wave"
- "the strictly-future innovation predicts nothing"
- "same bloc, same vote, same pressure"
- "coalitions re-form with fresh pairs"
- "the context is the coalition you stand in"
