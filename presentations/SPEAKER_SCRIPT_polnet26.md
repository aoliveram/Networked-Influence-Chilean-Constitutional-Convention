# Speaker Script — Political Networks 2026, Manchester

Target: 18 minutes of talk + 7 of questions. Slide numbers are *logical* slides (section dividers get a one-liner in passing).

---

## Slide 1 — Title (0:00–0:30)

> Hi everyone, thanks for being here. I'm Aníbal Olivera, PhD student in Social Complexity Sciences at Universidad del Desarrollo, in Chile.
> This is joint work with my advisor, Jorge Fábrega.
> The title says "tabula rasa legislature", and that's the whole point: we got to watch a legislature's collaboration network being born from zero.

---

## Slide 2 — A natural quasi-experiment (0:30–2:00)

> Here's the problem with studying collaboration in any normal parliament: the network you observe today is the sediment of decades. Careers, committees, party discipline, old favors. So when you ask "what organizes collaboration?", history contaminates every answer.
>
> The Chilean Constitutional Convention is the cleanest exception I know of. It ran for exactly one year. Most members were newcomers and independents, there were seventeen reserved indigenous seats, and the body *dissolved* when it delivered the draft — so no re-election, no shadow of the future.
>
> And two rules structure everything we do. First: every constitutional initiative needed **eight to sixteen sponsors**. So signing was not a cheap gesture — it was forming a visible, dated coalition. Second: every norm needed **two thirds of the floor — 103 out of 154 votes**. Being born was cheap; surviving was expensive.
>
> One note before we start: I'm a methodologist — my background is in the natural sciences. So I'll report what the models find, and I'll comment on some of it, but the deeper political reading is ongoing work with Jorge, who is the sociologist in this team.

---

## Slide 3 — The data (2:00–3:30) ← slow down here

> Let me walk you through the data, because everything downstream depends on it.
>
> The top is the year: the Convention convenes in July 2021. The **first month** gives us 147 roll-call votes — that's *before* the commissions, before any initiative, before the two-thirds rule was operative. We use that window to measure ideology *before the network existed*.
> Then commissions form in October, initiatives run from November to February, the amendment reports of each commission give us the network in waves, and from February 15th the floor votes actual constitutional content under the two-thirds rule.
>
> The bottom is the pipeline, fully linked: 154 members, nine thousand seven hundred signatures, 947 initiatives — every single one dated from the official platform — 1,809 genesis articles, 4,707 roll calls, and 498 articles that survive into the draft. That's 27 and a half percent.
>
> So we can follow one object — a signature, an article, a vote — through the entire year.

---

## Slide 4 — The seven commissions (3:30–4:15)

> The Convention worked in seven thematic commissions, and they are *very* different worlds. Look at the composition: Justice is 88 percent lawyers; Knowledge Systems has literally zero members with prior political experience. Fundamental Rights is a monster with 283 initiatives.
>
> This heterogeneity matters methodologically: every model I'll show is estimated *within* commission. If you pool everything with one intercept, cross-commission composition flips the homophily signs — it's Simpson's paradox, and we verified it numerically.

---

## Slide 5 — Pre-network ideology (4:15–5:15)

> Ideology is our key covariate, and it has to be *pre-network*.
> We estimate two-dimensional W-NOMINATE on that first month of votes only — replicating Fábrega's 2022 paper. That window predates everything relational, so these positions are exogenous to the network we study.
> Why two dimensions? Because the second dimension is not noise: it separates the reserved indigenous seats from the classic left–right axis, and classification improves from 89 to almost 92 percent.
>
> And for the dynamics we estimate a dynamic IRT over all 4,707 roll calls — that gives us each member's trajectory over 91 periods, with the equation you see: a probit link on the item, and positions that evolve as a random walk.

---

## Slide 6 — Research questions (5:15–6:00)

> Four questions, and they map onto the title.
> One: formation. With zero relational stock, could the network be *predicted* from what people brought with them?
> Two-a: positions. Does exposure to your co-signers *move* your ideal point — or do you just select similar people?
> Two-b: behavior. Does voting *defection* travel along co-sponsorship ties?
> Three: success. What makes an article survive — and, we'll get there, does the *context* an initiative is born into matter at all?

*(RQ1 divider: "First: could we predict the network?")*

---

## Slide 7 — The tool: hybrid bipartite ERGM (6:00–7:30) ← the methodological heart

> Our unit of analysis is the *real* one: person times initiative. A signature is one tie in a bipartite network. We never project, because projection makes one 16-sponsor initiative fabricate 120 pairs at once.
>
> On that network we fit an ERGM — the probability of the whole network, with a vector of sufficient statistics. And the version we fit is a *hybrid*, and this is the part I want you to take home.
> People kept asking us: "why don't you split the ERGM by political sector?" And the answer is: you should never *cut* a bipartite network by node attributes — you'd mutilate every coalition that mixes blocs. Instead, you keep the network whole and you **specialize the counters**.
> So: the mixing matrix counts co-signer pairs by bloc pair — fifteen cells. And every classic homophily counter — same lawyer status, same district, and so on — is **partitioned exactly** into five within-bloc counters plus one across-bloc counter. The partition sums back to the original statistic. That's how the ERGM answers the "by sector" question without breaking anything.
>
> Estimation is maximum pseudo-likelihood — literally a logistic on change statistics, which we certified against the ergm package to ten to the minus eleven — and the standard errors come from an initiative bootstrap: resample initiatives, rebuild the network, re-estimate, five hundred times per commission.

---

## Slides 8–11 — Table A, four passes (7:30–9:45)

**Pass I — District (7:30–8:30):**

> Here's the profile table — one row per bloc, one column per commission — and I'll walk through it with these red boxes, one story at a time.
> First: district. Sharing a district organizes co-signing **inside the right** — plus point four to plus one point four, significant in four commissions, positive in all. Inside the left it's the *opposite*: negative, significant in four. And the across-blocs row is positive in all seven.
> So territory is the right's internal glue, it's *suppressed* inside the left, and it's the main bridge across the political divide.
> And here's the twist: it's not opportunity. The right is the most spread-out bloc — 37 members over 26 districts, only 14 possible same-district pairs. The left has 42. The left has three times the opportunities and *suppresses* them; the right has few and exploits them.

**Pass II — Gender (8:30–9:00):**

> Second: gender. Within blocs it's null or negative — and strongly negative in the reserved seats, which is institutional: their candidacies were gender-paired by design.
> But look across blocs: small, positive, consistent — significant in five commissions. Gender helps signatures *cross* the political cleavage. It works between blocs, not within them.

**Pass III — The lawyer null (9:00–9:25):**

> Third, and this one is a null worth showing: the law degree. In *no* bloc does sharing it organize co-signing. Scattered, small, patternless. In a body drafting a constitution, you'd expect lawyers to seek lawyers. They don't — in any political sector.

**Pass IV — Experience (9:25–9:45):**

> And experience: the only bloc where experienced members cluster together is the left. Small — around point one — but consistent. Everywhere else, nothing.

---

## Slides 12–13 — Table B, two passes (9:45–11:00)

**Pass I — Bloc discipline (9:45–10:15):**

> Second table: dynamics of discipline and structure. The highlighted rows are the ideological *range of each bloc's own contingent* per document: when someone joins a document where their bloc is already present, do they stretch the delegation's range, or fit inside it?
> Where it's significant, it's negative — the left in the two biggest commissions, minus three point seven, minus three point eight. Blocs add signers that do *not* stretch their delegation. Contingents are built compact.

**Pass II — The tabula rasa signature (10:15–11:00):**

> And my favorite structural result: gwdsp — repeated pairs — negative and significant in *all seven* commissions.
> Conditional on activity, on bloc mixing, on every homophily: repeated pairs are **under-used**. When a new initiative forms, the previous coalition is not re-used — it re-forms with fresh pairs.
> Think about what sign you'd expect in the US Congress: entrenched partnerships, positive. Here there was no inherited stock of partnerships — and the model detects its *absence*. That's the structural signature of a tabula rasa.

*(RQ2 divider: "Does the network move positions, or behavior?")*

---

## Slide 14 — RQ2a design (11:00–11:45)

> Now, influence. The design: your exposure at wave t is the weighted mean position of your co-signers. The model is a within-person fixed-effects panel: does my *change* follow where my neighborhood *was*?
> If the network drags positions, lambda is positive. The estimate: plus point zero zero seven, standard error point zero zero four. **Null.**

---

## Slide 15 — Selection, not influence, in one picture (11:45–12:30)

> Why do we believe the null? This picture.
> Panel (a), between persons: my position and my neighborhood's position correlate at point nine five. We *choose* neighborhoods that already resemble us. That's selection.
> Panel (b), within person: if there were influence, this cloud would slope — the farther my neighborhood, the more I'd move toward it. It's flat. And the amber dots are the five percent whose neighborhood does *not* resemble them — the only people with real room to be dragged. Their correlation: minus point zero four.

---

## Slide 16 — The decay hint and its shadow (12:30–13:15)

> One specification grazes significance: exposure with temporal decay — recent collaboration weighted more. Point zero zero eight, p around point zero four.
> The full models are on the slide: the fit is carried almost entirely by mean reversion — minus point six six, identical across the three decays — and the exposure adds this small marginal signal on top.
> But before you get excited: the falsification fires too. *Future* exposure "predicts" today's change just as well — plus point zero one zero, p point zero three. When the future predicts the present, that's not influence — that's selection: I sign with people I'm already moving toward.

---

## Slide 17 — The dated exception (13:15–14:15) ← freshest result, slow down

> There is one exception, and it survived everything we threw at it.
> The clean test is a horse race: decompose future exposure into what the past already contained, and the *innovation*. Race them in one regression.
> With the standard theta, the lag dies — p point seven one. But measured with a **regime-homogeneous** theta — re-estimated using only the two-thirds-era votes, so a single agenda regime — the lag *survives*: plus point zero one six, p point zero zero seven, alongside the innovation.
> It survives measurement propagation — we re-simulated the era's votes fifty times, re-fit the IRT, re-ran everything; Rubin's total standard error gives z of two point five three. And it's *dated*: strong in February–March, fading by April–May.
> Reading: selection is the rule. But right when the 103-vote arithmetic was new, there's a bounded accommodation toward your working neighborhood. Lag and innovation coexist — so this is evidence of coexisting channels, not clean causality. I want to be precise about that.

---

## Slide 18 — RQ2b: defection travels (14:15–15:15)

> Positions barely move. Behavior does.
> Defection: voting against your own bloc's modal vote. Exposure: the weighted share of your co-signers defecting *in the same roll call*. Person fixed effects absorb rebels; vote fixed effects absorb votes that break everyone.
> Phi is eleven point two. Now, part of that is mechanical — if a vote splits my bloc, my co-signers and I defect together without any transmission. So we permute defector labels within bloc-times-vote, two hundred times: mechanics alone gives six point zero.
> Half the raw effect is mechanical — and what remains is still enormous. It travels more strongly *across* commissions, dies within days, and — my favorite detail — it's carried by the *newcomer* pairs. The old guard drags nobody.

*(RQ3 divider: "What makes an article survive?")*

---

## Slide 19 — The survival model (15:15–16:15)

> Now the texts themselves. Unit: the article — 1,565 of them; twenty percent survive into the draft.
> A logistic with commission intercepts, and three families: the coalition's ideological geometry, its network properties, its human capital.
> Three results. Distance to the two-thirds pivot: negative, strong. Ideological *heterogeneity*: positive — wide coalitions survive more. And internal density — pairs with a common history — positive and significant.
> Human capital: share of lawyers, experience, degrees — all null.
> Articles win by geometry and team history, not by credentials.

---

## Slide 20 — The arithmetic of 103 (16:15–16:45)

> The geometry in one picture. The two-thirds rule fixed a pivot at minus point one five. Survival by coalition position peaks slightly *left* of the pivot — the center of mass of the drafting majority — and the premium for widening lives exactly in the left coalitions that can still reach the pivot by stretching.
> The quorum rule became behavior, through the network.

---

## Slide 21 — Does the birth context matter? (16:45–17:45)

> Last analysis. Member-level success — the mean retention of the articles you co-signed — clusters heavily on the network: Moran's I point four four. The spatial Durbin model asks: does the *context* an initiative is born into matter?
> Rho is point eight nine. So yes, "context matters" — and by itself that explains *nothing*. The interesting question is *what the context is*.
> Two decompositions answer it. Your neighborhood's success is predictable from your *own* covariates with R-squared point eight one. And the only covariate that matters — distance to the pivot — *migrates* entirely from the person to the neighborhood when you go from OLS to the Durbin model: own distance, p point nine five; your neighborhood's distance, significant.
> So the context is not some diffuse influence field. **The context is the coalition you stand in** — who they are and where they sit relative to the pivot. Success is a coalition good. And that's why we read rho as clustering, not as influence.
>
> And let me state our position plainly: taken together — the coupling, the covariate migration, and the coalition-level mechanism — **our evidence supports the network-influence view of success**: where you stand in the network shapes how well you do, beyond who you are. What we have not yet done is certify that causally; that requires instrumenting the network itself — the design is ready, and it's ongoing work.

---

## Slide 22 — Takeaways (17:45–18:30)

> So, five things.
> The network *was* predictable from pre-existing traits — bloc, two-dimensional ideology, and, conditionally, territory. Credentials organized nothing, in any bloc.
> The bloc partition pays: territory is the right's glue and the system's bridge; gender crosses blocs; experience lives in the left; and repeated pairs are under-used everywhere — the tabula rasa signature.
> Positions barely move — selection — with one dated exception at the onset of the two-thirds regime.
> Behavior does travel, at twice the mechanical rate, carried by newcomers.
> And texts win by geometry and team history; the "context" of success is the coalition itself.
> The political interpretation of all this is ongoing work with Jorge. Thank you.

---

## Slide 23 — Thanks (18:30)

> Thanks — the QR is the project repository, everything is reproducible. Happy to take questions.

---

--------------------------------------------------------------------------------

### Timing cheat-sheet (target ~18:30)

| Block | Slides | Target |
|---|---|---|
| Setup + standpoint note | 2 | 0:30–2:00 |
| Data + commissions + ideology | 3–5 | 2:00–5:15 |
| RQs | 6 | 5:15–6:00 |
| Hybrid ERGM: tool | 7 | 6:00–7:30 |
| Table A (4 passes) | 8–11 | 7:30–9:45 |
| Table B (2 passes) | 12–13 | 9:45–11:00 |
| RQ2a: design, picture, decay, exception | 14–17 | 11:00–14:15 |
| RQ2b: defection | 18 | 14:15–15:15 |
| RQ3: survival + pivot picture | 19–20 | 15:15–16:45 |
| SDM: what is the context | 21 | 16:45–17:45 |
| Takeaways + thanks | 22–23 | 17:45–18:30 |

**Notes for delivery:**
- The spine: **counters, not cuts** (slide 7) → the table passes pay it off (8–13). If people remember one methodological move, it's the partition of homophily counters by bloc.
- **Release valves if running long** (we are ~30s over): (1) compress Pass III + IV of Table A into one sentence each (saves ~40s); (2) drop slide 16 (decay) and mention it in one line on slide 17 ("one decayed-exposure spec grazes significance, but…"); (3) slide 20 can be one sentence over the figure.
- Slide 17 (the dated exception) is the freshest result — slow down, and keep the "coexisting channels, not clean causality" line verbatim.
- Slide 21: never say "influence" or "contagion" for rho. The scripted line is "clustering, not influence".
- The standpoint note (slide 2) is armor: if a political-science question goes beyond the models, the honest answer is "that's exactly the part Jorge and I are working on — what the model pins down is X".

### Anticipated questions

- *"Why not one ERGM for the whole Convention?"* → Pooling with a single intercept flips homophily signs — Simpson's paradox, verified numerically. Commissions are different arenas (composition table, slide 4). The per-commission suite *is* the general model; a multi-network version with common parameters and per-commission intercepts is on the roadmap (ergm.multi).
- *"Is MPLE valid here?"* → For bipartite networks with this density MPLE point estimates are standard; the known problem is optimistic SEs, which is exactly what the initiative bootstrap fixes (measured inflation 1.2–3.6×). Full MCMC was computationally infeasible (weeks; documented). And our glm implementation is certified against `ergm` to 1e-11.
- *"Why b2twostar and not nodemix?"* → In bipartite networks nodemix crosses mode-1 × mode-2 attributes, which isn't the quantity of interest; the two-star census centered on documents gives exactly the co-signer pair mixing by bloc.
- *"Isn't the era-2/3 result endogenous selection anyway?"* → It coexists with the innovation term, so yes — we present it as coexisting channels, shielded against measurement error and falsification, not as clean causal influence. Latent homophily (Shalizi–Thomas) cannot be excluded in any observational design of this kind.
- *"Why not SAOM/Siena for joint selection and influence?"* → Right tool conceptually; our waves are report-dated (irregular, commission-specific) and positions are estimated with error, which SAOM doesn't natively propagate. It's on the roadmap as a robustness for the paper.
- *"Rho = 0.89 — isn't that just common shocks / mechanical coupling?"* → Partly mechanical, yes: co-signers share articles, so they share outcome components — that's one reason we read rho as clustering. The substantive content is the covariate migration (OLS → SDM), not rho itself. LeSage–Pace impacts and W-sensitivity (0.63–0.95) are in the paper.
- *"The lawyer null — measurement?"* → It's a triple null: conditional logit, hybrid ERGM (in every bloc), and article survival (share of lawyers). Same answer from three designs.
- *"District effect for the right — gerrymander of few districts?"* → No: the right is the most spread-out bloc (26 districts, one 4-member delegation in eastern Santiago); it has 14 possible same-district pairs against the left's 42. Usage, not opportunity.

### Pending before the talk
- Author's review of the data infographic (slide 3).
- Decide whether Table B pass I needs the "local lists" row highlighted too.
