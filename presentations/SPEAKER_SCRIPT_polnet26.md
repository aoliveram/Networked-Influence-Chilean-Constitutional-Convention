# Speaker Script — Political Networks 2026, Manchester (v2)

Target: 18 minutes of talk + 7 of questions. Slide numbers are *logical* slides (section dividers get a one-liner in passing).

---

## Slide 1 — Title (0:00–0:30)

> Hi everyone, thanks for being here. I'm Aníbal Olivera, PhD student in Social Complexity Sciences at Universidad del Desarrollo, in Chile.
> This is joint work with my advisor, Jorge Fábrega.
> The title says "tabula rasa legislature", and that's the whole point: we got to watch a legislature's collaboration network being born from zero.

---

## Slide 2 — A natural quasi-experiment (0:30–2:00)

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
> Commissions worked from October to May, and they started making initiatives that contained one or more articles, and their amendment if the articles were not approved in plenarium.
>
> The bottom is the pipeline: 
> We had 154 members (some of them had potilical experience, some not),
> 947 initiatives (all segned by up to 16 conventionals),
> which gave us 18-hundred genesis articles, which,
> after more than 4,000 roll calls (from wich we calculated political positions of each member),
> resultad in 498 articles that survive into the draft.
> 
> And all this information is fully mapped, and it will be available to anyone soon.

---

## Slide 3b — What we know about each member

> Beyond the documents and the votes, we know who each member *was* before day one: their list and political bloc, their district, age, gender, profession — in particular whether they held a law degree —, their education level, and whether they had **prior institutional experience** — meaning they had held public office before: former members of Congress, mayors, officials. Thirty-five of the 154 had.
> Keep that word, "experience" — it comes back several times.

---

## Slide 4 — The seven commissions (3:30–4:15)

> The Convention worked in seven thematic commissions, and they are *very* different worlds. Justice is 88 percent lawyers; Knowledge Systems has zero members with prior political experience; Fundamental Rights is a monster with 283 initiatives.

---

## Slide 5 — Positions over time, by commission (4:15–5:15)

> These are the revealed positions of all 154 members over the year, commission by commission — estimated from every roll call with a dynamic IRT.
> Two things to see: the left-right separation is stable, and the movement *within* each person is small — that's the thing we'll try to explain in a moment.
> (If asked about dimensions: positions are 2-D at baseline — the second axis separates the reserved indigenous seats; trajectories shown are the first dimension.)

---

## Slide 6 — Research questions (5:15–6:00)

> Four questions.
> One: formation — with zero relational stock, could the network be *predicted* from what people brought with them?
> Two-a: positions — does exposure to your co-signers *move* your ideal point?
> Two-b: behavior — does voting *defection* travel along co-sponsorship ties?
> Three-a: texts — what makes an *article* survive into the draft?
> Three-b: members — does the *context* an initiative is born into matter for its authors' success?

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
> I'll walk it top to bottom with these red boxes.
> 
> The first group is the law degree — and it's a null, with one honest nuance. A handful of cells do reach significance — five of thirty-five, slightly more than chance — but their signs contradict each other: the right is *negative* in one commission and *positive* in another, and nothing replicates across commissions. So: no bloc shows a *consistent* pattern. In a body drafting a constitution, you'd expect lawyers to seek lawyers; there is no consistent trace of that in any political sector.

**Pass II — Experience (7:45–8:15):**

> Second: prior political experience. The only bloc where experienced members cluster together is the left — around plus point one, significant in the two biggest commissions, positive in five. Everywhere else, nothing.
> The bracketed cells, by the way, are quasi-separation — tiny cells — and EMPTY means that pair never once co-signed. We show them as they are.

**Pass III — Gender (8:15–8:45):**

> Third: gender. Within blocs it's null or negative — strongly negative in the reserved seats, which is institutional: their candidacies were gender-paired by design.
> But look at the across-blocs row: small, positive, consistent. Gender helps signatures *cross* the political cleavage. It works between blocs, not within them.

**Pass IV — District (8:45–9:30) ← the star of this table:**

> And fourth, the strongest story: district. Sharing a district organizes co-signing **inside the right** — plus point four to plus one point four. Inside the left it's the *opposite*: negative, significant. And across blocs, positive in all seven — territory is the main bridge over the political divide.
> And it's not opportunity: the right is the most spread-out bloc — only 14 possible same-district pairs against the left's 42. The left has three times the chances and *suppresses* them; the right has few and exploits them.

---

## Slides 12–13 — Discipline and structure, two passes (9:30–10:30)

**Pass I — Compact contingents (9:30–10:00):**

> Second table, top block: when someone joins a document where their bloc is already present, do they *stretch* the delegation's ideological range, or fit inside it?
> Where it's significant, it's negative — the left in the two biggest commissions, around minus three point seven. Blocs add signers that do not stretch their delegation. Contingents are built compact.

**Pass II — The tabula rasa signature (10:00–10:30):**

> And my favorite structural result, the bottom row: gwdsp — repeated pairs — negative and significant in *all seven* commissions.
> Conditional on everything else, repeated pairs are **under-used**: when a new initiative forms, the previous coalition is not re-used — it re-forms with fresh pairs. In an old legislature you'd expect the opposite sign, entrenched partnerships. Here there was no inherited stock of partnerships — and the model detects its *absence*. That's the structural signature of a tabula rasa.

*(RQ2 divider: "Does the network move positions, or behavior?")*

---

## Slide 14 — RQ2a design: the norms era, and a careful clock (10:30–11:45)

> Now, influence on positions. Everything here uses only the *norms era* — the three months when the constitution was actually voted, under the two-thirds rule — and positions re-estimated with era votes only, so the thermometer doesn't mix two different political games.
>
> The design: your exposure is the weighted mean position of your co-signers. The model is within-person: does my *change* follow where my neighborhood *was*? With date fixed effects absorbing anything that moved everyone at once.
>
> And one thing we were careful about — the clock. Positions at t-plus-one are built from votes cast at t. So the lagged exposure uses t-minus-one — that's votes through t-minus-two — and when we build a "strictly future" exposure for the placebo logic, we go to t-plus-two, never t-plus-one. If you're sloppy with this clock, contemporaneous contamination will hand you coefficients that look spectacular and mean nothing.

---

## Slide 15 — The result (11:45–13:00) ← the freshest result, slow down

> Three columns, one story.
> M0: within each person, movement follows past exposure — plus point zero two one. In words: each wave, people close about two percent of the distance to their neighborhood.
> M1 adds date fixed effects: nothing changes. So it's not the common shocks of the era — not the polls, not the big deals.
> M2 is the referee's test. If this were really *selection* — I sign with people I'm already moving toward — then where my network is *heading* should predict my change today, and the past should die. We build the strictly-future innovation, race them… and the innovation predicts **nothing** — p point nine one — while the past exposure stands: plus point zero two, p point zero zero three.
>
> Honest label: an influence *component* that survives every test this panel supports. Latent homophily can never be fully excluded in observational networks — but the selection signature, the future predicting the present, is gone.

---

## Slide 16 — Robustness: decaying memory (13:00–13:40)

> Same family, but letting relational memory fade — recent collaboration weighted more.
> The result doesn't weaken; it sharpens: p below ten to the minus four in all three decay rates.
> And a note on why the thermometer mattered: with the standard positions — estimated mixing both agenda regimes — these same specifications barely grazed significance. Measure the era with its own thermometer and the signal is clean.

---

## Slide 17 — RQ2b: defection travels (13:40–14:50)

> So positions move a little. Behavior moves more.
> Here's the setup. Blocs vote together — that's discipline. But sometimes someone goes off script. The question: when someone breaks ranks, do they break alone — or together with the people they wrote initiatives with at the start? And if together — is it really those ties, or just votes that split everyone?
> Defection: voting against your own bloc's majority in that roll call. Exposure: the weighted share of your co-signers defecting in the *same* roll call. Person fixed effects absorb the born rebels; vote fixed effects absorb the votes that break everyone.
> **Phi is the parameter of interest: do I defect more when my people defect?**
> Phi is eleven point two. But part of that is mechanical — if a vote splits my bloc, my co-signers and I defect together without any transmission. So: the permutation. In each bloc-and-vote, keep *how many* defected, and shuffle *who*. Two hundred times. Mechanics alone gives six point zero.
> Half the raw effect was mechanical — and what remains is still enormous. It travels more strongly *across* commissions, dies within days, and it's carried by the newcomer pairs. The old guard drags nobody.

*(RQ3a divider: "What makes an article survive?")*

---

## Slide 18 — The survival model (14:50–15:50)

> Now the texts themselves. 1,565 articles; twenty percent reach the draft.
> A logistic with commission intercepts and three families of coalition traits: ideological geometry, network properties, human capital. The bold p-values are the survivors.
> Three results. Distance to the two-thirds pivot: negative, strong — coalitions far from the pivot die. Ideological *heterogeneity*: positive — wide coalitions survive more. Internal density — pairs with a common history — positive and significant.
> And human capital: lawyers, experience, degrees — all null. Articles win by geometry and team history, not by credentials.

---

## Slide 19 — The arithmetic of 103 (15:50–16:20)

> The geometry in one picture. The two-thirds rule fixed a pivot at minus point one five. Survival peaks slightly left of the pivot — the center of mass of the drafting majority — and the premium for widening lives exactly in the left coalitions that can still reach the pivot by stretching.
> The quorum rule became behavior, through the network.

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
> The network was predictable from pre-existing traits — bloc, two-dimensional ideology, and, conditionally, territory. Credentials organized nothing, in any bloc.
> The bloc partition pays: territory is the right's glue and the system's bridge; gender crosses blocs; repeated pairs are under-used everywhere — the tabula rasa signature.
> In the norms era, positions follow past exposure — two percent of the distance per wave — and the selection signature does not survive a strict clock.
> Defection travels along co-sponsorship ties at twice the mechanical rate, carried by newcomers.
> And success is a coalition good — the context is the coalition itself.
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
