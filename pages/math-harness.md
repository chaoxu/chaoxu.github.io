---
title: The Math Harness
---

<!-- Generated from ~/kb/notes/essays/math-harness.md. Edit the KB note, then rerun sync-kb-math-harness.py. -->

*Natural-language systems for mathematical research*

*Chao Xu · 14 August 2026*

A mathematical model can write a proof. A math harness decides what problem the model is supposed to solve, what information it may use, how long the search can retain useful state, which claims deserve another attempt, and what must happen before a result is shown to a mathematician.

I use **math harness** here for a system whose normal input is a mathematical question stated in natural language and whose normal deliverable is a human-readable proof, disproof, bound, construction, or precise account of the remaining gap. The system may call a proof assistant, computer algebra system, numerical solver, or search program, but none is required. Lean-first systems whose main task is to turn a formal theorem into kernel-checked code are outside this survey.

The model is only one component. The harness supplies the research loop around it: exact problem statements, context and literature, parallel or iterative proof search, durable memory, computation, adversarial review, and an honest stopping rule. This distinction matters because a larger pile of model calls does not automatically produce better mathematics. A useful harness must preserve the target, accumulate knowledge instead of transcripts, and make an incorrect proof harder to promote than to generate.

This page is a dated map of a fast-moving area, not a leaderboard. Reported solve counts below come from different problems, budgets, release policies, and review standards. They should be read as evidence about system designs, not compared as if they came from one controlled experiment.

# Benchmark

A benchmark for a natural-language math harness has an awkward job. The task must be hard enough to require real mathematical work, recent or private enough to resist memorization, self-contained enough that failure does not merely measure missing context, and judged by something stronger than the presence of proof-shaped prose. Cost, abstention, tool access, literature access, and human steering must also be visible.

## TCS-Bench

[TCS-Bench](https://arxiv.org/abs/2608.09538) is the clearest large benchmark currently aimed at research-style natural-language proof generation. Its 300 tasks are extracted from 190 papers published at STOC, FOCS, and SODA between 2020 and 2026. Each task contains up to 10,000 tokens of curated context, a target statement, and a withheld ground-truth proof. The construction recovers a paper's dependency graph and creates harder variants by hiding intermediate lemmas, so the model may have to reconstruct part of the paper rather than fill one local gap. The [task repository](https://github.com/TCSBench/TCSBench) is public.

The benchmark uses an automated natural-language proof verifier calibrated against 100 expert-labelled proof attempts; the paper reports accuracy above 90% on that alignment task, although it does not document a separate verifier-prompt test split. In the 13 August 2026 revision, the strongest reported direct model solved 204 of 300 tasks. The paper also evaluates an internal harness called **Colosseum**, which explores several strategies, decomposes the target, revises a proof, and uses cross-model criticism to choose between two runs. Cross-model selection solved 203 tasks, compared with 162 and 140 for its two individual arms. Colosseum is only sketched in the benchmark paper, so its result is evidence for cross-model selection rather than a reproducible description of the harness.

TCS-Bench has published reference proofs, scalable difficulty, and enough tasks for controlled comparisons. It does not remove the central difficulty of informal-proof evaluation: its grader is still a language model, and 90% agreement is not mathematical certainty. Papers from 2020 also predate current models, while newer additions offer a cleaner contamination test.

## FirstProof

[First Proof](https://arxiv.org/abs/2602.05192) and its [second batch](https://arxiv.org/abs/2606.18119) test systems on questions supplied by active researchers before solutions are publicly available. The second batch contains ten problems across several fields, allows only public models and tools, gives systems a 24-hour run, and sends the resulting proofs through expert journal-style review. The small sample makes percentages unstable, but the protocol tests something a large extracted benchmark cannot: whether a system can make useful progress on a question whose answer is not already in the literature it can retrieve.

FirstProof has become a common proving ground for Aletheia, RMA, and ProofCouncil. Their quoted results must remain attached to the exact batch and review language. “Correct up to minor revisions,” “complete,” “partial progress,” and “no returned answer” are different outcomes; collapsing them into one solved count hides the information a harness is supposed to preserve.

## LemmaBench and ProofBench

[LemmaBench](https://arxiv.org/abs/2602.24173) continuously extracts lemmas from recent arXiv papers and rewrites them as self-contained problems. Best-model pass@1 rose from roughly 10–15% on the earlier snapshots to 40.8% for GPT-5.5 on the April 2026 snapshot. That movement is part of the point: a refreshable benchmark resists saturation better than a fixed set, although automatic extraction and rewriting create another object that must itself be audited.

[ProofBench](https://arxiv.org/abs/2510.13888) tests the other half of the loop: grading natural-language proofs. It contains 435 model-generated solutions to 145 competition problems, scored by experts on a 0–7 scale. The accompanying ProofGrader reaches a mean absolute error of 0.926 against expert scores. Competition proofs are not research proofs, but this is the right kind of calibration work: a harness should measure its verifier before treating the verifier as an oracle.

The benchmark I would most trust combines these ideas: recent or private targets; a frozen, self-contained context; natural-language proof output; matched model, tool, and token budgets; explicit abstentions; blind expert review; and a separately calibrated automatic verifier used for scale, not as the final source of truth.

# Harness

The general-purpose systems below all accept natural-language mathematics and return natural-language mathematical work. They disagree on the unit of memory, the shape of search, and the authority allowed to say that a proof is finished.

## Aletheia

[Aletheia](https://arxiv.org/abs/2602.10177), from Google DeepMind, runs a generator, verifier, and reviser around Gemini Deep Think until the verifier accepts a solution or the run reaches a limit. It is explicitly end-to-end natural language. The system itself is not released, but [prompts and research outputs](https://github.com/google-deepmind/superhuman/tree/main/aletheia) are public.

Its 700-problem sweep over questions then marked open in the Erdős Problems database is instructive. Aletheia returned 212 candidates; human review found 63 technically correct answers, but only 13 that addressed the intended mathematical question through a relevant argument or literature result. The paper reports four autonomous solutions or partial solutions to open questions. The large drop from plausible candidate to meaningful answer shows why target interpretation and novelty checking belong inside the harness, not after it.

## Rethlas

[Rethlas](https://arxiv.org/abs/2604.03789) is an open-source natural-language reasoning agent with theorem retrieval through Matlas and an iterative worker–verifier loop. It explores literature, constructs toy examples and counterexamples, plans a proof, writes an informal argument, and sends it to a separate natural-language verifier. Its recursive-proving skill can dispatch several subagents across decomposition plans. The same project pairs Rethlas with Archon for optional Lean formalization, but Rethlas itself is useful without Lean and [its code is public](https://github.com/frenzymath/Rethlas).

Rethlas demonstrated that retrieval and verification can turn a coding agent into a research prover. Danus retains that worker–verifier core while adding persistent concurrent workers, global orchestration, and fact-graph memory.

## Danus

[Danus](https://arxiv.org/abs/2607.06447) organizes parallel Rethlas workers around a content-addressed fact graph. A main agent chooses directions; workers attack claims, counterexamples, or toy cases; a stateless verifier is the only component allowed to admit a claim into the graph. Each accepted fact carries its proof and dependencies. The graph, rather than any agent transcript, becomes the shared mathematical state from which a final paper is assembled. [Danus is open source.](https://github.com/frenzymath/Danus)

The paper presents six case studies in algebraic geometry, singularity theory, and combinatorics. Its strongest architectural claim comes from a matched case in which three Rethlas runs failed while Danus, using the same worker and verifier models, produced a theorem-level solution. Human review later found one local justification incomplete but repairable. This is still one case study, not a general benchmark result, but it isolates the value of parallel search and fact-level memory more cleanly than comparisons that also change the base model.

## ProofCouncil

[ProofCouncil](https://arxiv.org/abs/2607.09474) uses an author–critic loop. The author edits a proof, research notes, and references; a stateful critic follows revisions; every few rounds a fresh critic resets the review context. The author may call a council of other model families or a compute agent equipped with computer algebra systems. The proof returns only after the author, stateful critic, and fresh critic agree. Both the [harness and its DAG-based agent library](https://github.com/eth-sri/proof-council) are public.

On the second FirstProof batch, expert referees judged six of ten submissions correct up to at most minor revisions. On 30 researcher-supplied problems, 21 received feedback: five were judged complete, two possibly complete, eight useful partial progress, four had no apparent errors but little substantive progress, and two answered easier interpretations. The official FirstProof run cost about $350 per analyzed problem, while a roughly $12 one-query baseline solved four of nine analyzed problems rather than six. That tradeoff is as important as the headline result. A harness should earn the extra calls.

## QED

[QED](https://arxiv.org/abs/2604.24021) separates literature survey, proof planning, proof generation, structural verification, detailed verification, selection, and the retry decision. Its harder mode represents the proof plan as a dependency DAG. A regulator distinguishes an execution error from a bad plan and a bad overall approach, triggering proof revision, plan revision, or a complete rewrite. Different coding-agent backends can generate and verify in parallel. [The implementation and proof artifacts are public.](https://github.com/proofQED/QED)

The paper evaluates 18 research projects and reports five original works accepted by the corresponding domain experts. Two of those projects have separate public manuscripts that explicitly attribute their proofs to QED: [lower bounds for advection–diffusion equations](https://arxiv.org/abs/2605.20623) and the [return probability of a lamplighter walk on a regular tree](https://arxiv.org/abs/2605.21744). Both papers include a QED developer as an author, so they demonstrate output rather than independent third-party adoption. In the subset using one reported verifier configuration, all 17 verifier-accepted candidates were later accepted by experts. This is useful positive evidence, but not an estimate of false-acceptance probability: rejected candidates were not all independently labelled, and the projects were not a frozen random test set.

## RMA

[Research Math Agents (RMA)](https://arxiv.org/abs/2605.22875) combines problem analysis, controlled literature search, a compact knowledge bank, proof-generation agents, verifier agents, and disk-based structured memory. RMA reports eight correct solutions on the first FirstProof set under its own blind expert evaluation, compared with several direct and agentic baselines. The paper also reports ablations over memory, number of proposers and verifiers, and number of rounds.

RMA is evidence for a modular research workflow, but not yet a reusable public harness: the May 2026 paper says that solutions and implementation will be released upon acceptance. Its results should remain labelled as author-reported until the system and complete run artifacts can be inspected.

## AI co-mathematician

The [AI co-mathematician](https://arxiv.org/abs/2605.06651) is broader than a proof-returning loop. A project coordinator negotiates goals with a human, launches parallel workstreams, delegates to literature, reasoning, coding, and review agents, and writes incremental and final reports into a shared workspace. Human steering remains available while work continues. The current prototype's proofs are informal; formal provers are described as optional future components.

Its case studies emphasize collaboration rather than autonomous pass rates: a mathematician can inspect a failed proof, salvage its strategy, supply the missing idea, and ask the system to rebuild and review the argument. This is a genuine harness use case. For active research, the valuable output is often not “the agent solved it” but a well-preserved state from which the mathematician can act.

## qmd-prover

[qmd-prover](https://github.com/powergiant/qmd-prover) turns a natural-language theorem, rough idea, or existing proof development into a dependency-explicit natural-language proof project. Claude Code or Codex writes definitions, statements, and proofs as plain-text Quarto Markdown blocks with stable identifiers and explicit citations. The TypeScript tool locks the main statement, checks the document and dependency graph mechanically, optionally sends each proof with only its cited premises to a fresh AI reviewer, composes the local verdicts through the dependency graph, and can render the result as a navigable Quarto book.

The host coding assistant still supplies the proof search, and the optional reviewer remains a language model rather than a formal kernel. Model separation is configured rather than enforced, assumed facts may be permitted, and the default review policy allows advisory gaps that strict mode would reject. The repository reports a 34-fact Gödel-completeness demonstration, but publishes no benchmark or auditable verifier transcript. I found no public paper that cites qmd-prover or attributes a research result to it as of 14 August 2026.

## Coverify

[Coverify](https://github.com/chaoxu/coverify) is my open-source attempt to make the verification and durable-state contract explicit. One campaign freezes one exact statement, sends independent workers down different routes, records dead routes and precise gaps, and keeps every candidate, audit, reconstruction, and promotion as plain files. A candidate passes through a hostile audit, a check that the reconstruction brief does not leak the proof, a blind reconstruction from that brief, and a comparison between the independent reconstruction and the candidate. Reuse of a verifier record is keyed to the candidate, frozen statement, promoted-premises view, and the stage-specific dependency bundle, then checked against the saved artifact hash.

Coverify is not a proof assistant. “Promoted” means that specified language-model reviews passed on exact saved bytes; it does not mean the theorem is machine-checked or true. The current implementation is a working research harness with live campaign experience. Its decisive token-controlled comparison against the raw proof-search workflow has not yet been run, so cost-efficiency remains a design target rather than a result.

## Published mathematical output

Rethlas has the largest traceable public-paper footprint I found. For this snapshot I counted a distinct public result manuscript only when its own text names the harness and credits it with generating, discovering, completing, materially structuring, or verifying mathematical content. I excluded each harness's system paper, benchmark reports, private or repository-only artifacts, ordinary related-work citations, and statements that one harness merely descends from another.

| Harness | Separate public result manuscripts found |
| --- | ---: |
| Rethlas | 21: 19 arXiv papers and 2 public notes |
| Danus | 11 |
| Aletheia | 5: 4 central contributions and 1 substantive auxiliary theorem |
| AI co-mathematician | 4 |
| QED | 2 |
| ProofCouncil, RMA, qmd-prover | 0 found |

One manuscript credits both Rethlas and Danus, so the first two rows contain 31 distinct manuscripts rather than 32. The counts measure disclosed use, not mathematical correctness, autonomy, importance, or independent adoption. They are lower bounds dated 14 August 2026: papers can appear faster than indexing, and authors vary in how much AI assistance they disclose.

### Rethlas attribution ledger

The 21 manuscripts behind the leading count are:

1. [A Counterexample to Problem 19 on Integer-valued Polynomial Rings](https://arxiv.org/abs/2604.05922)
2. [An Integrally Closed Reduced Ring with McCoy Localizations That Is Neither McCoy nor Locally a Domain](https://arxiv.org/abs/2604.07465)
3. [On a question of Kollár and Kovács](https://arxiv.org/abs/2605.20585)
4. [An example of a very non-movable effective divisor](https://arxiv.org/abs/2605.20594)
5. [Optimal bend-and-break for foliations](https://arxiv.org/abs/2605.20754)
6. [On a question of Mauri and Moraga](https://arxiv.org/abs/2605.22052)
7. [A question on klt type varieties of Han and Jiang](https://arxiv.org/abs/2605.22250)
8. [Shokurov's global index conjecture for threefold foliations](https://arxiv.org/abs/2605.22735)
9. [Boundedness of total Cartier indices for rational singularities in families](https://arxiv.org/abs/2605.22782)
10. [Degenerate constants in degree inequalities for Sobolev circle maps](https://arxiv.org/abs/2605.24626)
11. [On some open problems in commutative algebra resolved by Rethlas](https://arxiv.org/abs/2605.25259)
12. [Lift-independence problem in the $p$-adic Simpson correspondence for curves](https://arxiv.org/abs/2605.29947)
13. [A counterexample to the near-quadratic Elekes–Rónyai expander conjecture over $\mathbb R$](https://arxiv.org/abs/2606.16738)
14. [On Injectivity of Phase Retrieval](https://arxiv.org/abs/2606.17922)
15. [Criteria of isolated weighted homogeneous hypersurface singularities using Logarithmic vector fields](https://arxiv.org/abs/2606.29891)
16. [Involution-equivariant topological recursion and mirror symmetry for the affine binary dihedral Calabi–Yau threefold](https://arxiv.org/abs/2607.07355)
17. [Kazhdan–Lusztig polynomials of matroids need not be unimodal](https://arxiv.org/abs/2607.24186)
18. [Analytic Bertini theorem II — The local case](https://arxiv.org/abs/2607.25230)
19. [A counterexample to the zero-mass conjecture](https://arxiv.org/abs/2607.26549)
20. [Factorial asymptotics of the Matryoshka numbers](https://jihaoliu.org/notes/Matryoshka.pdf)
21. [On a conjecture of Esser, Totaro, and Wang](https://jihaoliu.org/notes/ET74.pdf)

The [Rethlas results repository](https://github.com/frenzymath/Rethlas_results) preserves raw outputs for several of these projects. The individual manuscripts remain the attribution authority: some describe complete autonomous proofs, others a discovered construction, repaired proof, verified computation, or substantial proof architecture later checked and rewritten by humans.

## What the systems are converging on

Across these projects, the useful common core is smaller than their diagrams suggest:

1. **Freeze the target.** Keep the exact quantifiers, hypotheses, conventions, and acceptance criterion visible. A correct proof of an easier statement is a failure.
2. **Separate search from judgment.** Let workers explore freely, but do not let a proof author certify its own output. Reset reviewer context or use a fresh model when path dependence matters.
3. **Store mathematical state, not chat history.** Facts, dependencies, failed mechanisms, counterexamples, and open obligations survive. Most transcripts do not deserve to.
4. **Make failure productive.** A dead route needs an obstruction and a condition under which retrying it would be new. Otherwise parallelism buys duplicate attempts.
5. **Bind verdicts to artifacts.** If the proof, statement, dependency list, or supplied context changes, the old review no longer applies.
6. **Distinguish evidence levels.** Model-approved, cross-model-approved, human-reviewed, computationally certified, and formally checked are not synonyms.
7. **Measure the harness against the direct model.** Verification cost belongs inside the budget. More calls, more agents, and longer reports are expenses until they produce more correct mathematics or better durable progress.

Natural-language verification remains the weak joint. A fresh critic can catch a gap and still share the prover's blind spot. A human-readable proof has no kernel that returns an error at the first invalid inference. General harnesses compensate with independence, adversarial prompts, cross-model review, provenance, and human referees. When a problem admits a small exact checker, a more specialized design can do better.

# Problem Specific Harness

A problem-specific harness changes the interface between creative search and correctness. Instead of asking a language model to judge an entire unfamiliar proof, the designer identifies a restricted object—a polynomial inequality, interval certificate, geometric rule, finite witness, or candidate construction—that software can test exactly or conservatively. The model searches over meaningful mathematical objects; the domain-specific verifier supplies dense, local feedback.

This approach gives up generality. It also creates the clearest evidence that the harness, rather than only the base model, contributes something: the verifier exposes why a candidate failed and turns the next model call toward a smaller defect.

## Grothendieck constant

[Long-Horizon AI Research for Grothendieck Constant](https://arxiv.org/abs/2608.11195) describes a system built around one analytic optimization problem; the complete mathematics appears in a [companion paper](https://arxiv.org/abs/2608.11158). A natural-language reasoning model chooses directions and develops arguments. A coding agent runs experiments and certifies one-dimensional inequalities with Arb interval arithmetic. A human-editable bulletin steers the live search, while session reports preserve proofs, code, failures, and explanations for later sessions.

The run lasted from 16 June to 24 July 2026: roughly 240 research sessions, 2,091 reasoning-model calls, 152 million tokens, an estimated $5,400 in API cost, and about 40 dated human directives. The system discovered and first proved the lower bound $K_G \ge 6\pi/11$; the authors then independently checked and rewrote the proof. The upper-bound construction in the companion result came from an earlier conversation with a reasoning model and predates the reported long-horizon harness. Several stronger bounds produced later remain labelled “system-tested” because their certificates have not yet been checked by the authors.

The most valuable failure is documented. An exploratory numerical score lost its caveat during repeated state compression and was treated as a record for 25 days before a later test withdrew it. The full archive still contained the warning; the compact research state used for decisions did not. Long-term memory is therefore not solved by saving everything. The harness must preserve the few facts that govern whether later conclusions are valid.

## Gilbert–Pollak and the Steiner ratio

[Towards Solving the Gilbert–Pollak Conjecture via Large Language Models](https://arxiv.org/abs/2601.22365) narrows proof search much further. The model does not attempt the conjecture end to end. It generates rule-constrained geometric lemmas as executable structured code. Those lemmas instantiate **verification functions** whose shape reduces a continuous inequality over a region to checks at finitely many vertices, with symbolic algebra and recursive subdivision supplying the certificate. The [prompts, code, and certificate](https://github.com/keyisi2006/Steiner-Ratio) are public.

The search–verify–reflect loop reports a certified Steiner-ratio lower bound of 0.8559, improving the prior 0.824 bound while remaining below the conjectured $\sqrt{3}/2$. When a candidate set of lemmas fails, the verifier returns a localized region in which coverage is missing; that structured failure becomes the next prompt. This is much stronger feedback than “the proof may contain a gap.”

The system is not fully autonomous. The paper states that every model-proposed lemma was manually checked before being installed as a verification function, and its cylindrical-algebraic-decomposition verifier becomes expensive as dimension and degree grow. Those limitations define the bargain: human and symbolic effort build a narrow certified search space, and the language model explores it at a scale that manual lemma design could not.

## The design opportunity

The boundary between general and problem-specific harnesses should stay movable. Begin with natural-language search and durable adversarial review. When repeated attempts hit the same checkable bottleneck, extract that bottleneck into a project-owned verifier and feed its smallest counterexample or uncovered region back to the search. Do not put domain logic into the general harness, and do not pretend that a finite checker proves an unbounded theorem unless the mathematical reduction to that checker has itself been proved.

The long-term shape is a thin general harness around many local instruments. Natural language remains the interface for stating the question, choosing a direction, explaining a proof, and involving a mathematician. Exact tools enter where the problem supplies an exact interface. Lean can be one such tool. It does not have to be the center of the system.
