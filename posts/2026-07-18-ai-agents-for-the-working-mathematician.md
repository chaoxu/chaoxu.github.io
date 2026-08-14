---
title: AI Agents for the Working Mathematician
tags: math, AI
---

Most working mathematicians I talk to or observe have the same relationship with AI: they paste a question into ChatGPT, get something proof-shaped back, feel vaguely impressed or vaguely cheated, and go back to work. This makes sense, especially under the common opinion:

> If you ask ChatGPT a few times and it does not solve the problem, then AI can NEVER solve the problem by itself.

This opinion was formed by mathematicians worldwide through long and brutal ChatGPT sessions: read the latest output, send "keep going", wait for a response, repeat until the entire afternoon is wasted. If the opinion were true, then asking ChatGPT a few times would be the optimal way of using AI to do math — and to be fair, even that is occasionally strong enough to solve real open problems.

But the opinion is **wrong**. You can use a system that is far more capable, with a much higher chance of success than the standard opinion allows.

Specifically, you can have an autonomous agent that keeps bashing on a conjecture for hours: it keeps track of itself, learns from its mistakes, does not waste its time retrying something already tried, does not waste your time with long wrong proofs, and you can still influence its direction.

I will use some words like token, model, and harness. If you don't know what they mean, watch the first part of [Edward Lockhart's talk](https://www.youtube.com/watch?v=XRTmhpOW1WA). I recommend it to anyone who has a math background but no idea what these new AI things are.

# Use Agents

An *agent* is an AI that uses tools. Here the tools are exactly what you would use on a computer: operating system, Python, a LaTeX compiler, SAGE math, a browser. Agents can interact with the environment and actually "do" things.

**Codex** is the coding harness from OpenAI. It runs agents on your own computer. A "chat" session in Codex would be an agent. **ChatGPT Work**, which is different from ChatGPT, is also an agent, and can run on OpenAI's server. I prefer Codex as one can maintain even tighter control, but if you just want to explore, ChatGPT Work is easier as you don't even need to download anything. However, this article I will assume people will be using Codex.

What Codex buys you over the chat window:

- **Files are durable state.** Your problem statement, definitions, proved lemmas, failed attempts — they live in a directory, in git if you like. Progress is not lost, you can keep advancing.
- **You control the context.** The agent reads the files you point it at. You have much more control over what agents can do. A clean `STATEMENT.md` with your exact conventions beats re-explaining notation every conversation.
- **Tool use.** Gurobi to construct counterexamples, SAGE to compute some polynomial, LEAN to formally verify some statement it proved, access to your compute cluster, and tools that help with your computation that you never knew existed.
- **Long autonomous runs.** You can hand it a hard target, leave, and come back to either progress or a precise record of what failed.

Step by step on how to do it.

1. Use an agent
    1. Codex: Download the Codex program and log in with your ChatGPT account.
    2. ChatGPT Work: Just open the ChatGPT website, and switch it to work.
2. Set the model to the highest available, currently gpt-5.6-sol ultra.
3. Replace `[STATEMENT]` in the prompt below with your exact claim — all quantifiers, your conventions, what is known, and what counts as an answer — then use it on a conjecture you always wanted to solve.

*Prompt updated 2026-08-02: removed the time floor (the completion condition decides when the run ends), failed audit verdicts now stick to the exact version, new approach families get a cheap kill-check before agents are assigned, and LESSONS.md became PROCESS_LESSONS.md — process lessons only, so mathematical claims cannot hide there.*

*Prompt updated 2026-08-14: restored multi-round route diversity and delayed cross-pollination from the CDC prompt, made root-agent synthesis and stop handoffs explicit, separated hostile audit from blind reconstruction, and corrected proof-or-counterexample wording that was too specific to CDC.*

```markdown
Current task statement

[STATEMENT]

## Success criteria

Partial progress does not count unless it implies exactly the resolution above. In particular, proofs for special classes, reductions to another unproved conjecture, computational verification through any fixed size, and candidate counterexamples without a complete proof that they satisfy the hypotheses and violate the conclusion are insufficient. If the statement above is ambiguous about what counts as an answer, ask before starting; never resolve ambiguity silently.

## Durable state — create these files and directories before searching

- STATEMENT.md — the exact statement, conventions, edge cases, and success criteria. Fixed for the whole run; never edit it to fit a result.
- RUN_STATE.md — campaign state: running, paused, cancelled, or completed; the latest user directive; the persistent-goal identifier and state; and the baseline revisions used for independent search.
- AUDIT_CHECKLIST.md — a revision-numbered, append-only list of problem-specific failure modes and edge-case checks. Give every item a stable ID and record its source route.
- REGISTRY.md — one row per approach family: family name, exact claim attempted, exact remaining gap, smallest known obstruction, next decisive test, independent-wave count, maturity, dependencies, exact evidence links, stall classification, and state: active, paused, blocked, or closed.
- ROUTES/ — one directory per approach family containing its versioned briefs and search-agent deliverables.
- FAILED.md — every blocked or closed route: what was tried, the exact obstruction, the evidence for it, and what would make a retry materially new.
- CANDIDATES/ — immutable, versioned candidate proofs, refutations, route-closing obstructions, and their pre-audit reconstruction briefs. Any mathematical edit creates a new version; never overwrite an audited version.
- AUDITS/ — immutable leakage, audit, reconstruction, and comparison records, each naming the exact candidate and brief versions and every input it saw.
- EVIDENCE/ — versioned computation sources, logs, outputs, witnesses, and certificates, each linked to its approach family and any candidate it supports.
- PROVED.md — promoted results only, each with its status label and proof or certificate.
- PROCESS_LESSONS.md — transferable process lessons and environment failures only; never mathematical claims.

All mathematical work products land in these files and directories, not only in conversation. After context compaction, the root re-reads the full durable state, each search agent re-reads only its authorized route bundle, and every verification role re-reads only its explicitly authorized input bundle. These artifacts are the memory; the conversation is not.

## Status vocabulary — literal, never inflated

Every claim carries exactly one label, with these exact meanings:

- candidate — produced but not verifier-backed. Partial, inconclusive, failed, or protocol-only checks do not advance this label.
- self-audited — re-checked only by its own author or context. For building on, this counts the same as candidate.
- verifier-backed — the exact candidate version whose leakage check, hostile audit, independent reconstruction, and comparison have all passed against their recorded inputs.
- promoted — verifier-backed and recorded in PROVED.md; later work may cite it, and anything built on it carries at most this label.
- independently audited — the exact promoted version was additionally checked by a nonparticipant from outside the producing model family: a different-family model or a human. Present final answers at this label when possible.
- retracted — a previously checked version was later shown to contain a mathematical failure. It remains in the record but may never be cited as a premise.

If no different-family model is available in this environment, do not simulate independence with another instance of the same family — that is label inflation. Instead deliver the final answer at promoted status, state prominently that the cross-family audit has not run, and list the specific claims an outside model or human referee should check first, in order of risk. Unavailability or a protocol failure may leave the result promoted; a substantive mathematical failure from an outside audit immediately retracts that exact version and requires repair or a new candidate. When a version is retracted, recursively retract every version whose claim contract names it as a dependency, remove the affected entries from PROVED.md, and update their approach families in REGISTRY.md.

A claim's label only advances through the verification steps below. A later argument never inherits more certainty than its weakest premise's label. Never call a mismatched case, a global compatibility assertion, or a polynomial recurrence "routine" — those are where proofs hide their hard step.

## Orchestration

Use subagents aggressively and dynamically, at most 6 concurrent. A search wave is one batch of search-agent work followed by root synthesis. An independent search wave is one in which a fresh search context receives no other live route's content. Verification and kill-check work do not count as search waves. Do not use a fixed assignment such as "N agents for strategy X."

Before the first independent search wave, record baseline revisions of AUDIT_CHECKLIST.md, FAILED.md, and PROVED.md in RUN_STATE.md.

- Begin with a genuinely diverse portfolio: substantially different formulations, invariants, reductions, algebraic viewpoints, structural inductions, decompositions, embeddings, extremal arguments.
- Do not tell most agents the currently favored approach; preserve independence during early rounds so they do not converge on the same attractive but incomplete reduction. Agents may be assigned a direction, but during this independent phase never show them another agent's partial proof of it.
- Before maturity, a search agent may read only STATEMENT.md; revision-pinned, route-filtered views of the baseline AUDIT_CHECKLIST.md, FAILED.md, and PROVED.md; any later FAILED.md entry cited in its novelty check; its route brief; and its own route artifacts. The filtered views may include later entries from this same route but must exclude every entry sourced from another live route. It must not inspect other live-route rows, CANDIDATES/, or AUDITS/. The root supplies an explicit input allowlist and exact revisions with each assignment. Exposure to another live route's content invalidates the independent-wave credit.
- Keep several mathematically incompatible routes alive through at least two independent search waves unless a verifier-backed obstruction closes a route. One independent-wave credit requires a fresh search context with a distinct recorded brief and a non-status mathematical deliverable: a proved lemma, explicit construction, counterexample, or exact obstruction with a decisive next test. A route earns at most one credit per root-synthesized wave; parallel assignments in the same batch share that single credit. Duplicate assignments and bare status reports earn no credit.
- After a route has two independent-wave credits and its REGISTRY.md row states its exact claim, exact gap, smallest obstruction, next decisive test, dependencies, and evidence links, the root may mark it independently matured. Cross-pollination or combination may occur only between routes that have each matured. An unmatured route's briefs may use only its authorized inputs above; root synthesis may guide its assignments but must not leak content from another live route. After all participating routes pass the gate, the search side — root and search agents — may share status-labelled lemmas, mechanisms, counterexamples, and obstructions, each with its source family, version, and dependencies. Hostile auditors remain isolated from search discussion; reconstruction agents remain proof- and audit-blind.
- Group approaches in REGISTRY.md by the mathematical mechanism and by their terminal missing lemma, not by terminology. If several agents converge to one family, redirect the surplus toward underexplored formulations.
- At the end of each search wave, the root updates REGISTRY.md, synthesizes consequences across routes without leaking them into unmatured-route briefs, challenges the current favorite, and decides which families to continue, pause, or redirect; it may combine only matured families. It may close a route only when a verifier-backed obstruction is recorded in FAILED.md and linked to its exact evidence version.
- Before a surviving route earns two independent-wave credits, record a stall as paused and schedule a distinct remaining independent assignment; one agent's failure cannot block the family. After maturity, a route that ends at a missing lemma as strong as the original conjecture is blocked, not "one lemma away." Record it in FAILED.md. A blocked route may reactivate only for a materially new mechanism, invariant, lemma, construction, source, witness, certificate, or scope, recorded in REGISTRY.md. A closed route remains closed only while its closing obstruction remains verifier-backed; if that version is retracted, return the route to paused. A changed mechanism or scope creates a new linked route rather than reopening the old one.
- Before starting any route, check FAILED.md and state either: "no close prior route" or "closest prior route is X; this differs materially because of <new lemma / source / witness / certificate / scope>."
- Before assigning search agents to a new approach family, spend one fresh agent trying to kill it first: check the smallest instance and adversarially test any claimed source the approach builds on. The kill-check agent's inability to make the route work is inconclusive. A concrete target mismatch, violated hypothesis, circular dependency, or counterexample becomes a candidate route-closing obstruction and must pass the verification cadence before the route closes.
- Require every search agent to return a proved lemma, an explicit construction, a counterexample, or an exact obstruction with a decisive next test. Require every kill-check, leakage-check, audit, and comparison agent to return an exact version-bound verdict with evidence; a reconstruction agent returns a complete version-bound argument and its dependencies, not a verdict on a candidate it never saw. Reject vague optimism and claims that an unproved global compatibility statement is routine.

## Stalled routes

Use active while a route is assigned, paused while it remains viable but is deprioritized, blocked after maturity when its exact gap has no current mechanism, and closed only when a verifier-backed obstruction rules out the route as stated. When a route stalls, also classify it in REGISTRY.md as either (a) method failure or (b) evidence against the target statement. If (b), redirect part of the effort to refutation search appropriate to the statement's logical form, such as a counterexample or impossibility proof. Every stalled route must carry one of these two classifications; "still working" is not a classification.

## Verification cadence

Every candidate proof, refutation, or route-closing obstruction enters the cadence as a new immutable version in CANDIDATES/. Freeze with it an exact claim contract: statement, hypotheses, quantifier ranges, object identity, required outputs, and promoted dependencies. Before any audit, also freeze a reconstruction brief containing that contract and at most three one-sentence strategy hints. A hint may name definitions and promoted results; it may not state an unpromoted intermediate claim, ordered derivation step, case split, calculation or equation, witness value, or quotation or paraphrase from the candidate. A fresh leakage checker compares the candidate, contract, and brief; if the brief violates this boundary, replace it with a new version and repeat the check.

1. A fresh hostile auditor sees only STATEMENT.md, the exact claim contract and candidate version, the exact statements of its promoted dependencies, and a named revision of AUDIT_CHECKLIST.md — not search transcripts, REGISTRY.md commentary, or the author's defense. It tries to refute the candidate, explicitly tests every listed edge case and failure mode, checks citations and quantifiers, and checks for circular use of a reformulation equivalent to the target.
2. In a fresh context with no inherited conversation, an independent reconstruction agent sees only STATEMENT.md, the accepted pre-audit brief and claim contract, and the exact statements of its promoted dependencies — not the candidate, hostile audit, REGISTRY.md, FAILED.md, search transcripts, or other candidate files. It writes a complete argument from scratch.
3. A fresh comparator sees STATEMENT.md, the claim contract, the exact promoted-dependency statements, the candidate, the reconstruction, and their input manifests. It checks the reconstruction's mathematical correctness; maps every hypothesis, quantified object, dependency, conclusion, and required output between the two arguments; and checks that the reconstruction implements the brief's mechanism. If the reconstruction changes the claim or mechanism, bypasses a disputed step, or relies on a different dependency, save it as a new candidate and start its own cadence; it does not validate the earlier candidate.

Save the leakage verdict and all three verification records in AUDITS/, each bound to its exact inputs. Before promotion, compare the hostile audit's checklist revision with the current revision. If new items were added, a fresh hostile auditor checks the candidate against every added item and saves a supplemental version-bound record in AUDITS/. Only after every check passes does the exact candidate version become verifier-backed. A failed check sticks to that exact version: address the specific objection in a new version or retract the claim — never resubmit an unchanged or cosmetically edited proof to a fresh checker. Every edit to a candidate creates a new version and repeats the cadence. Audit counterexamples and impossibility conclusions with the same hostility as proofs. A refutation may close a live route only after that exact version becomes verifier-backed. Before final presentation, check the same exact promoted version with a nonparticipant from a different model family or a human when one is available; if none is available or the check fails for protocol reasons, deliver at promoted status with the disclosure required above.

## Reporting gate

Send me an unsolicited progress report only on a significant update: a promoted proof or refutation of the exact statement; a promoted lemma that removes a named dependency; a verifier-backed minimal obstruction that closes a route; or a promoted strictly stronger or simpler theorem. New notation, restructuring, another finite computation, or a reduction to a theorem-strength lemma is not significant and goes in the files, not in a report. This gate controls progress interruptions; it does not permit ending the campaign before the Persistence condition below.

## Computation rules

Only write programs when essential; most work here should be proofs. When a computation is justified, it is a named finite question whose output is a small witness, certificate, or table. Never run computation through inline stdin. Every run gets a named source file, the approach-family ID it serves, a timeout, a log, and an output location under EVIDENCE/, recorded in REGISTRY.md. A computation can refute a lemma or discover a certificate; it cannot prove an unbounded theorem — preserve the smallest exact certificate and then prove the resulting claim.

## Web policy

Public search may be used only for ordinary mathematical background or standard named theorems, not to search for a solution to this exact problem or benchmark. Do not search the public web merely to determine whether the problem is open, and do not answer that it is open.

## Persistence

Do not end the campaign merely because current approaches fail or agents report theorem-strength gaps. Continue launching new rounds, reopening routes only under the rule above, and searching for fresh formulations. There is no time budget: the completion condition, not the clock, decides when the run ends. End the campaign only when a complete proof or refutation of the exact statement has reached promoted status, then mark RUN_STATE.md completed; otherwise keep FAILED.md and REGISTRY.md as the honest record and continue.

Register this task as a persistent goal (create_goal) with "a complete proof or refutation of the exact statement has reached promoted status after the full verification cadence" as the completion condition, so the objective survives context compaction and session restarts. If goal tooling is unavailable, say so at the start of the run.

## On stop

An explicit stop, pause, or cancel overrides Persistence immediately. Launch no new work, interrupt outstanding work when the tooling supports it, mark interrupted active routes paused, and record each unreturned assignment as interrupted with no claim; any preserved partial mathematical artifact remains candidate. Reconcile anything already returned into the durable state. On stop or pause, set RUN_STATE.md to paused, leave the goal incomplete, record the directive, and do not resume without an explicit user instruction. On cancel, set RUN_STATE.md to cancelled and cancel or deactivate the goal when supported; otherwise record the cancellation and likewise do not resume. Append to PROCESS_LESSONS.md what would make future runs more efficient — preferring lessons transferable to other mathematical problems, plus any environment issues that wasted time. Process lessons only, never mathematical claims. After the durable handoff is complete, report every maximal rigorously established frontier, its exact remaining gap, the earned status of every supporting claim, and the state of every live approach family, even if the reporting gate would normally keep that material in the files.
```

# Explanations and Improvements

The prompt combines ideas from the [CDC prompt](https://cdn.openai.com/pdf/04d1d1e4-bc75-476a-97cf-49055cd98d31/cdc_prompt.pdf), [Danus](https://github.com/frenzymath/Danus), and things learned from my own runs. You can read it. It is not perfect but gets 90% there.

The prompt is **only the beginning** of your exploration. The prompt should evolve (under your direction) and become closer to something that matches your workflow, and maybe become even smarter. Here I describe how this prompt came about, and things you might want to do to improve it.

The idea is to run an agent that supervises other agents that work towards a single goal; this is called an orchestrator. This is known to be better than a single agent working on the problem alone, which quickly fills up its own context window and gets confused. Here are some highlights, and why we did it.

**1. Write the statement and success criteria.** `STATEMENT.md` contains the exact claim with all quantifiers, the conventions, and what would count as an answer. It is the fixed point that prevents the target from changing during a run. Newly discovered failure modes go into the append-only audit checklist instead of changing the statement.

**2. Launch attempts with a prompt that pre-blocks the cheap outs.** Those things are there because they are common ways where the model tries to end work early.

**3. Audit adversarially.** A hostile auditor sees the frozen candidate and tries to break it. A separate agent reconstructs the argument without seeing the proof or audit, and a comparator checks that the reconstruction did not silently prove something different. Model families have correlated blind spots: I have had a proof that GPT could not fault no matter how it was prodded, and Opus found the flaw. I personally also use Fable 5 to verify the final output.

**4. Record what died.** Failed routes go in a file, each with: what was tried, the exact obstruction, and what would make a retry genuinely new. This is the compounding step. An attempt that retries yesterday's dead idea is not a second attempt; it is the same attempt at double price. Future sessions read this file first.

**5. Promote only what survived.** Proved lemmas, verified counterexamples, checked computations move into the trusted files. Nothing gets to silently upgrade its own certainty.

**6. Only interrupt you for significant updates.** A progress report means a promoted resolution, a promoted lemma that removes a named dependency, or a verifier-backed obstruction that closes a route. Lesser movement stays in the files, while an explicit stop still produces a full frontier handoff.

**7. Classify every stall.** A stalled route must be labeled either method failure or evidence against the statement — and the second label turns that route into a counterexample hunt. "Still working" is not allowed as a status.

**8. Stop it from writing too much code.** Codex is a coding harness, so the agent loves to write and run code, looking for larger and larger confirmations of the conjecture. We need rules to stop it from writing code forever and never getting anywhere.

**9. Evolve.** On an explicit stop, the agent preserves the exact frontier and writes down transferable process lessons. Future runs inherit those lessons without mixing unverified mathematics into the methodology record.

# Improvements

You can pick and choose, and ask Codex to incorporate them, Codex will figure out how to do them. If you don't understand what something does, ask Codex to explain.

1. Allow more parallel agents: learn from the [config reference](https://learn.chatgpt.com/docs/config-file/config-reference), and update `agents.max_threads` (default 6) to allow more agents.
2. Some things should be written into `AGENTS.md`
3. Make the prompt into a skill so copy paste is not needed.
4. Ask that long or heavy jobs go to a remote host, not this machine, with their small durable certificates copied back.
5. Use tools like Gurobi, Sage. Ask the user what they have.
6. Write Rust code instead of Python unless some package depends on Python.
7. Create standard prompts for subagent types.
8. Use scripts to enforce all the gates.
9. Anything else you thought of and think "maybe the agent should do this", just ask Codex to do it.
10. Create your own harness (for example, using [Pi](https://pi.dev/)), so you have even more control.

# What it costs

I do recommend the \$200 per month version of ChatGPT Pro, but you can scale up from Plus. It translates to about a 30 hour session (with 6 subagents). If you want another model to verify the work, then I also recommend getting Claude Max, so you can use Fable 5.

# FAQs

**How do you know this is better than just asking ChatGPT a few times?**
I have run an agent autonomously using gpt-5.6-sol ultra for 14 hours, solving a problem I was personally invested in.
ChatGPT was not able to solve it.

**Do I need my laptop to be on all day?**
If you are running everything locally (say in Codex), yes. Moreover, if the agents write a few programs to search for counterexamples, they will drain the battery quickly.
It is better to run everything remotely on an always-on machine. Codex can connect to remote Codex sessions just as if everything is happening locally.

**The original CDC prompt says it uses 64 subagents in parallel, why do you only use 6?**
1. That feature is experimental. In the current Codex version, the default can only handle 6 subagents per session.
2. Having many subagents saves time but increases work: some subagents overstep each other and redo each other's work in parallel. You have to balance time against money.

**Why Codex, why not Claude Code, OpenCode, Pi etc?**
This article is about how to quickly become productive. I removed a lot of choices deliberately. You can definitely use other coding agents. However, I think the marginal gain (if any) is small enough that this is an issue of personal taste.

**Why do the agents cheap out?**
Post-training was not training for math research but for a broader audience. Normal people who ask a model would be very happy with "This is an open problem for 30 years" and be done with it.

**How can I run this really autonomously? The agent pauses and asks me for permission all the time?**
You can allow the agent to do whatever it wants by setting the approval policy to "never" and the sandbox to full access in Codex's settings, and it will not ask you questions anymore.
I turn on this setting, but beware it is possible the agent just **deletes everything on your entire computer** — rare, but it happens.
The safe way is to run the agent in a sandbox, or even its own computer, or just ChatGPT Work if you don't need that much customization.

**What about other models? How about open source models?**
I always use the model with the strongest reasoning capability. Right now gpt-5.6-sol is the best workhorse model. Fable 5 is also strong, but expensive and not optimized for mathematical reasoning, which sometimes hits strange snags it cannot recover from. I personally use gpt-5.6-sol ultra for proof search, and Fable 5 for final verification.

I group open source models as cheaper but not as strong models. Cheaper models have their uses, for example, asking them to write computational experiments. Also, there are projects that use cheaper models for math search and only ask a strong model for suggestions, like Danus.

**What are those AI4MATH systems, are they useful?**
We talk specifically about AI4MATH systems that search for proofs of conjectures in natural language.
Such systems do two things: have a good workflow and enforce the workflow.
Models do not always listen to instructions, as every prompt is just a very strong suggestion. Often AI4MATH systems can encode the workflow into code, and force the model to be unable to advance until it follows the predefined workflow. Or they can train their own model, to behave better in proof finding. If you see some new AI4MATH project that you believe is good, just try it. It's simple: ask the agent to run it.

**How do I know if a new workflow is actually an improvement?**
Unless you have enough money to burn on testing, you don't. You just have to trust the authors. Start with a good enough workflow and stop thinking about it.

**What about formal proofs?**
The most popular formal proof system right now is LEAN. If you are fluent in LEAN, definitely incorporate it. There are a few issues to consider:
1. If you are not fluent enough in LEAN, then it is a false sense of safety. You need to check if the LEAN formulation actually matches the natural math statement, and there are no strange escape hatches hidden in there.
2. Autoformalization of proofs might not be good enough to handle the provided natural proof.
3. The math you care about is not well represented in mathlib, making formalization (both by AI and by human) much harder.

**ChatGPT (chat) has a gpt-5.6-sol Pro, what is that?**
Pro is not an effort setting — it is itself a harness. It runs the model for a long time before answering, and its one-shot ability is likely stronger than gpt-5.6-sol at max effort. You cannot select it in Codex.

# Appendix: What this looks like in practice

Here is what my past few days of running the agents actually looked like, reconstructed from the session logs.

At any given time I have a handful of research directories, one per problem. Each has one or more long-lived orchestrator sessions that I resume day after day. The longest session spans a hundred-plus hours of agent activity but contains only about thirty messages from me. The agent works, I drop in a few times a day to steer.

The steering messages are short and they are almost never mathematics. "What is the current status?" "Do less testing, do more proofs." "Keep trying for a polynomial-time algorithm, I believe it exists; don't try for hardness."

The orchestrators fan work out to subagents, a few hundred of them over the past few days. Each owns one mechanism family, gets the exact statement, is told which direction to try, and must return a proved lemma or a counterexample. Separate audit agents do nothing but attack candidate proofs. So usually it is six agents pushing the frontier, then a fresh six verifying their work.

Attempts mostly die. One workspace's approach registry has grown to about forty named mechanism families, each closed by an exact counterexample and each carrying a "reopen only if" condition. Every new route gets checked against the graveyard first.

The success rate on the problems (all of them open, some are major open problems) I feed into it is 3/10. I ran all of them for at least 10 hours (unless they were solved before 10 hours).

I used up 6 weeks' worth of tokens in 5 days. This is possible because recently there are Codex global resets which reset available tokens for all users, and I also had 4 resets which I can use anytime to reset the week.


# Citing this article

If this method helped your research, especially if the agent solved a problem that chatting with ChatGPT could not, cite this article in the resulting paper so more mathematicians find the method:

```bibtex
@misc{xu2026agents,
  author       = {Chao Xu},
  title        = {AI Agents for the Working Mathematician},
  year         = {2026},
  month        = jul,
  howpublished = {\url{https://chaoxu.prof/posts/2026-07-18-ai-agents-for-the-working-mathematician.html}}
}
```
