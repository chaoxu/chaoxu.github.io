---
title: AI Agents for the Working Mathematician
tags: math, AI
---

Most working mathematicians I talk to or observe have the same relationship with AI: they paste a question into ChatGPT, get something proof-shaped back, feel vaguely impressed or vaguely cheated, and go back to work. This makes sense, especially under the common opinion:

> If you ask ChatGPT a few times and it does not solve the problem, then AI can NEVER solve the problem by itself.

This opinion was formed by mathematicians worldwide through long and brutal ChatGPT sessions: read the latest output, send "keep going", wait for a response, repeat until the entire afternoon is wasted. If the opinion were true, then asking ChatGPT a few times would be the optimal way of using AI to do math — and to be fair, even that is occasionally strong enough to solve real open problems.

But the opinion is **wrong** (see FAQ). You can use a system that is far more capable, with a much higher chance of success than the standard opinion allows.

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

```markdown
Current task statement

[STATEMENT]

## Success criteria

Partial progress does not count unless it implies exactly the resolution above. In particular, proofs for special classes, reductions to another unproved conjecture, computational verification through any fixed size, and candidate counterexamples without a complete nonexistence certificate are insufficient. If the statement above is ambiguous about what counts as an answer, ask before starting; never resolve ambiguity silently.

## Durable state — create these files before searching

- STATEMENT.md — the exact statement, conventions, and success criteria. Fixed for the whole run; never edit it to fit a result.
- REGISTRY.md — one row per approach family: family name, exact claim attempted, exact remaining gap, smallest known obstruction, next decisive test, status.
- FAILED.md — every closed route: what was tried, the exact obstruction, the evidence for it, and what would make a retry materially new.
- PROVED.md — promoted results only, each with its status label and proof or certificate.

All mathematical work products land in these files, not only in conversation. After any context compaction, re-read these files before continuing; they are the memory, the conversation is not.

## Status vocabulary — literal, never inflated

Every claim carries exactly one label, with these exact meanings:

- candidate — produced by an agent; no checks yet.
- self-audited — re-checked only by its own author or context. For building on, this counts the same as candidate.
- verifier-backed — survived both steps of the verification cadence below: a hostile audit by a fresh agent that tried to refute it, and an independent end-to-end reconstruction by an agent that never saw the proof.
- promoted — verifier-backed and recorded in PROVED.md; later work may cite it, and anything built on it carries at most this label.
- independently audited — additionally checked from outside the producing model family: a different-family model or a human. Present final answers at this label when possible.

If no different-family model is available in this environment, do not simulate independence with another instance of the same family — that is label inflation. Instead deliver the final answer at promoted status, state prominently that the cross-family audit has not run, and list the specific claims an outside model or human referee should check first, in order of risk.

A claim's label only advances through the verification steps below. A later argument never inherits more certainty than its weakest premise's label. Never call a mismatched case, a global compatibility assertion, or a polynomial recurrence "routine" — those are where proofs hide their hard step.

## Orchestration

Use subagents aggressively and dynamically, at most 6 concurrent. Work in waves: agents push the frontier, then fresh agents verify what came back. Do not use a fixed assignment such as "N agents for strategy X."

- Begin with a genuinely diverse portfolio: substantially different formulations, invariants, reductions, algebraic viewpoints, structural inductions, decompositions, embeddings, extremal arguments.
- Do not tell most agents the currently favored approach; preserve independence during early rounds so they do not converge on the same attractive but incomplete reduction. Agents may be assigned a direction, but never shown another agent's partial proof of it.
- Group approaches in REGISTRY.md by the mathematical mechanism and by their terminal missing lemma, not by terminology. If several agents converge to one family, redirect the surplus toward underexplored formulations.
- A route that ends at a missing lemma as strong as the original conjecture is blocked, not "one lemma away." Record it in FAILED.md. Reopen a blocked route only for a materially new mechanism, invariant, or construction, and say in REGISTRY.md what is new.
- Before starting any route, check FAILED.md and state either: "no close prior route" or "closest prior route is X; this differs materially because of <new lemma / source / witness / certificate / scope>."
- Require every agent to return a proved lemma, an explicit construction, or a counterexample. Reject status reports, vague optimism, and claims that an unproved global compatibility statement is routine.

## Stalled routes

When a route stalls, classify it explicitly in REGISTRY.md as either (a) method failure or (b) evidence against the target statement. If (b), redirect part of the effort of that family to counterexample search. Every stalled route must carry one of these two classifications; "still working" is not a classification.

## Verification cadence

Every candidate proof gets, in order: one focused hostile audit round (a subagent instructed to refute it: find any gap, unsupported claim, quantifier slip, or misapplied citation, and be skeptical), then one independent end-to-end reconstruction by a fresh agent that has not seen the proof, working only from the statement and the claimed key ideas. Only after both does the label advance to verifier-backed. Do not re-audit after prose-only edits; re-audit only when the mathematical content changes. Final candidate results should additionally be checked by a model from a different family before being presented as the answer.

## Reporting gate

Report a result to me only on a significant update: a complete proof or certified counterexample; a proved lemma that removes a named dependency; a minimal obstruction that closes a route; or a strictly stronger or simpler theorem with proof. New notation, restructuring, another finite computation, or a reduction to a theorem-strength lemma is not significant and goes in the files, not in a report.

## Computation rules

Only write programs when essential; most work here should be proofs. When a computation is justified, it is a named finite question whose output is a small witness, certificate, or table. Never run computation through inline stdin. Every run gets a named source file, the approach-family ID it serves, a timeout, a log, and an output location, recorded in REGISTRY.md. A computation can refute a lemma or discover a certificate; it cannot prove an unbounded theorem — preserve the smallest exact certificate and then prove the resulting claim.

## Web policy

Public search may be used only for ordinary mathematical background or standard named theorems, not to search for a solution to this exact problem or benchmark. Do not search the public web merely to determine whether the problem is open, and do not answer that it is open.

## Persistence

Do not return merely because current approaches fail or agents report theorem-strength gaps. Continue launching new rounds, reopening blocked approaches only under the reopen rule above, and searching for fresh formulations. Spend at least 8 hours before even considering returning. Return only when a complete affirmative resolution has survived the full verification cadence; otherwise keep FAILED.md and REGISTRY.md as the honest record and continue.

Register this task as a persistent goal (create_goal) with the success criteria above as the completion condition, so the objective survives context compaction and session restarts. If goal tooling is unavailable, say so at the start of the run.

## On stop

When I end the run: apply the reporting gate to what you present. Then append to a LESSONS.md: what you learned this session that would make future runs more efficient — preferring lessons transferable to other mathematical problems, plus any environment issues that wasted time.
```

# Explanations and Improvements

The prompt combines ideas from the [CDC prompt](https://cdn.openai.com/pdf/04d1d1e4-bc75-476a-97cf-49055cd98d31/cdc_prompt.pdf), [Danus](https://github.com/frenzymath/Danus), and things learned from my own runs. You can read it. It is not perfect but gets 90% there.

The idea is to run an agent that supervises other agents that work towards a single goal; this is called an orchestrator. This is known to be better than a single agent working on the problem alone, which quickly fills up its own context window and gets confused. You can start from this, and in the future, evolve it so it works even better for what you are doing. Here are some highlights, and why we did it.

**1. Write the statement and success criteria.** One file `STATEMENT.md`: the exact claim with all quantifiers, the conventions, what is known, and most importantly, what would count as an answer. This is required so that when the AI tries to give you an answer and stop, it will look at the statement and check whether it actually completed the task. This is a fixed point that does not change during a run.

**2. Launch attempts with a prompt that pre-blocks the cheap outs.** Those things are there because they are common ways where the model tries to end work early.

**3. Audit adversarially.** The generator of the proof is unreliable, so there will always be an auditing process, so it has a lower probability of error. Model families have correlated blind spots: I have had a proof that GPT could not fault no matter how it was prodded, and Opus found the flaw. I personally also use Fable 5 to verify the final output.

**4. Record what died.** Failed routes go in a file, each with: what was tried, the exact obstruction, and what would make a retry genuinely new. This is the compounding step. An attempt that retries yesterday's dead idea is not a second attempt; it is the same attempt at double price. Future sessions read this file first.

**5. Promote only what survived.** Proved lemmas, verified counterexamples, checked computations move into the trusted files. Nothing gets to silently upgrade its own certainty.

**6. Only interrupt you for significant updates.** The reporting gate is why the agent does not waste your time: a report means a complete proof or certified counterexample, a proved lemma that removes a named dependency, or a closed route — everything else stays in the files.

**7. Classify every stall.** A stalled route must be labeled either method failure or evidence against the statement — and the second label turns that route into a counterexample hunt. "Still working" is not allowed as a status.

**8. Stop it from writing too much code.** Codex is a coding harness, so the agent loves to write and run code, looking for larger and larger confirmations of the conjecture. We need rules to stop it from writing code forever and never getting anywhere.

**9. Evolve.** End every session by asking the agent what it learned. Those harvested lessons get distilled and written down, which future agents can access. The workflow effectively bootstrapped its own methodology document.

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

# What it costs

I do recommend the \$200 per month version of ChatGPT Pro, but you can scale up from Plus. It translates to about a 30 hour session (with 6 subagents). If you want another model to verify the work, then I also recommend getting Claude Max, so you can use Fable 5.

# FAQs

**How do you know this is better than just asking ChatGPT?**
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
I have no idea, I also want to be enlightened.

# Appendix: What this looks like in practice

Here is what my past few days of running the agents actually looked like while attending [Emléktábla workshop](https://users.renyi.hu/~emlektab/), reconstructed from the session logs.

At any given time I have a handful of research directories, one per problem. Each has one or more long-lived orchestrator sessions that I resume day after day. The longest session spans a hundred-plus hours of agent activity but contains only about thirty messages from me. The agent works, I drop in a few times a day to steer.

The steering messages are short and they are almost never mathematics. "What is the current status?" "Do less testing, do more proofs." "Keep trying for a polynomial-time algorithm, I believe it exists; don't try for hardness."

The orchestrators fan work out to subagents, a few hundred of them over the past few days. Each owns one mechanism family, gets the exact statement, is told which direction to try, and must return a proved lemma or a counterexample. Separate audit agents do nothing but attack candidate proofs. So usually it is six agents pushing the frontier, then a fresh six verifying their work.

Attempts mostly die. One workspace's approach registry has grown to about forty named mechanism families, each closed by an exact counterexample and each carrying a "reopen only if" condition. Every new route gets checked against the graveyard first.

The success rate on the problems (all of them open, some are major open problems) I feed into it is 3/10. I ran all of them for at least 10 hours (unless they were solved before 10 hours).

I used up 6 weeks' worth of tokens in 5 days. This is possible because recently there are Codex global resets which reset available tokens for all users, and I also had 4 resets which I can use anytime to reset the week.
