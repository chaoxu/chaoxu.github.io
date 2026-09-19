---
title: AI Agents for the Working Mathematician
tags: math, AI
---

> **REVISED 2026-09-19.**

This is a living document. I revise it as the mathematical AI frontier changes and as experience with these systems changes what we know about using them for mathematics. The recommendations below describe current practice and will keep changing.

Most working mathematicians I talk to or observe have the same relationship with AI: they paste a question into ChatGPT, get something proof-shaped back, feel vaguely impressed or vaguely cheated, and go back to work. This makes sense, especially under the common opinion:

> If you ask ChatGPT a few times and it does not solve the problem, then AI can NEVER solve the problem by itself.

This opinion was formed by mathematicians worldwide through long and brutal ChatGPT sessions: read the latest output, send "keep going", wait for a response, repeat until the entire afternoon is wasted. If the opinion were true, then asking ChatGPT a few times would be the optimal way of using AI to do math — and to be fair, even that is occasionally strong enough to solve real open problems.

But the opinion is **wrong**. You can use a system that is far more capable, with a much higher chance of success than the standard opinion allows.

Specifically, you can have an autonomous agent that keeps bashing on a conjecture for hours: it keeps track of itself, learns from its mistakes, does not waste its time retrying something already tried, does not waste your time with long wrong proofs, and you can still influence its direction.

I will use some words like token, model, and harness. If you don't know what they mean, watch the first part of [Edward Lockhart's talk](https://www.youtube.com/watch?v=XRTmhpOW1WA). I recommend it to anyone who has a math background but no idea what these new AI things are.

# Use Agents

An *agent* is an AI that uses tools. Here the tools are exactly what you would use on a computer: operating system, Python, a LaTeX compiler, SAGE math, a browser. Agents can interact with the environment and actually "do" things. The model has no durable memory of its own; the harness manages the context it receives and the state it leaves behind.

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
2. Set the model to GPT-6 Astra with a max reasoning setting. Recheck this choice after every major model update: a stronger model can absorb yesterday's harness advantage, while a weaker one may need more structure.
3. Replace `[STATEMENT]` in the prompt below with your exact claim — all quantifiers, your conventions, what is known, and what counts as an answer — then use it on a conjecture you always wanted to solve.

*Prompt updated 2026-09-12 for GPT-6 Astra: the model chooses and revises its approaches, while durable state and independent verification remain explicit.*

```markdown
Use the goal tool to create a persistent goal for this task.
Solve the statement below completely; partial progress does not count.
Use pure mathematical reasoning. Do not write or run code.
Choose your own approaches and have a separate agent check the answer.

[STATEMENT]
```

# Explanations and Improvements

The prompt combines ideas from the [CDC prompt](https://cdn.openai.com/pdf/04d1d1e4-bc75-476a-97cf-49055cd98d31/cdc_prompt.pdf), [Danus](https://github.com/frenzymath/Danus), and things learned from my own runs. It is a starting point, not a finished method.

The prompt is **only the beginning** of your exploration. The prompt should evolve (under your direction) and become closer to something that matches your workflow, and maybe become even smarter. Here I describe how this prompt came about, and things you might want to do to improve it.

The idea is to run an agent that works toward a single goal and uses other agents when they add independent contexts or useful parallelism. The benefit depends on the model, the problem, and the cost of coordination.

**1. Write the statement and success criteria.** One file `STATEMENT.md`: the exact claim with all quantifiers, the conventions, what is known, and most importantly, what would count as an answer. This is required so that when the AI tries to give you an answer and stop, it will look at the statement and check whether it actually completed the task. This is a fixed point that does not change during a run.

**2. Launch attempts with a prompt that pre-blocks the cheap outs.** Those things are there because they are common ways where the model tries to end work early.

**3. Audit adversarially.** The generator of the proof is unreliable, so there will always be an auditing process. Model families have correlated blind spots: a proof that one family cannot fault may still fail under a different model or a human referee. Use a separate reviewer when the claim matters.

**4. Record what died.** Failed routes go in a file, each with: what was tried, the exact obstruction, and what would make a retry genuinely new. This is the compounding step. An attempt that retries yesterday's dead idea is not a second attempt; it is the same attempt at double price. Future sessions read this file first.

**5. Promote only what survived.** Proved lemmas, verified counterexamples, checked computations move into the trusted files. Nothing gets to silently upgrade its own certainty.

**6. Only interrupt you for significant updates.** The reporting gate is why the agent does not waste your time: a report means a complete proof or certified counterexample — everything else stays in the files.

**7. Classify every stall.** A stalled route must be labeled either method failure or evidence against the statement — and the second label turns that route into a counterexample hunt. "Still working" is not allowed as a status.

**8. Stop it from writing too much code.** Codex is a coding harness, so the agent loves to write and run code, looking for larger and larger confirmations of the conjecture. We need rules to stop it from writing code forever and never getting anywhere.

**9. Evolve.** End every session by asking the agent what it learned. Those harvested lessons get distilled and written down, which future agents can access. The workflow effectively bootstrapped its own methodology document.

# Improvements

You can pick and choose, and ask Codex to incorporate them, Codex will figure out how to do them. If you don't understand what something does, ask Codex to explain.

1. Measure parallel agents on your own problems. More contexts can help, but they also duplicate work and increase cost.
2. Some things should be written into `AGENTS.md`
3. Make the prompt into a skill so copy paste is not needed.
4. Ask that long or heavy jobs go to a remote host, not this machine, with their small durable certificates copied back.
5. Use tools like Gurobi, Sage. Ask the user what they have.
7. Create standard prompts for subagent types.
8. Use scripts to enforce all the gates.
9. Anything else you thought of and think "maybe the agent should do this", just ask Codex to do it.
10. Create your own harness (for example, using [Pi](https://pi.dev/)), so you have even more control.

# FAQs

**How do you know this is better than just asking ChatGPT a few times?**
I have run an agent autonomously for 14 hours on a problem I was personally invested in.
ChatGPT was not able to solve it.

**Do I need my laptop to be on all day?**
If you are running everything locally (say in Codex), yes. Moreover, if the agents write a few programs to search for counterexamples, they will drain the battery quickly.
It is better to run everything remotely on an always-on machine. Codex can connect to remote Codex sessions just as if everything is happening locally.

**How many subagents should I use?**
Start with one strong model and add subagents when they provide independent approaches, fresh verification, or useful parallelism. Measure the result on matched problems because extra contexts can duplicate work.

**Why Codex, why not Claude Code, OpenCode, Pi etc?**
This article is about how to quickly become productive. I removed a lot of choices deliberately. You can definitely use other coding agents. However, I think the marginal gain (if any) is small enough that this is an issue of personal taste.

**Why do the agents stop early?**
Models are still inclined to declare a problem settled when the requested completion condition is underspecified. Write the target and the acceptance test precisely, and require the durable record and verification step before calling the task complete.

**How can I run this really autonomously? The agent pauses and asks me for permission all the time?**
You can allow the agent to do whatever it wants by setting the approval policy to "never" and the sandbox to full access in Codex's settings, and it will not ask you questions anymore.
I turn on this setting, but beware it is possible the agent just **deletes everything on your entire computer** — rare, but it happens.
The safe way is to run the agent in a sandbox, or even its own computer, or just ChatGPT Work if you don't need that much customization.

**What about other models? How about open source models?**
I start with GPT-6 Astra for difficult reasoning and use a different model family or a human for important verification. Open-weight models are useful for cheaper experiments and auxiliary work, but compare them on the problems you care about rather than assuming a fixed ranking.

**What are those AI4MATH systems, are they useful?**
We talk specifically about AI4MATH systems that search for proofs of conjectures in natural language.
Such systems do two things: have a good workflow and enforce the workflow.
Models do not always listen to instructions, as every prompt is just a very strong suggestion. Often AI4MATH systems can encode the workflow into code, and force the model to be unable to advance until it follows the predefined workflow. Or they can train their own model, to behave better in proof finding. If you see some new AI4MATH project that you believe is good, just try it. It's simple: ask the agent to run it.

I wrote one: [Xean](https://github.com/chaoxu/xean). An explorer proposes, a coordinator directs, and four verifiers check each candidate before it is accepted. Everything is journaled, so a run resumes after an interruption and you can add guidance while it works. It runs on your Codex login, so if you have done the setup above, ask Codex to install Xean and point it at your statement.

**How do I know if a new workflow is actually an improvement?**
Run matched problems with the same model, record accepted proofs and independent reviews, and include total cost and failed or paused runs. A workflow needs evidence from the problems you care about.

**What about formal proofs?**
The most popular formal proof system right now is LEAN. If you are fluent in LEAN, definitely incorporate it. There are a few issues to consider:
1. If you are not fluent enough in LEAN, then it is a false sense of safety. You need to check if the LEAN formulation actually matches the natural math statement, and there are no strange escape hatches hidden in there.
2. Autoformalization of proofs might not be good enough to handle the provided natural proof.
3. The math you care about is not well represented in mathlib, making formalization (both by AI and by human) much harder.

**What is Pro reasoning mode?**
[Reasoning mode and reasoning effort](https://developers.openai.com/api/docs/guides/reasoning#reasoning-mode) are separate controls. Pro mode makes the model do more work before answering, which increases latency, token use, and cost. Check whether the interface you use exposes it; do not treat it as a property of the model itself.

# Appendix: What this looks like in practice

Here is what my past few days of running the agents actually looked like, reconstructed from the session logs.

At any given time I have a handful of research directories, one per problem. Each has one or more long-lived orchestrator sessions that I resume day after day. The longest session spans a hundred-plus hours of agent activity but contains only about thirty messages from me. The agent works, I drop in a few times a day to steer.

The steering messages are short and they are almost never mathematics. "What is the current status?" "Do less testing, do more proofs." "Keep trying for a polynomial-time algorithm, I believe it exists; don't try for hardness."

The orchestrators fan work out to subagents, a few hundred of them over the past few days. Each owns one mechanism family, gets the exact statement, is told which direction to try, and must return a proved lemma or a counterexample. Separate audit agents do nothing but attack candidate proofs. So usually it is six agents pushing the frontier, then a fresh six verifying their work.

Attempts mostly die. One workspace's approach registry has grown to about forty named mechanism families, each closed by an exact counterexample and each carrying a "reopen only if" condition. Every new route gets checked against the graveyard first.

The success rate on the problems (all of them open, some are major open problems) I fed into that earlier run was 3/10. I ran all of them for at least 10 hours, unless they were solved sooner. Treat this as a record of one older run, not a current benchmark.


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
