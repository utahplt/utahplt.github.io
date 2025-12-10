    Title: Nice To Meet You: Synthesizing Practical MLIR Abstract Transformers
    Date: 2025-12-10T17:18:27
    Tags: 

<!-- more -->

Until today, building a new static analysis inside a compiler meant writing
dozens of abstract transformers by hand---one per instruction, per abstract
domain.

Weeks of engineering effort, often only to discover that the analysis
doesn’t even help the optimizer.

At POPL 2026, we introduce NiceToMeetYou, a framework that automatically
synthesizes abstract transformers for entire MLIR dialects and LLVM instruction
sets. No sketches, no templates, no expert tuning—just the abstract domain
definitions. Everything else is fully automated and provably sound, backed by
the SMT-based verifier infrastructure developed over years of LLVM/MLIR
research.

NiceToMeetYou rests on two key ideas:
- Synthesize many small transformers and meet them together into a precise, sound whole.
- Synthesize conditional transformers by generating precise-but-unsound candidates, then synthesizing the conditions that make them sound.

Both strategies can be implemented efficiently using Monte Carlo search.

And yes, sometimes NiceToMeetYou even synthesized transformers more precise
than LLVM’s hand-written ones.

Paper: <https://arxiv.org/abs/2512.06442>

Original LinkedIn Post: <https://www.linkedin.com/posts/loris-d-antoni-5a4b5a17_until-today-building-a-new-static-analysis-activity-7404005979824300032-Rm_H>

