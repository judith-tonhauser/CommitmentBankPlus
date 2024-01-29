README

This GitHub repository contains experiments that were the experiments designed i) to compare projection across different entailment-canceling operators, and ii) to compare different diagnostics for not-at-issueness, and iii) to investigate the Gradient Projection Principle (Tonhauser, Beaver, Degen 2018, Journal of Semantics) across different embeddings and with different diagnostics for not-at-issueness.

The 12 experiments used the stimuli from Tonhauser & Degen (under re-review) and Degen & Tonhauser (accepted, Open Mind): 20 clause-embedding predicates, 20 complement clauses, 2 blocks in random order (projection, not-at-issueness).

The 12 experiments all use the "certain that" diagnostic for projectio; they differ in which not-at-issueness diagnostic was used:
- Experiments 1-4: asking whether (Q), "are you sure" (N, M, C)
- Experiments 5-8: assent with positive continuation (of complement clause)
- Experiments 9-12: assent with negative continuation (of main clause)

---------------------

1_projaiQ: polar question stimuli, certainty ratings, "asking whether" at-issueness ratings
[these data already exist, copied over from JT's attitude_preds_projection github repo]

2_projaiN: negation stimuli, certainty ratings, "are you sure that" at-issueness ratings

3_projaiM: modal stimuli, certainty ratings, "are you sure that" at-issueness ratings

4_projaiC: conditional stimuli, certainty ratings, "are you sure that" at-issueness ratings

---------------------
projection: certainty ratings
not-at-issueness: assent diagnostic with positive continuation

A: ...Cole discover that Julian dances salsa... [with different entailment-canceling operators)
B: Yes, (that's true), he dances salsa.

5_projaiQ
6_projaiN
7_projaiM
8_projaiC

---------------------
projection: certainty ratings
not-at-issueness: assent diagnostic with negative continuation

A: ...Cole discover that Julian dances salsa... [with different entailment-canceling operators)
B: Yes, (that's true), but he (didn't) discover(ed) it.

9_projaiQ
10_projaiN
11_projaiM
12_projaiC
