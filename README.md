# Abstract 

Nova is a purely-functional, statically-typed language in the spirit of PureScript, designed for live, incremental development inside long-running Erlang systems.
Instead of compiling whole programs at once, Nova’s compiler exposes a streaming API: individual type declarations, data definitions, and function bodies are pushed—one by one—into a target namespace. Each submission is parsed, elaborated, and fully Hindley-Milner–type-checked in isolation before it mutates the shared environment. If the declaration passes, it is atomically merged into the namespace and becomes immediately callable from both Nova and Elixir through a lightweight FFI layer; if it fails, the environment remains unchanged and the compiler returns a precise, range-annotated error.

This incremental model enables hot-reloading, REPL-like exploration, and fine-grained rollback in production services while preserving the strong guarantees of static typing. Coupled with Elixir’s concurrency and supervision primitives, Nova aims to bring the safety of a modern ML-style type system to dynamic, continuously running applications without sacrificing the rapid feedback loop developers expect from interactive environments.

Embedding Nova’s “one-declaration-at-a-time” workflow inside an AI agent loop unlocks several practical advantages that are hard to achieve with traditional batch compilers.

# Safe autonomous code synthesis and repair
Modern AI agents frequently generate or mutate code on the fly—whether to fix a failing workflow, extend capabilities, or tailor logic to new data. Nova’s incremental pipeline provides the guardrails they need: every snippet must typecheck against the current namespace before it can be loaded. An agent can therefore prototype, submit, and refine functions iteratively, getting immediate, compiler-level feedback instead of discovering errors later at runtime. The strong static types act as an automated verifier, dramatically reducing the risk that a hallucinated patch will crash the host system or corrupt shared state.

# Continual learning without downtime
Many real-time AI services (speech assistants, recommendation engines, task-oriented bots) benefit from continual improvement—new heuristics, updated feature extractors, ad-hoc business rules—yet can’t afford restarts. Running inside BEAM supervision trees, Nova modules can be hot-reloaded exactly like native Elixir code. An agent observing live metrics can synthesize a refined ranking function, push it, and—once the compiler confirms soundness—swap it into production instantly. This marries the flexibility of dynamic languages with the robustness of statically-typed guarantees.

# Introspectable reasoning and proof of correctness
Because every accepted declaration is retained in a typed AST, agents can query the compiler for kinds, inferred types, or exhaustiveness of pattern matches, effectively gaining a structured knowledge base describing their own logic. This metadata can feed higher-order reasoning: e.g., “find all functions consuming a UserId and returning an Html snippet” or “prove that no path calls unsafeDecode without prior validation.” Such machine-readable assurances are invaluable when agents must convince humans (or other agents) that a change is safe.

# Fine-grained rollback and experimentation
Nova’s transactional namespace behaves like a versioned knowledge graph: failed submissions leave the environment unchanged, and accepted ones are individually addressable. Agents can run A/B tests by cloning a namespace, injecting alternative implementations, and measuring outcomes under identical traffic—all without recompiling a full service. If a variant underperforms, it can be rolled back by simply discarding that single declaration.

# Seamless bridging to high-performance runtimes
Finally, the compiler’s foreign-function interface means an agent can generate pure Nova code for the bulk of its logic while still delegating heavy numeric kernels or GPU ops to specialized Elixir/Erlang NIFs or Rustler modules. The tight type harmony across the boundary ensures that values exchanged with native libraries remain consistent—an essential property when autonomous systems juggle tensors, embeddings, and domain objects at scale.

Taken together, Nova supplies AI agents with an interactive yet disciplined coding substrate: rapid iteration, zero-downtime deployment, and mathematically enforced safety—exactly the ingredients needed for reliable self-improving software.
