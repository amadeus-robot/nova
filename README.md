# Nova Lang 🔥  
* a coding environment and compiler for AI by AI *

[![CI](https://github.com/nova-lang/nova/actions/workflows/ci.yml/badge.svg)](https://github.com/nova-lang/nova/actions/workflows/ci.yml)
[![Hex docs](https://img.shields.io/badge/hex-docs-green)](https://hex.pm/packages/nova_lang)
[![License](https://img.shields.io/github/license/nova-lang/nova.svg)](LICENSE)

Nova brings ML-style type safety and Hindley–Milner inference to your chain of thought.  
Think *PureScript-level guarantees* while thinking your code.

---

## Table of Contents
1. [Key Ideas](#key-ideas)
2. [Why Incremental Compilation?](#why-incremental-compilation)
3. [AI-Centric Workflows](#ai-centric-workflows)
4. [Quick Start](#quick-start)
5. [FFI with Elixir](#ffi-with-elixir)
6. [Roadmap](#roadmap)
7. [Contributing](#contributing)
8. [License](#license)

---

## Key Ideas
| Feature | What it means |
| --- | --- |
| **Streaming compiler API** | Push type declarations, data definitions, and function bodies *one at a time*. |
| **H-M type checking per submission** | Each snippet is parsed, elaborated, and fully type-checked *in isolation*. |
| **Transactional namespaces** | A declaration that fails leaves the environment untouched. |
| **Hot-reloading** | Accepted declarations become immediately callable from Nova *and* Elixir. |
| **Typed AST introspection** | Query kinds, inferred types, exhaustiveness proofs, etc., at runtime. |
| **Lightweight FFI** | Seamless interop with Elixir, Erlang NIFs, Rustler modules, and GPUs. |

---

## Why Incremental Compilation?
Traditional batch compilers force you to rebuild entire programs for a single
change—unacceptable in 24 × 7 systems. Nova’s compiler instead acts like a
*transactional database of code*:

```text
┌────────┐    push decl     ┌─────────────┐      merge
│  IDE / │ ───────────────► │ Nova daemon │ ─────────────► Shared NS
│  Agent │ ◄─────────────── │  (BEAM)     │ ◄─────────────
└────────┘    error / ok    └─────────────┘      rollback
```

*   **Hot-reloading**—new logic is live as soon as it passes the type checker.  
*   **Fine-grained rollback**—discard only the faulty declaration, not a whole
    module.  
*   **REPL-like exploration**—iterate on production code with millisecond
    feedback.

---

## AI-Centric Workflows
> Nova was built for autonomous agents that write code.

### 1 · Safe Code Synthesis & Repair
Every generated snippet must type-check before loading—your agent gets compiler
feedback *immediately* instead of crashing later.

### 2 · Continual Learning *Without Downtime*
Update ranking functions, heuristics, or feature extractors on-the-fly while the
service keeps handling traffic.

### 3 · Introspectable Reasoning
Query the live typed AST to prove properties like
“no path calls `unsafeDecode/1` without validation”.

### 4 · Versioned Experimentation
Clone a namespace, inject a variant implementation, A/B test, discard or adopt—
all without a full recompilation.

---

## Quick Start
todo

---

## FFI with Elixir
Nova ↔ Elixir type mapping:

| Nova | Elixir term |
| ---- | ----------- |
| `Int` | `integer()` |
| `Float` | `float()` |
| `String` | `binary()` |
| `List a` | `[a]` |
| `Maybe a` | `{:just, a} | :nothing` |

Define foreign functions in Elixir:

```elixir
defmodule MathNative do
  def add_one(n) when is_integer(n), do: n + 1
end
```

and import them in Nova:

```haskell
foreign import addOne :: Int -> Int
```

---

## Roadmap
- [ ] Exhaustiveness checker for pattern matches  
- [ ] Exhaustive test-suite for type inference edge-cases  
- [ ] IDE protocol (LSP + incremental diagnostics)  
- [ ] Native codegen via LLVM / Cranelift  
- [ ] Self hosted  

---

## Contributing
1. **Fork** the repo and create a branch.  
2. Run `mix test --include integration`.  
3. Follow the
   [contributor guidelines](CONTRIBUTING.md).  
4. Open a **PR**—someone will review within 48 h.

---

## License
Nova is released under the **Apache 2.0** license. See [LICENSE](LICENSE) for
details.

