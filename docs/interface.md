# InterfaceRegistry – Design & Usage Guide

> **Scope:** Nova live‑compiler runtime (Elixir)
> **Audience:** Nova core maintainers, plugin authors, advanced users embedding the compiler.

---

## 1  Motivation

Nova is engineered for *live* editing and incremental type‑checking.  Developing across multiple modules demands:

* **Fast imports** – no disk IO in the hot path.
* **Open‑ended namespaces** – modules can grow new declarations at any time.
* **Isolation of un‑committed edits** – a work‑in‑progress file should not break the public view until explicitly promoted.
* **Hot reload** – evaluate expressions immediately after a change without restarting the VM.

A single global ETS table cannot satisfy *isolation*; conversely, re‑parsing every dependency on each keystroke destroys *speed*.  **InterfaceRegistry** answers both with a *layered* in‑memory store.

---

## 2  Conceptual Model

```
Root (immutable) ─┬─► Std‑lib (`Prelude`, …)
                  │
                  ├─► Project‑baseline (commit X)
                  │
Batch‑layer A ────►│  (current edit session)
                  │
Batch‑layer B ────►┘  (e.g. separate feature branch)
```

Each **layer** now maintains *two* anonymous ETS `:set` tables:

* **T1 – Schemes table**
  **Key**: `{namespace :: atom, elem :: atom}`
  **Value**: `Types.Scheme.t()`  (≈ 100‑200 B)
  **Access pattern**: *every* identifier lookup hits this table and copies only a tiny term.

* **T2 – AST table**
  **Key**: `{namespace :: atom, elem :: atom}`
  **Value**: `TypedAst.t()` representing **only that element** (the function body, data constructor, etc.).
  Accessed mainly by code‑gen, doc tools, and IDE hovers.

```elixir
# example
:nova_schemes |> :ets.lookup({:Prelude, :map}) # => [%Scheme{…}]
:nova_ast     |> :ets.lookup({:Prelude, :map}) # => [%TypedAst.FunctionDeclaration{…}]
```

**Why per‑element AST?**

* **No duplication** – each row stores just what it needs; a 2 KiB `foldl` AST isn’t replicated across 180 other names.
* **Fine‑grained edits** – recompiling a single function updates one row instead of rewriting a big module blob.
* **Whole‑module inspection still easy** – use `:ets.match/3` (or a helper) to fetch all rows with the same `namespace` prefix when needed.

When a layer is dropped (e.g. `discard_batch/1`), *both* tables vanish automatically with their owner process, freeing all memory in O(1).elixir
%{
exports: %{atom => Types.Scheme.t()},
ast: TypedAst.Module.t(),          # full typed AST (optional for code‑gen)
env: Types.Env.t(),               # final typing environment
rev: non\_neg\_integer()            # monotonic revision counter
}

````

A layer optionally points to a *parent* layer, forming a read‑only chain.  Look‑ups walk the chain top‑down; writes always hit the *current* layer.

### 2.1  Why anonymous ETS?

- No global name clashes across test instances / tenants.
- The owner process can garbage‑collect the layer simply by exiting.
- Faster lookup (`:ets.lookup/2`) than an Agent/GenServer round‑trip while staying thread‑safe (public ETS is read‑concurrent).

---

## 3  Public API (high‑level)

```elixir
# Lifecycle --------------------------------------------------------------
root = InterfaceRegistry.new_root!(stdlib_path)

batch = InterfaceRegistry.begin_batch(root)
…                                # type‑check modified files
InterfaceRegistry.commit_batch!(batch)  # or discard_batch/1

# Core operations --------------------------------------------------------
InterfaceRegistry.put(batch, :Foo, exports, typed_ast, env)
{:ok, %{} = data} = InterfaceRegistry.get(batch, :Foo)
:ok = InterfaceRegistry.bump!(batch, :Foo)   # manual rev bump (rare)

# Introspection ----------------------------------------------------------
InterfaceRegistry.depends_on?(batch, :Bar, :Foo)  # cycle detection helper
InterfaceRegistry.revision(batch, :Foo)          # returns `rev`
````

All functions are *pure* with respect to the calling process – they use only ETS operations.

---

## 4  Lookup Algorithm

```text
get(registry, mod):
  layer = registry
  while layer != nil:
    case :ets.lookup(layer.table, mod) of
      [entry] -> return {:ok, entry}
      []      -> layer = layer.parent
  return :error
```

Time complexity: **O(depth)** where depth is the number of stacked batches (typically 1‑3).

---

## 5  Batch Workflow & Isolation

1. **Begin:** `batch = InterfaceRegistry.begin_batch(current_layer)`
2. **Edit:** The IDE feeds modified modules to the compiler → they are (re)type‑checked and **put** into `batch`.
3. **Test / evaluate** expressions.  All look‑ups prefer the batch’s table, so new definitions shadow older ones.
4. **Commit:** `InterfaceRegistry.commit_batch!(batch)` merges its ETS pairs into the parent layer *atomically* (using `:ets.safe_fixtable/2` to avoid races).  Alternatively, `discard_batch/1` simply drops the table.

This guarantees:

* **No dirty reads** – other processes still referencing the parent layer see only stable interfaces.
* **Instant flip** – commit is O(N) `:ets.insert/2`, no re‑parse needed.

---

## 6  Persistence (Optional)

```elixir
InterfaceRegistry.dump!(layer, "state.nvc")
layer2 = InterfaceRegistry.restore!("state.nvc")
```

* CBOR or term‑to‑binary encoding (≈ 1 KiB per medium‑sized module).
* Useful for IDE restarts or CI caching.
* Not used by the core compiler unless the host requests it.

---

## 7  Concurrency Notes

* **Readers** are lock‑free (`:ets.lookup/2`).
* **Writers** are confined to the process that owns the layer table.
* When two editors need parallel batches, each spawns its own child layer from the same parent; they remain isolated until merged.

---

## 8  Future Extensions

* **Dependency graph** inside each layer for smart invalidation.
* **Observer hook** so tools can subscribe to `:interface_updated` events and re‑run tests automatically.
* **Fine‑grained visibility**: re‑export tracking, orphan instance warnings.

---

## 9  Design Reasoning Recap

| Requirement                        | Design choice                                |
| ---------------------------------- | -------------------------------------------- |
| Zero‑latency imports               | Anonymous ETS, no disk IO                    |
| Open‑ended modules                 | `put/4` overwrites entry, `rev` aids caching |
| Isolated edit batches              | Layer stack with parent fallback             |
| Fast restart, optional persistence | `dump!/restore!` separate from core workflow |
| Multi‑consumer, thread‑safe reads  | Public ETS, no GenServer lock                |

The result is a *simple* yet *scalable* foundation: the common case (editing a few files) is a handful of ETS operations, while advanced workflows (branching, reverting, caching) build on the same primitives without extra complexity in the type‑checker itself.
