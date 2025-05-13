# `Nova.InterfaceRegistry` — Layer‑Aware Interface Store

*(concise reference for compiler engineers – 13 May 2025)*

---

## Purpose & Core Idea

A **multi‑layer, in‑memory registry** that stores typed module interfaces produced by the Nova compiler.  Each layer owns two anonymous ETS tables:

| Table                       | Key                 | Value              |
| --------------------------- | ------------------- | ------------------ |
| **Schemes** (`schemes_tab`) | `{namespace, elem}` | `Types.Scheme.t()` |
| **AST** (`ast_tab`)         | `{namespace, elem}` | `TypedAst.t()`     |

Layers form a parent‑chain ( `current → parent → …` ); look‑ups walk up the chain until a hit is found.  Writes *never* escape the current layer, enabling **transaction‑like batches** that can be committed or discarded atomically.

---

## Key Types

| Type        | Definition                                                                          |
| ----------- | ----------------------------------------------------------------------------------- |
| `namespace` | `atom` – usually a module name (`:Main`, `:Data_List`)                              |
| `elem_name` | `atom` – exported identifier (`:map`, `:Nil`, `:++`)                                |
| `t`         | `%InterfaceRegistry{schemes_tab: :ets.tid(), ast_tab: :ets.tid(), parent: t \ nil}` |

---

## Public API

### Layer lifecycle

* **`new_root!/0 :: () → t`**
  Create the root layer (no parent). Call once at VM start‑up.

* **`begin_batch/1 :: t → t`**
  Start a child layer that isolates further edits. `parent` remains unchanged until commit.

* **`commit_batch!/1 :: t → t`**
  Merge child tables into parent and delete child tables. Raises if called on root.

* **`discard_batch/1 :: t → :ok`**
  Delete child tables without merging (cancel WIP).

### Write helpers

* **`put_scheme/4 :: (t, namespace, elem_name, Scheme.t()) → :ok`**
  Store a type‑scheme in the current layer.

* **`put_ast/4 :: (t, namespace, elem_name, TypedAst.t()) → :ok`**
  Store a typed AST in the current layer.

* **`put/5 :: (t, namespace, elem_name, Scheme.t(), TypedAst.t()) → :ok`**
  Convenience wrapper; stores scheme and AST in one call.

### Lookup & query

* **`get_scheme/3 :: (t, namespace, elem_name) → {:ok, Scheme.t()} | :error`**
  Follow the parent chain; returns first match.

* **`get_ast/3 :: (t, namespace, elem_name) → {:ok, TypedAst.t()} | :error`**
  Same semantics as `get_scheme/3`.

* **`list_namespace/2 :: (t, namespace) → [elem_name]`**
  Collect all element names exported by a namespace; closest definition wins.

---

## Implementation Details

* **ETS options**: `[:set, :public, read_concurrency: true]` – fast, concurrent reads; writes serialised by Erlang scheduler.
* **Isolation**: Because tables are anonymous and owned by the layer process, they vanish automatically when the process dies, preventing stale data.
* **Complexity**:

  * Local read/write: **O(1)** via ETS.
  * Cross‑layer lookup: **O(chain depth)** (usually small: root + N batches).
* **Thread‑safety**: Writers must serialise commits to avoid lost updates; recommended pattern is *single writer / many readers* per layer.

---

## Typical Usage Pattern

```elixir
root = InterfaceRegistry.new_root!()

# ↳ type‑check file A in isolation
batch = InterfaceRegistry.begin_batch(root)
…populate batch with inferred interfaces…
InterfaceRegistry.commit_batch!(batch)

# ↳ type‑check file B, but compilation fails
batch2 = InterfaceRegistry.begin_batch(root)
…put tentative entries…
InterfaceRegistry.discard_batch(batch2)  # nothing leaked into root
```

---

## Design Rationale (Why two tables?)

* **Memory** – Schemes (\~small) change frequently; typed ASTs (\~larger) are read rarely.  Splitting allows separate eviction / compaction strategies later.
* **Granularity** – Keys are `{namespace, elem}` for both tables; this avoids copying the whole module AST into every element (unlike storing one large AST per module).

---

## Future Extensions

* **Eviction policy** for stale layers in long‑running REPLs.
* **Persistent cache**: serialise root tables to disk between compiler sessions.
* **Telemetry hooks** for cache‑hit ratios and batch sizes.

---

> **Tip for LLM prompts:** When asking about interface resolution, mention *layered ETS registry* so the model can recall that lookups cascade parent‑first.
