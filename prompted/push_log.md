Here’s a practical sketch you can build on to give Nova a “git-style” history of declarations with selective-invalidate/revalidate semantics.

---

## 1  Log model

| Field | Purpose |
|-------|---------|
| **id** (`binary()` SHA-256 or an ever-increasing integer) | Stable handle for the entry (like a commit hash). |
| **kind** (`:type | :function`) | What sort of declaration was pushed. |
| **name** (`String.t()`) | Fully-qualified identifier (`"My.Module.map"`). |
| **payload** (`binary()` or rich struct) | The raw source slice plus its elaborated AST + inferred type. |
| **deps** (`[id]`) | Direct dependency ids captured *after* type-checking. |
| **timestamp / author** *(optional)* | Helpful for tooling & UX. |

> **Tip:** keep the whole thing append-only; it makes concurrent pushes trivial and lets you keep O(1) “git blame” metadata for any definition.

---

## 2  Index structures

1. **Name → head-id**: fast lookup of the *current* declaration for a given name.  
2. **Reverse graph** (id → dependants): so you can walk *forward* from an invalidated entry.  
3. **Memo cache** (id → compiled BEAM module / byte-code): what you actually want to throw away on invalidation.

Both 1 & 2 are tiny ETS tables; entries are rewritten atomically right after a declaration is accepted.

---

## 3  Write path

```text
push(source_chunk)
  ├─ tokenize / parse
  ├─ elaborate + HM-type-check
  │    ↳ Capture direct deps (lookups in table #1)
  ├─ codegen → Elixir source
  ├─ Code.compile_string!/1   ─┐
  └─ :ok                      (enters memo cache)
```

Finally append the **LogEntry**, then update:

* lookup table → new head id  
* reverse graph for *each* dep → `+= [new_id]`

(Do this in a single ETS transaction or inside a `:ets.safe_fixtable/2` window.)

---

## 4  Invalidation algorithm

```elixir
def invalidate(id) do
  queue = :queue.from_list([id])
  seen  = MapSet.new()

  while {:value, cur, q1} = :queue.out(queue) do
    unless MapSet.member?(seen, cur) do
      evict_compiled(cur)             # drop BEAM + purge module
      :ets.delete(memo_cache, cur)

      # enqueue dependants
      for dep <- dependants(cur) do
        queue = :queue.in(dep, q1)
      end
      seen = MapSet.put(seen, cur)
    end
  end

  revalidate_in_topo_order(seen)
end
```

### `revalidate_in_topo_order/1`

1. Topologically sort the pruned sub-graph (Kahn’s algorithm using `deps` links).  
2. Re-run *exactly* the **“write path”** for each node in order.  
   * If re-type-checking fails, stop; leave the rest invalidated and surface an error.

Because every declaration stores its own *parsed + elaborated* AST, you re-use the original front-end work and only pay for type-checking + codegen.

---

## 5  Edge cases & niceties

| Scenario | Handling |
|----------|----------|
| **Transitive rename** (push a new `foo`, but invalidate an older `foo`) | The *head* pointer in table 1 always points at the newest entry; invalidating `id₀` touches only those that explicitly list `id₀` in `deps`. Renames are naturally respected. |
| **Recursive groups** | Detect SCCs during *initial* push; commit or reject the whole SCC as one LogEntry so you don’t split mutually-recursive decls. |
| **Concurrent pushes** | Because the log is append-only, race window is just the ETS updates. Guard with `:ets.update_counter/3` CAS or a `GenServer`. |
| **Disk persistence** | Periodically snapshot ETS → DETS or RocksDB and replay the log on boot (same as git’s reflog + fsck). |
| **“Checkout” an older state** | Keep a stack of *branch heads* (pointers into the log). Switching branches just swaps the head-id table, then lazy-loads compiled artifacts on demand. |

---

## 6  Why this scales

* **Hot-reload in < 1 ms**: most edits touch < 10 nodes, so the queue is tiny.  
* **Crash-safe**: worst-case you lose the last in-flight entry; the log is always valid.  
* **Audit-friendly**: you can diff two snapshots exactly like `git diff <id₁> <id₂>` for any type/function.  
* **GC-able**: prune unreachable ids (not on any branch) just like `git gc`.

---

### Next steps

1. Flesh out the LogEntry struct and write a thin persistence layer (ETS → RocksDB).  
2. Replace your current in-memory environment with lookups through the *head-id* table.  
3. Instrument with Telemetry to time “invalidate → revalidate” cycles under real workload traces.

That should give Nova the same robustness and time-travel ergonomics developers expect from git, while keeping the live, incremental nature that makes it great for AI-driven services.
