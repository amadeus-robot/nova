
# Nova Compiler — Public API & Type Cheat‑Sheet  
*(concise reference for engineering prompts – 26 Apr 2025)*

---

## `Nova.Compiler.Tokenizer`

| Item | Description |
|------|-------------|
| `%Token{}` | `%Token{type :: atom, value :: String.t(), line :: pos_integer(), column :: pos_integer(), pos :: non_neg_integer()}` |
| `@keywords` | Reserved words ( `foreign`, `module`, …, `do` ). |
| `@operators` | Recognised operators ( `==`, `!=`, `<=`, `>=`, `->`, `::`, `++`, … ). |
| **`tokenize/1`** | `tokenize(source :: String.t()) :: [Token.t()]` – returns a flat list of tokens, including explicit `:newline` tokens for layout‑sensitive parsing. |

---

## `Nova.Compiler.Parser`

| Item | Description |
|------|-------------|
| **`parse/1`** | `parse(tokens) :: {:ok, Ast.Module.t() \| Ast.Declaration.t() \| Ast.Expression.t()} \| {:error, term}` – tries *module* → *declaration* → *expression* in order. |
| **`parse_declaration/1`** | Parses a single import/data/type/class/function declaration. |
| **`parse_expression/1`** | Parses any expression using full precedence & layout rules. |
| Key helpers | `parse_let_expression/1`, `parse_case_expression/1`, `expect_keyword/3`, `expect_operator/3`, `expect_delimiter/3` (handy in tests). |
| Returns | Structs from `Nova.Compiler.Ast` (`Module`, `FunctionDeclaration`, `LetBinding`, `BinaryOp`, `Identifier`, `Literal`, …). |

---

## `Nova.Compiler.CodeGen`

| Item | Description |
|------|-------------|
| **`compile/1`** | `compile(ast_module) :: String.t()` – turns a *complete* Nova `Ast.Module` into executable Elixir source (`defmodule`). |
| **`compile_expression/1`** | `compile_expression(expr) :: String.t()` – renders an arbitrary AST expression to Elixir (useful for REPL / tests). |
| Notes | First‑pass generator – supports literals, identifiers, binary ops, function calls, `if`, `let`, lambdas, `case`, lists, tuples, and foreign imports. Unsupported constructs raise. |

---

## `Nova.Compiler.Types`

| Item | Description |
|------|-------------|
| `%TVar{}` | Type variable struct – `%TVar{id :: integer, name :: atom}`. |
| `%TCon{}` | Concrete type constructor – `%TCon{name :: atom, args :: [t()]}`. |
| `t()` | Alias: `t :: TVar.t() | TCon.t()`. |
| Built‑ins | `t_int/0`, `t_bool/0`, `t_string/0`, `t_char/0`, `t_list/1`, `t_tuple/1`, `t_arrow/2`. |
| **Substitutions** (`Subst`) | `Subst.t()` is `%{TVar.id => t}`. Helpers: `lookup/2`, `single/2`, `compose/2`, `s_apply/2`. |
| **Type schemes** (`Scheme`) | `Scheme.new(vars, type)` – ∀‑quantified polymorphic type. |
| **Environment** (`Env`) | Immutable map of identifier → `Scheme`; also tracks a `counter` for fresh vars. Key fns: `empty/0`, `extend/3`, `lookup/2`, `fresh_var/2`. |

---

## `Nova.Compiler.Unify`

| Item | Description |
|------|-------------|
| **`unify/2`** | `unify(t1, t2) :: {:ok, Subst.t()} \| {:error, String.t()}` – pure algorithm that attempts to produce a substitution making `t1` and `t2` equal. Performs occurs‑check. |
| Private helpers | `occurs?/2`, `bind/3`, with `TVar`/`TCon` pattern‑matching. |

---

## `Nova.Compiler.TypeChecker`  *(algorithm W)*

| Item | Description |
|------|-------------|
| **`check_module/2`** | `check_module(ast_module, env \\ Env.empty()) :: {:ok, Env.t()} \| {:error, reason}` – walks declarations, updating the typing environment. |
| **`check_declaration/2`** | Handles function declarations & explicit signatures; integrates with `Env`. |
| **`infer_expression/2`** | Core expression inference with substitution threading; supports literals, identifiers, lambdas, let‑bindings, applications & selected binary ops. |
| Internal helpers | `infer_function/2`, `generalize_bind/4`, `instantiate/2`, `convert_type/1`. |
| Returns | Always `{ :ok, type, subst, env }` on success. |

---

### Abbreviated AST & Type Flow
```
┌──────────┐   tokenize    ┌──────────┐   parse     ┌────────────┐   infer/check   ┌─────────┐
│  Source  │──────────────▶│  Tokens  │────────────▶│    AST     │─────────────────▶│  Types  │
└──────────┘               └──────────┘             └────────────┘                  └─────────┘
          ▲                                   codegen │
          └────────────────────────────────────────────┘
```

Use this sheet while crafting engineering prompts or tests – it captures the **stable, public surface** of Nova’s tokenizer, parser, code generator, type core, unifier, and type‑checker.


