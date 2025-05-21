might want to force more unicode symbols
example → instead of ->
as the code isn't meant to be typed by humans anyways


APL, J, BQN, K - Core vocabulary is 1-glyph verbs/ado verbs (e.g. ⍴, ⌈/, ⍟).

Block (range)	Typical width	Example chars<sup>*</sup>	What they’re good for
Basic Latin U+0020–007E	1	`+-*/=<>&	^~!?`
Latin-1 Supplement U+00A0–00FF	1	± ÷ µ º ¬ « »	Familiar math & punctuation that compile almost everywhere.
Greek & Coptic U+0370–03FF	1	α β γ Δ Σ λ π	Variable names in math-heavy code (λ for lambdas, π for pi).
Superscripts & Subscripts U+2070–209F	1	⁰ ¹ ² ₀ ₁ ₂	Pretty-printed indices/exponents inside test data or DSLs.
General Punctuation U+2000–206F	1	“ ” ‘ ’ - … ‖	Typographic quotes, figure dashes, ellipsis, parallel bars.
Mathematical Operators U+2200–22FF	1	∀ ∃ ∑ ∏ ∩ ∪ ∘ ⊗ ⊕ ⇒ ⇔	Set, logic & algebraic operators; widely used in theorem provers.
Supplemental Math Operators U+2A00–2AFF	1–2	⟂ ⫽ ⨂ ⨯ ⩽ ⩾	Extra set & lattice operators (Agda, Lean, Coq, etc.).
Arrows U+2190–21FF	1	← → ↦ ⇒ ⇐ ↔ ⟹	Function types, monad fish ⟼, rewrite arrows.
