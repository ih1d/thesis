# CCOM 6990 - Independent Research

Goal: Research on computational/executional models, runtime environments, etc. for Machine Learning (ML) libraries.

## Reports

### Week: September 29, 2025 to October 3, 2025

* Read _On Machine Learning and Programming Languages_ and annotate
* Read _A Computational Model for TensorFlow_ and annotate
* Read _Exploiting Vector_ and annotate

## Conclusion

{(b) A Differentiable Core Language

Design a λ-calculus with first-class differentiation (automatic differentiation).

Implement forward/reverse mode AD in Haskell, but tied to your language syntax, not just Haskell’s AD.

Compare: “what if differentiation were a language primitive instead of a library trick?”

Tool: A small VM/interpreter for this differentiable calculus.

(c) An ML Graph Optimizer

Parse TensorFlow/ONNX/MLIR graphs into your IR.

Implement equational reasoning in Haskell (rewrite rules, algebraic laws).

Example: (x*1) → x, relu(relu(x)) → relu(x), fusing linear ops, etc.

Tool: a graph rewriting engine in Haskell that optimizes ML models declaratively.

This ties nicely to your interest in semantics: the rewrite rules are semantics-preserving transformations.}
