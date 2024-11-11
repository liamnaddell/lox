1. We need to move eval code out of parse.rs
2. we need to create an Environment structure to track variables (which implements inheritance and shadowing)

Problem: How to walk the tree...

BC needs to tree-walk in leaf-first order.
Eval needs to start at a Program, and eval in an on-demand basis.

We could solve this by creating a struct which implements a bunch of methods which take a self& pointer, and a specific AST node. To recurse, we call a self.recurse() function on a particular AST item. This recurse function is defined by the AstWalk trait, which is default-implemented to match ast { dispatch methods }.
