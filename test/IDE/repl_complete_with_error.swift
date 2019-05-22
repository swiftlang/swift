// While running completion on this, the typechecker produces a diagnostic and
// an AST with an error type. If the ASTVerifier ran on the AST with the error
// type, then it would fail. This test verifies that the ASTVerifier does not
// run on ASTs with error types produced by completion requests.

// RUN: %target-swift-ide-test -repl-code-completion -source-filename=%s

let bar: NotARealType = 0; ba
