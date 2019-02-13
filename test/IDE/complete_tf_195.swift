// https://bugs.swift.org/browse/TF-195: repl completer crash while defining
// struct

// The ASTVerifier doesn't like this AST.
// XFAIL: swift_ast_verifier

// RUN: %target-swift-ide-test -repl-code-completion -source-filename=%s
struct Foo { var ba
