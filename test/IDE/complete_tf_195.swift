// https://bugs.swift.org/browse/TF-195: repl completer crash while defining
// struct.

// TODO(TF-195): Re-enable after swift-DEVELOPMENT-SNAPSHOT-2019-05-15-a merge.
// XFAIL: *

// The ASTVerifier doesn't like this AST.
// XFAIL: swift_ast_verifier

// RUN: %target-swift-ide-test -repl-code-completion -source-filename=%s
struct Foo { var ba
