// https://bugs.swift.org/browse/TF-194: invalid unary ops crash repl completer

// The ASTVerifier doesn't like this AST.
// XFAIL: swift_ast_verifier

// RUN: %target-swift-ide-test -repl-code-completion -source-filename=%s
%invalidunary le
